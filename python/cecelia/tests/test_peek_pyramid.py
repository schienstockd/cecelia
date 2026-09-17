"""Unit tests for the pre-import pyramid-levels advisor's peek runner.

Covers the pure recommendation rule and the JVM (`showinf`) fallback's parse + gating. The
fast-reader path is exercised end-to-end through the HTTP route in the frontend tests
(pyramidPeek.test.ts) and doesn't need a repeat here."""
import importlib
import os
import sys
import unittest
from unittest.mock import patch

# The runner lives under `app/src/tasks/…`, which isn't a Python package. Import it by loading the
# module from disk — the same shape `test_centroid_migrate.py` uses for `centroid_migrate_run.py`.
_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
_RUNNER = os.path.join(_REPO, 'app', 'src', 'tasks', 'importImages', 'peek_pyramid_run.py')

_spec = importlib.util.spec_from_file_location('peek_pyramid_run', _RUNNER)
peek = importlib.util.module_from_spec(_spec)
sys.modules['peek_pyramid_run'] = peek
_spec.loader.exec_module(peek)


class RecommendationRuleTest(unittest.TestCase):
    """The rule ships pre-import knowledge that post-import QC can't have — timelapses want a
    smaller deepest level so each frame is cheap to fetch. Pinned against VJy1Nx (1057×1111×31×181)
    to keep the tiering from drifting under refactors."""

    def test_still_image_lands_at_one_chunk_on_the_long_side(self):
        # A still whose long side already fits the chunk needs just N=1 (level 0 is the full-res).
        self.assertEqual(peek.recommend_levels(800, 600, chunk=1024), 1)
        # A still overshoots by ~9% ⇒ N=2 (one downsample fits it). This is what a stills-only
        # user gets for a 1057×1111 still — matches qc.jl::pyramid_layout.
        self.assertEqual(peek.recommend_levels(1057, 1111, chunk=1024), 2)

    def test_timelapse_picks_the_playback_target_not_the_still_target(self):
        # T>1 ⇒ TARGET=256. 1111 / 256 = 4.34 → ceil(log2)=3, +1 for level 0 → 4 levels.
        # The measured-good answer for VJy1Nx; N=5 was over-eager (~69×66×31 per frame).
        chunk = peek.target_for_shape(nz=31, nt=181)
        self.assertEqual(chunk, peek.PLAYBACK_TARGET)
        self.assertEqual(peek.recommend_levels(1057, 1111, chunk=chunk), 4)

    def test_a_target_change_is_a_deliberate_call_not_a_silent_one(self):
        # Guardrail: if the tiering constants get retuned, this fails loudly rather than the
        # recommendation drifting for every user under a refactor.
        self.assertEqual(peek.DEFAULT_CHUNK, 1024)
        self.assertEqual(peek.PLAYBACK_TARGET, 256)


class JvmFallbackTest(unittest.TestCase):
    """The showinf shell-out and OME-XML parse. Mocks subprocess so no JVM is required."""

    _OMEXML = (
        'Reading OME metadata\n'
        '<?xml version="1.0" encoding="UTF-8"?>\n'
        '<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">\n'
        '  <Image ID="Image:0">\n'
        '    <Pixels ID="Pixels:0" DimensionOrder="XYCZT" Type="uint16"\n'
        '            SizeX="2048" SizeY="1536" SizeZ="7" SizeT="24" SizeC="3"/>\n'
        '  </Image>\n'
        '</OME>\n'
    )

    def test_parses_dims_from_a_real_ome_xml_banner_and_all(self):
        # showinf prints a banner before the XML; the parser has to skip past it. Real dims come
        # from a small OME-XML the test provides directly.
        class _R:
            def __init__(self, stdout): self.returncode, self.stdout, self.stderr = 0, stdout, ''
        with patch('subprocess.run', return_value=_R(self._OMEXML)):
            self.assertEqual(peek._peek_showinf('/x.czi', '/opt/bftools/showinf'),
                             (2048, 1536, 7, 24, 3))

    def test_a_showinf_error_surfaces_as_an_exception_the_caller_catches(self):
        # peek_one wraps this in try/except so the whole run doesn't die on one broken file; the
        # important property here is that a non-zero exit does NOT get parsed as if it were XML.
        class _R:
            def __init__(self): self.returncode, self.stdout, self.stderr = 1, '', 'file not readable'
        with patch('subprocess.run', return_value=_R()):
            with self.assertRaises(RuntimeError):
                peek._peek_showinf('/x.czi', '/opt/bftools/showinf')

    def test_jvm_path_only_fires_for_jvm_eligible_extensions(self):
        # peek_one routes to showinf ONLY when the extension is in _JVM_EXTS AND a showinfBin is
        # given. Any other unknown extension stays `unsupported` — otherwise the fallback would
        # spin the JVM on random files the user drops in.
        with patch('subprocess.run') as m:
            r = peek.peek_one('/x.random', showinf_bin='/opt/bftools/showinf')
            self.assertEqual(r['reader'], 'unsupported')
            m.assert_not_called()

    def test_no_showinf_bin_means_jvm_formats_come_back_unsupported_silently(self):
        # An install without bftools shouldn't crash the peek route; JVM-only formats simply skip
        # the recommendation the same way an unsupported suffix does today.
        with patch('subprocess.run') as m:
            r = peek.peek_one('/x.czi', showinf_bin=None)
            self.assertEqual(r['reader'], 'unsupported')
            m.assert_not_called()

    def test_settled_jvm_peek_carries_the_recommendation_the_advisor_reads(self):
        # End-to-end shape: a CZI + a showinfBin ⇒ reader='showinf' + a recommendedPyramidLevels
        # the advisor can render. Timelapse (T>1) tier so we're on the playback target.
        class _R:
            def __init__(self, stdout): self.returncode, self.stdout, self.stderr = 0, stdout, ''
        with patch('subprocess.run', return_value=_R(self._OMEXML)):
            r = peek.peek_one('/x.czi', showinf_bin='/opt/bftools/showinf')
        self.assertEqual(r['reader'], 'showinf')
        self.assertEqual(r['nX'], 2048)
        self.assertEqual(r['nT'], 24)
        # 2048 / 256 = 8 → ceil(log2)=3, +1 → 4 levels for playback.
        self.assertEqual(r['recommendedPyramidLevels'], 4)
        self.assertEqual(r['targetChunk'], peek.PLAYBACK_TARGET)


if __name__ == '__main__':
    unittest.main()
