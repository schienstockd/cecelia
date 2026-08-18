"""A task must not die on a log line.

Every task log line is a stdout write that Julia reads as UTF-8, but a Python child on Windows gets a
cp1252 stdout by default, where one non-ASCII character raises UnicodeEncodeError. That propagated out
of `log()` and killed the task: an import that had already read its file and written its output died on
the line announcing it, surfacing only as "[ERROR] Track import failed".

It is not one task's problem. Three log lines in the repo carry a character cp1252 cannot encode, and
two of them are SHIPPED tasks on a normal success path: `cell_contacts_mesh_run` logs its contact
summary with a `\u2264`, and `branching_run` logs integrated-time labels with a `\u2192`. Note the
characters that matter are narrower than "non-ASCII" — cp1252 encodes the em dash and \u00b5 fine, so
most of the repo's typography is safe and only the arrows, \u2264/\u2265 and box-drawing rules are not.
Fixed once at the sink rather than by policing characters at every call site.
"""
import io
import unittest

from cecelia.utils.script_utils import StdoutLogger, _utf8_stdio


class CharmapStream(io.TextIOBase):
    """Stands in for a Windows cp1252 stdout: anything outside the codec raises, as it really does."""

    def __init__(self):
        self.written = []
        self.reconfigured = False

    def write(self, s):
        if not self.reconfigured:
            s.encode('cp1252')          # raises UnicodeEncodeError, exactly like the real stream
        self.written.append(s)
        return len(s)

    def reconfigure(self, encoding=None, errors=None):
        self.reconfigured = (encoding == 'utf-8')

    def flush(self):
        pass


class TestScriptLogging(unittest.TestCase):
    def test_reconfigures_stdio_to_utf8(self):
        import sys
        stdout, stderr = sys.stdout, sys.stderr
        sys.stdout, sys.stderr = CharmapStream(), CharmapStream()
        try:
            _utf8_stdio()
            self.assertTrue(sys.stdout.reconfigured)
            self.assertTrue(sys.stderr.reconfigured)     # a traceback is non-ASCII just as often
        finally:
            sys.stdout, sys.stderr = stdout, stderr

    def test_non_ascii_line_does_not_raise(self):
        """The three characters that actually crash a real log line: an arrow, \u2264, a box rule."""
        import sys
        stdout = sys.stdout
        sys.stdout = CharmapStream()
        sys.stdout.reconfigure(encoding='utf-8')
        try:
            StdoutLogger().log('[INFO] Wrote 4367 points in 314 tracks → tm.h5ad')
            StdoutLogger().log('>> contactsMeshes: 3/9 A cells contact B (≤5.0)')
            StdoutLogger().log('# ── done ──')
            self.assertEqual(len(sys.stdout.written), 6)   # each print writes text then a newline
        finally:
            sys.stdout = stdout

    def test_survives_a_stream_that_cannot_be_reconfigured(self):
        """Belt and braces: reconfigure is not always possible, and a '?' beats a dead task."""
        import sys
        stdout = sys.stdout
        sys.stdout = CharmapStream()        # never reconfigured — every non-ASCII write raises
        try:
            StdoutLogger().log('integrated time → labels')
            self.assertIn('integrated time ? labels', ''.join(sys.stdout.written))
        finally:
            sys.stdout = stdout


if __name__ == '__main__':
    unittest.main()
