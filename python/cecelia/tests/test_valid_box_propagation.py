"""A derived store must make a DECISION about the valid box — carry it, write its own, or say why not.

The box records which part of a canvas is data (`docs/ARCHITECTURE.md` → *The valid box*). Drift
correction writes it; everything downstream inherits a canvas that is 3–56% padding on real movies
here. But a box only helps if it SURVIVES the pipeline, and it did not: `af_correct` and
`cellpose_correct` dropped it silently, and `smooth` carried it through `read_valid_box(path)`, which
on a per-frame box returns the UNION over frames — nearly the whole canvas once the window drifts.
So the store people actually segment reported "all valid" and there was nothing to skip.

Silence is the failure mode, so silence is what this forbids: a runner that creates a derived store
either calls `carry_valid_box`/`write_valid_box`, or carries a `VALID-BOX-EXEMPT:` note saying why
not. Absent is always SAFE (a consumer reads None as "all valid" and merely does more work) — it is
just never something to arrive at by accident.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import re
import unittest
from pathlib import Path

_TASKS = Path(__file__).resolve().parents[3] / 'app' / 'src' / 'tasks'

# A runner that opens a staged store is writing a derived store; that is the moment the decision is due.
_DERIVED = re.compile(r'staged_store\s*\(')
_DECIDED = re.compile(r'carry_valid_box|write_valid_box|VALID-BOX-EXEMPT:')


def _runners():
    return sorted(_TASKS.rglob('*_run.py')) if _TASKS.is_dir() else []


@unittest.skipUnless(_TASKS.is_dir(), f'task tree not present at {_TASKS}')
class ValidBoxPropagationTest(unittest.TestCase):

    def test_the_scan_finds_derived_store_writers(self):
        """A pattern that matched nothing would make the assertion below vacuous."""
        writers = [p for p in _runners() if _DERIVED.search(p.read_text(encoding='utf-8'))]
        self.assertGreaterEqual(len(writers), 4)

    def test_every_derived_store_writer_decides_about_the_box(self):
        for path in _runners():
            src = path.read_text(encoding='utf-8')
            if not _DERIVED.search(src):
                continue
            with self.subTest(runner=path.name):
                self.assertTrue(
                    _DECIDED.search(src),
                    f'{path.name} writes a derived store but never mentions the valid box. Call '
                    f'zarr_utils.carry_valid_box(src, staging) if it does not move pixels, or '
                    f'write_valid_box if it computes its own, or add a "VALID-BOX-EXEMPT: <why>" '
                    f'comment. Dropping it silently is what made segmentation process padding.')

    def test_nobody_carries_the_box_by_summarising_it(self):
        """`read_valid_box(path)` + `write_valid_box` collapses per-frame boxes to their union."""
        for path in _runners():
            src = path.read_text(encoding='utf-8')
            if 'write_valid_box' not in src:
                continue
            with self.subTest(runner=path.name):
                # Reading a box WITHOUT a timepoint and writing it back is the union bug. A producer
                # computing its own box (drift) never reads one first.
                union = re.search(r'read_valid_box\s*\(\s*[^)]*\)', src)
                if union and 'timepoint' not in union.group(0):
                    self.fail(f'{path.name} reads a box without a timepoint and writes it back — '
                              f'that is the UNION over frames, not the geometry. Use carry_valid_box.')
