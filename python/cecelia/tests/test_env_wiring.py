"""Is the `cecelia` you IMPORT the one in this checkout?

Not a test of the code — a test of the environment the code runs in. It exists because the answer was
once no, silently, for hours.

`pixi.toml` installs the package as an editable path dep (`cecelia = { path = "python" }`), which
setuptools implements as a **meta-path finder** holding one ABSOLUTE path. In a git worktree that path
can point at a DIFFERENT checkout — observed here: an `af-followup` worktree whose env resolved
`cecelia` to `cecelia-feijoa/python`, because the uv cache key derives from the relative `./python` and
the first checkout to build it wins.

Nothing errors when that happens. Half the app runs this checkout's files (anything launched BY PATH —
the napari bridge, the preview worker, task runners) and the other half imports another checkout's
library. They only diverge once one side gains something the other lacks, and then the failure names a
missing attribute rather than the split that caused it:

    AttributeError: module 'cecelia.utils.correction_utils' has no attribute 'af_derived_values'

…raised by a worker whose own file plainly had the caller. The launchers now pin `PYTHONPATH`
(`run_py`, `preview.jl`, `napari.jl`), which makes the APP immune — but `pixi run test-py`, a REPL, and
anything else importing `cecelia` by name are not, so the condition still needs to be visible.

Fix when this fails: reinstall the editable package into THIS worktree's env, e.g.
``pixi run python -m pip install -e python --no-deps --no-build-isolation``, or rebuild the env.
"""
import os
import unittest
from pathlib import Path

import cecelia


class EnvironmentWiringTest(unittest.TestCase):
    def test_imported_cecelia_belongs_to_this_checkout(self):
        # this file: <repo>/python/cecelia/tests/test_env_wiring.py → <repo>/python/cecelia
        expected = Path(__file__).resolve().parent.parent
        actual = Path(cecelia.__file__).resolve().parent

        self.assertEqual(
            actual, expected,
            '\n\n`import cecelia` resolves OUTSIDE this checkout — the env is wired to another one.\n'
            f'  this checkout : {expected}\n'
            f'  imported from : {actual}\n'
            'Code launched by PATH (napari bridge, preview worker, task runners) would run this\n'
            "checkout's files while importing that one's library. See this module's docstring.")

    def test_the_check_would_actually_notice(self):
        """Guard the guard: a path comparison that normalises everything away proves nothing."""
        here = Path(__file__).resolve().parent.parent
        elsewhere = Path(os.sep) / 'somewhere' / 'else' / 'python' / 'cecelia'
        self.assertNotEqual(here, elsewhere)


if __name__ == '__main__':
    unittest.main()
