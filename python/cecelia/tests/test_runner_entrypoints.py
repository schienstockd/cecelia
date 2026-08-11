"""Every task runner must actually RUN when `run_py` executes it.

`run_py` launches a runner as a subprocess — `python <script> --params <file>` — so a runner that
defines `run(params)` but has no `if __name__ == '__main__'` block is executed, defines a function,
and **exits 0 having done nothing**. `run_py` checks the exit code, sees success, and the task
reports that its output is missing rather than that it never ran.

That shipped in the OME-TIFF export. It survived a green suite because the runner test imported the
module and called `run(params)` DIRECTLY, which is the one path production never takes — so the
entry point was the only untested line in the file.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import ast
import unittest
from pathlib import Path

_TASKS = Path(__file__).resolve().parents[3] / 'app' / 'src' / 'tasks'


def _runners():
    return sorted(_TASKS.rglob('*_run.py')) if _TASKS.is_dir() else []


@unittest.skipUnless(_TASKS.is_dir(), f'task tree not present at {_TASKS}')
class RunnerEntrypointTest(unittest.TestCase):

    def test_the_scan_finds_runners(self):
        """A path typo that matched nothing would make every assertion below vacuous."""
        self.assertGreaterEqual(len(_runners()), 5)

    def test_every_runner_has_a_main_guard(self):
        for path in _runners():
            with self.subTest(runner=path.name):
                tree = ast.parse(path.read_text(encoding='utf-8'))
                guards = [n for n in tree.body
                          if isinstance(n, ast.If)
                          and any(isinstance(c, ast.Name) and c.id == '__name__'
                                  for c in ast.walk(n.test))]
                self.assertTrue(guards,
                                f'{path.name} has no `if __name__ == "__main__"` block, so run_py '
                                f'would execute it, define its functions and exit 0 without running '
                                f'anything — a silent no-op the exit code cannot reveal.')

    def test_every_runner_defines_run_and_reaches_it_from_main(self):
        for path in _runners():
            with self.subTest(runner=path.name):
                src = path.read_text(encoding='utf-8')
                tree = ast.parse(src)
                names = {n.name for n in tree.body if isinstance(n, ast.FunctionDef)}
                self.assertIn('run', names, f'{path.name} defines no run(params)')
                # The guard has to lead to run() — a main() that only parses params is the same
                # silent no-op with more code in the way.
                called = {n.func.id for n in ast.walk(tree)
                          if isinstance(n, ast.Call) and isinstance(n.func, ast.Name)}
                self.assertTrue({'run', 'main'} & called,
                                f'{path.name} never calls run() or main()')
