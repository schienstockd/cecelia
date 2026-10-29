"""The headless launcher (``headless.py``, docs/todo/HEADLESS_MODE_PLAN.md P3): the project picker,
the env `claude` runs under, and that Ctrl-C reaches Claude rather than the launcher.

The session itself (argv, MCP config, profile env) is built server-side and pinned by the Julia
testset (`app/test/suite/observer.jl`); this covers only what the launcher decides.
"""
import importlib.util
import pathlib
import signal
import sys
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[3]


def _load():
    sys.path.insert(0, str(ROOT))          # headless.py imports app.py, its sibling
    try:
        spec = importlib.util.spec_from_file_location("cecelia_headless_launcher", ROOT / "headless.py")
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        return mod
    finally:
        sys.path.remove(str(ROOT))


class PickProjectTest(unittest.TestCase):
    PROJECTS = [{"uid": "aaa111", "name": "Newest"}, {"uid": "bbb222", "name": "Older"}]

    def setUp(self):
        self.h = _load()

    def _pick(self, answers, projects=PROJECTS):
        it = iter(answers)
        said = []
        got = self.h.pick_project(projects, ask=lambda _: next(it), say=said.append)
        return got, said

    def test_enter_takes_the_most_recently_opened(self):
        self.assertEqual(self._pick([""])[0], "aaa111")

    def test_new_project_is_zero_and_listed_first(self):
        got, said = self._pick(["0"])
        self.assertEqual(got, "")
        self.assertEqual(said[1], "  0) new project")
        self.assertIn("Newest (aaa111)", said[2])

    def test_out_of_range_asks_again_and_q_quits(self):
        got, said = self._pick(["7", "x", "2"])
        self.assertEqual(got, "bbb222")
        self.assertEqual(sum("pick 0–2" in s for s in said), 2)
        self.assertIsNone(self._pick(["q"])[0])

    def test_no_projects_defaults_to_new(self):
        self.assertEqual(self._pick([""], projects=[])[0], "")


class ClaudeEnvTest(unittest.TestCase):
    def test_unset_removed_and_set_applied(self):
        h = _load()
        env = h.claude_env({"PATH": "/bin", "ANTHROPIC_API_KEY": "stray", "HOME": "/h"},
                           {"set": {"CLAUDE_CONFIG_DIR": "/p/ana", "PATH": "/cli:/bin"},
                            "unset": ["ANTHROPIC_API_KEY"]})
        self.assertEqual(env, {"PATH": "/cli:/bin", "HOME": "/h", "CLAUDE_CONFIG_DIR": "/p/ana"})


class RunClaudeTest(unittest.TestCase):
    def test_ctrl_c_is_swallowed_meanwhile_and_restored_after(self):
        # A handler (not SIG_IGN, which survives exec): the launcher must not die on Claude's Ctrl-C,
        # and the child must still get the default behaviour.
        h = _load()
        before = signal.getsignal(signal.SIGINT)
        seen = []

        class FakeProc:
            def __init__(self, argv, env):
                seen.append(signal.getsignal(signal.SIGINT))

            def wait(self):
                return 3

        real = h.subprocess.Popen
        h.subprocess.Popen = FakeProc
        try:
            self.assertEqual(h.run_claude(["claude"], {}), 3)
        finally:
            h.subprocess.Popen = real
        self.assertTrue(callable(seen[0]))
        self.assertIsNot(seen[0], signal.SIG_IGN)
        self.assertIs(signal.getsignal(signal.SIGINT), before)


if __name__ == "__main__":
    unittest.main()
