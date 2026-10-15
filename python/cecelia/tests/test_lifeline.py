"""`cecelia.utils.lifeline`: a backend-launched worker exits once its stdin (the lifeline) hits EOF.

The whole chain — a real launcher SIGKILLed, its lifeline children gone — is `app/test/suite/logging.jl`
("a lifeline child exits when its launcher dies"); this pins the Python half on its own.
"""
import os
import pathlib
import subprocess
import sys
import unittest

from cecelia.utils import lifeline

_CHILD = ("import os, time; from cecelia.utils import lifeline; "
          "print(lifeline.exit_with_parent(), lifeline.LIFELINE_ENV in os.environ, flush=True); time.sleep(60)")
_REPO = pathlib.Path(__file__).resolve().parents[3]


def _spawn(flag: bool) -> subprocess.Popen:
    env = {k: v for k, v in os.environ.items() if k != lifeline.LIFELINE_ENV}
    if flag:
        env[lifeline.LIFELINE_ENV] = "1"
    env["PYTHONPATH"] = str(_REPO / "python")
    return subprocess.Popen([sys.executable, "-c", _CHILD], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            env=env, text=True, encoding="utf-8")


class LifelineTest(unittest.TestCase):
    def test_exits_when_the_launchers_end_closes(self):
        p = _spawn(True)
        try:
            # watching — and the flag is gone, so a child it spawns does not inherit it pipe-less
            self.assertEqual(p.stdout.readline().strip(), "True False")
            p.stdin.close()                      # what the OS does when the launcher dies
            self.assertEqual(p.wait(timeout=10), 0)
        finally:
            p.kill()

    def test_a_standalone_run_is_not_watched(self):
        p = _spawn(False)
        try:
            self.assertEqual(p.stdout.readline().strip(), "False False")
            p.stdin.close()                      # a terminal's stdin closing must not end it
            with self.assertRaises(subprocess.TimeoutExpired):
                p.wait(timeout=2)
        finally:
            p.kill()
            p.wait()

    def test_same_env_name_as_julia(self):
        src = (_REPO / "app" / "src" / "lifeline.jl").read_text(encoding="utf-8")
        self.assertIn(f'const LIFELINE_ENV = "{lifeline.LIFELINE_ENV}"', src)


if __name__ == "__main__":
    unittest.main()
