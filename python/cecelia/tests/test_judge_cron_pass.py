"""Tests for `scripts/judge/cron_pass.sh` — the timer's wrapper, and its wait-out-the-limit loop.

Design: docs/ai-assist/WEEKLY_JUDGE.md. `pixi` and the sleep are stubs on a temp PATH: no pass runs,
nothing waits. The stub `pixi` plays one exit per call from `PLAN` (`75r` = usage limit, retry;
`75n` = usage limit, no retry; a number = that exit) and writes `judge-ratelimit.json` like weekly.py.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]
_SCRIPT = _REPO / "scripts" / "judge" / "cron_pass.sh"

_PIXI = r"""#!/usr/bin/env bash
n=$(( $(wc -l < "$STUB_DIR/calls" 2>/dev/null || echo 0) + 1 ))
echo "$JUDGE_RETRY_LEFT $*" >> "$STUB_DIR/calls"
step=$(echo "$PLAN" | cut -d, -f"$n")
case "$step" in
    75r|75n)
        retry=false; [ "$step" = 75r ] && retry=true
        printf '{"reset": "%s", "message": "resets", "retry": %s, "stage": "bugs"}\n' \
            "$(date -Iseconds -d '+10 min')" "$retry" > "$STATE_DIR/judge-ratelimit.json"
        exit 75 ;;
    *) exit "$step" ;;
esac
"""
_SLEEP = '#!/usr/bin/env bash\necho "$1" >> "$STUB_DIR/sleeps"\n'


@unittest.skipUnless(sys.platform.startswith("linux") and all(shutil.which(t) for t in ("flock", "ionice", "bash")),
                     "the wrapper runs under systemd on Linux (GNU date, flock, ionice)")
class CronPassTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)
        self.bin = self.tmp / "bin"
        self.bin.mkdir()
        for name, text in (("pixi", _PIXI), ("fake-sleep", _SLEEP)):
            (self.bin / name).write_text(text, encoding="utf-8")
            (self.bin / name).chmod(0o755)
        (self.tmp / "state").mkdir()

    def run_wrapper(self, plan: str) -> subprocess.CompletedProcess:
        env = {"PATH": f"{self.bin}{os.pathsep}/usr/bin{os.pathsep}/bin", "HOME": str(self.tmp), "PLAN": plan,
               "STUB_DIR": str(self.tmp), "STATE_DIR": str(self.tmp / "state"),
               "CECELIA_EFFECTIVENESS_LOG": str(self.tmp / "state" / "events.jsonl"),
               "CECELIA_EVAL_CRON_LOG_DIR": str(self.tmp / "cron"), "CECELIA_JUDGE_SLEEP": "fake-sleep"}
        return subprocess.run(["bash", str(_SCRIPT), "--pinned"], env=env, capture_output=True, text=True,
                              encoding="utf-8", timeout=60, check=False)

    def lines(self, name: str) -> list[str]:
        path = self.tmp / name
        return path.read_text(encoding="utf-8").splitlines() if path.is_file() else []

    def test_the_script_parses(self):
        subprocess.run(["bash", "-n", str(_SCRIPT)], check=True)

    def test_a_clean_pass_runs_once(self):
        self.assertEqual(self.run_wrapper("0").returncode, 0)
        self.assertEqual(self.lines("calls"), ["2 run judge-weekly --ref HEAD"])
        self.assertEqual(self.lines("sleeps"), [])

    def test_a_usage_limit_waits_past_the_reset_and_reruns_with_fewer_retries_left(self):
        proc = self.run_wrapper("75r,75r,0")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        # every attempt pins the same commit: `--ref HEAD`; the last has no retry left
        self.assertEqual(self.lines("calls"), [f"{n} run judge-weekly --ref HEAD" for n in (2, 1, 0)])
        waits = [int(s) for s in self.lines("sleeps")]
        self.assertEqual(len(waits), 2)
        for w in waits:   # reset 10 min off + 5 min after it
            self.assertTrue(14 * 60 <= w <= 15 * 60 + 5, w)
        log = "".join(p.read_text(encoding="utf-8") for p in (self.tmp / "cron").glob("judge-*.log"))
        self.assertIn("then attempt 2/3 (lock held)", log)
        self.assertIn("(attempt 3/3)", log)

    def test_no_retry_stops_with_the_limit_exit(self):
        self.assertEqual(self.run_wrapper("75n").returncode, 75)
        self.assertEqual((len(self.lines("calls")), self.lines("sleeps")), (1, []))

    def test_three_attempts_at_most(self):
        # weekly.py says no retry on the last attempt; even if it didn't, the wrapper stops
        self.assertEqual(self.run_wrapper("75r,75r,75r,0").returncode, 75)
        self.assertEqual((len(self.lines("calls")), len(self.lines("sleeps"))), (3, 2))

    def test_another_failure_is_not_retried(self):
        self.assertEqual(self.run_wrapper("1").returncode, 1)
        self.assertEqual(len(self.lines("calls")), 1)


if __name__ == "__main__":
    unittest.main()
