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
# `git` fails `fetch` as often as $FETCH_FAILS says, then succeeds; every call is recorded
_GIT = r"""#!/usr/bin/env bash
echo "${*:3}" >> "$STUB_DIR/git"
if [ "$3" = fetch ] && [ "$(grep -c '^fetch' "$STUB_DIR/git")" -le "${FETCH_FAILS:-0}" ]; then exit 1; fi
"""


@unittest.skipUnless(sys.platform.startswith("linux") and all(shutil.which(t) for t in ("flock", "ionice", "bash")),
                     "the wrapper runs under systemd on Linux (GNU date, flock, ionice)")
class CronPassTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)
        self.bin = self.tmp / "bin"
        self.bin.mkdir()
        for name, text in (("pixi", _PIXI), ("fake-sleep", _SLEEP), ("git", _GIT)):
            (self.bin / name).write_text(text, encoding="utf-8")
            (self.bin / name).chmod(0o755)
        (self.tmp / "state").mkdir()

    def run_wrapper(self, plan: str, mode: str = "--pinned", **env_extra: str) -> subprocess.CompletedProcess:
        env = {"PATH": f"{self.bin}{os.pathsep}/usr/bin{os.pathsep}/bin", "HOME": str(self.tmp), "PLAN": plan,
               "STUB_DIR": str(self.tmp), "STATE_DIR": str(self.tmp / "state"),
               "CECELIA_EFFECTIVENESS_LOG": str(self.tmp / "state" / "events.jsonl"),
               "CECELIA_EVAL_CRON_LOG_DIR": str(self.tmp / "cron"), "CECELIA_JUDGE_SLEEP": "fake-sleep", **env_extra}
        return subprocess.run(["bash", str(_SCRIPT), mode], env=env, capture_output=True, text=True,
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

    _RESET = ["checkout --quiet --detach", "reset --quiet --hard origin/main"]

    def test_sync_resets_to_origin_main_retrying_the_fetch_and_runs_nothing(self):
        proc = self.run_wrapper("0", "--sync", FETCH_FAILS="2")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual(self.lines("git"), ["fetch --quiet origin"] * 3 + self._RESET)
        self.assertEqual((self.lines("sleeps"), self.lines("calls")), (["30", "30"], []))

    def test_chart_resets_then_pushes_the_chart(self):
        proc = self.run_wrapper("0", "--chart")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual(self.lines("git"), ["fetch --quiet origin", *self._RESET])
        self.assertEqual(self.lines("calls"), [" run effectiveness-chart -- --push"])   # no JUDGE_RETRY_LEFT
        self.assertEqual(self.run_wrapper("1", "--chart").returncode, 1)   # a failed push shows red

    def test_auto_works_the_fix_bugs_without_touching_the_checkout(self):
        proc = self.run_wrapper("0", "--auto")
        self.assertEqual(proc.returncode, 0, proc.stderr)
        self.assertEqual((self.lines("calls"), self.lines("git")), ([" run judge-review --auto"], []))
        self.assertTrue(list((self.tmp / "cron").glob("auto-*.log")))

    def test_auto_on_the_usage_limit_exits_75_and_says_the_rest_wait(self):
        proc = self.run_wrapper("75", "--auto")
        self.assertEqual((proc.returncode, len(self.lines("calls")), self.lines("sleeps")), (75, 1, []))
        self.assertIn("the rest wait for next week", proc.stderr)

    def test_chart_and_sync_skip_while_a_pass_holds_the_lock(self):
        import fcntl   # POSIX only; the class runs on Linux alone
        (self.tmp / "cron").mkdir()
        with open(self.tmp / "cron" / ".lock", "wb") as held:
            fcntl.flock(held, fcntl.LOCK_EX)   # a pass's lock: flock(1) locks the same way
            for mode in ("--chart", "--sync", "--auto"):
                self.assertEqual(self.run_wrapper("0", mode).returncode, 0)
        self.assertEqual((self.lines("git"), self.lines("calls")), ([], []))


if __name__ == "__main__":
    unittest.main()
