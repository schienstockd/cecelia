"""Tests for `scripts/agent_eval/cron_night.sh` — the nightly wrapper, and what it does on the usage limit.

Design: docs/todo/AGENT_OVERNIGHT_PLAN.md. `pixi` is a stub on a temp PATH: no agent runs. It plays one
exit per brief from `PLAN` (75 = the run hit the usage limit) and writes the run's record like
`run_overnight.py` does.

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
_SCRIPT = _REPO / "scripts" / "agent_eval" / "cron_night.sh"

_PIXI = r"""#!/usr/bin/env bash
n=$(( $(wc -l < "$STUB_DIR/calls" 2>/dev/null || echo 0) + 1 ))
echo "$*" >> "$STUB_DIR/calls"
root=""; while [ $# -gt 0 ]; do [ "$1" = --root ] && root="$2"; shift; done
step=$(echo "$PLAN" | cut -d, -f"$n")
if [ "$step" = 0 ] || [ "$step" = 75 ]; then
    mkdir -p "$root"; echo '{}' > "$root/record.json"; echo "# run" > "$root/record.md"
fi
exit "$step"
"""


@unittest.skipUnless(sys.platform.startswith("linux") and all(shutil.which(t) for t in ("flock", "ionice", "bash")),
                     "the wrapper runs under systemd on Linux (flock, ionice)")
class CronNightTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)
        (self.tmp / "bin").mkdir()
        (self.tmp / "bin" / "pixi").write_text(_PIXI, encoding="utf-8")
        (self.tmp / "bin" / "pixi").chmod(0o755)

    def run_wrapper(self, plan: str) -> subprocess.CompletedProcess:
        env = {"PATH": f"{self.tmp / 'bin'}{os.pathsep}/usr/bin{os.pathsep}/bin", "HOME": str(self.tmp),
               "PLAN": plan, "STUB_DIR": str(self.tmp),
               "CECELIA_EFFECTIVENESS_LOG": str(self.tmp / "eff" / "events.jsonl"),
               "CECELIA_EVAL_CRON_LOG_DIR": str(self.tmp / "cron"),
               "CECELIA_AGENT_NIGHT_ROOT": str(self.tmp / "runs")}
        return subprocess.run(["bash", str(_SCRIPT)], env=env, capture_output=True, text=True,
                              encoding="utf-8", timeout=60, check=False)

    def calls(self) -> list[str]:
        path = self.tmp / "calls"
        return path.read_text(encoding="utf-8").splitlines() if path.is_file() else []

    def stored(self) -> list[str]:
        return sorted(p.name.split("-", 1)[1] for p in (self.tmp / "eff" / "agent-runs").glob("*"))

    def test_the_script_parses(self):
        subprocess.run(["bash", "-n", str(_SCRIPT)], check=True)

    def test_both_briefs_run_and_are_stored(self):
        self.assertEqual(self.run_wrapper("0,0").returncode, 0)
        self.assertEqual(len(self.calls()), 2)
        self.assertEqual(self.stored(), ["guided.json", "guided.md", "vague.json", "vague.md"])

    def test_the_usage_limit_stores_the_record_and_skips_the_rest(self):
        proc = self.run_wrapper("75,0")
        self.assertEqual(proc.returncode, 75)
        self.assertEqual(len(self.calls()), 1)                     # guided would hit the same limit
        self.assertEqual(self.stored(), ["vague.json", "vague.md"])   # marked rateLimited by the runner
        self.assertIn("usage limit: vague recorded, not scored", proc.stdout)

    def test_another_failure_stops_without_a_record(self):
        self.assertEqual(self.run_wrapper("1,0").returncode, 1)
        self.assertEqual((len(self.calls()), self.stored()), (1, []))


if __name__ == "__main__":
    unittest.main()
