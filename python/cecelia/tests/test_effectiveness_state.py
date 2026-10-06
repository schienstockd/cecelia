"""Tests for `cecelia.effectiveness.state` — the state folder and the one-at-a-time lock that the
weekly judge and guide runs share. Run with `pixi run test-py`."""
from __future__ import annotations

import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness.state import state_dir, try_lock


class StateTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)

    def test_the_state_dir_follows_the_log(self):
        with mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.tmp / "eff" / "events.jsonl")}):
            self.assertEqual(state_dir(), self.tmp / "eff")

    @unittest.skipUnless(os.name == "posix", "the lock guards POSIX only")
    def test_a_held_lock_refuses_a_second_taker_until_it_is_released(self):
        path = self.tmp / "sub" / "job.lock"
        first = try_lock(path)
        self.assertIsNotNone(first)
        self.assertIsNone(try_lock(path))
        first.close()
        again = try_lock(path)
        self.assertIsNotNone(again)
        again.close()


if __name__ == "__main__":
    unittest.main()
