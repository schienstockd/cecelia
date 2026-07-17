"""Tests for the session monitor — the 10-attempts pattern + frame normalization.

Pure logic, no socket, no MCP SDK. This is the core observer signal, so it's tested hard here rather
than left to a live Claude Code session.
"""
import unittest

from cecelia_mcp.monitor import SessionMonitor, normalize_frame
from cecelia_mcp.wsclient import api_url_to_ws, feed_raw


def _chain_done(uid, fn, project="P"):
    return {"type": "chain:node:done", "imageUid": uid, "fn": fn, "projectUid": project}


def _chain_failed(uid, fn, project="P", status="failed"):
    return {"type": "chain:node:failed", "imageUid": uid, "fn": fn, "projectUid": project,
            "status": status}


def _task_status(uid, fun, status):
    return {"type": "task:status", "imageUid": uid, "fun": fun, "status": status}


class NormalizeFrameTest(unittest.TestCase):
    def test_chain_frames_map_to_events(self):
        self.assertEqual(normalize_frame(_chain_done("img1", "segment.cellpose"))["kind"], "task_completed")
        self.assertEqual(normalize_frame(_chain_failed("img1", "segment.cellpose"))["kind"], "task_failed")
        run = normalize_frame({"type": "chain:node:running", "imageUid": "i", "fn": "f"})
        self.assertEqual(run["kind"], "task_running")

    def test_chain_skipped_and_cancelled_are_not_attempts(self):
        # node:failed doubles as skipped/cancelled — those aren't real attempts.
        self.assertIsNone(normalize_frame(_chain_failed("i", "f", status="skipped")))
        self.assertIsNone(normalize_frame(_chain_failed("i", "f", status="cancelled")))

    def test_task_status_uses_fun_field(self):
        done = normalize_frame(_task_status("img7", "segment.cellpose", "done"))
        self.assertEqual(done["kind"], "task_completed")
        self.assertEqual(done["fn"], "segment.cellpose")
        self.assertEqual(normalize_frame(_task_status("i", "f", "failed"))["kind"], "task_failed")

    def test_task_status_funName_fallback(self):
        f = normalize_frame({"type": "task:status", "imageUid": "i", "funName": "measure.labels", "status": "done"})
        self.assertEqual(f["fn"], "measure.labels")

    def test_task_status_queued_and_cancelled_ignored(self):
        self.assertIsNone(normalize_frame(_task_status("i", "f", "queued")))
        self.assertIsNone(normalize_frame(_task_status("i", "f", "cancelled")))

    def test_note_and_lablog_events(self):
        self.assertEqual(normalize_frame(
            {"type": "image_note_added", "imageUid": "i", "note": "hi"})["kind"], "image_note_added")
        self.assertEqual(normalize_frame(
            {"type": "lab_log_entry_added", "summary": "s"})["kind"], "lab_log_entry_added")

    def test_unknown_frame_is_none(self):
        self.assertIsNone(normalize_frame({"type": "task:progress", "progress": 0.5}))
        self.assertIsNone(normalize_frame({"type": "chain:log", "line": "x"}))


class RepeatAttemptsTest(unittest.TestCase):
    def test_fires_only_after_threshold(self):
        m = SessionMonitor()  # threshold 3 ⇒ fires on the 4th
        for _ in range(3):
            m.record(normalize_frame(_chain_done("img7", "segment.cellpose")))
        self.assertEqual(m.poll("P"), [])  # 3 runs — still silent
        m.record(normalize_frame(_chain_done("img7", "segment.cellpose")))
        obs = m.poll("P")
        self.assertEqual(len(obs), 1)
        self.assertEqual(obs[0]["type"], "repeat_attempts")
        self.assertEqual(obs[0]["attempts"], 4)
        self.assertEqual(obs[0]["fn"], "segment.cellpose")

    def test_counts_across_launch_paths(self):
        # chain nodes AND module-page task frames for the same (image, fn) share one tally.
        m = SessionMonitor()
        m.record(normalize_frame(_chain_done("img7", "segment.cellpose")))
        m.record(normalize_frame(_chain_failed("img7", "segment.cellpose")))
        m.record(normalize_frame(_task_status("img7", "segment.cellpose", "done")))
        m.record(normalize_frame(_task_status("img7", "segment.cellpose", "failed")))
        obs = m.poll()
        self.assertEqual(obs[0]["attempts"], 4)
        self.assertEqual(obs[0]["completed"], 2)
        self.assertEqual(obs[0]["failed"], 2)
        self.assertEqual(obs[0]["lastOutcome"], "failed")

    def test_coalesces_and_updates_count(self):
        m = SessionMonitor()
        for _ in range(5):
            m.record(normalize_frame(_chain_done("img7", "segment.cellpose")))
        first = m.poll()
        self.assertEqual(len(first), 1)
        self.assertEqual(first[0]["attempts"], 5)
        # poll drains; two more runs → a fresh, updated observation (not two duplicates)
        m.record(normalize_frame(_chain_done("img7", "segment.cellpose")))
        m.record(normalize_frame(_chain_done("img7", "segment.cellpose")))
        second = m.poll()
        self.assertEqual(len(second), 1)
        self.assertEqual(second[0]["attempts"], 7)

    def test_separate_keys_dont_mix(self):
        m = SessionMonitor()
        for _ in range(4):
            m.record(normalize_frame(_chain_done("imgA", "segment.cellpose")))
        for _ in range(4):
            m.record(normalize_frame(_chain_done("imgB", "segment.cellpose")))
        for _ in range(2):
            m.record(normalize_frame(_chain_done("imgA", "measure.labels")))  # below threshold
        obs = {(o["imageUid"], o["fn"]): o for o in m.poll()}
        self.assertEqual(set(obs), {("imgA", "segment.cellpose"), ("imgB", "segment.cellpose")})

    def test_unattributable_attempt_is_dropped(self):
        # A module-page task frame from a backend without `fun` — no fn ⇒ can't attribute, don't count.
        m = SessionMonitor()
        for _ in range(5):
            m.record(normalize_frame(_task_status("img7", "", "done")))
        self.assertEqual(m.poll(), [])


class PendingEventsTest(unittest.TestCase):
    def test_note_and_lablog_surface_and_drain(self):
        m = SessionMonitor()
        m.record(normalize_frame({"type": "image_note_added", "imageUid": "i", "note": "odd cells",
                                  "projectUid": "P"}))
        m.record(normalize_frame({"type": "lab_log_entry_added", "summary": "user note", "projectUid": "P"}))
        obs = m.poll("P")
        kinds = {o["type"] for o in obs}
        self.assertEqual(kinds, {"image_note_added", "lab_log_entry_added"})
        self.assertEqual(m.poll("P"), [])  # drained


class ProjectFilterTest(unittest.TestCase):
    def test_poll_filters_by_project_but_keeps_others_pending(self):
        m = SessionMonitor()
        for _ in range(4):
            m.record(normalize_frame(_chain_done("imgP", "f", project="P")))
        for _ in range(4):
            m.record(normalize_frame(_chain_done("imgQ", "f", project="Q")))
        p_obs = m.poll("P")
        self.assertEqual([o["imageUid"] for o in p_obs], ["imgP"])
        q_obs = m.poll("Q")  # Q's was left pending, not dropped
        self.assertEqual([o["imageUid"] for o in q_obs], ["imgQ"])

    def test_unknown_project_observation_always_returned(self):
        # module-page task frames carry no projectUid → returned for any project poll.
        m = SessionMonitor()
        for _ in range(4):
            m.record(normalize_frame(_task_status("img7", "segment.cellpose", "done")))
        self.assertEqual(len(m.poll("anything")), 1)


class WsClientHelpersTest(unittest.TestCase):
    def test_api_url_to_ws(self):
        self.assertEqual(api_url_to_ws("http://127.0.0.1:8080"), "ws://127.0.0.1:8080/ws")
        self.assertEqual(api_url_to_ws("http://127.0.0.1:8080/"), "ws://127.0.0.1:8080/ws")
        self.assertEqual(api_url_to_ws("https://host:9000"), "wss://host:9000/ws")

    def test_feed_raw_folds_valid_frame(self):
        m = SessionMonitor()
        for _ in range(4):
            feed_raw(m, '{"type":"chain:node:done","imageUid":"i","fn":"f","projectUid":"P"}')
        self.assertEqual(len(m.poll("P")), 1)

    def test_feed_raw_ignores_garbage(self):
        m = SessionMonitor()
        self.assertIsNone(feed_raw(m, "not json"))
        self.assertIsNone(feed_raw(m, "[1,2,3]"))       # not a dict
        self.assertIsNone(feed_raw(m, '{"type":"pong"}'))  # untracked
        self.assertEqual(m.poll(), [])


if __name__ == "__main__":
    unittest.main()
