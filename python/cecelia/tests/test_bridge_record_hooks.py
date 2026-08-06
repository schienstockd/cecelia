"""The bridge's half of "a recording behaves like a batch": a cancel that lands MID-render, and progress.

Why this needs pinning rather than review: the bridge runs one command at a time on the Qt thread, so a
recording occupies that loop for its whole run (minutes at 4K). A cancel routed through the normal
command queue would be executed *after* the recording it was meant to stop — the bug would look like
"Cancel does nothing", and only on long renders. `handle()` therefore answers `record_cancel` on the
asyncio WS thread and never queues it; these tests pin the registry it writes to and the hooks the frame
loop reads.

Cancels are keyed by TASK ID on purpose: a single global flag would let a cancel that arrives late kill
the *next* recording the user starts.

Headless: only the module-level helpers are exercised — no viewer, no Qt. Skipped where napari isn't
importable (the bridge is a runtime process, not a package dep). Part of `pixi run test-py`.
"""
import os
import sys
import unittest

_BRIDGE_DIR = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'napari')


def _load_bridge():
    """The bridge module, or None when napari/qtpy aren't installed. Only an ImportError is tolerated —
    a renamed helper must FAIL rather than silently skip (see test_view_change_dedup)."""
    sys.path.insert(0, os.path.abspath(_BRIDGE_DIR))
    try:
        import napari_bridge
    except ImportError:
        return None
    finally:
        sys.path.pop(0)
    return napari_bridge


class TestRecordCancelRegistry(unittest.TestCase):
    def setUp(self):
        self.bridge = _load_bridge()
        if self.bridge is None:
            self.skipTest('napari not importable here')
        self.bridge._record_cancelled.clear()

    def test_cancel_is_per_task(self):
        self.bridge.request_record_cancel('task-a')
        self.assertTrue(self.bridge._record_cancel_requested('task-a'))
        # the recording the user did NOT cancel must be unaffected — a global flag would kill it
        self.assertFalse(self.bridge._record_cancel_requested('task-b'))

    def test_clearing_is_scoped_too(self):
        self.bridge.request_record_cancel('task-a')
        self.bridge.request_record_cancel('task-b')
        self.bridge._clear_record_cancel('task-a')
        self.assertFalse(self.bridge._record_cancel_requested('task-a'))
        self.assertTrue(self.bridge._record_cancel_requested('task-b'))

    def test_an_untracked_or_blank_id_is_never_cancelled(self):
        self.bridge.request_record_cancel(None)          # must not blow up, must not flag anything
        self.assertFalse(self.bridge._record_cancel_requested(None))
        self.assertFalse(self.bridge._record_cancel_requested(''))
        self.assertFalse(self.bridge._record_cancel_requested('never-seen'))

    def test_the_dispatcher_answers_a_cancel_without_queueing_it(self):
        # The property, asserted on the source because the alternative needs a live asyncio server: the
        # `record_cancel` branch sits in `handle()` BEFORE the queue put, and returns without one.
        with open(os.path.join(_BRIDGE_DIR, 'napari_bridge.py'), encoding='utf-8') as fh:
            src = fh.read()
        handler = src[src.index('async def handle('):src.index('async def ws_server(')]
        cancel_at = handler.index('record_cancel')
        queue_at = handler.index('command_queue.put')
        self.assertLess(cancel_at, queue_at, 'record_cancel must be handled before the command queue')
        self.assertIn('continue', handler[cancel_at:queue_at])


class TestRecordHooks(unittest.TestCase):
    def setUp(self):
        self.bridge = _load_bridge()
        if self.bridge is None:
            self.skipTest('napari not importable here')
        self.bridge._record_cancelled.clear()

    def test_no_task_id_means_no_hooks(self):
        # a REPL/script record isn't on the task rail: no progress posts, no cancel polling
        self.assertEqual(self.bridge._record_hooks(None, 'http://x'), (None, None))

    def test_cancel_hook_reads_the_registry_live(self):
        _, should_cancel = self.bridge._record_hooks('task-x', None)
        self.assertFalse(should_cancel())
        self.bridge.request_record_cancel('task-x')
        self.assertTrue(should_cancel())      # the loop polls this per frame, mid-render

    def test_progress_is_throttled_but_never_drops_the_last_frame(self):
        posted = []
        on_progress, _ = self.bridge._record_hooks('task-x', 'http://localhost:1')
        # no api_url reachable, so the POST fails quietly; count the attempts instead
        original = self.bridge.urllib.request.urlopen

        def fake(req, timeout=None):
            posted.append(req.data)
            raise OSError('no server')

        self.bridge.urllib.request.urlopen = fake
        try:
            for i in range(1, 20):
                on_progress(i, 20)
            self.assertEqual(len(posted), 1, 'frames inside the throttle window must not each POST')
            on_progress(20, 20)
            self.assertEqual(len(posted), 2, 'the final frame always posts, throttle or not')
        finally:
            self.bridge.urllib.request.urlopen = original


if __name__ == '__main__':
    unittest.main()
