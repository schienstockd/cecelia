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


class TestMultiPassProgress(unittest.TestCase):
    """One bar across a job made of several bridge calls — the side-by-side version comparison records
    once per version and then stitches. Without an offset each pass would report `1/40` again and the
    bar would jump back to the start twice, which reads as a stuck or restarted render."""

    def setUp(self):
        self.bridge = _load_bridge()
        if self.bridge is None:
            self.skipTest('napari not importable here')
        self.bridge._record_cancelled.clear()

    def _posts(self, hooks_args, calls):
        import json
        posted = []
        on_progress, _ = self.bridge._record_hooks(*hooks_args)
        original = self.bridge.urllib.request.urlopen

        def fake(req, timeout=None):
            posted.append(json.loads(req.data.decode()))
            raise OSError('no server')

        self.bridge.urllib.request.urlopen = fake
        try:
            for frame, total in calls:
                on_progress(frame, total)
        finally:
            self.bridge.urllib.request.urlopen = original
        return posted

    def test_a_later_pass_reports_its_place_in_the_whole_job(self):
        # pass 2 of 2: 40 frames already recorded, 84 in the job (2 × 40 + a 4-frame stitch)
        posted = self._posts(('task-x', 'http://localhost:1', 40, 84), [(1, 40)])
        self.assertEqual((posted[0]['frame'], posted[0]['total']), (41, 84))

    def test_no_offset_is_the_single_record_it_always_was(self):
        posted = self._posts(('task-x', 'http://localhost:1'), [(3, 40)])
        self.assertEqual((posted[0]['frame'], posted[0]['total']), (3, 40))

    def test_the_total_is_never_beaten_by_the_frame_count(self):
        # the job total is estimated before the passes run; an under-estimate must not post 90/84
        posted = self._posts(('task-x', 'http://localhost:1', 80, 84), [(10, 12)])
        self.assertEqual((posted[0]['frame'], posted[0]['total']), (90, 90))


class TestStitchCommandReply(unittest.TestCase):
    """The `stitch_movies` command's REPLY shape, which `api/src/napari_api.jl` reads to decide whether
    the comparison finished or was cancelled. Called unbound against a duck-typed `self` — the command
    touches the viewer only to prepend a title card, so a stitch with no card needs no Qt (same
    headless approach as the other bridge tests)."""

    def setUp(self):
        self.bridge = _load_bridge()
        if self.bridge is None:
            self.skipTest('napari not importable here')
        self.bridge._record_cancelled.clear()

    def _shim(self):
        bridge = self.bridge

        class _State:                      # borrows the real methods, brings no Qt
            _viewer = None
            _recorded_size = bridge.NapariState._recorded_size
            stitch_movies = bridge.NapariState.stitch_movies

        return _State()

    def _clip(self, path, n=3):
        import numpy as np
        from cecelia.utils import movie_io
        with movie_io.movie_writer(path, 10) as out:
            for i in range(n):
                out.append_data(np.full((34, 66, 3), 20 + i * 30, dtype=np.uint8))

    def test_a_finished_stitch_reports_frames_columns_and_the_size_written(self):
        import os
        import tempfile
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._clip(a); self._clip(b)
            res = self._shim().stitch_movies(out, [a, b], labels=['default', 'corrected'], fps=10)
            self.assertEqual(res['frames'], 3)
            self.assertEqual(res['columns'], 2)
            self.assertEqual(res['path'], out)
            self.assertEqual(res['sizeX'], 134)          # 2 tiles + the 2px divider, read back off the file
            self.assertGreater(res['sizeY'], 34)         # + the caption strip
            self.assertNotIn('cancelled', res)

    def test_a_cancelled_stitch_replies_like_a_cancelled_record(self):
        import os
        import tempfile
        with tempfile.TemporaryDirectory() as d:
            a, b, out = (os.path.join(d, f) for f in ('a.mp4', 'b.mp4', 'out.mp4'))
            self._clip(a); self._clip(b)
            self.bridge.request_record_cancel('task-stitch')
            res = self._shim().stitch_movies(out, [a, b], fps=10, task_id='task-stitch')
            self.assertTrue(res['cancelled'])
            self.assertFalse(os.path.exists(out), 'a cancel promotes nothing onto the target path')
            # and the flag is cleared, so the NEXT job with a recycled id is not killed by a stale one
            self.assertFalse(self.bridge._record_cancel_requested('task-stitch'))


if __name__ == '__main__':
    unittest.main()
