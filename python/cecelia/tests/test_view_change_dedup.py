"""The bridge reports a view change only when the PREVIEWABLE REGION moved.

Why this is a test and not a one-liner left to review: the events the bridge listens to (camera
zoom/centre, the dims slider, ndisplay) are proxies for the region a preview would run on, and they
fire for things that don't move it — a canvas refresh, a window resize, the bridge's own labels-layer
swap. Each spurious post becomes a real cellpose run, whose layer swap can fire them again. That is a
self-sustaining loop: it presents as a preview permanently stuck on "Previewing…" with the mask
flickering, which is exactly what it did live (2026-08-01). Deduping on the region makes the loop
impossible rather than unlikely, so the property is pinned here.

Headless: `_post_view_changed` only touches `self._view_listener_url`, `self.preview_region` and
`self._last_posted_region`, so it is exercised against a stub `self` — no napari, no Qt, no viewer.
Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import os
import sys
import types
import unittest
import unittest.mock

# The bridge is a runtime process, not part of the `cecelia` package, so it isn't on the import path.
_BRIDGE_DIR = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'napari')


def _load_post_view_changed():
    """The unbound `_post_view_changed`, or None when the bridge can't be imported here.

    Only an ImportError is tolerated (an env without napari/qtpy — the bridge is not a package dep).
    Anything else, a renamed class included, must FAIL: a suite that silently skips itself pins
    nothing, and this one existed for eight green-looking skips before the name was checked.
    """
    sys.path.insert(0, os.path.abspath(_BRIDGE_DIR))
    try:
        import napari_bridge                      # noqa: PLC0415 — optional, guarded above
    except ImportError:
        return None
    finally:
        sys.path.pop(0)
    return napari_bridge.NapariState._post_view_changed


class _Stub:
    """Everything `_post_view_changed` reads, and nothing else."""

    def __init__(self, regions):
        self._view_listener_url = 'http://localhost:8080'
        self._last_posted_region = None
        self._regions = list(regions)
        self.asked = 0

    def preview_region(self, verbose=True):
        assert verbose is False, 'the dedup path must not spam the log'
        self.asked += 1
        r = self._regions[min(self.asked - 1, len(self._regions) - 1)]
        if isinstance(r, Exception):
            raise r
        return r


R1 = {'xy': {'X': [0, 100], 'Y': [0, 100]}, 'z': 3, 't': 7, 'ndisplay': 2}
R2 = {'xy': {'X': [50, 150], 'Y': [0, 100]}, 'z': 3, 't': 7, 'ndisplay': 2}


class ViewChangeDedupTest(unittest.TestCase):
    def setUp(self):
        self.fn = _load_post_view_changed()
        if self.fn is None:
            self.skipTest('napari bridge not importable in this environment')

    def _posts(self, regions):
        """Run `_post_view_changed` once per region and count the POSTs it actually made."""
        stub = _Stub(regions)
        with unittest.mock.patch('urllib.request.urlopen') as urlopen:
            for _ in regions:
                self.fn(stub)
        return urlopen.call_count, stub

    def test_first_change_always_posts(self):
        n, _ = self._posts([R1])
        self.assertEqual(n, 1)

    def test_an_unchanged_region_is_not_reported(self):
        # the loop-breaker: three events, one region → one preview
        n, _ = self._posts([R1, R1, R1])
        self.assertEqual(n, 1)

    def test_a_real_move_is_reported(self):
        n, _ = self._posts([R1, R2])
        self.assertEqual(n, 2)

    def test_moving_back_is_a_change_again(self):
        # dedup is against the LAST posted region, not a set of everything seen — returning to a
        # region must re-preview, because the parameters may have changed since
        n, _ = self._posts([R1, R2, R1])
        self.assertEqual(n, 3)

    def test_z_or_t_alone_counts_as_a_move(self):
        # a slider step changes no camera value; if only XY were compared, scrolling z would silently
        # keep showing the previous plane's mask
        for key, val in (('z', 4), ('t', 8), ('ndisplay', 3)):
            moved = dict(R1, **{key: val})
            n, _ = self._posts([R1, moved])
            self.assertEqual(n, 2, f'{key} change must be reported')

    def test_nothing_previewable_posts_nothing(self):
        # preview_region raises when no image layer is open — a preview would fail too, so there is
        # nothing worth telling the frontend
        n, stub = self._posts([RuntimeError('no image layer open')])
        self.assertEqual(n, 0)
        self.assertIsNone(stub._last_posted_region)

    def test_a_failed_post_is_retried_on_the_next_event(self):
        # the region is recorded as posted BEFORE the HTTP call, so a transient failure would
        # otherwise suppress the retry until the user moved again. Assert the recovery path exists:
        # a raising urlopen must not leave the bridge unable to report the same region later.
        stub = _Stub([R1, R1])
        with unittest.mock.patch('urllib.request.urlopen', side_effect=OSError('refused')):
            self.fn(stub)
        with unittest.mock.patch('urllib.request.urlopen') as ok:
            self.fn(stub)
            self.assertEqual(ok.call_count, 1)

    def test_no_listener_url_means_no_work(self):
        stub = _Stub([R1])
        stub._view_listener_url = None
        with unittest.mock.patch('urllib.request.urlopen') as urlopen:
            self.fn(stub)
        self.assertEqual(urlopen.call_count, 0)
        self.assertEqual(stub.asked, 0)          # must not even compute a region


if __name__ == '__main__':
    unittest.main()
