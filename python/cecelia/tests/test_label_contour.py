"""`add_labels(contour=N)` against a REAL napari Labels layer.

`contour` draws each label as an N-px outline instead of a filled region, which is what lets the
channel signal under a mask stay readable in a movie. The rest of the suite exercises `add_labels`
through a duck-typed fake viewer (`test_napari_utils.py`), and a fake accepts any keyword — so it
cannot answer the only question that matters here: **does napari?**

It does not, not as a constructor argument. `contour` is a settable *property* on `Labels`;
`Labels.__init__` has no such parameter (checked on napari 0.7.1), so passing it through to
`viewer.add_labels(**kw)` raises `TypeError: unexpected keyword argument` and takes every mask layer
down with it. `add_labels` therefore sets it AFTER the add. That is an implementation detail nobody
would infer from the call site, and a fake viewer would have stayed green through it.

`test_runtime_call_signatures.py` does not cover this: it binds the runtime processes' calls into
`cecelia.utils`, not `cecelia.utils`' own calls into napari.

Pinned as BEHAVIOUR (the layer ends up with the contour), not as "napari lacks the kwarg" — a future
napari that accepts it in the constructor should let this simplify, not fail.

``ViewerModel`` needs no Qt and no display, so this runs in CI. Part of the Python (analysis-env)
suite — run with `pixi run test-py`.
"""
import sys
import unittest

import numpy as np

from cecelia.utils import napari_utils

#: Building a real napari Labels layer SIGSEGVs on the macOS runner — see the note in
#: `test_preview_layers.py`, which hit the same thing first. Linux/Windows still cover it.
_LABELS_CRASH_ON_MACOS = sys.platform == 'darwin'


def _viewer():
    """A headless napari ViewerModel, or None when napari is absent."""
    try:
        from napari.components import ViewerModel
    except ImportError:
        return None
    return ViewerModel()


@unittest.skipIf(_LABELS_CRASH_ON_MACOS, 'napari Labels layer SIGSEGVs on the macOS runner')
class LabelContourTest(unittest.TestCase):
    def setUp(self):
        self.v = _viewer()
        if self.v is None:
            self.skipTest('napari not installed')
        self.data = np.zeros((8, 8), dtype='uint32')
        self.data[2:6, 2:6] = 1

    def _add(self, name, **kw):
        return napari_utils.add_labels(self.v, self.data, scale=(1, 1), name=name, **kw)

    def test_default_is_filled(self):
        # the value every mask had before the control existed — the default path must not move
        self.assertEqual(self._add('(a) Labels').contour, 0)

    def test_contour_reaches_the_layer(self):
        self.assertEqual(self._add('(b) Labels', contour=2).contour, 2)

    def test_negative_and_none_are_floored_to_filled(self):
        self.assertEqual(self._add('(c) Labels', contour=-3).contour, 0)
        self.assertEqual(self._add('(d) Labels', contour=None).contour, 0)

    def test_survives_a_capture_and_apply_round_trip(self):
        # this is what carries the outline across a movie's re-open: the recorder captures the view
        # after the first cell and re-applies it to the rest (`_record_grid!`), and the props file
        # restores it on the next open. Neither works unless `contour` is in _VIEW_LAYER_KEYS.
        layer = self._add('(e) Labels', contour=3)
        snap = napari_utils.capture_view_state(self.v)
        self.assertEqual(snap['layers']['(e) Labels'].get('contour'), 3)
        layer.contour = 0
        napari_utils.apply_view_state(self.v, snap)
        self.assertEqual(layer.contour, 3)

    def test_capturing_an_image_layer_is_unaffected(self):
        # `contour` is Labels-only; listing it in _VIEW_LAYER_KEYS must stay free for every other
        # layer type (each read is guarded by hasattr)
        self.v.add_image(np.zeros((8, 8), dtype='uint16'), name='CD3')
        self.assertNotIn('contour', napari_utils.capture_view_state(self.v)['layers']['CD3'])


if __name__ == '__main__':
    unittest.main()
