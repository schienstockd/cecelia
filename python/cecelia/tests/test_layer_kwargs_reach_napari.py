"""Every keyword `napari_utils` builds for napari must be one napari actually accepts.

`test_runtime_call_signatures.py` closes the mirror-image gap — the runtime processes' calls INTO
`cecelia.utils` — and cannot close this one, for a reason worth stating rather than rediscovering:
all three layer helpers end in a **splat**.

    result = viewer.add_image(data, **kw)     # add_image
    layer  = viewer.add_labels(labels, **kw)  # add_labels
    return viewer.add_tracks(tracks, **kw)    # add_tracks

A static binder can say nothing about `**kw` — that check explicitly skips splatting calls — so the
keys are only ever validated by napari itself, at the moment a real layer is constructed. Every other
napari test in this suite passes a duck-typed fake viewer, and a fake accepts any keyword, so a key
napari has never heard of is invisible until a user clicks something.

That is not hypothetical. `contour` (the mask outline) was added to `add_labels`' `kw` dict on the
assumption that a settable napari property is also a constructor argument. It is not:
`Labels.__init__` has no `contour` parameter, so `viewer.add_labels(..., contour=2)` raises
`TypeError: unexpected keyword argument` — which would have taken down every mask layer in the app,
with the whole Python suite green.

So this exercises the helpers for real, against a headless `ViewerModel`, across the OPTIONAL keyword
combinations (the ones a `kw` dict adds conditionally are exactly the ones a signature drifts under).
It asserts nothing about the resulting pixels — `test_napari_utils.py` and `test_layer_axes.py` own
that. All it asks is: does napari take these keys?

``ViewerModel`` needs no Qt and no display, so this runs in CI. Part of the Python (analysis-env)
suite — run with `pixi run test-py`.
"""
import sys
import unittest

import numpy as np

from cecelia.utils import napari_utils

#: Building a real napari Labels layer SIGSEGVs on the macOS runner — see test_preview_layers.py.
#: Narrowed to the Labels cases so macOS keeps covering the Image and Tracks helpers.
_LABELS_CRASH_ON_MACOS = sys.platform == 'darwin'


def _viewer():
    try:
        from napari.components import ViewerModel
    except ImportError:
        return None
    return ViewerModel()


class LayerKwargsReachNapariTest(unittest.TestCase):
    def setUp(self):
        self.v = _viewer()
        if self.v is None:
            self.skipTest('napari not installed')

    # ── add_image ────────────────────────────────────────────────────────────
    def test_add_image_minimal(self):
        img = np.random.rand(4, 8, 8).astype('float32')
        self.assertIsNotNone(napari_utils.add_image(self.v, img, scale=(1, 1, 1)))

    def test_add_image_every_optional_keyword(self):
        # units / blending / cache are the three the kw dict adds CONDITIONALLY — the shape a
        # signature change slips through
        img = np.random.rand(4, 8, 8).astype('float32')
        self.assertIsNotNone(napari_utils.add_image(
            self.v, img, scale=(1, 1, 1), units=('um', 'um', 'um'), blending='additive',
            cache=False, visible=True, name='one', contrast=True))

    def test_add_image_per_channel(self):
        # channel_axis makes napari return a LIST of layers, and `colormaps` is a per-channel list —
        # a different call shape through the same kw dict
        img = np.random.rand(2, 8, 8).astype('float32')
        out = napari_utils.add_image(self.v, img, scale=(1, 1), channel_axis=0,
                                     channel_names=['CD3', 'CD8'], colormaps=['green', 'red'])
        self.assertEqual(len(out), 2)

    # ── add_labels ───────────────────────────────────────────────────────────
    @unittest.skipIf(_LABELS_CRASH_ON_MACOS, 'napari Labels layer SIGSEGVs on the macOS runner')
    def test_add_labels_minimal(self):
        lab = np.zeros((8, 8), dtype='uint32')
        self.assertIsNotNone(napari_utils.add_labels(self.v, lab, scale=(1, 1)))

    @unittest.skipIf(_LABELS_CRASH_ON_MACOS, 'napari Labels layer SIGSEGVs on the macOS runner')
    def test_add_labels_every_optional_keyword(self):
        # `contour` is the one that caught us: a settable PROPERTY, not a constructor argument, so it
        # must reach the layer AFTER the add. If someone folds it back into `kw`, this raises.
        lab = np.zeros((8, 8), dtype='uint32')
        layer = napari_utils.add_labels(self.v, lab, scale=(1, 1), units=('um', 'um'),
                                        opacity=0.5, name='(a) Labels', visible=True,
                                        cache=True, contour=2)
        self.assertEqual(layer.contour, 2)

    # ── add_tracks ───────────────────────────────────────────────────────────
    def _tracks(self):
        # [track_id, t, y, x] — two vertices of one track
        return np.array([[1, 0, 1.0, 1.0], [1, 1, 2.0, 2.0]], dtype='float32')

    def test_add_tracks_minimal(self):
        self.assertIsNotNone(napari_utils.add_tracks(self.v, self._tracks(), scale=(1, 1, 1)))

    def test_add_tracks_with_a_named_colormap(self):
        self.assertIsNotNone(napari_utils.add_tracks(
            self.v, self._tracks(), scale=(1, 1, 1), units=('s', 'um', 'um'),
            color_by='track_id', colormap='turbo', tail_width=2, tail_length=10,
            blending='additive', visible=True, name='(track) Tracks'))

    def test_add_tracks_with_a_categorical_colormaps_dict(self):
        # `colormaps_dict` REPLACES `colormap` in the kw dict, so it is a branch the `colormap` cases
        # above never touch. Built through the helper the bridge itself uses, so the VALUE type is the
        # real one too — a dict of napari Colormaps, not a placeholder that would pass either way.
        layer = napari_utils.add_tracks(
            self.v, self._tracks(), scale=(1, 1, 1), color_by='cc_pop',
            properties={'cc_pop': np.array([0, 0])},
            colormaps_dict={'cc_pop': napari_utils.solid_track_colormap('#ff0000')})
        self.assertIsNotNone(layer)


if __name__ == '__main__':
    unittest.main()
