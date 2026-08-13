"""Drift-correction geometry: where each timepoint lands in the expanded canvas.

`drift_correct_im` writes every frame into a ZEROED canvas at a per-frame offset, so the canvas is
mostly padding — 3–56% on the movies this was written for, one of which goes from 8 z-planes to 18.
`drift_frame_slices` is that placement as a pure function, so a consumer can skip the padding
without re-deriving the geometry or reading a voxel, and — the point — WITHOUT a second
implementation that can disagree with the writer.

Two things are pinned here:
  1. the refactor is bit-identical to the inline loop it replaced (verbatim oracle below), and
  2. the slices really are the data box — everything outside them is zero.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

import cecelia.utils.correction_utils as cu
import cecelia.utils.zarr_utils as zarr_utils


def _drift_correct_im_ORIGINAL(input_array, dim_utils, shifts, timepoints=None):
    """The placement loop EXACTLY as it stood before `drift_frame_slices` was extracted
    (commit b1ecf13, correction_utils.py:226-268), kept verbatim as the oracle. Do not tidy it —
    its value is being untouched. Only the parts the refactor could affect are retained."""
    if timepoints is None:
        timepoints = range(dim_utils.dim_val('T'))
    drift_im_shape_round, first_im_pos = cu.drift_correct_shape(input_array, dim_utils, shifts)
    result = np.zeros(drift_im_shape_round, dtype=input_array.dtype)
    tp_shape = list(drift_im_shape_round)
    tp_shape[dim_utils.dim_idx('T')] = 1
    tp_shape = tuple(tp_shape)
    slices = list(first_im_pos)

    for i in timepoints:
        if i > 0:
            new_slices = []
            for j, y in enumerate(slices):
                new_slices.append(slice(y.start + shifts[i - 1, j], y.stop + shifts[i - 1, j], 1))
            slices = new_slices

        new_slices = [slice(None)] * len(drift_im_shape_round)
        im_slices = [slice(None)] * len(drift_im_shape_round)
        for j, y in enumerate(dim_utils.spatial_axis()):
            new_slices[dim_utils.dim_idx(y)] = slice(
                round(slices[j].start), round(slices[j].stop), 1)
        im_slices[dim_utils.dim_idx('T')] = slice(i, i + 1, 1)
        new_slices = tuple(new_slices)
        im_slices = tuple(im_slices)

        src = zarr_utils.fortify(input_array[im_slices])
        new_image = np.zeros(tp_shape, dtype=result.dtype)
        if new_image[new_slices].shape != src.shape:
            dif_dim = [x - y for x, y in zip(new_image[new_slices].shape, src.shape)]
            adj = list(new_slices)
            for j, y in enumerate(dif_dim):
                if y > 0:
                    adj[j] = slice(adj[j].start + y, adj[j].stop, 1)
                elif y < 0:
                    if adj[j].start - y >= 0:
                        adj[j] = slice(adj[j].start + y, adj[j].stop, 1)
                    elif adj[j].stop + y < result.shape[j]:
                        adj[j] = slice(adj[j].start, adj[j].stop + y, 1)
            new_slices = tuple(adj)
        new_image[new_slices] = src
        result[im_slices] = new_image
    return result


_OME = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="d"><Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint8"
    SizeT="{t}" SizeC="1" SizeZ="{z}" SizeY="{y}" SizeX="{x}"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
    PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
    <Channel ID="Channel:0:0" SamplesPerPixel="1"/><MetadataOnly/></Pixels></Image></OME>"""


def _fixture(shape=(6, 1, 4, 12, 10), seed=0):
    """(array, dim_utils) for a [T,C,Z,Y,X] movie of noise — noise, so any mis-placement by even
    one pixel shows up as a value mismatch rather than blending into a smooth gradient."""
    import ome_types
    from cecelia.utils.dim_utils import DimUtils
    t, c, z, y, x = shape
    du = DimUtils(ome_types.from_xml(_OME.format(t=t, z=z, y=y, x=x)), use_channel_axis=True)
    du.calc_image_dimensions(shape)
    arr = np.random.default_rng(seed).integers(1, 255, size=shape, dtype=np.uint8)  # no 0s: 0 = pad
    return arr, du


# Drift patterns chosen to hit the branches: a steady creep (the real-data case, which accumulates
# past the stack depth), sign changes, sub-pixel rounding, and a shift big enough to push a frame
# against the canvas edge and trigger the clamp.
_PATTERNS = {
    'steady creep':   lambda n: np.tile([0.4, 0.3, 0.2], (n, 1)),
    'sign changes':   lambda n: np.array([[(-1) ** i * 0.6, 0.5, -0.4] for i in range(n)]),
    'sub-pixel':      lambda n: np.tile([0.5, 0.5, 0.5], (n, 1)),
    'mixed + zero':   lambda n: np.array([[0.0, 1.2, -0.7] if i % 2 else [0.9, 0.0, 0.3]
                                          for i in range(n)]),
    'large':          lambda n: np.tile([1.5, 2.0, -1.5], (n, 1)),
}


class DriftFrameSlicesTest(unittest.TestCase):
    def test_refactor_is_bit_identical_to_the_original_loop(self):
        for name, make in _PATTERNS.items():
            with self.subTest(pattern=name):
                arr, du = _fixture()
                shifts = make(du.dim_val('T') - 1)
                got = cu.drift_correct_im(arr, du, 0, shifts=shifts)
                want = _drift_correct_im_ORIGINAL(arr, du, shifts)
                self.assertEqual(got.shape, want.shape)
                self.assertTrue(np.array_equal(got, want),
                                f"{name}: placement changed ({int((got != want).sum())} voxels)")

    def test_slices_are_exactly_the_non_zero_box(self):
        """The contract a consumer relies on: outside these slices the canvas is untouched zero,
        inside it is the source frame. The fixture has no 0 values, so this is unambiguous."""
        for name, make in _PATTERNS.items():
            with self.subTest(pattern=name):
                arr, du = _fixture()
                shifts = make(du.dim_val('T') - 1)
                out = cu.drift_correct_im(arr, du, 0, shifts=shifts)
                boxes = cu.drift_frame_slices(arr, du, shifts)
                t_idx = du.dim_idx('T')
                for t in range(du.dim_val('T')):
                    frame = out[(slice(None),) * t_idx + (t,)]
                    inside = np.zeros(frame.shape, dtype=bool)
                    sl = list(boxes[t])
                    sl.pop(t_idx)                       # the frame has no T axis
                    inside[tuple(sl)] = True
                    self.assertTrue(np.all(frame[~inside] == 0),
                                    f"{name} t={t}: non-zero data OUTSIDE the reported box")
                    self.assertTrue(np.all(frame[inside] != 0),
                                    f"{name} t={t}: padding INSIDE the reported box")

    def test_every_frames_z_span_is_the_source_depth(self):
        """Why `_valid_z_span`'s thin-span guard never fires on real data.

        A frame is placed whole — the canvas grows to hold the trajectory, but each frame keeps its
        own depth — so the valid box is always `source_z` planes deep, wherever it sits. Measured
        across the 17 corrected stores on this machine that carry a box: span is 8, 13 or 31 planes
        and NEVER below 2, so `SegmentationUtils._valid_z_span`'s `min_span` widening is a safety
        net for a malformed box, not a live path. Pinned here because if this stops holding, that
        guard starts silently disabling the skip instead of erroring.
        """
        for name, make in _PATTERNS.items():
            with self.subTest(pattern=name):
                arr, du = _fixture()
                shifts = make(du.dim_val('T') - 1)
                src_z = arr.shape[du.dim_idx('Z')]
                for t, box in cu.drift_frame_origins(arr.shape, du, shifts).items():
                    self.assertEqual(box['Z'][1] - box['Z'][0], src_z,
                                     f'{name} t={t}: frame is not its own depth')

    def test_origins_need_no_array_only_a_shape(self):
        """Replayable from a QC sidecar: same answer from the plain shape as from the array, so a
        consumer never has to open the store to know where the padding is."""
        arr, du = _fixture()
        shifts = _PATTERNS['steady creep'](du.dim_val('T') - 1)
        self.assertEqual(cu.drift_frame_slices(arr, du, shifts),
                         cu.drift_frame_slices(arr.shape, du, shifts))
        org = cu.drift_frame_origins(arr.shape, du, shifts)
        self.assertEqual(sorted(org), list(range(du.dim_val('T'))))
        self.assertEqual(sorted(org[0]), ['X', 'Y', 'Z'])
        # steady positive creep => the Z origin only ever advances
        z0 = [org[t]['Z'][0] for t in range(du.dim_val('T'))]
        self.assertEqual(z0, sorted(z0))
        # and the box is the source depth, so a consumer reading it skips real padding
        self.assertEqual(org[0]['Z'][1] - org[0]['Z'][0], arr.shape[du.dim_idx('Z')])


if __name__ == "__main__":
    unittest.main()
