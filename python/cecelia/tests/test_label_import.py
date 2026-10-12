"""Label import — `cecelia.utils.label_import_utils` and its runner `segment/import_labels_run.py`.

What is pinned: the shape check (a mismatch names both shapes and writes nothing), the uint32 cast and
what it refuses, ids made unique across timepoints only where they collide, the base/non-base id
pairing (kept when matching is off and flagged when it does not hold; overlap-matched exactly like the
segmenters' `_match_nuc_cyto` when on), and a pyramid as deep as the image's. Every run is forced
through several blocks per frame, because the streaming is the point of the module.

The full app path (registered labels → Measure labels → `label_props`) is the Julia testset
`label import end to end` in `app/test/suite/label_import.jl`.
"""
import importlib.util
import os
import shutil
import tempfile
import types
import unittest
from pathlib import Path

import numpy as np
import ome_types
import tifffile

import cecelia.utils.label_import_utils as liu
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'segment' / 'import_labels_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('import_labels_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _ome_xml(size_t, size_z, size_c, size_y, size_x):
    channels = ''.join(
        f'<Channel ID="Channel:0:{i}" Name="CH{i + 1}" SamplesPerPixel="1"/>' for i in range(size_c))
    return f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="{size_t}" SizeC="{size_c}" SizeZ="{size_z}" SizeY="{size_y}" SizeX="{size_x}"
            PhysicalSizeX="0.5" PhysicalSizeY="0.5" PhysicalSizeZ="1.0"
            PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm"
            TimeIncrement="30.0" TimeIncrementUnit="s">
      {channels}
    </Pixels>
  </Image>
</OME>"""


def write_image(path, size_t, size_z, size_c, size_y, size_x, nscales):
    """A TCZYX uint16 image store with OME-XML, as the import writes one."""
    omexml = ome_types.from_xml(_ome_xml(size_t, size_z, size_c, size_y, size_x))
    du = DimUtils(omexml, use_channel_axis=True)
    shape = (size_t, size_c, size_z, size_y, size_x)
    du.calc_image_dimensions(list(shape))
    group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
        path, shape, np.uint16, du, nscales=nscales)
    level0[:] = np.random.default_rng(0).integers(0, 1000, size=shape, dtype=np.uint16)
    zarr_utils.write_multiscale_pyramid(group, level0, du, nscales, list(pchunks))
    ome_xml_utils.save_meta_in_zarr(path, omexml=omexml)


def write_label_zarr(path, labels):
    """A bare label store (no image metadata) — what another tool hands over."""
    du = None
    axes = ['T', 'Z', 'Y', 'X'][-labels.ndim:]
    group, level0, _ = zarr_utils.open_multiscales_for_writing(
        path, labels.shape, labels.dtype, du, axes=axes, kind='labels')
    level0[:] = labels


def blobs(shape, n, seed, start=1):
    """`n` non-overlapping square objects on a (Y, X) plane, ids from `start`."""
    rng = np.random.default_rng(seed)
    out = np.zeros(shape, dtype=np.int64)
    placed = 0
    while placed < n:
        y, x = rng.integers(0, shape[0] - 6), rng.integers(0, shape[1] - 6)
        if out[y:y + 6, x:x + 6].any():
            continue
        out[y:y + 6, x:x + 6] = start + placed
        placed += 1
    return out


def oracle_match(base, nuc, threshold):
    """The segmenters' own matching (`SegmentationUtils._match_nuc_cyto`) on one frame."""
    ns = types.SimpleNamespace(
        match_threshold=threshold, remove_unmatched=False,
        _compute_iou_matrix=lambda a, b: SegmentationUtils._compute_iou_matrix(None, a, b))
    return SegmentationUtils._match_nuc_cyto(ns, base, nuc, None, 1)[1]


class _Log:
    def __init__(self):
        self.lines = []

    def log(self, msg):
        self.lines.append(str(msg))

    def progress(self, n, total):
        pass


class UnitTest(unittest.TestCase):
    def test_label_layout_drops_c(self):
        self.assertEqual(liu.label_layout((3, 2, 1, 40, 48), 'TCZYX'),
                         ((3, 1, 40, 48), ['T', 'Z', 'Y', 'X']))

    def test_axis_map_accepts_equal_and_squeezable_shapes(self):
        axes = ['T', 'Z', 'Y', 'X']
        self.assertEqual(liu.axis_map((3, 1, 40, 48), (3, 1, 40, 48), axes), [0, None, 2, 3])
        self.assertEqual(liu.axis_map((3, 40, 48), (3, 1, 40, 48), axes), [0, None, 1, 2])
        self.assertEqual(liu.axis_map((40, 48), (1, 1, 40, 48), axes), [None, None, 0, 1])

    def test_axis_map_mismatch_names_both_shapes(self):
        with self.assertRaises(liu.LabelImportError) as cm:
            liu.axis_map((40, 50), (3, 1, 40, 48), ['T', 'Z', 'Y', 'X'], what='base labels')
        msg = str(cm.exception)
        self.assertIn('(40, 50)', msg)
        self.assertIn('(3, 1, 40, 48)', msg)
        self.assertIn('TZYX', msg)

    def test_axis_map_refuses_a_z_stack_read_as_time(self):
        """Same sizes, different axes: NGFF names catch what the numbers cannot."""
        with self.assertRaises(liu.LabelImportError):
            liu.axis_map((5, 40, 48), (5, 1, 40, 48), ['T', 'Z', 'Y', 'X'], src_axes=['Z', 'Y', 'X'])

    def test_cast_to_uint32(self):
        out = liu.to_label_ids(np.array([[0, 3], [70000, 2]], dtype=np.int64))
        self.assertEqual(out.dtype, np.uint32)
        np.testing.assert_array_equal(out, [[0, 3], [70000, 2]])
        self.assertEqual(liu.to_label_ids(np.array([1.0, 2.0], np.float32)).dtype, np.uint32)

    def test_cast_refuses_what_it_would_change(self):
        for bad in (np.array([-1, 2], np.int32), np.array([1.5], np.float64),
                    np.array([2 ** 33], np.int64), np.array([np.nan], np.float32)):
            with self.assertRaises(liu.LabelImportError):
                liu.to_label_ids(bad)
        with self.assertRaises(liu.LabelImportError):
            liu.check_source_dtype(np.bool_)

    def test_frame_offsets_only_where_ids_collide(self):
        self.assertEqual(liu.frame_offsets([(1, 5), (6, 9), None, (10, 12)]), [0, 0, 0, 0])
        self.assertEqual(liu.frame_offsets([(1, 5), (1, 4), (2, 3)]), [0, 5, 9])
        with self.assertRaises(liu.LabelImportError):
            liu.frame_offsets([(1, 2 ** 32 - 2), (1, 5)])


class ImportRunTest(unittest.TestCase):
    T, Z, C, Y, X = 3, 1, 2, 40, 48
    NSCALES = 3

    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)
        self.im_path = os.path.join(self.dir, 'img.ome.zarr')
        write_image(self.im_path, self.T, self.Z, self.C, self.Y, self.X, self.NSCALES)
        # several blocks per frame, so the block loop and the per-frame reductions are exercised
        orig = liu._block_steps
        liu._block_steps = lambda *a: [min(s, 16) for s in orig(*a)]
        self.addCleanup(setattr, liu, '_block_steps', orig)
        self.runner = _load_runner()

    def _run(self, sources, **over):
        out = {r: os.path.join(self.dir, 'labels', 'imp.zarr' if r == 'base' else f'imp_{r}.zarr')
               for r in sources}
        params = dict(imPath=self.im_path, sources=sources, outPaths=out,
                      qcOutPath=os.path.join(self.dir, 'qc.json'), matchIds=False,
                      matchThreshold=0.3)
        params.update(over)
        self.runner.run(params)
        import json
        with open(params['qcOutPath'], encoding='utf-8') as f:
            qc = json.load(f)
        return {r: zarr_utils.open_as_zarr(p)[0] for r, p in out.items()}, qc

    def _per_frame_base(self):
        """Per-frame labels numbered from 1 in every frame — the collision case."""
        return np.stack([blobs((self.Y, self.X), 6, seed=t) for t in range(self.T)])

    def test_tiff_base_becomes_uint32_unique_and_pyramided(self):
        base = self._per_frame_base()
        tif = os.path.join(self.dir, 'base.tif')
        tifffile.imwrite(tif, base.astype(np.uint16), tile=(16, 16))
        levels, qc = self._run({'base': tif})
        lab = levels['base']
        self.assertEqual(len(lab), self.NSCALES)
        self.assertEqual(lab[0].dtype, np.uint32)
        self.assertEqual(lab[0].shape, (self.T, self.Z, self.Y, self.X))
        got = np.asarray(lab[0][:])
        ids = [set(np.unique(got[t])) - {0} for t in range(self.T)]
        for a in range(self.T):
            for b in range(a + 1, self.T):
                self.assertTrue(ids[a].isdisjoint(ids[b]), f'frames {a} and {b} share ids')
        # same objects, only renumbered: the foreground is untouched
        np.testing.assert_array_equal(got[:, 0] > 0, base > 0)
        np.testing.assert_array_equal(np.asarray(lab[1][:]), got[:, :, ::2, ::2])
        self.assertEqual(qc['labelCounts']['base'], 6 * self.T)
        self.assertTrue(qc['idsOffset'])

    def test_already_unique_ids_are_kept(self):
        base = np.stack([blobs((self.Y, self.X), 4, seed=t, start=1 + 10 * t)
                         for t in range(self.T)])[:, None]
        src = os.path.join(self.dir, 'base.zarr')
        write_label_zarr(src, base.astype(np.int32))
        levels, qc = self._run({'base': src})
        np.testing.assert_array_equal(np.asarray(levels['base'][0][:]), base)
        self.assertFalse(qc['idsOffset'])

    def test_shape_mismatch_fails_and_writes_nothing(self):
        tif = os.path.join(self.dir, 'base.tif')
        tifffile.imwrite(tif, np.ones((self.T, self.Y, self.X + 2), np.uint16))
        with self.assertRaises(SystemExit):
            self._run({'base': tif})
        self.assertFalse(os.path.exists(os.path.join(self.dir, 'labels', 'imp.zarr')))

    def test_paired_ids_are_kept_and_report_no_mismatch(self):
        base = self._per_frame_base()
        nuc = np.where(np.roll(base, 1, axis=2) == base, base, 0)   # inside each cell, same id
        bp, nps = os.path.join(self.dir, 'b.tif'), os.path.join(self.dir, 'n.tif')
        tifffile.imwrite(bp, base.astype(np.uint16))
        tifffile.imwrite(nps, nuc.astype(np.uint16))
        levels, qc = self._run({'base': bp, 'nuc': nps})
        b, n = np.asarray(levels['base'][0][:]), np.asarray(levels['nuc'][0][:])
        self.assertTrue(np.all((n == 0) | (n == b)), 'a nucleus lost its cell id')
        self.assertEqual(qc['idMismatchFrac'], 0.0)
        self.assertEqual(len(levels['nuc']), self.NSCALES)

    def test_unpaired_ids_are_flagged_when_matching_is_off(self):
        base = self._per_frame_base()
        nuc = np.where(base > 0, base + 100, 0)                     # same objects, other ids
        bp, nps = os.path.join(self.dir, 'b.tif'), os.path.join(self.dir, 'n.tif')
        tifffile.imwrite(bp, base.astype(np.uint16))
        tifffile.imwrite(nps, nuc.astype(np.uint16))
        _, qc = self._run({'base': bp, 'nuc': nps})
        self.assertEqual(qc['idMismatchFrac'], 1.0)

    def test_matching_reproduces_the_segmenters_rule(self):
        """Independently numbered nuclei, overlap-matched: per frame, identical to what
        `_match_nuc_cyto` makes of the same pair (ids made unique first, as the segmenter does)."""
        base = self._per_frame_base()
        rng = np.random.default_rng(7)
        nuc = np.zeros_like(base)
        for t in range(self.T):
            shifted = np.roll(base[t], (1, 1), axis=(0, 1))
            ids = np.unique(shifted[shifted > 0])
            perm = dict(zip(ids, rng.permutation(len(ids)) + 50))
            nuc[t] = np.vectorize(lambda v: perm.get(v, 0))(shifted)
        nuc[0, :3, :3] = 999                                        # a nucleus in no cell
        bp, nps = os.path.join(self.dir, 'b.tif'), os.path.join(self.dir, 'n.tif')
        tifffile.imwrite(bp, base.astype(np.uint16))
        tifffile.imwrite(nps, nuc.astype(np.uint16))
        levels, qc = self._run({'base': bp, 'nuc': nps}, matchIds=True, matchThreshold=0.3)
        b, n = np.asarray(levels['base'][0][:])[:, 0], np.asarray(levels['nuc'][0][:])[:, 0]
        for t in range(self.T):
            want = oracle_match(b[t], nuc[t].astype(np.uint32), 0.3)
            np.testing.assert_array_equal(n[t], want, f'frame {t}')
        self.assertTrue(set(np.unique(n)) - {0} <= set(np.unique(b)))
        self.assertGreater(qc['nMatched']['nuc'], 0)


if __name__ == '__main__':
    unittest.main()
