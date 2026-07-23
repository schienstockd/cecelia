"""Regression: create_multiscales must write pixel-exact data even when the source dask array's
chunking does NOT match the per-plane destination grid.

The per-plane-chunking change made the destination chunks (1 along T/C/Z, 512-tiled X/Y) differ from
the source's own (auto) chunks, but kept `da.store(..., lock=False)`. When two source blocks map into
one destination chunk, lock-free stores race on the zarr chunk file (read-modify-write) → scrambled
planes, worst on EXPANDED outputs like drift correction. The fix rechunks the source to the dest grid
first (one writer per chunk). This writes a deliberately misaligned, non-512-aligned (expanded-canvas-
like) source and asserts an exact round-trip; the buggy pattern fails it (corrupts ~5/5 trials).

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import os
import sys
import shutil
import tempfile
import unittest

import numpy as np
import dask.array as da
import zarr

# `cecelia.*` resolves via the editable install in the pixi env — no sys.path needed.
import cecelia.utils.zarr_utils as zu


class CreateMultiscalesStoreTest(unittest.TestCase):
    def test_misaligned_source_roundtrips_exact(self):
        rng = np.random.default_rng(0)
        shape = (6, 2, 3, 730, 620)                 # [T,C,Z,Y,X] — Y/X NOT multiples of 512 (expanded case)
        base = rng.integers(0, 65535, size=shape, dtype=np.uint16)
        # source chunks span T/C with odd spatial blocks → misaligned vs the (1,1,1,512,512) dest grid
        src = da.from_array(base, chunks=(3, 2, 3, 300, 400))

        d = tempfile.mkdtemp()
        try:
            path = os.path.join(d, "out.ome.zarr")
            zu.create_multiscales(src, path, nscales=1)
            back = zarr.open_group(path, mode="r")["0"][:]
            self.assertTrue(np.array_equal(back, base),
                            "create_multiscales corrupted data on a misaligned source")
        finally:
            shutil.rmtree(d, ignore_errors=True)


_OME_XML = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeT="1" SizeZ="1" SizeC="2" SizeY="4" SizeX="3"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm"
            PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
            PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm">
      <Channel ID="Channel:0:0" SamplesPerPixel="1"/>
      <Channel ID="Channel:0:1" SamplesPerPixel="1"/>
    </Pixels>
  </Image>
</OME>
"""


class CreateMultiscalesOnDiskTest(unittest.TestCase):
    """End-to-end: create_multiscales derives axes + scale from a real DimUtils and writes them into
    the on-disk .zattrs (the wiring around the shared multiscales_metadata builder)."""

    def test_writes_axes_and_scale_from_dim_utils(self):
        import ome_types
        from cecelia.utils.dim_utils import DimUtils

        omexml = ome_types.from_xml(_OME_XML)
        du = DimUtils(omexml, use_channel_axis=True)
        du.calc_image_dimensions((2, 4, 3))   # C,Y,X (size-1 T,Z dropped)

        arr = da.from_array(np.zeros((2, 4, 3), dtype=np.uint16), chunks=(1, 4, 3))
        d = tempfile.mkdtemp()
        try:
            path = os.path.join(d, "img.ome.zarr")
            zu.create_multiscales(arr, path, dim_utils=du, nscales=1)
            g = zarr.open_group(path, mode="r")
            ms = g.attrs["multiscales"][0]
            self.assertEqual([a["name"] for a in ms["axes"]], ["c", "y", "x"])
            # C has no physical size → 1.0; Y/X → 0.5, in dim order
            self.assertEqual(ms["datasets"][0]["coordinateTransformations"][0]["scale"],
                             [1.0, 0.5, 0.5])
            self.assertEqual(tuple(g["0"].shape), (2, 4, 3))
        finally:
            shutil.rmtree(d, ignore_errors=True)


class StreamingWritersTest(unittest.TestCase):
    """The generic streaming helpers used by the drift / AF / cellpose correction tasks:
    open_multiscales_for_writing (empty per-plane-chunked level 0 + metadata) and copy_stream
    (per-timepoint carry-through — cellpose's unchanged channels/frames)."""

    def _du(self, size_t, size_c, size_y, size_x):
        import ome_types
        from cecelia.utils.dim_utils import DimUtils
        xml = f"""<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="t"><Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
    SizeT="{size_t}" SizeZ="1" SizeC="{size_c}" SizeY="{size_y}" SizeX="{size_x}"
    PhysicalSizeX="0.5" PhysicalSizeXUnit="µm" PhysicalSizeY="0.5" PhysicalSizeYUnit="µm">
    {''.join(f'<Channel ID="Channel:0:{c}" SamplesPerPixel="1"/>' for c in range(size_c))}
  </Pixels></Image></OME>"""
        du = DimUtils(ome_types.from_xml(xml), use_channel_axis=True)
        shape = [s for s in (size_t, size_c, size_y, size_x) if s != 1]
        du.calc_image_dimensions(tuple(shape))
        return du

    def test_copy_stream_roundtrips_timeseries_and_static(self):
        for size_t in (4, 1):   # timeseries (per-frame copy) and static (whole copy)
            du = self._du(size_t=size_t, size_c=2, size_y=13, size_x=11)
            rng = np.random.default_rng(size_t)
            base = rng.integers(0, 65535, size=tuple(du.im_dim), dtype=np.uint16)
            d = tempfile.mkdtemp()
            try:
                path = os.path.join(d, "cp.ome.zarr")
                _, level0, _ = zu.open_multiscales_for_writing(
                    path, base.shape, base.dtype, du, nscales=1)
                zu.copy_stream(level0, da.from_array(base, chunks=base.shape), du)
                back = zarr.open_group(path, mode="r")["0"][:]
                self.assertTrue(np.array_equal(back, base), f"copy_stream mismatch (T={size_t})")
            finally:
                shutil.rmtree(d, ignore_errors=True)


class MultiscalesMetadataTest(unittest.TestCase):
    """The shared NGFF metadata builder used by both the image writer (create_multiscales) and
    the label writer (segmentation_utils._write_labels_zarr)."""

    def test_image_shape_xy_downsampled_per_level(self):
        axes = ['T', 'C', 'Y', 'X']
        scale = {'T': 1.0, 'C': 1.0, 'Y': 0.5, 'X': 0.5}
        ms = zu.multiscales_metadata(axes, 2, scale_for_axis=scale)
        self.assertEqual(len(ms), 1)
        self.assertEqual([a['name'] for a in ms[0]['axes']], ['t', 'c', 'y', 'x'])
        ds = ms[0]['datasets']
        self.assertEqual([d['path'] for d in ds], ['0', '1'])
        # level 0 = base; level 1 = XY *2, other axes unchanged
        self.assertEqual(ds[0]['coordinateTransformations'][0]['scale'], [1.0, 1.0, 0.5, 0.5])
        self.assertEqual(ds[1]['coordinateTransformations'][0]['scale'], [1.0, 1.0, 1.0, 1.0])

    def test_label_axes_drop_channel_and_missing_axis_defaults_to_one(self):
        # label array has no C axis; the scale map still carries C (ignored). Z missing → 1.0
        label_axes = ['T', 'Y', 'X']
        ax_to_scale = {'T': 1.0, 'C': 1.0, 'Y': 0.25, 'X': 0.25}   # note: no Z
        ms = zu.multiscales_metadata(label_axes, 1, scale_for_axis=ax_to_scale)
        self.assertEqual([a['name'] for a in ms[0]['axes']], ['t', 'y', 'x'])
        self.assertEqual(ms[0]['datasets'][0]['coordinateTransformations'][0]['scale'],
                         [1.0, 0.25, 0.25])

    def test_no_axes_writes_paths_only(self):
        ms = zu.multiscales_metadata([], 2)
        self.assertNotIn('axes', ms[0])
        self.assertEqual(ms[0]['datasets'], [{'path': '0'}, {'path': '1'}])


if __name__ == "__main__":
    unittest.main()
