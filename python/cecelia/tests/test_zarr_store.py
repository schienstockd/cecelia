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


_OME_XML_TIMELAPSE = _OME_XML.replace(
    'SizeT="1"', 'SizeT="3"').replace(
    '<Channel ID="Channel:0:0"', '{extra}<Channel ID="Channel:0:0"')


class TimeAxisUnitTest(unittest.TestCase):
    """A `unit` on the t axis is a CLAIM that the frame interval is known.

    Every writer falls the t scale back to 1.0 when there is no `TimeIncrement`, and
    `im_time_increment_unit()` returns 's' by default whether or not one was found — so stamping
    the unit unconditionally turned "we don't know" into "1 second per frame". Both readers gate on
    the unit being present (`read_time_increment` here, `read_ome_metadata` on the Julia side), and
    that gate only works if the writer keeps the placeholder unit-less.
    """

    def _write(self, xml, shape, dims):
        import ome_types
        from cecelia.utils.dim_utils import DimUtils
        du = DimUtils(ome_types.from_xml(xml), use_channel_axis=True)
        du.calc_image_dimensions(shape)
        d = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, d, True)
        path = os.path.join(d, "img.ome.zarr")
        zu.create_multiscales(da.from_array(np.zeros(shape, dtype=np.uint16), chunks=shape),
                              path, dim_utils=du, nscales=1)
        ms = zarr.open_group(path, mode="r").attrs["multiscales"][0]
        return path, {a["name"]: a for a in ms["axes"]}

    def test_unknown_interval_leaves_the_t_axis_unit_less(self):
        xml = _OME_XML_TIMELAPSE.format(extra="")           # timelapse, but no TimeIncrement
        path, axes = self._write(xml, (3, 2, 4, 3), "TCYX")
        self.assertIn("t", axes)
        self.assertNotIn("unit", axes["t"])
        # …so the resolver reports "unknown" rather than the 1.0 placeholder
        self.assertIsNone(zu.read_time_increment(path))

    def test_known_interval_writes_the_unit_and_is_read_back(self):
        xml = _OME_XML_TIMELAPSE.format(extra="").replace(
            'SizeT="3"', 'SizeT="3" TimeIncrement="10.0" TimeIncrementUnit="s"')
        path, axes = self._write(xml, (3, 2, 4, 3), "TCYX")
        self.assertEqual(axes["t"].get("unit"), "second")
        self.assertEqual(zu.read_time_increment(path), 10.0)


class CreateMultiscalesAxesOverrideTest(unittest.TestCase):
    """`axes=` lets a caller declare the axes of the array it is actually storing.

    Needed by any writer whose store is not the source image's shape — the branch-labels store has
    no channel axis and can lose Z (flattenBranching) or T (integrateTime). Without the override it
    inherited the image's axes verbatim, tagging a 3-axis array `t,c,z,y,x` and handing Y the Z
    step. See docs/todo/SPATIAL_ANISOTROPY_PLAN.md finding A8.

    The load-bearing assertion is that scale is mapped by axis NAME, not zipped positionally — a
    positional zip is precisely how the dropped axis shifts everything after it.
    """

    def _dim_utils(self):
        import ome_types
        from cecelia.utils.dim_utils import DimUtils
        du = DimUtils(ome_types.from_xml(_OME_XML), use_channel_axis=True)
        du.calc_image_dimensions((2, 4, 3))       # C,Y,X
        return du

    def test_dropping_the_channel_axis_keeps_yx_scales(self):
        du = self._dim_utils()
        arr = np.zeros((4, 3), dtype=np.uint16)   # the C axis is gone from the STORE
        d = tempfile.mkdtemp()
        try:
            path = os.path.join(d, "labels.zarr")
            zu.create_multiscales(arr, path, dim_utils=du, axes=["Y", "X"],
                                  im_chunks=(4, 3), nscales=1)
            ms = zarr.open_group(path, mode="r").attrs["multiscales"][0]
            self.assertEqual([a["name"] for a in ms["axes"]], ["y", "x"])
            # Y and X keep THEIR OWN 0.5 — a positional zip would have handed Y the C entry (1.0).
            self.assertEqual(ms["datasets"][0]["coordinateTransformations"][0]["scale"], [0.5, 0.5])
        finally:
            shutil.rmtree(d, ignore_errors=True)

    def test_no_override_is_unchanged(self):
        """Every existing caller passes no `axes` and must be completely unaffected."""
        du = self._dim_utils()
        arr = np.zeros((2, 4, 3), dtype=np.uint16)
        d = tempfile.mkdtemp()
        try:
            path = os.path.join(d, "img.zarr")
            zu.create_multiscales(arr, path, dim_utils=du, im_chunks=(1, 4, 3), nscales=1)
            ms = zarr.open_group(path, mode="r").attrs["multiscales"][0]
            self.assertEqual([a["name"] for a in ms["axes"]], ["c", "y", "x"])
            self.assertEqual(ms["datasets"][0]["coordinateTransformations"][0]["scale"],
                             [1.0, 0.5, 0.5])
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

    def test_per_plane_write_into_block_via_reshape(self):
        # cellpose_correct writes a 2-D (Y,X) plane into a (1,1,1,Y,X) block of an on-disk store.
        # zarr's orthogonal write does NOT broadcast a lower-rank value into the selection the way
        # numpy assignment does (that was the IndexError), so the value must be reshaped to the block.
        d = tempfile.mkdtemp()
        try:
            p = os.path.join(d, "pp.zarr")
            _, level0, _ = zu.open_multiscales_for_writing(p, (2, 2, 3, 8, 6), np.uint8, None, nscales=1)
            rng = np.random.default_rng(5)
            plane = rng.integers(1, 255, size=(8, 6), dtype=np.uint8)          # 2-D (Y,X)
            sl = (slice(0, 1), slice(1, 2), slice(2, 3), slice(None), slice(None))
            block = tuple(len(range(*s.indices(dd))) for s, dd in zip(sl, level0.shape))
            level0[sl] = np.reshape(plane, block)                              # the fix
            back = zarr.open_group(p, mode="r")["0"][0, 1, 2]                  # the written plane
            self.assertTrue(np.array_equal(back, plane), "per-plane block write corrupted values")
        finally:
            shutil.rmtree(d, ignore_errors=True)

    def test_writer_forces_native_byteorder(self):
        # Big-endian source (e.g. >u2 from bioformats2raw) must be stored NATIVE — big-endian
        # mis-renders in napari/OpenGL on little-endian systems. Both writers coerce; values kept.
        be = (np.arange(2 * 3 * 4, dtype=np.uint16).reshape(2, 3, 4)).astype(">u2")
        self.assertFalse(np.dtype(be.dtype).isnative)   # sanity: the source really is big-endian
        d = tempfile.mkdtemp()
        try:
            p1 = os.path.join(d, "stream.zarr")
            _, level0, _ = zu.open_multiscales_for_writing(p1, be.shape, be.dtype, None, nscales=1)
            level0[:] = be
            g1 = zarr.open_group(p1, mode="r")["0"]
            self.assertTrue(np.dtype(g1.dtype).isnative, "streaming writer left non-native dtype")
            self.assertTrue(np.array_equal(g1[:], be), "byte-order coercion corrupted values")

            p2 = os.path.join(d, "ms.zarr")
            zu.create_multiscales(da.from_array(be, chunks=be.shape), p2, nscales=1)
            g2 = zarr.open_group(p2, mode="r")["0"]
            self.assertTrue(np.dtype(g2.dtype).isnative, "create_multiscales left non-native dtype")
            self.assertTrue(np.array_equal(g2[:], be))
        finally:
            shutil.rmtree(d, ignore_errors=True)

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

    def test_axes_carry_ngff_type_and_unit(self):
        """Without `type`/`unit` a reader gets a bare number and cannot tell seconds from
        micrometres — napari then labels the time axis with the spatial unit."""
        ms = zu.multiscales_metadata(
            ['T', 'C', 'Z', 'Y', 'X'], 1,
            scale_for_axis={'T': 10.0, 'C': 1.0, 'Z': 5.0, 'Y': 0.5, 'X': 0.5},
            unit_for_axis={'T': 's', 'Z': 'um', 'Y': 'um', 'X': 'um'})
        axes = ms[0]['axes']
        self.assertEqual([a['type'] for a in axes],
                         ['time', 'channel', 'space', 'space', 'space'])
        # abbreviations map to the UDUNITS names napari expects; channel takes no unit
        self.assertEqual(axes[0]['unit'], 'second')
        self.assertEqual(axes[2]['unit'], 'micrometer')
        self.assertNotIn('unit', axes[1])
        # the frame interval reaches the transform rather than defaulting to 1.0
        self.assertEqual(ms[0]['datasets'][0]['coordinateTransformations'][0]['scale'][0], 10.0)

    def test_axis_entry_builder_is_shared_with_set_ngff_axes(self):
        """One builder decides an axis entry's shape, so a migrated store and a freshly
        written one describe their axes identically (cecelia rule: no duplicated logic)."""
        self.assertEqual(zu.ngff_axis_entry('t', 's'),
                         {'name': 't', 'type': 'time', 'unit': 'second'})
        self.assertEqual(zu.ngff_axis_entry('c'), {'name': 'c', 'type': 'channel'})


def _write_store(path, value, nscales=2, shape=(2, 8, 6)):
    """Write a small complete 2-level store filled with `value`; return the level-0 array."""
    arr = np.full(shape, value, dtype=np.uint32)
    g = zarr.open_group(path, mode='w', zarr_format=2)
    g.attrs['multiscales'] = zu.multiscales_metadata(
        ['T', 'Y', 'X'], nscales, scale_for_axis={'T': 1.0, 'Y': 1.0, 'X': 1.0})
    g.create_array('0', shape=shape, chunks=(1,) + shape[1:], dtype=np.uint32)[:] = arr
    for lvl in range(1, nscales):
        ds = arr[:, ::2 ** lvl, ::2 ** lvl]
        g.create_array(str(lvl), shape=ds.shape, chunks=(1,) + ds.shape[1:],
                       dtype=np.uint32)[:] = ds
    return arr


def _levels(path):
    return set(zarr.open_group(path, mode='r').array_keys())


class StagedStoreTest(unittest.TestCase):
    """`staged_store` — a store write must never leave a REGISTERED store partial.

    See docs/SEGMENTATION.md → *Stores are written staged, never in place*.

    The failure this pins used to be silent: the writers opened the final path in mode 'w' (after an
    `rmtree`), so re-running an already-registered value_name and then cancelling left ccid.json
    advertising a store missing most of its frames. On a single-level store the unwritten frames read
    as zeros with no error at all, and measurement/tracking happily consumed them.
    """

    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.final = os.path.join(self.d, 'labels', 'X.zarr')
        os.makedirs(os.path.dirname(self.final), exist_ok=True)

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_clean_write_lands_at_final_path_and_leaves_no_staging(self):
        with zu.staged_store(self.final) as staging:
            self.assertNotEqual(staging, self.final, 'must not write the final path directly')
            self.assertFalse(os.path.exists(self.final), 'final path written before completion')
            expected = _write_store(staging, 7)

        self.assertTrue(os.path.isdir(self.final))
        self.assertFalse(os.path.exists(self.final + zu.STAGING_SUFFIX))
        self.assertFalse(os.path.exists(self.final + zu.SUPERSEDED_SUFFIX))
        self.assertTrue(np.array_equal(zarr.open_group(self.final, mode='r')['0'][:], expected))

    def test_killed_run_leaves_a_registered_store_complete(self):
        # This IS the bug. Enter the context and abandon it without unwinding — a SIGKILL runs no
        # `finally`, so anything that survives only because of cleanup code would be a false pass.
        expected = _write_store(self.final, 3, nscales=2)

        cm = zu.staged_store(self.final)
        staging = cm.__enter__()
        _write_store(staging, 9, nscales=1)          # a half-finished re-run: level 0 only
        del cm                                        # no __exit__, no promote

        self.assertEqual(_levels(self.final), {'0', '1'}, 'registered store lost a pyramid level')
        self.assertTrue(np.array_equal(zarr.open_group(self.final, mode='r')['0'][:], expected),
                        'registered store was overwritten by an incomplete run')

    def test_exception_drops_staging_and_keeps_previous_store(self):
        expected = _write_store(self.final, 4)

        with self.assertRaises(RuntimeError):
            with zu.staged_store(self.final) as staging:
                _write_store(staging, 9, nscales=1)
                raise RuntimeError('task failed')

        self.assertFalse(os.path.exists(self.final + zu.STAGING_SUFFIX))
        self.assertTrue(np.array_equal(zarr.open_group(self.final, mode='r')['0'][:], expected))

    def test_stale_staging_from_a_previous_kill_is_cleaned_on_entry(self):
        stale = self.final + zu.STAGING_SUFFIX
        _write_store(stale, 1, nscales=1)

        with zu.staged_store(self.final) as staging:
            self.assertEqual(staging, stale)
            self.assertFalse(os.path.exists(staging), 'stale staging debris was not cleared')
            _write_store(staging, 5)

        self.assertTrue(np.array_equal(
            zarr.open_group(self.final, mode='r')['0'][:], np.full((2, 8, 6), 5, dtype=np.uint32)))

    def test_promote_replaces_a_previous_store_entirely(self):
        _write_store(self.final, 4, nscales=2)
        with zu.staged_store(self.final) as staging:
            _write_store(staging, 8, nscales=1)      # fewer levels than the store it replaces

        # No level '1' left over from the old store — a rename, not a merge.
        self.assertEqual(_levels(self.final), {'0'})
        self.assertTrue(np.array_equal(
            zarr.open_group(self.final, mode='r')['0'][:], np.full((2, 8, 6), 8, dtype=np.uint32)))

    def test_promote_without_a_staging_store_raises(self):
        with self.assertRaises(FileNotFoundError):
            zu.promote_store(self.final + zu.STAGING_SUFFIX, self.final)


if __name__ == "__main__":
    unittest.main()
