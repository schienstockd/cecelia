"""Unit tests for the shared OME-ZARR / OME-XML readers (cecelia.utils.zarr_utils +
cecelia.utils.ome_xml_utils).

These readers used to live as private copies inside the napari bridge (napari/napari_bridge.py);
they were consolidated here so images are opened and their geometry read ONE way across the bridge,
the analysis pipeline and coastal. This is their first unit coverage — it pins:
  - series_base: structural bioformats2raw-series-wrapper detection (nested `0/` vs flat root),
  - read_axes / read_scale: NGFF axes + coordinateTransformations, with the OME-XML scale fallback,
  - open_as_zarr: a read-only multiscale open on both layouts,
  - ox.load_ome_xml + read_pixel_unit / read_scale_from_ome_xml / read_time_increment.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import unittest

import numpy as np
import tifffile
import zarr

import cecelia.utils.zarr_utils as zu
import cecelia.utils.ome_xml_utils as ox

AXES = ("t", "c", "z", "y", "x")
SHAPE = (1, 1, 4, 8, 8)
SCALE = [1.0, 1.0, 2.0, 0.5, 0.5]


def _multiscales(scale=SCALE, with_scale=True):
    ds = {"path": "0"}
    if with_scale:
        ds["coordinateTransformations"] = [{"type": "scale", "scale": list(scale)}]
    return [{"axes": [{"name": a} for a in AXES], "datasets": [ds]}]


def _make_flat_store(path, with_scale=True):
    """Flat store (create_multiscales / segmentation_utils layout): multiscales + numeric arrays at root."""
    g = zarr.open_group(path, mode="w", zarr_format=2)
    g.attrs["multiscales"] = _multiscales(with_scale=with_scale)
    g.create_array("0", data=np.zeros(SHAPE, dtype=np.uint16), chunks=SHAPE)
    return path


def _make_nested_store(path):
    """bioformats2raw layout: a series group at `0/` holds the multiscales; arrays at `0/0`."""
    root = zarr.open_group(path, mode="w", zarr_format=2)
    series = root.create_group("0")
    series.attrs["multiscales"] = _multiscales()
    series.create_array("0", data=np.zeros(SHAPE, dtype=np.uint16), chunks=SHAPE)
    return path


_OME_XML = """<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06">
  <Image ID="Image:0" Name="test">
    <Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16"
            SizeX="8" SizeY="8" SizeZ="4" SizeC="1" SizeT="1"
            PhysicalSizeX="0.5" PhysicalSizeXUnit="µm"
            PhysicalSizeY="0.5" PhysicalSizeYUnit="µm"
            PhysicalSizeZ="2.0" PhysicalSizeZUnit="µm"
            TimeIncrement="30.0" TimeIncrementUnit="s">
      <Channel ID="Channel:0" SamplesPerPixel="1"/>
      <MetadataOnly/>
    </Pixels>
  </Image>
</OME>
"""


def _write_ome_xml(store):
    ome_dir = os.path.join(store, "OME")
    os.makedirs(ome_dir, exist_ok=True)
    with open(os.path.join(ome_dir, "METADATA.ome.xml"), "w") as f:
        f.write(_OME_XML)


class SeriesBaseTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_flat_returns_root(self):
        p = _make_flat_store(os.path.join(self.d, "flat.ome.zarr"))
        # a flat store also has a `0/` dir (its level-0 array) but it carries NO multiscales attr,
        # so series_base must NOT step into it
        self.assertEqual(zu.series_base(p), p)

    def test_nested_steps_into_series(self):
        p = _make_nested_store(os.path.join(self.d, "nested.ome.zarr"))
        self.assertEqual(zu.series_base(p), os.path.join(p, "0"))


class NgffGeometryTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_axes_and_scale_flat(self):
        p = _make_flat_store(os.path.join(self.d, "flat.ome.zarr"))
        self.assertEqual(zu.read_axes(p), list(AXES))
        self.assertEqual(zu.read_scale(p), SCALE)

    def test_axes_and_scale_nested(self):
        p = _make_nested_store(os.path.join(self.d, "nested.ome.zarr"))
        self.assertEqual(zu.read_axes(p), list(AXES))
        self.assertEqual(zu.read_scale(p), SCALE)

    def test_scale_falls_back_to_ome_xml(self):
        # no coordinateTransformations in the NGFF metadata → read_scale reads OME-XML physical sizes
        p = _make_flat_store(os.path.join(self.d, "noscale.ome.zarr"), with_scale=False)
        _write_ome_xml(p)
        self.assertEqual(zu.read_scale(p), SCALE)   # t,c → 1.0; z,y,x from OME-XML

    def test_open_as_zarr_readonly_both_layouts(self):
        for maker, name in ((_make_flat_store, "flat"), (_make_nested_store, "nested")):
            p = maker(os.path.join(self.d, f"{name}.ome.zarr"))
            data, _ = zu.open_as_zarr(p, as_dask=True)
            self.assertEqual(len(data), 1)
            self.assertEqual(tuple(data[0].shape), SHAPE)


class OmeXmlReaderTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()
        self.store = _make_flat_store(os.path.join(self.d, "img.ome.zarr"))
        _write_ome_xml(self.store)

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_load_ome_xml_found_and_missing(self):
        self.assertIsNotNone(ox.load_ome_xml(self.store))
        self.assertIsNone(ox.load_ome_xml(os.path.join(self.d, "does-not-exist")))

    def test_pixel_unit_preserves_micron(self):
        self.assertEqual(ox.read_pixel_unit(self.store), "µm")   # NOT normalised to 'um'
        self.assertEqual(ox.read_pixel_unit(os.path.join(self.d, "nope")), "µm")  # default

    def test_scale_from_ome_xml_in_axis_order(self):
        self.assertEqual(ox.read_scale_from_ome_xml(self.store, AXES), SCALE)

    def test_time_increment_seconds(self):
        self.assertEqual(ox.read_time_increment(self.store), 30.0)


class ImageJMetadataTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def test_reads_imagej_calibration(self):
        p = os.path.join(self.d, "stack.tif")
        tifffile.imwrite(p, np.zeros((4, 8, 8), dtype=np.uint16), imagej=True,
                         metadata={"unit": "nm", "spacing": 0.5})
        meta = ox.read_imagej_metadata(p)
        self.assertIsNotNone(meta)
        self.assertEqual(meta.get("unit"), "nm")
        self.assertAlmostEqual(meta.get("spacing"), 0.5)

    def test_plain_tiff_has_no_imagej_metadata(self):
        p = os.path.join(self.d, "plain.tif")
        tifffile.imwrite(p, np.zeros((8, 8), dtype=np.uint16))   # not imagej=True
        self.assertIsNone(ox.read_imagej_metadata(p))

    def test_non_tiff_raises(self):
        # a soft skip is the caller's job (it wraps in try/except) — the helper surfaces the error
        with self.assertRaises(Exception):
            ox.read_imagej_metadata(os.path.join(self.d, "nope.tif"))


if __name__ == "__main__":
    unittest.main()


class OmeXmlStagedWriteTest(unittest.TestCase):
    """OME-XML must survive being written into a STAGED store.

    Regression: `zarr_utils.staged_store` writes to `<store>.ome.zarr.partial` and renames on
    success, but the OME writers gated on `os.path.splitext(path)[1] == '.zarr'` — so every
    write into a staging path silently did nothing. Because `save_meta_in_zarr` created the
    `OME/` directory first, the result was an empty `OME/` dir, no error, and a task log
    reporting success; the sidecar (pixel sizes, channel names, TimeIncrement) was simply gone
    from every imported/corrected store.
    """

    def setUp(self):
        self.d = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def _store(self, name):
        """A minimal zarr-looking store directory."""
        p = os.path.join(self.d, name)
        os.makedirs(p, exist_ok=True)
        with open(os.path.join(p, ".zgroup"), "w") as f:
            f.write('{"zarr_format": 2}')
        return p

    def test_writes_into_a_staging_path(self):
        staged = self._store("img.ome.zarr" + zu.STAGING_SUFFIX)
        ox.save_meta_in_zarr(staged, omexml=ox.from_xml(_OME_XML))
        self.assertTrue(os.path.exists(os.path.join(staged, "OME", "METADATA.ome.xml")),
                        "OME-XML was skipped because the staging path does not end in .zarr")

    def test_survives_the_rename_onto_the_final_path(self):
        """End to end through staged_store — the shape the import task actually uses."""
        final = os.path.join(self.d, "img.ome.zarr")
        with zu.staged_store(final) as staging:
            os.makedirs(staging, exist_ok=True)
            with open(os.path.join(staging, ".zgroup"), "w") as f:
                f.write('{"zarr_format": 2}')
            ox.save_meta_in_zarr(staging, omexml=ox.from_xml(_OME_XML))
        self.assertTrue(os.path.exists(os.path.join(final, "OME", "METADATA.ome.xml")))
        # and it round-trips: the value the timestamp overlay reads
        self.assertEqual(ox.read_time_increment(final), 30.0)

    def test_change_pixel_type_also_works_on_a_staging_path(self):
        """The same guard was copy-pasted here, so it no-opped too."""
        staged = self._store("img.ome.zarr" + zu.STAGING_SUFFIX)
        ox.save_meta_in_zarr(staged, omexml=ox.from_xml(_OME_XML))
        ox.change_pixel_type(staged, "uint8")
        self.assertEqual(ox.parse_meta(staged).images[0].pixels.type.value, "uint8")

    def test_a_tiff_is_still_not_a_zarr_store(self):
        """Non-store callers must keep no-opping rather than growing an OME/ dir."""
        tiff = os.path.join(self.d, "plain.tiff")
        with open(tiff, "w") as f:
            f.write("not a store")
        self.assertFalse(ox.is_zarr_store(tiff))
        ox.write_ome_xml(tiff, _OME_XML)
        self.assertFalse(os.path.exists(os.path.join(tiff, "OME")))

    def test_a_skipped_write_leaves_no_empty_ome_dir(self):
        """An empty OME/ reads as 'half written'; a skip must leave nothing behind."""
        plain = os.path.join(self.d, "notastore.txt")
        with open(plain, "w") as f:
            f.write("x")
        ox.save_meta_in_zarr(plain, omexml=ox.from_xml(_OME_XML))
        self.assertFalse(os.path.exists(os.path.join(plain, "OME")))
