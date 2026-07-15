"""Unit tests for the shared OME-ZARR / OME-XML readers (cecelia.utils.zarr_utils +
cecelia.utils.ome_xml_utils).

These readers used to live as private copies inside the napari bridge (napari/napari_bridge.py);
they were consolidated here so images are opened and their geometry read ONE way across the bridge,
the analysis pipeline and coastal. This is their first unit coverage — it pins:
  - series_base: structural bioformats2raw-series-wrapper detection (nested `0/` vs flat root),
  - read_axes / read_scale: NGFF axes + coordinateTransformations, with the OME-XML scale fallback,
  - open_as_zarr: a read-only multiscale open on both layouts,
  - ome_xml_utils.load_ome_xml + read_pixel_unit / read_scale_from_ome_xml / read_time_increment.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import unittest

import numpy as np
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


if __name__ == "__main__":
    unittest.main()
