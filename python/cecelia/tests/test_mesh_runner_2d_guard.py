"""The mesh runners (contactsMeshes / aggregatesMeshes) refuse a 2D image before reading any label frame.

Marching cubes builds no mesh from a 2D frame, so on a 2D tile scan the runners used to read each
whole label frame into RAM (an OOM on a large scan) and then return nothing — aggregates even wrote
all-zero columns. `mesh_utils.require_volume` makes that fail loudly up front."""
import importlib.util
import os
import unittest
from unittest import mock

from cecelia.utils import mesh_utils

_DIR = os.path.normpath(os.path.join(os.path.dirname(__file__), "..", "..", "..",
                                     "app", "src", "tasks", "spatialAnalysis"))


def _load(name):
    spec = importlib.util.spec_from_file_location(name, os.path.join(_DIR, f"{name}.py"))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _NoRead:
    """A label array that fails the test if anything reads it."""
    shape = (40000, 40000)
    ndim = 2

    def __getitem__(self, _):
        raise AssertionError("label store was read")

    def __array__(self, *a, **k):
        raise AssertionError("label store was read")


class _Dims2D:
    def __init__(self, *a, **k):
        self.im_dim_order = ["C", "Y", "X"]
        self.im_dim = [2, 40000, 40000]

    def calc_image_dimensions(self, _shape):
        pass


def _patched(mod):
    return [mock.patch.object(mod, "DimUtils", _Dims2D),
            mock.patch.object(mod.ome_xml_utils, "parse_meta", lambda _p: None),
            mock.patch.object(mod.zarr_utils, "open_as_zarr", lambda _p, **k: ([_NoRead()], None))]


class TestRequireVolume(unittest.TestCase):
    def test_3d_passes(self):
        mesh_utils.require_volume(3, "t")

    def test_2d_raises(self):
        with self.assertRaisesRegex(ValueError, "3D"):
            mesh_utils.require_volume(2, "t")


class TestRunnersReject2D(unittest.TestCase):
    def _run(self, name, params):
        mod = _load(name)
        patches = _patched(mod)
        for p in patches:
            p.start()
        try:
            with self.assertRaisesRegex(ValueError, "3D"):
                mod.run(params)
        finally:
            for p in patches:
                p.stop()

    def test_aggregates_meshes(self):
        self._run("cell_aggregates_mesh_run",
                  {"imPath": "im", "labelPath": "lab", "labels": [1, 2],
                   "physicalSizes": [1.0, 1.0], "propsPath": "props"})

    def test_contacts_meshes(self):
        self._run("cell_contacts_mesh_run",
                  {"imPath": "im", "aLabelPath": "a", "bLabelPath": "b", "aLabels": [1],
                   "targets": [{"target": "x", "bLabels": [2]}],
                   "physicalSizes": [1.0, 1.0], "propsPath": "props"})


if __name__ == "__main__":
    unittest.main()
