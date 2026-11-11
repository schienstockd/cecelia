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


class _DimsZStack(_Dims2D):
    def __init__(self, *a, **k):
        self.im_dim_order = ["C", "Z", "Y", "X"]
        self.im_dim = [2, 12, 40000, 40000]


def _patched(mod, dims=_Dims2D, label_axes=None):
    patches = [mock.patch.object(mod, "DimUtils", dims),
               mock.patch.object(mod.ome_xml_utils, "parse_meta", lambda _p: None),
               mock.patch.object(mod.zarr_utils, "open_as_zarr",
                                 lambda _p, **k: ([_NoRead()], None))]
    if label_axes is not None:
        # the axes the label store declares — a ridges Z-MIP of a Z stack is written (T)YX
        patches.append(mock.patch.object(mod.zarr_utils, "read_axes", lambda _p: label_axes))
    return patches


class TestRequireVolume(unittest.TestCase):
    def test_3d_passes(self):
        mesh_utils.require_volume(3, "t")

    def test_2d_raises(self):
        with self.assertRaisesRegex(ValueError, "3D"):
            mesh_utils.require_volume(2, "t")


class TestRunnersReject2D(unittest.TestCase):
    def _run(self, name, params, **kw):
        mod = _load(name)
        patches = _patched(mod, **kw)
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

    def test_2d_labels_on_a_z_stack(self):
        """A Z-projected label set (ridges `perZ` off) on a Z stack: the IMAGE is a volume, the labels
        are not. The guard must read the store's axes, not the image's."""
        z = dict(dims=_DimsZStack, label_axes=["y", "x"])
        self._run("cell_aggregates_mesh_run",
                  {"imPath": "im", "labelPath": "lab", "labels": [1, 2],
                   "physicalSizes": [2.0, 1.0, 1.0], "propsPath": "props"}, **z)
        self._run("cell_contacts_mesh_run",
                  {"imPath": "im", "aLabelPath": "a", "bLabelPath": "b", "aLabels": [1],
                   "targets": [{"target": "x", "bLabels": [2]}],
                   "physicalSizes": [2.0, 1.0, 1.0], "propsPath": "props"}, **z)

    def test_contacts_refuses_a_and_b_with_different_axes(self):
        """3D cells (A) against a Z-projected fibre set (B): B would be read with A's axes."""
        mod = _load("cell_contacts_mesh_run")
        patches = _patched(mod, dims=_DimsZStack)
        patches.append(mock.patch.object(
            mod.zarr_utils, "read_axes", lambda p: ["z", "y", "x"] if p == "a" else ["y", "x"]))
        _NoRead3 = type("_NoRead3", (_NoRead,), {"ndim": 3})
        stores = {"im": _NoRead(), "a": _NoRead3(), "b": _NoRead()}
        patches[2] = mock.patch.object(mod.zarr_utils, "open_as_zarr",
                                       lambda p, **k: ([stores[p]], None))
        for p in patches:
            p.start()
        try:
            with self.assertRaisesRegex(ValueError, "differ in axes"):
                mod.run({"imPath": "im", "aLabelPath": "a", "bLabelPath": "b", "aLabels": [1],
                         "targets": [{"target": "x", "bLabels": [2]}],
                         "physicalSizes": [2.0, 1.0, 1.0], "propsPath": "props"})
        finally:
            for p in patches:
                p.stop()


if __name__ == "__main__":
    unittest.main()
