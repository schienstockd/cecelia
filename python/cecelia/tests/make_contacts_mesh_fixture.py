"""Regenerate the `test-data/projects/CONTMESH/` fixture for the mesh contact route (#1772).

A tiny SYNTHETIC still image + its label store + the measured labelProps — the one committed fixture
with a LABEL ZARR, so the mesh route (`spatialAnalysis.contactsMeshes`, which meshes the label masks
in Python) can run end to end from the Julia suite. Run it with `pixi run python` after any change to
the store layout: `pixi run python python/cecelia/tests/make_contacts_mesh_fixture.py`.

Not a TestCase (the discovery pattern is `test_*.py`) — a generator for the fixture `runtests.jl` reads.

Layout (one segmentation, `seg`; 1 µm x/y, 2 µm z — deliberately anisotropic so a scale mix-up
moves the distances): six 4x4x2-voxel cubes in two rows. Channel 0 encodes the population, so
gates on `mean_intensity_0` pick them:

  row y0:  A  (label 1, ch0=100)  near (label 2, ch0=200, 2 px right)  far (label 3, ch0=300, 14 px right)
  row y1:  A  (label 4)           near (label 5)                       far (label 6)

Every A cell's nearest `near` surface is ~2 µm away and its nearest `far` one ~14 µm, so per-target
columns differ, a 5 µm threshold puts A in contact with `near` only, and the pooled column equals the
`near` one.
"""
import os
import shutil

import numpy as np
import ome_types

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils

_ROOT = os.environ.get("CECELIA_TEST_DATA") or os.path.normpath(
    os.path.join(os.path.dirname(__file__), "..", "..", "..", "test-data", "projects"))
PROJ, UID, VN = "CONTMESH", "CtMsh1", "seg"
DATA_DIR = os.path.join(_ROOT, PROJ, "0", UID)
IMG_DIR = os.path.join(_ROOT, PROJ, "1", UID)
IM_PATH = os.path.join(DATA_DIR, "ccidImage.ome.zarr")
LABEL_PATH = os.path.join(IMG_DIR, "labels", f"{VN}.zarr")

C, Z, Y, X = 2, 4, 16, 32
XML = f'''<?xml version="1.0" encoding="UTF-8"?>
<OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0" Name="contacts">
<Pixels ID="Pixels:0" DimensionOrder="XYZCT" Type="uint16" SizeT="1" SizeC="{C}" SizeZ="{Z}"
 SizeY="{Y}" SizeX="{X}" PhysicalSizeX="1.0" PhysicalSizeY="1.0" PhysicalSizeZ="2.0"
 PhysicalSizeXUnit="µm" PhysicalSizeYUnit="µm" PhysicalSizeZUnit="µm">
<Channel ID="Channel:0:0" Name="pop" SamplesPerPixel="1"/><Channel ID="Channel:0:1" Name="flat" SamplesPerPixel="1"/>
</Pixels></Image></OME>'''

# (label, ch0 intensity, x0) per cube; rows at y = 2 and y = 10, z = 1..2
CELLS = [(1, 100, 2), (2, 200, 8), (3, 300, 20)]
ROWS = [(0, 2), (3, 10)]          # (label offset, y0)


def main():
    for d in (DATA_DIR, os.path.join(IMG_DIR, "labels"), os.path.join(IMG_DIR, "labelProps")):
        shutil.rmtree(d, ignore_errors=True)
    os.makedirs(DATA_DIR); os.makedirs(os.path.join(IMG_DIR, "labels"))

    labels = np.zeros((Z, Y, X), np.uint16)
    im = np.full((C, Z, Y, X), 10, np.uint16)
    for off, y0 in ROWS:
        for lab, val, x0 in CELLS:
            labels[1:3, y0:y0 + 4, x0:x0 + 4] = lab + off
            im[0, 1:3, y0:y0 + 4, x0:x0 + 4] = val
            im[1, 1:3, y0:y0 + 4, x0:x0 + 4] = 50

    omexml = ome_types.from_xml(XML)
    du = DimUtils(omexml, use_channel_axis=True)
    du.calc_image_dimensions(list(im.shape))
    g, l0, ch = zarr_utils.open_multiscales_for_writing(IM_PATH, im.shape, np.uint16, du, nscales=1)
    l0[:] = im
    ome_xml_utils.save_meta_in_zarr(IM_PATH, omexml=omexml)

    _, ll, _ = zarr_utils.open_multiscales_for_writing(LABEL_PATH, labels.shape, labels.dtype, None,
                                                        axes=["Z", "Y", "X"], kind="labels")
    ll[:] = labels

    # the labelProps table is a REAL measure run over the two stores above
    import importlib.util
    runner = os.path.normpath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "app", "src",
                                           "tasks", "segment", "measure_labels_run.py"))
    spec = importlib.util.spec_from_file_location("measure_labels_run", runner)
    mod = importlib.util.module_from_spec(spec); spec.loader.exec_module(mod)
    mod.run({"imPath": IM_PATH, "taskDir": IMG_DIR, "outputValueName": VN,
             "labelDir": os.path.join(IMG_DIR, "labels"), "labelFiles": [f"{VN}.zarr"],
             "intensityMeasure": "mean", "gaussianFilter": 0, "extendedMeasures": False,
             "nThreads": 1})


if __name__ == "__main__":
    main()
