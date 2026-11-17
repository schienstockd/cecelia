"""Regenerate the neighbour-graph fixtures under `test-data/projects/testpr/1/KDIeEm/spatialGraph/`.

Real builder output, not synthetic: `spatial_utils.build_pooled_image_graph` + `save_graph` — exactly
what `spatialAnalysis.cellNeighbours` runs — over the committed `labelProps/B.h5ad` (3D, 20 frames),
its cells on frames 0–2 only (~200 nodes: the fixture size cap). Run with
`pixi run python python/cecelia/tests/make_spatial_graph_fixture.py` after any change to the graph's
on-disk layout.

  net.h5ad     per timepoint (`_t` in obs): every edge within one frame
  pooled.h5ad  frames pooled: most edges join a cell to its neighbours on other frames
  knn.h5ad     per timepoint, kNN (k = 3): DIRECTED — a cell's nearest need not list it back

Not a TestCase (the discovery pattern is `test_*.py`) — a generator for the fixtures the Julia API
suite reads (`api/test/suite/spatial_graph_lines.jl`).
"""
import os

from cecelia.utils import spatial_utils

# <repo>/test-data/projects, or CECELIA_TEST_DATA — the same resolver as make_aniso_fixture.py.
_ROOT = os.environ.get("CECELIA_TEST_DATA") or os.path.normpath(
    os.path.join(os.path.dirname(__file__), "..", "..", "..", "test-data", "projects"))
IMG = os.path.join(_ROOT, "testpr", "1", "KDIeEm")
# the fixture image has no calibration, so `img_physical_sizes` answers 1 µm/px on every axis
PHYS = [1.0, 1.0, 1.0]
RADIUS = 60.0
FRAMES = 3


def _labels():
    from cecelia.utils.label_props_utils import LabelPropsView
    view = LabelPropsView(os.path.join(IMG, "labelProps", "B.h5ad")).only_centroid_cols()
    d = view.as_df(); view.close()
    return d.loc[d["centroid_t"] < FRAMES, "label"].astype(int).tolist()


def _write(name, per_timepoint, method="delaunay", n_neighs=6):
    segs = [{"valueName": "B", "propsPath": os.path.join(IMG, "labelProps", "B.h5ad"), "labels": _labels()}]
    adata, obs = spatial_utils.build_pooled_image_graph(
        segs, PHYS, method=method, radius=RADIUS, n_neighs=n_neighs, per_timepoint=per_timepoint)
    meta = {"method": method, "radius": RADIUS, "nNeighbours": n_neighs, "perTimepoint": per_timepoint,
            "valueNames": ["B"]}
    path = os.path.join(IMG, "spatialGraph", f"{name}.h5ad")
    spatial_utils.save_graph(adata, obs, path, meta=meta)
    conn = adata.obsp["spatial_connectivities"]
    # undirected edges: a pair linked in EITHER direction, once (the tests' expected counts)
    sym = ((conn + conn.T) > 0).astype(int)
    sym.setdiag(0); sym.eliminate_zeros()
    print(path, adata.n_obs, "nodes", conn.nnz, "stored entries", sym.nnz // 2, "edges")


if __name__ == "__main__":
    _write("net", True)
    _write("pooled", False)
    _write("knn", True, method="knn", n_neighs=3)
