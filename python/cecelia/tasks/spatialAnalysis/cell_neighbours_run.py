"""
spatialAnalysis.cellNeighbours (image-scope) — Python runner.

Builds a per-cell spatial neighbour graph with squidpy and writes it as a `{vn}.spatial.h5ad`
sidecar (obsp `spatial_connectivities` / `spatial_distances`, plus obsm `spatial` in physical
units). The shared substrate for region composition + neighbourhood statistics
(docs/todo/SPATIAL_REGIONS_PLAN.md, Phase 2). Re-homes the legacy `cell_neighbours.py` onto the
new boundary: membership + physical-size resolution happen in Julia (cellNeighbours.jl); this
script reads centroids through the sanctioned LabelPropsView, scales them, and runs squidpy.

Invoked by `app/src/tasks/spatialAnalysis/cellNeighbours.jl` via a params JSON. Params:
  propsPath        the segmentation's label-props .h5ad (read centroids from here)
  graphPath        where to write the graph .h5ad (a NEW file — sanctioned creation exception)
  physicalSizes    [sz, sy, sx] µm/pixel (skimage order; the last len(centroid_cols) are used)
  labels           member label IDs to restrict to, or null for every cell
  neighbourMethod  "delaunay" | "knn" | "radius"
  neighbourRadius  radius in µm (fixed-radius graph + Delaunay edge pruning)
  nNeighbours      k for the kNN graph
  qcOutPath        where to dump objective graph metrics for Julia to bank as QC
"""
import json

import numpy as np

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
from cecelia.utils.label_props_utils import LabelPropsView
import cecelia.utils.script_utils as script_utils
import cecelia.utils.spatial_utils as spatial_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    props_path = script_utils.get_param(params, "propsPath", default=None)
    graph_path = script_utils.get_param(params, "graphPath", default=None)
    phys       = np.asarray(script_utils.get_param(params, "physicalSizes", default=[1.0, 1.0, 1.0]), dtype=float)
    labels     = script_utils.get_param(params, "labels", default=None)
    method     = script_utils.get_param(params, "neighbourMethod", default="delaunay")
    radius     = float(script_utils.get_param(params, "neighbourRadius", default=30.0))
    k          = int(script_utils.get_param(params, "nNeighbours", default=6))
    qc_path    = script_utils.get_param(params, "qcOutPath", default=None)

    if props_path is None or graph_path is None:
        log.log("[ERROR] cellNeighbours: propsPath / graphPath missing")
        return

    # ── read centroids (skimage order z?,y,x) through the sanctioned reader ──
    view = LabelPropsView(props_path).only_centroid_cols()
    if labels is not None:
        view = view.filter_by_label(labels)
    df = view.as_df()
    ccols = view.centroid_columns()          # spatial cols only (temporal excluded from the graph)
    view.close()

    n = df.shape[0]
    if n == 0:
        log.log("[ERROR] cellNeighbours: no cells")
        _dump_qc(qc_path, {"nCells": 0, "nEdges": 0, "meanDegree": 0.0, "isolatedFrac": 0.0})
        return

    # ── scale pixel centroids → physical µm (the radius is in µm) ──
    coords = df[ccols].to_numpy(dtype=np.float64)
    scale = phys[-coords.shape[1]:]           # align to the number of spatial dims (drops z for 2D)
    coords = coords * scale.reshape(1, -1)

    import anndata as ad
    adata = ad.AnnData(coords.astype(np.float32))
    adata.var_names = [str(c) for c in ccols]
    adata.obs_names = [str(int(l)) for l in df["label"].to_numpy()]
    adata.obs["label"] = df["label"].to_numpy()
    adata.obsm["spatial"] = coords

    log.log(f">> neighbour graph: {n} cells, method={method}")
    spatial_utils.build_spatial_graph(adata, method=method, radius=radius, n_neighs=k)

    # ── write the graph sidecar (a NEW file — the sanctioned creation exception) ──
    adata.write_h5ad(graph_path)
    m = spatial_utils.graph_metrics(adata)
    log.log(f">> wrote {graph_path}: {m['nEdges']} edges, mean degree {m['meanDegree']:.2f}, "
            f"{int(m['isolatedFrac'] * n)} isolated")
    _dump_qc(qc_path, m)


def _dump_qc(qc_path, qc):
    if qc_path is not None:
        with open(qc_path, "w") as f:
            json.dump(qc, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
