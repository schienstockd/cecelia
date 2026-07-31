"""
spatialAnalysis.cellNeighbours (image-scope) — Python runner.

Builds THE spatial neighbour graph for one image, pooled across the selected segmentations, and writes
it to `spatialGraph/{suffix}.h5ad`: squidpy's obsp `spatial_connectivities` / `spatial_distances`, obsm
`spatial` (physical µm coordinates), and obs holding each node's IDENTITY (`valueName`, `label`) — and
NO population codes. Downstream analyses (`neighbourStats`, `clustRegions`) load this graph and attach
their own labelling, so the neighbourhood definition and its parameters exist in exactly one place
(docs/todo/SPATIAL_REGIONS_PLAN.md Phase 2; legacy parity: old-R `cellNeighbours` → `spatialDT`).

All of the graph construction — the centroid read through the sanctioned LabelPropsView, the µm scaling,
the cross-segmentation pooling, the per-timepoint block-diagonal mode and the squidpy call — lives in
`spatial_utils.build_pooled_image_graph`, which is also what the graph's consumers were built on. This
runner is only the task's Julia↔Python boundary.

Node resolution happens in Julia (cellNeighbours.jl). Params: segments [{valueName, propsPath, labels}]
(`labels: null` = every cell of that segmentation), graphPath, physicalSizes [sz,sy,sx], neighbourMethod,
neighbourRadius, nNeighbours, perTimepoint, qcOutPath.
"""

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
import cecelia.utils.spatial_utils as spatial_utils
from cecelia.utils.atomic_io import write_json_atomic

_EMPTY_QC = {"nCells": 0, "nEdges": 0, "meanDegree": 0.0, "isolatedFrac": 0.0}


def run(params):
    log = script_utils.get_logfile_utils(params)

    segments   = script_utils.get_param(params, "segments", default=[])
    graph_path = script_utils.get_param(params, "graphPath", default=None)
    phys       = script_utils.get_param(params, "physicalSizes", default=[1.0, 1.0, 1.0])
    method     = script_utils.get_param(params, "neighbourMethod", default="delaunay")
    radius     = float(script_utils.get_param(params, "neighbourRadius", default=30.0))
    k          = int(script_utils.get_param(params, "nNeighbours", default=6))
    per_t      = bool(script_utils.get_param(params, "perTimepoint", default=False))
    qc_path    = script_utils.get_param(params, "qcOutPath", default=None)

    if len(segments) == 0 or graph_path is None:
        log.log("[ERROR] cellNeighbours: no segments or no output path")
        _dump(qc_path, _EMPTY_QC)
        return

    adata, obs = spatial_utils.build_pooled_image_graph(
        segments, phys, method=method, radius=radius, n_neighs=k, per_timepoint=per_t)
    if adata is None:
        log.log("[ERROR] cellNeighbours: no cells found in the selected segmentations")
        _dump(qc_path, _EMPTY_QC)
        return

    meta = {"method": method, "radius": radius, "nNeighbours": k, "perTimepoint": per_t,
            "valueNames": sorted({str(s["valueName"]) for s in segments})}
    spatial_utils.save_graph(adata, obs, graph_path, meta=meta)

    m = spatial_utils.graph_metrics(adata)
    log.log(f">> pooled {', '.join(meta['valueNames'])}: {m['nCells']} cells, {m['nEdges']} edges, "
            f"mean degree {m['meanDegree']:.2f}, {m['isolatedFrac'] * 100:.1f}% isolated")
    log.log(f">> wrote {graph_path}")
    _dump(qc_path, m)


def _dump(path, payload):
    if path is not None:
        write_json_atomic(path, payload)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
