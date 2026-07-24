"""
spatialAnalysis.neighbourStats (image-scope) — Python runner.

Pairwise cell-type contact statistics: pools the basis cells across the image's segmentations into ONE
neighbour graph and computes the log-odds ratio of observed vs expected contacts between every pair of
populations — the canonical CODEX statistic (Goltsev et al., Cell 2018; docs/todo/SPATIAL_REGIONS_PLAN.md
Phase 5). Positive log-odds = selective association, negative = avoidance. Writes a flat records table to
a per-image spatialStats sidecar for the analysis canvas + MCP to read.

Membership + basis codes come from Julia (neighbourStats.jl). Params: segments [{uID, valueName,
propsPath, labels, popCodes}], basis (population names ↔ code index), physicalSizes {uID:[sz,sy,sx]},
statsPath, neighbourMethod, neighbourRadius, nNeighbours, qcOutPath.
"""
import json
import os

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
import cecelia.utils.spatial_utils as spatial_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    segments   = script_utils.get_param(params, "segments", default=[])
    basis      = script_utils.get_param(params, "basis", default=[])
    phys       = script_utils.get_param(params, "physicalSizes", default={})
    stats_path = script_utils.get_param(params, "statsPath", default=None)
    method     = script_utils.get_param(params, "neighbourMethod", default="delaunay")
    radius     = float(script_utils.get_param(params, "neighbourRadius", default=30.0))
    k          = int(script_utils.get_param(params, "nNeighbours", default=6))
    qc_path    = script_utils.get_param(params, "qcOutPath", default=None)
    n_basis    = len(basis)

    if len(segments) == 0 or n_basis < 2:
        log.log("[ERROR] neighbourStats: no segments or <2 basis populations")
        return

    uid = segments[0]["uID"]
    a, codes, obs = spatial_utils.build_pooled_image_graph(
        segments, phys.get(uid, [1.0, 1.0, 1.0]), method=method, radius=radius, n_neighs=k)
    if a is None:
        log.log("[ERROR] neighbourStats: no cells pooled")
        _dump(qc_path, {"nCells": 0, "nEdges": 0, "meanDegree": 0.0})
        return

    conn = a.obsp["spatial_connectivities"]
    observed, expected, log_odds = spatial_utils.pairwise_contact_logodds(conn, codes, n_basis)
    m = spatial_utils.graph_metrics(a)

    # flat records — one row per unordered population pair (MCP-friendly, Decision 9)
    records = []
    for i in range(n_basis):
        for j in range(i, n_basis):
            records.append({
                "popA": basis[i], "popB": basis[j],
                "observed": float(observed[i, j]), "expected": float(expected[i, j]),
                "logOdds": float(log_odds[i, j]),
                "association": ("associated" if log_odds[i, j] > 0 else "avoided"),
            })

    if stats_path is not None:
        os.makedirs(os.path.dirname(stats_path), exist_ok=True)
        with open(stats_path, "w") as f:
            json.dump({"basis": list(basis), "nCells": m["nCells"], "nEdges": m["nEdges"],
                       "records": records}, f, indent=1)
        log.log(f">> wrote {stats_path}: {len(records)} population pairs over {m['nEdges']} edges")

    _dump(qc_path, m)
    log.log(">> neighbourStats done")


def _dump(path, obj):
    if path is not None:
        with open(path, "w") as f:
            json.dump(obj, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
