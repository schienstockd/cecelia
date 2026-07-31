"""
spatialAnalysis.neighbourStats (image-scope) — Python runner.

Pairwise cell-interaction statistics on the SHARED neighbour graph. LOADS
`spatialGraph/{suffix}.h5ad` (built once by `spatialAnalysis.cellNeighbours`), joins the analysis's
population codes onto its nodes by (valueName, label), and computes per population pair:
the observed/expected contact counts, the CODEX log-odds effect size, and a permutation z-score +
empirical p-value from `n_permutations` random relabellings (see
`spatial_utils.pairwise_contact_logodds` for the statistics and their citations).

Nothing here builds a graph — that is `cellNeighbours`' job, so a neighbourhood means the same thing in
every readout (docs/todo/SPATIAL_REGIONS_PLAN.md Phase 5). Writes a flat records table to a per-image
spatialStats sidecar for the interaction heatmap + MCP (Decision 9).

Membership + basis codes come from Julia (neighbourStats.jl). Params: graphPath, graphSuffix,
segments [{valueName, labels, popCodes}], basis (population names ↔ code index), statsPath,
nPermutations, randomState, qcOutPath.
"""

import numpy as np

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic
import cecelia.utils.spatial_utils as spatial_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    graph_path = script_utils.get_param(params, "graphPath", default=None)
    graph_sfx  = script_utils.get_param(params, "graphSuffix", default="default")
    segments   = script_utils.get_param(params, "segments", default=[])
    basis      = script_utils.get_param(params, "basis", default=[])
    stats_path = script_utils.get_param(params, "statsPath", default=None)
    n_perm     = int(script_utils.get_param(params, "nPermutations", default=1000))
    rs         = int(script_utils.get_param(params, "randomState", default=0))
    qc_path    = script_utils.get_param(params, "qcOutPath", default=None)
    n_basis    = len(basis)

    if graph_path is None or len(segments) == 0 or n_basis < 2:
        log.log("[ERROR] neighbourStats: no graph, no segments, or <2 basis populations")
        _dump(qc_path, {"nCells": 0, "nEdges": 0, "meanDegree": 0.0, "coverage": 0.0})
        return

    a, obs = spatial_utils.load_graph(graph_path)
    meta = spatial_utils.graph_meta(a)
    log.log(f">> graph '{graph_sfx}': {a.n_obs} cells, method={meta.get('method')} "
            f"radius={meta.get('radius')} k={meta.get('nNeighbours')}")

    # attach THIS analysis's labelling to the shared graph (the legacy (uID, valueName, label) join)
    codes, coverage = spatial_utils.pop_codes_for(obs, segments, n_basis)
    if coverage == 0.0:
        log.log("[ERROR] neighbourStats: none of the selected cells are in this graph — was the graph "
                "built over these segmentations?")
        _dump(qc_path, {"nCells": a.n_obs, "nEdges": 0, "meanDegree": 0.0, "coverage": 0.0})
        return
    log.log(f">> {int((codes >= 0).sum())}/{len(codes)} graph cells in the selected populations "
            f"({coverage * 100:.1f}%)")

    conn = a.obsp["spatial_connectivities"]
    # per-timepoint graph → permute labels WITHIN a frame (a global shuffle would migrate labels
    # across frames and invalidate the null; see pairwise_contact_logodds).
    times = obs["_t"].to_numpy() if "_t" in obs.columns else None
    observed, expected, log_odds, zscore, pvalue = spatial_utils.pairwise_contact_logodds(
        conn, codes, n_basis, n_permutations=n_perm, random_state=rs, times=times)
    m = spatial_utils.graph_metrics(a)
    n_pair_edges = int(observed.sum() + np.trace(observed)) // 2

    # flat records — one row per unordered population pair (MCP-friendly, Decision 9)
    records, n_sig = [], 0
    for i in range(n_basis):
        for j in range(i, n_basis):
            p = float(pvalue[i, j]) if np.isfinite(pvalue[i, j]) else None
            z = float(zscore[i, j]) if np.isfinite(zscore[i, j]) else None
            sig = p is not None and p < 0.05
            n_sig += int(sig)
            records.append({
                "popA": basis[i], "popB": basis[j],
                "observed": float(observed[i, j]), "expected": float(expected[i, j]),
                "logOdds": float(log_odds[i, j]),
                "zScore": z, "pValue": p, "significant": sig,
                "association": ("associated" if log_odds[i, j] > 0 else "avoided"),
            })

    if stats_path is not None:
        # atomic: this is a DURABLE sidecar, and discovery is a directory listing filtered by `.json`
        # (`app/src/ai/spatial.jl`), so a half-written file would be picked up as a real stats result
        write_json_atomic(stats_path,
                          {"basis": list(basis), "nCells": m["nCells"], "nEdges": n_pair_edges,
                           "graphSuffix": graph_sfx, "graph": meta,
                           "nPermutations": n_perm, "coverage": coverage,
                           "records": records}, indent=1)
        log.log(f">> wrote {stats_path}: {len(records)} population pairs over {n_pair_edges} contacts")
    if n_perm > 0:
        log.log(f">> {n_sig}/{len(records)} pair(s) differ from chance at p<0.05 ({n_perm} permutations)")
    else:
        log.log(">> permutation test skipped (permutations = 0) — log-odds are descriptive only")

    qc = dict(m)
    qc.update({"nEdges": n_pair_edges, "coverage": coverage,
               "nSignificant": n_sig if n_perm > 0 else -1})
    _dump(qc_path, qc)
    log.log(">> neighbourStats done")


def _dump(path, obj):
    # the per-run QC handoff (task_run_dir), not durable — but one write idiom per file, so it uses the
    # same helper as the durable sidecar above rather than sitting next to it as a second way
    if path is not None:
        write_json_atomic(path, obj)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
