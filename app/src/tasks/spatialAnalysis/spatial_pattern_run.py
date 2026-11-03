"""
spatialAnalysis.spatialPattern (image-scope) — Python runner.

Is one population randomly placed among the cells of a neighbour graph? LOADS
`spatialGraph/{suffix}.h5ad` (built by `spatialAnalysis.cellNeighbours`) and uses its nodes as the
reference — the graph's coordinates and node identity only, not its edges. Each target population is
a mask over those nodes, scored by `spatial_pattern_utils.pattern_test` against random relabelling
(docs/todo/SPATIAL_PATTERN_PLAN.md). Writes a flat records table to `spatialPattern/{suffix}.json`.

Membership comes from Julia (spatialPattern.jl). Params: graphPath, graphSuffix, propsPaths {valueName → labelProps path}, targets
[{pop, segments: [{valueName, labels}]}], rMax, nRadii, nPermutations, randomState, outPath, qcOutPath.
"""

import numpy as np

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import json_safe, write_json_atomic
import cecelia.utils.spatial_utils as spatial_utils
from cecelia.utils.spatial_pattern_utils import pattern_test


def _target_mask(obs, segments):
    """Graph nodes whose (valueName, label) is in the target, and the fraction of target cells found."""
    want = {(str(s["valueName"]), int(l)) for s in segments for l in s["labels"]}
    vns = obs["valueName"].astype(str).to_numpy()
    labs = obs["label"].astype(np.int64).to_numpy()
    mask = np.fromiter(((v, int(l)) in want for v, l in zip(vns, labs)), dtype=bool, count=len(labs))
    found = len({(v, int(l)) for v, l, m in zip(vns, labs, mask) if m})
    return mask, (found / len(want) if want else 0.0)


def run(params):
    log = script_utils.get_logfile_utils(params)

    graph_path = script_utils.get_param(params, "graphPath", default=None)
    graph_sfx  = script_utils.get_param(params, "graphSuffix", default="default")
    targets    = script_utils.get_param(params, "targets", default=[])
    r_max      = float(script_utils.get_param(params, "rMax", default=100.0))
    n_radii    = int(script_utils.get_param(params, "nRadii", default=25))
    n_perm     = int(script_utils.get_param(params, "nPermutations", default=1000))
    rs         = int(script_utils.get_param(params, "randomState", default=0))
    out_path   = script_utils.get_param(params, "outPath", default=None)
    qc_path    = script_utils.get_param(params, "qcOutPath", default=None)
    props_paths = script_utils.get_param(params, "propsPaths", default={}) or {}

    if graph_path is None or len(targets) == 0:
        log.log("[ERROR] spatialPattern: no graph or no target populations")
        _dump(qc_path, {"records": []})
        return

    a, obs = spatial_utils.load_graph(graph_path)
    meta = spatial_utils.graph_meta(a)
    coords = np.asarray(a.obsm["spatial"], dtype=np.float64)
    times = obs["_t"].to_numpy() if "_t" in obs.columns else None
    frames_from = "graph"
    if times is None:
        # a pooled-frame graph of a time-lapse: score WITHIN frames anyway, or the same cell one frame
        # later is its own nearest neighbour (SPATIAL_PATTERN_PLAN Decision 4)
        times = spatial_utils.graph_node_times(obs, props_paths)
        frames_from = "cell tables"
        if times is not None and np.isnan(times).any():
            log.log("[WARN] spatialPattern: some graph cells have no frame — they are left out")
            keep = ~np.isnan(times)
            coords, obs, times = coords[keep], obs[keep].reset_index(drop=True), times[keep]
    n_frames = len(np.unique(times)) if times is not None else 1
    log.log(f">> graph '{graph_sfx}': {len(obs)} reference cells in {coords.shape[1]}D"
            + (f", scored within {n_frames} frames (from the {frames_from})" if n_frames > 1 else ""))
    radii = np.linspace(r_max / n_radii, r_max, n_radii) if r_max > 0 and n_radii > 0 else None

    total = len(targets) * max(n_perm, 1)
    log.progress(0, total)
    records = []
    for k, t in enumerate(targets):
        pop = str(t["pop"])
        mask, coverage = _target_mask(obs, t["segments"])
        base = k * max(n_perm, 1)
        out = pattern_test(coords, mask, times=times, radii=radii, n_permutations=n_perm,
                           random_state=rs, on_permutation=lambda i, b=base: log.progress(b + i, total))
        out.update({"pop": pop, "coverage": coverage,
                    "significant": bool(np.isfinite(out["pValue"]) and out["pValue"] < 0.05)})
        records.append(out)
        log.progress(base + max(n_perm, 1), total)
        log.log(f">> {pop}: {out['nTarget']}/{out['nReference']} cells, NN ratio "
                f"{out['nnRatio']:.3f} (p={out['pValue']:.3g}), Clark-Evans R {out['ceR']:.3f}")

    if out_path is not None:
        # atomic: a DURABLE sidecar discovered by directory listing (img_pattern_suffixes)
        write_json_atomic(out_path, json_safe({
            "graphSuffix": graph_sfx, "graph": meta, "nPermutations": n_perm,
            "rMax": r_max, "records": records}), indent=1)
        log.log(f">> wrote {out_path}: {len(records)} population(s)")
    _dump(qc_path, json_safe({"records": [{k: r[k] for k in (
        "pop", "nTarget", "nReference", "targetFraction", "coverage", "significant")} for r in records]}))
    log.log(">> spatialPattern done")


def _dump(path, obj):
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
