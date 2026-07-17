"""
clustTracks.cluster (set-scope) — Python runner.

The track analogue of `clustPops/cluster_run.py`. Clusters TRACKS pooled across the whole set so
cluster IDs are comparable, then writes `clusters.{suffix}` (integer-code obs) + `obsm['X_umap.{suffix}']`
back into each segmentation's per-TRACK table (`{vn}__tracks.h5ad`, label == track_id) — so they flow
through `track_props` and are gateable as `trackclust`.

Unlike the cell runner, the feature matrix arrives INLINE (params `X`/`uIDs`/`valueNames`/`labels`),
not as paths to read: track features are motility ⊕ on-read aggregates (HMM-state / transition
frequencies, per-track cell-measure means) that only Julia's `track_props` can produce (compute-on-read,
nothing persisted — see app/src/tracking/track_props.jl and tasks/clustTracks/cluster.jl). So Julia
builds the matrix and this script just clusters it and writes the labels back. The scanpy engine
(`find_populations`) and the per-segment write-back (`split_back_and_write`) are shared with the cell
runner — only the matrix assembly differs.

Invoked by `app/src/tasks/clustTracks/cluster.jl` via a params JSON. Params:
  suffix                output column suffix → clusters.{suffix} / X_umap.{suffix}
  segments              [{uID, valueName, propsPath}] — propsPath is each per-track table to write
  featureCols           per-track feature column names (var of the matrix)
  X                     n_tracks × n_features matrix (JSON null = NaN)
  uIDs, valueNames      per-row segment tags (parallel to X rows)
  labels                per-row track_id (== the per-track table's obs label index)
  resolution, normaliseAxis, normaliseToMedian, maxFraction, normalisePercentile,
  normalisePercentileBottom, transformation, logBase, createUmap, usePaga, pagaThreshold, randomState
"""
import json

import numpy as np

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
import cecelia.utils.clustering_utils as clustering_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    suffix       = script_utils.get_param(params, "suffix", default="default")
    segments     = script_utils.get_param(params, "segments", default=[])
    feature_cols = script_utils.get_param(params, "featureCols", default=[])
    rows_uid     = script_utils.get_param(params, "uIDs", default=[])
    rows_vn      = script_utils.get_param(params, "valueNames", default=[])
    rows_label   = script_utils.get_param(params, "labels", default=[])
    X_in         = script_utils.get_param(params, "X", default=[])

    if len(segments) == 0 or len(feature_cols) == 0 or len(X_in) == 0:
        log.log("[ERROR] cluster_tracks: no segments / features / tracks")
        return

    # ── build the feature AnnData from the inline matrix (JSON null → NaN) ──
    log.log(f">> {len(X_in)} tracks pooled across the set on {len(feature_cols)} feature(s)")
    import anndata as ad
    X = np.array([[np.nan if v is None else v for v in row] for row in X_in], dtype=np.float32)
    adata = ad.AnnData(X)
    adata.var_names = list(feature_cols)
    adata.obs_names = [str(i) for i in range(X.shape[0])]
    adata.obs["uID"]       = np.asarray(rows_uid, dtype=object)
    adata.obs["valueName"] = np.asarray(rows_vn, dtype=object)
    adata.obs["label"]     = np.asarray(rows_label)

    # ── cluster the whole set at once (shared scanpy engine) ──
    clustering_utils.find_populations(
        adata,
        resolution=script_utils.get_param(params, "resolution", default=1.0),
        axis=script_utils.get_param(params, "normaliseAxis", default="channels"),
        to_median=script_utils.get_param(params, "normaliseToMedian", default=False),
        max_fraction=script_utils.get_param(params, "maxFraction", default=0.0),
        percentile=script_utils.get_param(params, "normalisePercentile", default=99.8),
        percentile_bottom=script_utils.get_param(params, "normalisePercentileBottom", default=0.0),
        transformation=script_utils.get_param(params, "transformation", default="NONE"),
        log_base=script_utils.get_param(params, "logBase", default=0),
        create_umap=script_utils.get_param(params, "createUmap", default=True),
        use_paga=script_utils.get_param(params, "usePaga", default=False),
        paga_threshold=script_utils.get_param(params, "pagaThreshold", default=0.1),
        backend="auto",
        random_state=script_utils.get_param(params, "randomState", default=0),
        log=log.log,
    )

    # ── split back per segment and write into each per-track table (shared write-back) ──
    qc = clustering_utils.split_back_and_write(adata, segments, suffix, log=log.log)

    # QC (advisory): per-segment cluster distribution for Julia to bank (qc.jl). Best-effort; never
    # fails the task. Same qcOutPath pattern as clustPops/cellpose runners.
    qc_out_path = params.get("qcOutPath")
    if qc_out_path is not None:
        with open(qc_out_path, "w") as f:
            json.dump(qc, f)
        log.log(f">> saved cluster QC: {qc['nClusters']} clusters over {len(qc['perSegment'])} segment(s)")

    log.log(">> cluster_tracks done")


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
