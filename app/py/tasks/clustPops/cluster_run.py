"""
clustPops.cluster (set-scope) — Python runner.

Pools the selected populations across ALL the set's images/segmentations into one feature
matrix, runs Leiden once (so cluster IDs are comparable across the whole set), then writes
`clusters.{suffix}` (integer-code obs column) + `obsm['X_umap.{suffix}']` back into each
segmentation's labelProps. Mirrors the old R cecelia `leiden_clust.py` (concatenate → cluster
→ split-back), but membership + feature-column selection are done in Julia (boundary), so this
script just reads the columns it is told, clusters, and writes back.

Invoked by `app/src/tasks/clustPops/cluster.jl` via a params JSON. Params:
  suffix                output column suffix → clusters.{suffix} / X_umap.{suffix}
  segments              [{uID, valueName, propsPath, labels|null}] — one per (image, segmentation)
  featureCols           var columns to cluster on (channels + object measures), same names across segments
  resolution, normaliseAxis, normaliseToMedian, maxFraction, normalisePercentile,
  normalisePercentileBottom, transformation, logBase, createUmap, usePaga, pagaThreshold, randomState
"""
import numpy as np
import pandas as pd

# `py.*` resolves via PYTHONPATH=app/, set by the Julia launcher (app/src/py_runner.jl::run_py).
from py.utils.label_props_utils import LabelPropsView
import py.utils.script_utils as script_utils
import py.utils.clustering_utils as clustering_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    suffix       = script_utils.get_param(params, "suffix", default="default")
    segments     = script_utils.get_param(params, "segments", default=[])
    feature_cols = script_utils.get_param(params, "featureCols", default=[])

    if len(segments) == 0 or len(feature_cols) == 0:
        log.log("[ERROR] cluster_cells: no segments or no feature columns")
        return

    # ── pool every segment's cells into one matrix (one row per cell, tagged by segment) ──
    log.log(f">> Pooling {len(segments)} segment(s) on {len(feature_cols)} feature(s)")
    frames = []
    for seg in segments:
        path   = seg["propsPath"]
        labels = seg.get("labels", None)
        view = LabelPropsView(path).view_cols(feature_cols)
        if labels is not None:
            view = view.filter_by_label(labels)
        df = view.as_df()
        view.close()
        if df.shape[0] == 0:
            log.log(f"> {seg['uID']}/{seg['valueName']}: no cells, skipped")
            continue
        # keep only the requested feature columns that are present (+ label); tag the segment
        cols = [c for c in feature_cols if c in df.columns]
        missing = [c for c in feature_cols if c not in df.columns]
        if missing:
            log.log(f"[WARN] {seg['uID']}/{seg['valueName']}: missing columns {missing} (filled 0)")
            for c in missing:
                df[c] = 0.0
        df = df[["label"] + feature_cols].copy()
        df["uID"]       = seg["uID"]
        df["valueName"] = seg["valueName"]
        frames.append(df)

    if len(frames) == 0:
        log.log("[ERROR] cluster_cells: no cells across the set")
        return
    pooled = pd.concat(frames, axis=0, ignore_index=True)
    log.log(f">> {pooled.shape[0]} cells pooled across the set")

    # ── build the feature AnnData (X = feature matrix, obs = segment id + label) ──
    import anndata as ad
    X = pooled[feature_cols].to_numpy(dtype=np.float32)
    adata = ad.AnnData(X)
    adata.var_names = list(feature_cols)
    adata.obs_names = [str(i) for i in range(pooled.shape[0])]
    adata.obs["uID"]       = pooled["uID"].to_numpy()
    adata.obs["valueName"] = pooled["valueName"].to_numpy()
    adata.obs["label"]     = pooled["label"].to_numpy()

    # ── cluster the whole set at once ──
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

    # ── split back per segment and write into each cell labelProps (shared write-back) ──
    # clusters are scanpy categorical strings ("0".."N"); the helper stores INTEGER codes (a
    # `clusters.*` name-rule pins them categorical above the level cap — see track_props.jl).
    clustering_utils.split_back_and_write(adata, segments, suffix, log=log.log)

    log.log(">> cluster_cells done")


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
