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
  channelCols           the intensity subset of featureCols (ref-channel divide applies to these)
  refChannelCol         intensity column to divide channels by, or null
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
    channel_cols = script_utils.get_param(params, "channelCols", default=[])
    ref_col      = script_utils.get_param(params, "refChannelCol", default=None)

    if len(segments) == 0 or len(feature_cols) == 0:
        log.log("[ERROR] cluster_cells: no segments or no feature columns")
        return

    cluster_col = f"clusters.{suffix}"
    umap_key    = f"X_umap.{suffix}"

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

    # ref-channel normalisation: divide the intensity channels by a reference channel (old refChannel)
    if ref_col is not None and ref_col in feature_cols:
        chans = [c for c in channel_cols if c in feature_cols]
        if chans:
            ref_idx = feature_cols.index(ref_col)
            ref = adata.X[:, ref_idx].copy()
            ref[ref == 0] = 1.0
            for c in chans:
                adata.X[:, feature_cols.index(c)] = adata.X[:, feature_cols.index(c)] / ref

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

    # clusters are scanpy categorical strings ("0".."N") — store as INTEGER codes (not strings):
    # the new stack auto-detects integer obs columns as categorical (track_props.jl), and consumers
    # pass the column through the `categorical` override above the 20-level cap.
    codes = adata.obs["clusters"].astype(int).to_numpy()
    n_clusters = len(np.unique(codes))
    has_umap = "X_umap" in adata.obsm
    log.log(f">> {n_clusters} clusters; writing {cluster_col}"
            + (f" + obsm['{umap_key}']" if has_umap else " (no UMAP)"))

    # ── split back per segment and write into each labelProps ──
    for seg in segments:
        mask = (adata.obs["uID"].to_numpy() == seg["uID"]) & \
               (adata.obs["valueName"].to_numpy() == seg["valueName"])
        if not mask.any():
            continue
        sub_labels = adata.obs["label"].to_numpy()[mask]
        sub_codes  = codes[mask]
        view = LabelPropsView(seg["propsPath"]).add_obs(
            pd.DataFrame({"label": sub_labels, cluster_col: sub_codes}))
        if has_umap:
            view = view.add_obsm(umap_key, sub_labels, adata.obsm["X_umap"][mask])
        view.save()
        view.close()
        log.log(f"> wrote {seg['uID']}/{seg['valueName']}: {mask.sum()} cells")

    log.log(">> cluster_cells done")


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
