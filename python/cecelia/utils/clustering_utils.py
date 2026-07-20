"""
Shared Leiden clustering engine (cells + tracks).

Granularity-blind and I/O-free: a caller builds a feature-matrix AnnData (cell
intensities + object measures, or per-track aggregates), hands it here, and gets cluster
labels (+ an optional UMAP embedding) back. The `cluster_cells` / `cluster_tracks` run
scripts own the I/O — they read the labelProps via `LabelPropsView`, build the matrix, call
`find_populations`, and write `clusters.{suffix}` (+ `obsm['X_umap']`) back to the H5AD.

Port of the old R cecelia `inst/py/scanpy_utils.py` `find_populations`
(transform -> normalise -> `sc.pp.neighbors(use_rep='X')` -> `sc.tl.leiden(key_added='clusters')`
-> optional UMAP). Reference: scanpy (Wolf et al. 2018, Genome Biology, doi:10.1186/s13059-017-1382-0);
Leiden (Traag et al. 2019, Sci Rep, doi:10.1038/s41598-019-41695-z).

Backend is AUTO-DETECTED, never a task param (CLAUDE.md hard rule — no `useGPU`). CPU path
(scanpy + leidenalg) is the cross-platform default and ships now. GPU path (rapids_singlecell,
CUDA-only) is built but parked — see docs/todo/CLUSTERING_PLAN.md (Decision 5).
"""
import importlib.util

import numpy as np
import pandas as pd
import scanpy as sc

from cecelia.utils.label_props_utils import LabelPropsView


# ── backend detection (cached) ───────────────────────────────────────────────────
_GPU_AVAILABLE = None


def gpu_available() -> bool:
    """True iff rapids_singlecell + a CUDA device are usable. Cached after the first call."""
    global _GPU_AVAILABLE
    if _GPU_AVAILABLE is None:
        _GPU_AVAILABLE = _detect_gpu()
    return _GPU_AVAILABLE


def _detect_gpu() -> bool:
    try:
        if importlib.util.find_spec("rapids_singlecell") is None:
            return False
        import cupy  # noqa: only imported to probe the runtime
        return cupy.cuda.runtime.getDeviceCount() > 0
    except Exception:
        return False


def _resolve_backend(backend: str) -> str:
    """'auto' -> 'gpu' when available else 'cpu'; pass-through otherwise."""
    if backend == "auto":
        return "gpu" if gpu_available() else "cpu"
    return backend


# ── normalisation / transform (ports of scanpy_utils) ─────────────────────────────
def apply_transform(adata, transformation: str = "NONE", log_base: float = 10):
    """Log / reversed-log transform of `adata.X` in place (no-op for 'NONE')."""
    if transformation == "NONE":
        return
    if transformation == "log":
        sc.pp.log1p(adata, base=(log_base if log_base and log_base > 0 else None))
    elif transformation == "reversedLog":
        col_max = np.max(adata.X, axis=0)
        if log_base == 0:
            adata.X = np.log((-adata.X + col_max) + 1)
        elif log_base == 2:
            adata.X = np.log2((-adata.X + col_max) + 1)
        else:  # default base 10
            adata.X = np.log10((-adata.X + col_max) + 1)
        adata.X = -adata.X + np.max(adata.X, axis=0)


def normalise_per_channel(adata, percentile: float, percentile_bottom: float = 0.0,
                          to_median: bool = False):
    """Rescale each channel to [0,1] over its [percentile_bottom, percentile] range, in place.
    Degenerate columns (flat / all-zero) fall back to the column min/max, then to [0,1], to avoid
    divide-by-zero (matches the old behaviour for rare-transition columns)."""
    if to_median:
        adata.X = adata.X / np.median(adata.X, axis=0)

    max_p = np.percentile(adata.X, percentile, axis=0)
    min_p = np.percentile(adata.X, percentile_bottom, axis=0)
    max_total = np.max(adata.X, axis=0)
    min_total = np.min(adata.X, axis=0)

    failed = max_p == min_p
    if np.any(failed):
        max_p[failed] = max_total[failed]
        min_p[failed] = min_total[failed]
    failed = max_p == min_p           # still degenerate (all-zero column) -> map to [0,1]
    if np.any(failed):
        max_p[failed] = 1
        min_p[failed] = 0

    adata.X = (adata.X - min_p) / (max_p - min_p)
    adata.X[adata.X < 0] = 0
    adata.X[adata.X > 1] = 1


def normalise_adata(adata, axis: str = "channels", to_median: bool = False,
                    max_fraction: float = 0.0, percentile: float = 99.8,
                    percentile_bottom: float = 0.0):
    """Normalise `adata.X` in place. axis='channels' -> per-channel percentile rescale;
    'cells' -> `sc.pp.normalize_total` per cell; 'NONE' -> no-op."""
    adata.X[np.isnan(adata.X)] = 0
    if axis == "cells":
        sc.pp.normalize_total(adata, target_sum=1,
                              exclude_highly_expressed=(max_fraction > 0),
                              max_fraction=max_fraction, inplace=True)
    elif axis == "channels" and percentile > 0:
        normalise_per_channel(adata, percentile=percentile,
                              percentile_bottom=percentile_bottom, to_median=to_median)


# ── main entry point ──────────────────────────────────────────────────────────────
def find_populations(adata, resolution: float = 1.0, axis: str = "channels",
                     to_median: bool = False, max_fraction: float = 0.0,
                     percentile: float = 99.8, percentile_bottom: float = 0.0,
                     transformation: str = "NONE", log_base: float = 0,
                     create_umap: bool = True, use_paga: bool = False,
                     paga_threshold: float = 0.1, backend: str = "auto",
                     random_state: int = 0, log=None):
    """Cluster `adata` in place: transform -> normalise -> neighbours (use_rep='X') -> Leiden
    (`obs['clusters']`, categorical string codes) -> optional UMAP (`obsm['X_umap']`).

    Granularity-blind — `adata.X` is whatever feature matrix the caller built. Returns
    `(clusters, x_umap)` for convenience; the mutation on `adata` is the source of truth.
    `random_state` makes a run reproducible. `backend='auto'` uses the GPU when available and
    silently falls back to CPU (logged)."""
    _log = log if callable(log) else (lambda _m: None)

    apply_transform(adata, transformation=transformation, log_base=log_base)
    normalise_adata(adata, axis=axis, to_median=to_median, max_fraction=max_fraction,
                    percentile=percentile, percentile_bottom=percentile_bottom)
    adata.X[np.isnan(adata.X)] = 0

    backend = _resolve_backend(backend)
    if backend == "gpu":
        try:
            _log(">> clustering on GPU (rapids_singlecell)")
            _find_gpu(adata, resolution=resolution, create_umap=create_umap,
                      random_state=random_state)
            return adata.obs["clusters"], adata.obsm.get("X_umap")
        except Exception as e:  # CUDA / import failure mid-run -> fall back, don't crash the task
            _log(f">> GPU clustering failed ({e}); falling back to CPU")

    _log(">> clustering on CPU (scanpy + leidenalg)")
    sc.pp.neighbors(adata, use_rep="X")
    # flavor='leidenalg' keeps parity with the old engine (scanpy's default flavor is migrating to
    # 'igraph', which would change labels); random_state pins reproducibility.
    sc.tl.leiden(adata, resolution=resolution, key_added="clusters",
                 flavor="leidenalg", random_state=random_state)
    if create_umap:
        if use_paga:
            sc.tl.paga(adata, groups="clusters")
            sc.pl.paga(adata, plot=False, show=False, threshold=paga_threshold)
            sc.tl.umap(adata, init_pos="paga", random_state=random_state)
        else:
            sc.tl.umap(adata, random_state=random_state)

    return adata.obs["clusters"], adata.obsm.get("X_umap")


# ── QC: per-segment cluster distribution ─────────────────────────────────────────
def cluster_seg_stats(seg_codes):
    """Objective QC stats for one segment's cluster assignment (PURE → unit-tested).

    `seg_codes` is the integer cluster code of every point (cell/track) belonging to one
    (image, segmentation). Returns `{n, nClusters, largestClusterFrac}` — the point count, how
    many distinct clusters those points occupy, and the fraction in the single largest cluster
    (dominance). An image whose points collapse into far fewer clusters (or one dominant cluster)
    than the cohort is a batch/quality outlier — see qc.jl `cluster_qc_findings` + COHORT_METRICS."""
    n = int(np.asarray(seg_codes).size)
    if n == 0:
        return {"n": 0, "nClusters": 0, "largestClusterFrac": 0.0}
    _uniq, counts = np.unique(seg_codes, return_counts=True)
    return {"n": n, "nClusters": int(counts.size),
            "largestClusterFrac": float(counts.max() / n)}


# ── shared write-back (cells + tracks) ─────────────────────────────────────────────
def split_back_and_write(adata, segments, suffix: str, log=None, col_prefix: str = "clusters"):
    """Split a pooled, clustered AnnData back per segment and write the cluster assignment
    into each segment's labelProps via `LabelPropsView` (the sanctioned writer — CLAUDE.md).

    `col_prefix` selects the obs-column family: "clusters" (default; clustPops/clustTracks →
    `clusters.{suffix}`) or "regions" (clustRegions spatial regions → `regions.{suffix}`). Both are
    pinned categorical by the same name-rule (track_props.jl `_is_categorical_col`), so region reuses
    this writer verbatim rather than a second copy (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5).

    Granularity-blind, like `find_populations`: the caller decides what a "segment" and a "label"
    mean. For `clustPops` a segment is one (image, segmentation) and `label` is the CELL label;
    for `clustTracks` a segment is one (image, segmentation) and `label` is the track_id (the
    per-track table's obs index). Either way the mechanics are identical, so this lives here once
    rather than being re-implemented per runner.

    Requirements:
      • `adata.obs` carries `uID`, `valueName`, `label` (label matching each target file's index).
      • `adata.obs['clusters']` holds the scanpy categorical string codes ("0".."N").
      • each `segments` entry is a dict with `uID`, `valueName`, `propsPath` (the h5ad to write).

    Writes integer-code `clusters.{suffix}` obs (the new stack auto-detects integer obs as a
    categorical code set; a `clusters.*` name-rule pins it categorical above the level cap — see
    track_props.jl) and, when present, `obsm['X_umap.{suffix}']`.

    Returns a QC dict `{nClusters, nTotal, perSegment:[{uID, valueName, n, nClusters,
    largestClusterFrac}]}` — the run total plus each segment's cluster distribution (via
    `cluster_seg_stats`), so the Julia caller can bank per-image QC metrics + findings (qc.jl)."""
    _log = log if callable(log) else (lambda _m: None)

    cluster_col = f"{col_prefix}.{suffix}"
    umap_key    = f"X_umap.{suffix}"

    codes      = adata.obs["clusters"].astype(int).to_numpy()
    n_clusters = len(np.unique(codes))
    has_umap   = "X_umap" in adata.obsm
    _log(f">> {n_clusters} clusters; writing {cluster_col}"
         + (f" + obsm['{umap_key}']" if has_umap else " (no UMAP)"))

    uid_arr   = adata.obs["uID"].to_numpy()
    vn_arr    = adata.obs["valueName"].to_numpy()
    label_arr = adata.obs["label"].to_numpy()

    per_segment = []
    for seg in segments:
        mask = (uid_arr == seg["uID"]) & (vn_arr == seg["valueName"])
        if not mask.any():
            continue
        sub_labels = label_arr[mask]
        view = LabelPropsView(seg["propsPath"]).add_obs(
            pd.DataFrame({"label": sub_labels, cluster_col: codes[mask]}))
        if has_umap:
            view = view.add_obsm(umap_key, sub_labels, adata.obsm["X_umap"][mask])
        view.save()
        view.close()
        stats = cluster_seg_stats(codes[mask])
        per_segment.append({"uID": seg["uID"], "valueName": seg["valueName"], **stats})
        _log(f"> wrote {seg['uID']}/{seg['valueName']}: {int(mask.sum())} rows")

    return {"nClusters": int(n_clusters), "nTotal": int(codes.size), "perSegment": per_segment}


def _find_gpu(adata, resolution: float, create_umap: bool, random_state: int):
    """GPU path (rapids_singlecell). Mirrors the CPU pipeline; PAGA init is CPU-only so a GPU run
    always uses the plain UMAP. Parked until RAPIDS ships (CUDA-only) — see docs/todo/CLUSTERING_PLAN.md."""
    import rapids_singlecell as rsc

    rsc.get.anndata_to_GPU(adata)
    rsc.pp.neighbors(adata, use_rep="X")
    rsc.tl.leiden(adata, resolution=resolution, key_added="clusters", random_state=random_state)
    if create_umap:
        rsc.tl.umap(adata, random_state=random_state)
    rsc.get.anndata_to_CPU(adata)
