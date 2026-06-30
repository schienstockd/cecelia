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
CUDA-only) is built but parked — see ~/cc-workspace/cecelia/CLUSTERING_PLAN.md (Decision 5).
"""
import importlib.util

import numpy as np
import scanpy as sc


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


def _find_gpu(adata, resolution: float, create_umap: bool, random_state: int):
    """GPU path (rapids_singlecell). Mirrors the CPU pipeline; PAGA init is CPU-only so a GPU run
    always uses the plain UMAP. Parked until RAPIDS ships (CUDA-only) — see CLUSTERING_PLAN.md."""
    import rapids_singlecell as rsc

    rsc.get.anndata_to_GPU(adata)
    rsc.pp.neighbors(adata, use_rep="X")
    rsc.tl.leiden(adata, resolution=resolution, key_added="clusters", random_state=random_state)
    if create_umap:
        rsc.tl.umap(adata, random_state=random_state)
    rsc.get.anndata_to_CPU(adata)
