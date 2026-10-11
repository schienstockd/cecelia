"""
Per-cell label measurement utility.

Computes morphology properties (via skimage.measure.regionprops_table) and
per-channel intensity statistics for each labelled cell, then writes the
result as an AnnData .h5ad file.

For 3D images, basic shape metrics come from skimage.  When extendedMeasures
is True, a trimesh mesh is built per cell via marching cubes, giving accurate
surface area, volume, convex hull metrics, and ellipsoid axes.  Meshes are
transient (never written to disk) — spatial contact analysis regenerates them
on the fly from label masks (see docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 11).

Memory is bounded by a block, not a frame: every object is measured exactly once, from a crop of the
block that OWNS it (the block holding its bounding-box start), so a tile scan with tens of channels
and millions of cells never has a whole timepoint in RAM. See docs/todo/MEASURE_TILING_PLAN.md.
"""

from __future__ import annotations

import concurrent.futures as cf
import multiprocessing as mp
import os
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import skimage.measure as skmeas
import skimage.filters as skfilt
from scipy import ndimage as ndi
from cecelia.utils.atomic_io import write_h5ad_atomic
import cecelia.utils.zarr_utils as zarr_utils
import anndata as ad

from cecelia.utils.label_props_utils import skimage_centroid_axis_names

try:
    import trimesh as _trimesh
    _HAS_TRIMESH = True
except ImportError:
    _HAS_TRIMESH = False

# 2D regionprops (standard set). `bbox` is intentionally NOT measured — it's a structural extent,
# not a QC morphology measure, and nothing downstream reads it (it only cluttered the h5ad var).
_PROPS_2D = [
    'label', 'centroid', 'area',
    'eccentricity', 'orientation', 'perimeter',
    'convex_area', 'equivalent_diameter', 'extent',
    'feret_diameter_max', 'major_axis_length', 'minor_axis_length', 'solidity',
]

# 3D regionprops — used when extendedMeasures is False or trimesh is unavailable.
# solidity excluded: qhull convex hull fails for cells spanning only one Z-slice,
# flooding stderr. feret_diameter_max excluded: O(n²) voxel distance matrix. bbox excluded (see 2D).
_PROPS_3D = [
    'label', 'centroid', 'area', 'extent',
    'equivalent_diameter_area', 'euler_number', 'inertia_tensor_eigvals',
]

# Minimal 3D props used when trimesh handles shape descriptors.
_PROPS_3D_BASE = [
    'label', 'centroid', 'area', 'extent',
]


# Smallest XY block edge when the task doesn't send one. Rounded UP to a whole number of the label
# store's chunks (`block_size_for`), so a block read never over-fetches (MEASURE_TILING_PLAN, D8).
DEFAULT_BLOCK_SIZE = 1024

# How far an object may reach past its owning block's end before it is measured from its own crop
# instead of widening the block's read (Decision 2). Bounds a block read at (block + cap)².
MARGIN_CAP = 512

# scipy/skimage `gaussian` kernel radius for a given sigma at the default `truncate=4.0` — a block
# read with this halo smooths to the same values as the whole plane (Decision 5).
_GAUSS_TRUNCATE = 4.0


def block_size_for(label_arr, target: int = DEFAULT_BLOCK_SIZE) -> int:
    """`target` rounded up to a whole number of `label_arr`'s XY chunks (the store's own tile)."""
    tile = zarr_utils.store_xy_tile(label_arr) or zarr_utils.DEFAULT_XY_TILE
    return tile * max(1, -(-target // tile))


def scan_label_bboxes(arr, la_t, t_idx: int, spatial_axes: list, block: int,
                      pool=None, store_axes: list | None = None) -> pd.DataFrame | None:
    """Bounding box of every label in one timepoint of a label store, read block by block.

    Returns a DataFrame indexed by label id with `lo_<ax>` / `hi_<ax>` (half-open) per spatial axis,
    or None when the timepoint has no labels. Only one XY block (full Z) is ever resident.

    `store_axes` is the store's non-T axis order when it differs from `spatial_axes` — i.e. when a
    singleton Z is dropped (`_store_index`); default: the same.

    Each block is relabelled densely before `find_objects`, which is O(max label id) per call —
    with millions of ids that per-block cost, not the pixels, would dominate the scan.
    """
    store_axes = store_axes or spatial_axes
    store_shape = dict(zip(store_axes, (arr.shape[i] for i in range(arr.ndim) if i != la_t)))
    shape = [store_shape[ax] for ax in spatial_axes]
    iy, ix = spatial_axes.index('Y'), spatial_axes.index('X')
    starts = [(y, x) for y in range(0, shape[iy], block) for x in range(0, shape[ix], block)]

    def _one(yx):
        y0, x0 = yx
        sl = [slice(None)] * len(spatial_axes)
        sl[iy] = slice(y0, min(y0 + block, shape[iy]))
        sl[ix] = slice(x0, min(x0 + block, shape[ix]))
        crop = _read_label_region(arr, la_t, t_idx, _store_index(store_axes, spatial_axes, sl))
        ids = np.unique(crop)
        ids = ids[ids != 0]
        if ids.size == 0:
            return None
        dense = _dense_labels(crop, ids)
        objs = ndi.find_objects(dense)
        origin = [s.start or 0 for s in sl]
        lo = np.array([[o[k].start + origin[k] for k in range(len(o))] for o in objs])
        hi = np.array([[o[k].stop + origin[k] for k in range(len(o))] for o in objs])
        return ids, lo, hi

    parts = list(pool.map(_one, starts)) if pool is not None else [_one(s) for s in starts]
    parts = [p for p in parts if p is not None]
    if not parts:
        return None
    ids = np.concatenate([p[0] for p in parts])
    cols = {}
    for k, ax in enumerate(spatial_axes):
        cols[f'lo_{ax}'] = np.concatenate([p[1][:, k] for p in parts])
        cols[f'hi_{ax}'] = np.concatenate([p[2][:, k] for p in parts])
    df = pd.DataFrame(cols, index=pd.Index(ids, name='label'))
    agg = {c: ('min' if c.startswith('lo_') else 'max') for c in df.columns}
    return df.groupby(level=0).agg(agg)


def plan_regions(bbox: pd.DataFrame, block: int, cap: int = MARGIN_CAP):
    """Assign each object to the block holding its bbox start and return the crops to measure.

    A region is `(lo, hi, ids)`: the union of the bboxes of the objects one block owns, so every
    owned object lies entirely inside it and is measured exactly once (Decision 1). An object
    reaching more than `cap` past its block's end gets a region of its own (Decision 2) — measured,
    never dropped. Returns `(regions, n_outliers)`.
    """
    axes = [c[3:] for c in bbox.columns if c.startswith('lo_')]
    lo_y, lo_x = bbox['lo_Y'].to_numpy(), bbox['lo_X'].to_numpy()
    by, bx = lo_y // block, lo_x // block
    reach = np.maximum(bbox['hi_Y'].to_numpy() - (by + 1) * block,
                       bbox['hi_X'].to_numpy() - (bx + 1) * block)
    outlier = reach > cap

    regions = []
    lo_cols = [f'lo_{a}' for a in axes]
    hi_cols = [f'hi_{a}' for a in axes]
    owned = bbox.loc[~outlier].assign(_by=by[~outlier], _bx=bx[~outlier])
    for _, g in owned.groupby(['_by', '_bx'], sort=True):
        regions.append((g[lo_cols].min().to_numpy(), g[hi_cols].max().to_numpy(),
                        np.sort(g.index.to_numpy())))
    for lbl, row in bbox.loc[outlier].iterrows():
        regions.append((row[lo_cols].to_numpy(), row[hi_cols].to_numpy(), np.array([lbl])))
    return regions, int(outlier.sum())


def _store_index(store_axes: list, spatial: list, sl: list) -> list:
    """Per-axis slices over the MEASURED axes → one index per store axis. A store axis that is not
    measured (a singleton Z, dropped) is read at 0, which squeezes it out of the crop."""
    by_ax = dict(zip(spatial, sl))
    return [by_ax.get(ax, 0) for ax in store_axes]


def _read_label_region(arr, la_t, t_idx: int, spatial_sl: list) -> np.ndarray:
    """One timepoint of a label store, cropped to `spatial_sl` (one index per non-T axis)."""
    it = iter(spatial_sl)
    sl = tuple(t_idx if i == la_t else next(it) for i in range(arr.ndim))
    return np.asarray(arr[sl])


def _dense_labels(crop: np.ndarray, ids: np.ndarray) -> np.ndarray:
    """Relabel `crop` so `ids[k]` becomes k+1 and every other value 0. `ids` sorted, non-empty."""
    idx = np.searchsorted(ids, crop)
    np.minimum(idx, len(ids) - 1, out=idx)
    return np.where(ids[idx] == crop, idx + 1, 0).astype(np.int32)


class _NullLog:
    def log(self, msg):
        pass


_WORKER: dict = {}


def _worker_init(mu, ctx):
    """Process-pool initializer: the measurer and its open stores, once per worker."""
    _WORKER['mu'] = mu
    _WORKER['ctx'] = {**ctx, 'log': _NullLog()}


def _worker_measure(region):
    return _WORKER['mu']._measure_region(*region, _WORKER['ctx'])


class MeasureUtils:
    def __init__(self, params: dict, dim_utils):
        self.params             = params
        self.dim_utils          = dim_utils
        self.task_dir           = params['taskDir']
        self.output_value_name  = params.get('outputValueName', 'default')
        # P1d pass-through: chain-run pinned version (docs/todo/VN_VERSIONING_PLAN.md → D3);
        # stored for logs / future writer routing. Path assembly stays legacy (P4b rewrites).
        self.version            = params.get('version')
        self.intensity_measure  = params.get('intensityMeasure', 'mean')
        self.gaussian_filter    = float(params.get('gaussianFilter', 0.0))
        self.extended_measures  = bool(params.get('extendedMeasures', False))

    # ── public entry point ────────────────────────────────────────────────────

    def measure_from_zarr(self, label_zarrs: dict, im_dat, log, label_passes=None,
                          n_workers: int = 1):
        """
        label_zarrs : {'base': [level0, level1, …], 'nuc': […], …}  – multiscale level lists
                      (as returned by zarr_utils.open_as_zarr; level 0 is full-res)
        im_dat      : multiscale level list for the intensity image (im_dat[0] = full-res)
        log         : script_utils logfile helper (has .log(str))
        label_passes: `zarr_utils.read_label_passes` entries for the BASE store, or None/[].
                      Turns into an `obs['pass']` column naming the model group that found each
                      object — which is what makes a multi-pass run usable downstream. Named
                      one-based, as the form and the preview number the same passes; see
                      `zarr_utils.pass_display_name`.
        n_workers   : worker processes for the measurement (threads for the cheap bbox scan).

        All label types are measured together and written to a single .h5ad file.
        Non-base types (nuc, cyto, halo) contribute extra intensity columns to the
        base measurement: {type}_mean_intensity_{c}.

        **Why the pass belongs here and not in a napari colour.** A two-pass run exists to find two
        POPULATIONS — cells in one pass, apoptotic fragments in the second — and the split between
        them is a gating decision, not a segmentation parameter (docs/SEGMENTATION.md). Gating reads
        `.h5ad`, so a column is the only form of this fact that anything downstream can act on; a
        colour in the viewer would answer "which pass" for the eye and for nothing else. The store
        has recorded the id ranges since two-pass shipped and NOTHING read them, which is why a
        two-pass result could not be told apart from a single-pass one.

        Returns the path to the written .h5ad file, or None on failure.
        """
        dim_order = self.dim_utils.im_dim_order
        dim_sizes = dict(zip(dim_order, self.dim_utils.im_dim))

        # Dimensionality is the image's REAL Z extent, not whether its axes name a Z: a single-plane
        # image keeps a length-1 Z whenever its source had one, and measuring that as 3D gave it the 3D
        # prop set (no perimeter, solidity, Feret, eccentricity) and an all-zero `centroid_z`. The
        # singleton Z is dropped at read time — the same rule segmentation applies (`drop_z` in
        # `predict_from_zarr`, docs/SEGMENTATION.md), so crops, centroids and blocks are all Y,X.
        is_3d  = self.dim_utils.is_3D()
        drop_z = 'Z' in dim_order and not is_3d
        n_t    = int(dim_sizes.get('T', 1))
        n_c    = int(dim_sizes.get('C', 1))
        la_t   = dim_order.index('T') if 'T'  in dim_order else None
        la_c   = dim_order.index('C') if 'C'  in dim_order else None

        # label axis order has no C
        label_dim_order = [ax for ax in dim_order if ax != 'C']
        l_la_t = label_dim_order.index('T') if 'T' in label_dim_order else None

        base_arr = label_zarrs['base'][0]   # full-res array (level 0)
        im_arr   = im_dat[0]

        if is_3d and self.extended_measures and not _HAS_TRIMESH:
            log.log('[WARN] trimesh not installed — falling back to skimage 3D props')

        # One binary search per object rather than a dict per label — the ranges exist because the
        # label count is the big number here. `None` for a single-pass store, which is the common
        # case and must add no column at all (an all-one-value `pass` would be noise in every gate).
        group_of = zarr_utils.label_pass_lookup(label_passes) if label_passes else None
        pass_of = None
        if group_of is not None:
            # Stamped as the number the user has ALREADY been shown, not the raw key — see
            # `zarr_utils.pass_display_name`. The preview said "pass 1: 36, pass 2: 4"; a gate
            # offering 0 and 1 for that same run is one fact under two numberings.
            def pass_of(label, _group_of=group_of):
                group = _group_of(label)
                return None if group is None else zarr_utils.pass_display_name(group)

            names = list(dict.fromkeys(zarr_utils.pass_display_name(e['group'])
                                       for e in label_passes))
            log.log(f'>> label passes: {len(label_passes)} ranges over '
                    f'{len(names)} passes ({", ".join(names)})')

        store_axes = [ax for ax in label_dim_order if ax != 'T']        # the label store's own axes
        spatial = [ax for ax in store_axes if not (drop_z and ax == 'Z')]   # the axes measured
        block = max(64, int(self.params.get('blockSize') or block_size_for(base_arr)))
        n_workers = max(1, int(n_workers))
        pool = cf.ThreadPoolExecutor(max_workers=n_workers) if n_workers > 1 else None
        ctx = dict(label_zarrs=label_zarrs, im_arr=im_arr, l_la_t=l_la_t, la_t=la_t, la_c=la_c,
                   n_c=n_c, is_3d=is_3d, spatial=spatial, store_axes=store_axes, log=log)

        try:
            # Pass 1 — labels only: where is every object? Secondary stores (nuc/cyto/halo) widen
            # an object's box, since a halo reaches past its cell and must fit in the same crop.
            regions: list = []
            n_outliers = n_objects = 0
            for t_idx in range(n_t):
                bbox = scan_label_bboxes(base_arr, l_la_t, t_idx, spatial, block, pool, store_axes)
                if bbox is None:
                    log.log(f'>> t {t_idx + 1}/{n_t}: no labels')
                    continue
                for ltype, lzarr in label_zarrs.items():
                    if ltype == 'base':
                        continue
                    sec = scan_label_bboxes(lzarr[0], l_la_t, t_idx, spatial, block, pool,
                                            store_axes)
                    if sec is not None:
                        sec = sec.reindex(bbox.index)
                        for c in bbox.columns:
                            op = np.fmin if c.startswith('lo_') else np.fmax
                            bbox[c] = op(bbox[c], sec[c]).astype(bbox[c].dtype)
                regs, n_out = plan_regions(bbox, block, MARGIN_CAP)
                regions += [(t_idx, lo, hi, ids) for lo, hi, ids in regs]
                n_outliers += n_out
                n_objects += len(bbox)

            if not regions:
                log.log('[WARN] No cells found — no output written')
                return None
            log.log(f'>> measuring {n_objects} objects in {len(regions)} blocks of {block} px '
                    f'across {n_workers} worker(s)')
            if n_outliers:
                log.log(f'>> {n_outliers} object(s) reach more than {MARGIN_CAP} px past their '
                        f'block; measured from their own crop')

            # Pass 2 — each owned object from its block's crop. PROCESSES, not threads: skimage's
            # per-object props hold the GIL, so threads stall at ~2 workers and get slower past 4,
            # while processes scale ~5x at 8 (measured, docs/todo/MEASURE_TILING_PLAN.md → P2).
            results: list = []
            step = max(1, len(regions) // 100)

            def _done(i, df):
                if df is not None:
                    if pass_of is not None:
                        # HERE, not after the concat: `pd.concat(..., ignore_index=True)` discards
                        # the index, and the index is the label id. Stamped as a column it survives.
                        df['pass'] = [pass_of(lbl) for lbl in df.index]
                    results.append(df)
                if (i + 1) % step == 0 or i + 1 == len(regions):
                    log.progress(i + 1, len(regions))

            if n_workers == 1 or len(regions) == 1:
                for i, r in enumerate(regions):
                    _done(i, self._measure_region(*r, ctx))
            else:
                # spawn on every OS: Windows has nothing else, it is macOS's default, and forking a
                # process that has run blosc/thread pools can deadlock on Linux. One behaviour on all
                # three, at the cost of each worker importing this module (~1-2 s, in parallel).
                wctx = {k: v for k, v in ctx.items() if k != 'log'}
                with cf.ProcessPoolExecutor(
                        max_workers=min(n_workers, len(regions)),
                        mp_context=mp.get_context('spawn'),
                        initializer=_worker_init, initargs=(self, wctx)) as ex:
                    futs = [ex.submit(_worker_measure, r) for r in regions]
                    for i, fut in enumerate(cf.as_completed(futs)):
                        _done(i, fut.result())
        finally:
            if pool is not None:
                pool.shutdown()

        all_dfs: list[pd.DataFrame] = results

        if not all_dfs:
            log.log('[WARN] No cells found — no output written')
            return None

        # Carry the label index through the concat — otherwise `_to_anndata` writes a 0..N-1 row
        # index which `label_props.jl` then reads AS the label id, silently shifting every row.
        for d in all_dfs:
            d['label'] = d.index
        # Blocks finish in any order; restore the (t, label) order a single whole-plane pass gives.
        full_df = (pd.concat(all_dfs, ignore_index=True)
                     .sort_values(['t', 'label'], kind='stable').set_index('label'))
        out_path = self._to_anndata(full_df, is_3d, n_t)
        log.log(f'>> wrote {out_path}')
        return out_path

    # ── one block ─────────────────────────────────────────────────────────────

    def _measure_region(self, t_idx: int, lo, hi, ids: np.ndarray, ctx: dict) -> pd.DataFrame | None:
        """Measure the objects `ids` (sorted) from the crop `lo:hi` of timepoint `t_idx`.

        Every object in `ids` lies entirely inside the crop (`plan_regions`). Everything else in it
        is zeroed, and the owned ids are relabelled 1..k: skimage's per-call cost is O(max label id),
        and each object's values depend only on its own pixels, so this is the whole-plane result.
        """
        spatial = ctx['spatial']
        sl = [slice(int(a), int(b)) for a, b in zip(lo, hi)]
        store_sl = _store_index(ctx['store_axes'], spatial, sl)
        dense = _dense_labels(
            _read_label_region(ctx['label_zarrs']['base'][0], ctx['l_la_t'], t_idx, store_sl), ids)

        df = self._measure_morphology(dense, ctx['is_3d'], ctx['log'],
                                      origin=[s.start for s in sl])
        if df is None or len(df) == 0:
            return None
        # Coordinates come out crop-local; move them into the image frame.
        for k, s in enumerate(sl):
            col = f'centroid-{k}'
            if col in df:
                df[col] = df[col] + s.start

        crops = {'base': dense}
        for ltype, lzarr in ctx['label_zarrs'].items():
            if ltype != 'base':
                crops[ltype] = _dense_labels(
                    _read_label_region(lzarr[0], ctx['l_la_t'], t_idx, store_sl), ids)
        # The label→pixel grouping is the same for every channel, so build it once per label type.
        groups = {lt: np.unique(c.ravel(), return_inverse=True) for lt, c in crops.items()} \
            if self.intensity_measure == 'mean' else None

        for c in range(ctx['n_c']):
            chan = self._read_channel(ctx['im_arr'], ctx['la_t'], ctx['la_c'], t_idx, c, sl, spatial)
            for ltype, crop in crops.items():
                prefix = '' if ltype == 'base' else f'{ltype}_'
                col = f'{prefix}{self.intensity_measure}_intensity_{c}'
                if groups is not None:
                    s = _mean_by_label(*groups[ltype], chan)
                else:
                    s = self._median_intensity(crop, chan, df.index, col)
                df[col] = df.index.map(s)

        df.index = ids[df.index.to_numpy() - 1]
        df.index.name = 'label'
        df['t'] = t_idx
        return df

    def _read_channel(self, im_arr, la_t, la_c, t_idx: int, c: int, sl: list,
                      spatial: list) -> np.ndarray:
        """Channel `c` of timepoint `t_idx` over the crop `sl`, as float32, smoothed if asked.

        With a Gaussian, the read carries a halo of the kernel radius on Y and X and is trimmed
        after — the same values as smoothing the whole plane (Z is always read whole). An image axis
        not in `spatial` (a dropped singleton Z) is read at 0, so the plane comes back Y,X."""
        halo = int(_GAUSS_TRUNCATE * self.gaussian_filter + 0.5) if self.gaussian_filter > 0 else 0
        read_sl, trim = [], []
        for ax, s in zip(spatial, sl):
            if halo and ax in ('Y', 'X'):
                n = im_arr.shape[self.dim_utils.im_dim_order.index(ax)]
                a, b = max(0, s.start - halo), min(n, s.stop + halo)
                read_sl.append(slice(a, b))
                trim.append(slice(s.start - a, s.start - a + (s.stop - s.start)))
            else:
                read_sl.append(s)
                trim.append(slice(None))
        by_ax = dict(zip(spatial, read_sl))
        full = tuple(t_idx if i == la_t else c if i == la_c else by_ax.get(ax, 0)
                     for i, ax in enumerate(self.dim_utils.im_dim_order))
        chan = np.asarray(im_arr[full]).astype(np.float32)
        if halo:
            chan = skfilt.gaussian(chan, sigma=self.gaussian_filter,
                                   preserve_range=True).astype(np.float32)[tuple(trim)]
        return chan

    # ── morphology ────────────────────────────────────────────────────────────

    def _measure_morphology(self, vol: np.ndarray, is_3d: bool, log,
                            origin=None) -> pd.DataFrame | None:
        use_trimesh = is_3d and self.extended_measures and _HAS_TRIMESH
        if is_3d:
            props_list = _PROPS_3D_BASE if use_trimesh else _PROPS_3D
        else:
            props_list = _PROPS_2D

        with warnings.catch_warnings():
            warnings.simplefilter('ignore')
            tbl = skmeas.regionprops_table(vol, properties=props_list)

        df = pd.DataFrame(tbl).set_index('label')
        if len(df) == 0:
            return None

        if is_3d:
            if not use_trimesh:
                df = self._add_3d_derived(df)
            if self.extended_measures:
                df = self._extended_3d_measures(vol, df, log, origin=origin)
        else:
            df = self._add_2d_derived(df)

        return df

    def _add_2d_derived(self, df: pd.DataFrame) -> pd.DataFrame:
        # 2D shape descriptors — ported verbatim from the old R measure_utils.py so the two versions
        # agree (2D block, lines ~570-596). Axis-length ratios (NOT the circularity proxy the interim
        # port used): oblate/prolate are minor↔major axis ratios, matching the 3D ellipticity set.
        if 'major_axis_length' in df and 'minor_axis_length' in df:
            maj  = df['major_axis_length'].replace(0, np.nan)
            minr = df['minor_axis_length'].replace(0, np.nan)
            df['oblate']  = df['minor_axis_length'] / maj      # ∈ (0,1], 1 = round
            df['prolate'] = df['major_axis_length'] / minr     # ≥ 1
        if 'major_axis_length' in df and 'equivalent_diameter' in df:      # Mesmer-style aspect ratio
            df['aspect_ratio'] = df['major_axis_length'] / df['equivalent_diameter'].replace(0, np.nan)
        if 'perimeter' in df and 'area' in df:
            df['perimeter_to_area'] = (df['perimeter'] ** 2) / df['area'].replace(0, np.nan)
        if 'convex_area' in df and 'area' in df:                          # fraction of hull NOT filled
            ca = df['convex_area'].replace(0, np.nan)
            df['fill'] = (df['convex_area'] - df['area']) / ca
        return df

    def _add_3d_derived(self, df: pd.DataFrame) -> pd.DataFrame:
        # Derive axis lengths from inertia tensor eigenvalues.
        # For a solid ellipsoid of uniform density:
        #   I_x = (m/5)(b² + c²), etc.  → solve for a,b,c up to a scale.
        # We use: axis_k ∝ sqrt(I_j + I_k − I_i) for cyclic (i,j,k).
        ev_cols = [c for c in df.columns if c.startswith('inertia_tensor_eigvals')]
        if len(ev_cols) == 3:
            ev = df[ev_cols].values
            s  = ev.sum(axis=1, keepdims=True)
            # each axis² ∝ s − 2*ev_i
            axis_sq = np.clip(s - 2 * ev, 0, None)
            axes    = np.sqrt(axis_sq)
            idx     = np.argsort(-axes, axis=1)   # descending
            df['major_axis_length']  = axes[np.arange(len(axes)), idx[:, 0]]
            df['interm_axis_length'] = axes[np.arange(len(axes)), idx[:, 1]]
            df['minor_axis_length']  = axes[np.arange(len(axes)), idx[:, 2]]
            df.drop(columns=ev_cols, inplace=True)
            # ellipticity ratios from the ellipsoid axis lengths — cheap (no mesh, no extendedMeasures),
            # so a plain 3D segmentation gets oblate/prolate too. Ports the old R 3D ellipticity set
            # (measure_utils.py lines ~557-560): minor↔major and the intermediate-axis variants.
            df = self._add_ellipticity(df)
        return df

    @staticmethod
    def _add_ellipticity(df: pd.DataFrame) -> pd.DataFrame:
        """3D ellipticity ratios from major ≥ interm ≥ minor axis lengths (old R `ellipticity_*`)."""
        need = ('major_axis_length', 'interm_axis_length', 'minor_axis_length')
        if not all(c in df for c in need):
            return df
        maj    = df['major_axis_length'].replace(0, np.nan)
        interm = df['interm_axis_length'].replace(0, np.nan)
        minr   = df['minor_axis_length'].replace(0, np.nan)
        df['ellipticity_oblate']         = df['minor_axis_length']  / maj      # ∈ (0,1], 1 = spherical
        df['ellipticity_prolate']        = df['major_axis_length']  / minr     # ≥ 1
        df['ellipticity_interm_oblate']  = df['minor_axis_length']  / interm
        df['ellipticity_interm_prolate'] = df['interm_axis_length'] / minr
        return df

    # ── extended 3D via trimesh ───────────────────────────────────────────────

    def _extended_3d_measures(self, vol: np.ndarray, df: pd.DataFrame, log,
                              origin=None) -> pd.DataFrame:
        # Meshes come from the SHARED builder (mesh_utils.build_label_meshes), which marches cubes on
        # each label's BOUNDING BOX and offsets the vertices back into the full-volume frame. This
        # used to march cubes over `vol == lb` — the WHOLE volume — once per cell, which is
        # O(n_cells x volume) and was the single largest cost in the whole segment+measure pipeline:
        # 9.88 s of an 11.35 s timepoint on a 32x420x441 crop. It scales the wrong way twice, because
        # a bigger image has both more voxels AND more cells: 6.88x the volume measured 6.54x the
        # per-cell cost, so a full 37x1039x1060 x 181 T movie projected to ~27 h of measurement.
        #
        # The vertex OFFSET is not cosmetic — it is what keeps qhull's input coordinates identical to
        # the whole-volume form. With it, all nine mesh measures below are bit-identical to the old
        # code (verified over 719 cells); without it the three convex-hull axis lengths drift by up
        # to 0.8%, because qhull is sensitive to the coordinate range it is handed.
        #
        # phys=1: these measures are in VOXEL units (the physical scale is applied downstream), which
        # is the one thing this caller needs that the spatial/mesh caller does not.
        from cecelia.utils.mesh_utils import build_label_meshes

        rows: dict[int, dict] = {}
        labels = df.index.tolist()
        # `origin` puts a block crop's vertices back in the image frame, for the same qhull reason.
        meshes = build_label_meshes(vol, labels, phys=(1.0, 1.0, 1.0), min_voxels=4, origin=origin)

        for lb, mesh in meshes.items():
            ch = mesh.convex_hull
            row: dict = {
                'surface_area':       float(mesh.area),
                'volume_mesh':        float(mesh.volume),
                'convex_hull_area':   float(ch.area),
                'convex_hull_volume': float(ch.volume),
                'euler_number_mesh':  float(mesh.euler_number),
            }

            # solidity from mesh volumes (only meaningful measure of solidity in 3D → keep the plain
            # name, matching old R; the 2D regionprops `solidity` never coexists with the mesh path)
            if ch.volume > 0:
                row['solidity'] = float(mesh.volume) / float(ch.volume)

            # surface-to-volume
            if mesh.volume > 0:
                row['surface_to_volume'] = float(mesh.area) / float(mesh.volume)

            # sphericity (Wadell): area of the equal-volume sphere / actual area. Range (0,1], 1 = sphere.
            # NOTE: `compactness` was intentionally DROPPED. The old R version defined it as
            # surface_area**1.5 / volume, but that is a monotone transform of sphericity (both are pure
            # functions of the isoperimetric ratio A**3/V**2), so it carries no independent information —
            # and the earlier port had re-derived it to a formula that equalled sphericity exactly. Use
            # `solidity` / `extent` for genuinely orthogonal shape descriptors.
            if mesh.area > 0 and mesh.volume > 0:
                r_eq = (3 * mesh.volume / (4 * np.pi)) ** (1 / 3)
                row['sphericity'] = (4 * np.pi * r_eq ** 2) / float(mesh.area)

            # ellipsoid fit from convex hull vertices
            try:
                verts  = np.array(ch.vertices)
                cov    = np.cov(verts.T)
                eigvals = np.sort(np.linalg.eigvalsh(cov))[::-1]
                radii  = np.sqrt(np.maximum(eigvals, 0)) * 2
                row['major_axis_length']  = float(radii[0])
                row['interm_axis_length'] = float(radii[1])
                row['minor_axis_length']  = float(radii[2])
                # ellipticity ratios are derived from these axis lengths below (via _add_ellipticity),
                # so both the mesh and moments paths use the SAME formula (was inconsistent here).
            except Exception:
                pass

            # Feret diameter from mesh bounding box
            bb = mesh.bounding_box
            if hasattr(bb, 'extents') and bb.extents is not None:
                row['feret_diameter_max_mesh'] = float(np.max(bb.extents))

            rows[lb] = row

        if rows:
            ext_df = pd.DataFrame.from_dict(rows, orient='index')
            ext_df.index.name = 'label'
            # override axis length columns if ellipsoid fit succeeded
            for col in ('major_axis_length', 'interm_axis_length', 'minor_axis_length'):
                if col in ext_df.columns:
                    df[col] = ext_df[col]
            for col in ext_df.columns:
                if col not in ('major_axis_length', 'interm_axis_length', 'minor_axis_length'):
                    df[col] = ext_df[col]
            # ellipticity ratios from the mesh-derived axis lengths — same formula as the moments path
            df = self._add_ellipticity(df)

        return df

    # ── intensity measurement ─────────────────────────────────────────────────

    def _median_intensity(self, label_vol: np.ndarray, chan: np.ndarray,
                          label_ids, col_name: str) -> pd.Series:
        vals = {}
        for lb in skmeas.regionprops(label_vol):
            slices = lb.slice
            mask   = label_vol[slices] == lb.label
            vals[lb.label] = float(np.median(chan[slices][mask]))
        return pd.Series(vals, name=col_name)

    # ── AnnData output ────────────────────────────────────────────────────────

    def _to_anndata(self, df: pd.DataFrame, is_3d: bool, n_t: int) -> str:
        # skimage names its centroid columns positionally (`centroid-0..N`, z,y,x order) — rename to
        # explicit axis names on the way into obsm so `centroid-N` never lands on disk (the readers
        # reject it; see docs/todo/CENTROID_AXES_PLAN.md). Sort by index to fix z,y,x order.
        spatial_src   = sorted((c for c in df.columns if c.startswith('centroid-')),
                               key=lambda c: int(c.split('-')[1]))
        temporal_src  = ['t'] if 't' in df.columns else []
        # A model-group key, not a quantity — categorical in `obs`, never a float in X, where it
        # would join every distance and clustering computation as if a pass number were a measurement.
        obs_src       = ['pass'] if 'pass' in df.columns else []

        # separate spatial/temporal into obsm; keep everything else in X
        obsm_spatial  = df[spatial_src].values.astype(np.float32) if spatial_src  else None
        obsm_temporal = df[temporal_src].values.astype(np.float32) if temporal_src else None

        feature_cols = [c for c in df.columns
                        if c not in spatial_src and c not in temporal_src
                        and c not in obs_src]
        X = df[feature_cols].values.astype(np.float32)

        # Strip the index name: AnnData stores an unnamed index at `obs/_index` (the readers'
        # convention). A named index writes to `obs/<name>` with the name in an `_index` attribute,
        # which `label_props.jl` doesn't follow and reads as an empty obs table.
        obs = pd.DataFrame(index=df.index.rename(None).astype(str))
        for c in obs_src:
            # str, and NaN spelt as a real category: an id no range covers means the store predates
            # pass recording, so "unknown" is the honest value and dropping the row would be a lie.
            obs[c] = pd.Categorical(df[c].astype('object')
                                        .where(df[c].notna(), 'unknown').astype(str).values)

        adata = ad.AnnData(
            X   = X,
            obs = obs,
            var = pd.DataFrame(index=feature_cols),
        )
        if obsm_spatial is not None:
            adata.obsm['spatial']  = obsm_spatial
            adata.uns['spatial_cols'] = skimage_centroid_axis_names(len(spatial_src))  # centroid_z/_y/_x
        if obsm_temporal is not None:
            adata.obsm['temporal'] = obsm_temporal
            adata.uns['temporal_cols'] = ['centroid_t']
        adata.uns['intensity_measure'] = self.intensity_measure

        out_dir = os.path.join(self.task_dir, 'labelProps')
        os.makedirs(out_dir, exist_ok=True)
        out_path = os.path.join(out_dir, f'{self.output_value_name}.h5ad')
        write_h5ad_atomic(adata, out_path)
        return out_path


def _mean_by_label(uniq: np.ndarray, inv: np.ndarray, chan: np.ndarray) -> pd.Series:
    """Per-label mean of `chan` from a precomputed `np.unique(labels, return_inverse=True)`.

    A sum and a count per label: ~5× faster than `regionprops` `mean_intensity` per channel."""
    inv = inv.ravel()
    sums = np.bincount(inv, weights=chan.ravel(), minlength=uniq.size)
    cnts = np.bincount(inv, minlength=uniq.size)
    keep = uniq != 0
    return pd.Series(sums[keep] / cnts[keep], index=uniq[keep])
