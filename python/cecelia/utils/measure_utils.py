"""
Per-cell label measurement utility.

Computes morphology properties (via skimage.measure.regionprops_table) and
per-channel intensity statistics for each labelled cell, then writes the
result as an AnnData .h5ad file.

For 3D images, basic shape metrics come from skimage.  When extendedMeasures
is True, a trimesh mesh is built per cell via marching cubes, giving accurate
surface area, volume, convex hull metrics, and ellipsoid axes.  The meshes
can optionally be saved to disk for downstream spatial analysis.
"""

from __future__ import annotations

import os
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import zarr
import skimage.measure as skmeas
import skimage.filters as skfilt
import anndata as ad

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


class MeasureUtils:
    def __init__(self, params: dict, dim_utils):
        self.params             = params
        self.dim_utils          = dim_utils
        self.task_dir           = params['taskDir']
        self.output_value_name  = params.get('outputValueName', 'default')
        self.intensity_measure  = params.get('intensityMeasure', 'mean')
        self.gaussian_filter    = float(params.get('gaussianFilter', 0.0))
        self.extended_measures  = bool(params.get('extendedMeasures', False))
        self.save_meshes        = bool(params.get('saveMeshes', False))

    # ── public entry point ────────────────────────────────────────────────────

    def measure_from_zarr(self, label_zarrs: dict, im_dat, log):
        """
        label_zarrs : {'base': zarr.Group, 'nuc': zarr.Group, …}  – multiscale groups
        im_dat      : multiscale zarr group for the intensity image
        log         : script_utils logfile helper (has .log(str))

        All label types are measured together and written to a single .h5ad file.
        Non-base types (nuc, cyto, halo) contribute extra intensity columns to the
        base measurement: {type}_mean_intensity_{c}.

        Returns the path to the written .h5ad file, or None on failure.
        """
        dim_order = self.dim_utils.im_dim_order
        dim_sizes = dict(zip(dim_order, self.dim_utils.im_dim))

        is_3d  = 'Z' in dim_order
        n_t    = int(dim_sizes.get('T', 1))
        n_c    = int(dim_sizes.get('C', 1))
        la_t   = dim_order.index('T') if 'T'  in dim_order else None
        la_c   = dim_order.index('C') if 'C'  in dim_order else None

        # label axis order has no C
        label_dim_order = [ax for ax in dim_order if ax != 'C']
        l_la_t = label_dim_order.index('T') if 'T' in label_dim_order else None

        base_arr = label_zarrs['base']['0']   # full-res array
        im_arr   = im_dat[0]

        if is_3d and self.extended_measures and not _HAS_TRIMESH:
            log.log('[WARN] trimesh not installed — falling back to skimage 3D props')

        all_dfs: list[pd.DataFrame] = []

        for t_idx in range(n_t):
            log.log(f'>> measuring timepoint {t_idx + 1}/{n_t}')

            base_vol = self._extract_t(base_arr, l_la_t, t_idx)
            if base_vol.max() == 0:
                log.log('   (no labels – skipping)')
                print(f'[PROGRESS] {t_idx + 1}/{n_t}', flush=True)
                continue

            im_vol = self._extract_t(im_arr, la_t, t_idx).astype(np.float32)

            if self.gaussian_filter > 0.0:
                im_vol = self._gaussian_smooth(im_vol, la_c, la_t, n_c)

            mesh_dir = None
            if self.save_meshes or self.extended_measures:
                mesh_dir = os.path.join(self.task_dir, 'meshes',
                                        self.output_value_name, f't{t_idx:04d}')

            morph_df = self._measure_morphology(base_vol, is_3d, mesh_dir, log)
            if morph_df is None or len(morph_df) == 0:
                print(f'[PROGRESS] {t_idx + 1}/{n_t}', flush=True)
                continue

            morph_df = self._measure_intensities(
                morph_df, base_vol, im_vol, la_c, la_t, n_c)

            # Non-base label types contribute per-type intensity columns
            for ltype, lzarr in label_zarrs.items():
                if ltype == 'base':
                    continue
                sec_vol = self._extract_t(lzarr['0'], l_la_t, t_idx)
                morph_df = self._measure_secondary_intensities(
                    morph_df, sec_vol, im_vol, la_c, la_t, n_c, ltype)

            morph_df['t'] = t_idx
            all_dfs.append(morph_df)
            print(f'[PROGRESS] {t_idx + 1}/{n_t}', flush=True)

        if not all_dfs:
            log.log('[WARN] No cells found — no output written')
            return None

        full_df = pd.concat(all_dfs, ignore_index=True)
        out_path = self._to_anndata(full_df, is_3d, n_t)
        log.log(f'>> wrote {out_path}')
        return out_path

    # ── morphology ────────────────────────────────────────────────────────────

    def _measure_morphology(self, vol: np.ndarray, is_3d: bool,
                            mesh_dir: str | None, log) -> pd.DataFrame | None:
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
                df = self._extended_3d_measures(vol, df, mesh_dir, log)
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

    def _extended_3d_measures(self, vol: np.ndarray, df: pd.DataFrame,
                              mesh_dir: str | None, log) -> pd.DataFrame:
        import trimesh  # guaranteed available — caller checks _HAS_TRIMESH

        if mesh_dir and self.save_meshes:
            os.makedirs(mesh_dir, exist_ok=True)

        rows: dict[int, dict] = {}
        labels = df.index.tolist()

        for lb in labels:
            mask = (vol == lb)
            if mask.sum() < 4:
                continue

            try:
                mesh = trimesh.voxel.ops.matrix_to_marching_cubes(mask)
                if not mesh.is_watertight:
                    mesh.fill_holes()
            except Exception:
                continue

            if self.save_meshes and mesh_dir:
                mesh.export(os.path.join(mesh_dir, f'{lb}.stl'))

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

    def _measure_intensities(self, df: pd.DataFrame, label_vol: np.ndarray,
                             im_vol: np.ndarray, la_c, la_t, n_c: int) -> pd.DataFrame:
        """Add per-channel intensity columns to df (in-place, returns df)."""
        for c in range(n_c):
            chan = self._extract_channel(im_vol, la_c, la_t, c)
            col  = f'{self.intensity_measure}_intensity_{c}'
            if self.intensity_measure == 'mean':
                tbl = skmeas.regionprops_table(
                    label_vol, intensity_image=chan, properties=['label', 'mean_intensity'])
                s = pd.Series(tbl['mean_intensity'], index=tbl['label'], name=col)
            else:
                s = self._median_intensity(label_vol, chan, df.index, col)
            df[col] = df.index.map(s)
        return df

    def _measure_secondary_intensities(self, df: pd.DataFrame,
                                       sec_vol: np.ndarray, im_vol: np.ndarray,
                                       la_c, la_t, n_c: int,
                                       ltype: str) -> pd.DataFrame:
        """Add {ltype}_{measure}_intensity_{c} columns to df using the secondary label mask."""
        for c in range(n_c):
            chan = self._extract_channel(im_vol, la_c, la_t, c)
            col  = f'{ltype}_{self.intensity_measure}_intensity_{c}'
            if self.intensity_measure == 'mean':
                tbl = skmeas.regionprops_table(
                    sec_vol, intensity_image=chan, properties=['label', 'mean_intensity'])
                s = pd.Series(tbl['mean_intensity'], index=tbl['label'], name=col)
            else:
                s = self._median_intensity(sec_vol, chan, df.index, col)
            df[col] = df.index.map(s)
        return df

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
        spatial_cols   = [c for c in df.columns if c.startswith('centroid-')]
        temporal_cols  = ['t'] if 't' in df.columns else []

        # separate spatial/temporal into obsm; keep everything else in X
        obsm_spatial  = df[spatial_cols].values.astype(np.float32) if spatial_cols  else None
        obsm_temporal = df[temporal_cols].values.astype(np.float32) if temporal_cols else None

        feature_cols = [c for c in df.columns
                        if c not in spatial_cols and c not in temporal_cols]
        X = df[feature_cols].values.astype(np.float32)

        adata = ad.AnnData(
            X   = X,
            obs = pd.DataFrame(index=df.index.astype(str)),
            var = pd.DataFrame(index=feature_cols),
        )
        if obsm_spatial is not None:
            adata.obsm['spatial']  = obsm_spatial
            adata.uns['spatial_cols'] = spatial_cols
        if obsm_temporal is not None:
            adata.obsm['temporal'] = obsm_temporal
            adata.uns['temporal_cols'] = temporal_cols
        adata.uns['intensity_measure'] = self.intensity_measure

        out_dir = os.path.join(self.task_dir, 'labelProps')
        os.makedirs(out_dir, exist_ok=True)
        out_path = os.path.join(out_dir, f'{self.output_value_name}.h5ad')
        adata.write_h5ad(out_path)
        return out_path

    # ── helpers ───────────────────────────────────────────────────────────────

    @staticmethod
    def _extract_t(arr, la_t, t_idx: int) -> np.ndarray:
        if la_t is None:
            return np.asarray(arr[:])
        sl = tuple(t_idx if i == la_t else slice(None) for i in range(arr.ndim))
        return np.asarray(arr[sl])

    @staticmethod
    def _extract_channel(im_vol: np.ndarray, la_c, la_t, c: int) -> np.ndarray:
        if la_c is None:
            return im_vol
        # la_c is the axis in the full (T,Z,Y,X,C) array; after removing T
        # (already done in _extract_t), adjust the axis index accordingly.
        ax = la_c - (1 if la_t is not None else 0)
        sl = tuple(c if i == ax else slice(None) for i in range(im_vol.ndim))
        return im_vol[sl]

    def _gaussian_smooth(self, im_vol: np.ndarray, la_c, la_t, n_c: int) -> np.ndarray:
        if la_c is None:
            return skfilt.gaussian(im_vol, sigma=self.gaussian_filter,
                                   preserve_range=True).astype(np.float32)
        ax = la_c - (1 if la_t is not None else 0)
        out = im_vol.copy()
        for c in range(n_c):
            sl = tuple(c if i == ax else slice(None) for i in range(im_vol.ndim))
            out[sl] = skfilt.gaussian(im_vol[sl], sigma=self.gaussian_filter,
                                      preserve_range=True)
        return out
