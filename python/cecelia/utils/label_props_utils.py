"""
Minimal Python reader for the AnnData label-property files (`labelProps/{value_name}.h5ad`).

Mirrors the Julia `app/src/label_props.jl` reader on the Python side. It is the *local*
column source for the no-bulk-data-over-HTTP contract (docs/POPULATION.md): membership
(which cells) comes from the Julia API; the measurement / centroid columns for those cells
are read here, directly from disk. Used by `pop_utils.PopUtils.pop_df` and by the napari
bridge (centroid lookup for population overlays + spatial cell selection).

H5AD layout (written by `measure_run.py`): `X` = feature matrix (var_names = feature cols);
`obs.index` = integer cell label; `obsm['spatial']` + `uns['spatial_cols']` = centroids
(`centroid-0..N`, skimage order: z?, y, x); `obsm['temporal']` + `uns['temporal_cols']` =
time (`t`). Files are small (hundreds of KB), so a plain in-memory read is fine.
"""
import os

import anndata as ad
import numpy as np
import pandas as pd


class LabelPropsView:
    """A chainable, read-only view over one label-props H5AD. Methods return ``self`` so
    calls compose: ``view.view_centroid_cols().filter_by_label(ids).as_df()``."""

    def __init__(self, filepath: str):
        self.filepath = filepath
        self.adata = ad.read_h5ad(filepath)
        self._cols = None            # None = all feature columns; else a subset
        self._with_centroids = False
        self._only_centroids = False
        self._label_ids = None       # None = all cells
        self._pending_obs = None     # staged obs columns to write; flushed by save()
        self._pending_drop = None    # staged obs column names to delete; flushed by save()
        self._pending_obsm = None    # staged obsm matrices to write; flushed by save()

    # ── metadata ────────────────────────────────────────────────────────────────
    def labels(self) -> np.ndarray:
        """Integer cell label IDs (the obs index)."""
        return self.adata.obs.index.astype(np.int64).to_numpy()

    def centroid_columns(self) -> list:
        return list(self.adata.uns.get("spatial_cols", []))

    def temporal_columns(self) -> list:
        return list(self.adata.uns.get("temporal_cols", []))

    def var_names(self) -> list:
        return list(self.adata.var_names)

    def obsm_keys(self) -> list:
        return list(self.adata.obsm.keys())

    def obsm(self, key: str):
        """The full (n_obs, k) obsm matrix for `key` (e.g. an embedding 'X_umap'), in obs order,
        or None if absent. Pair with `labels()` for label alignment. Mirrors the Julia `obsm`."""
        return np.asarray(self.adata.obsm[key]) if key in self.adata.obsm else None

    # ── view builders ─────────────────────────────────────────────────────────────
    def view_cols(self, cols):
        self._cols = list(cols)
        return self

    def view_centroid_cols(self):
        self._with_centroids = True
        return self

    def only_centroid_cols(self):
        """Centroids (+ temporal) + label only — skip the feature matrix."""
        self._with_centroids = True
        self._only_centroids = True
        return self

    def filter_by_label(self, label_ids):
        """Filter rows to these label IDs. Intersection semantics (mirrors the Julia
        `filter_rows`): IDs absent from the file are silently skipped — no row, no NaN, no
        error — so the result has ``<= len(label_ids)`` rows in file order."""
        self._label_ids = set(int(x) for x in label_ids)
        return self

    # ── materialise ───────────────────────────────────────────────────────────────
    def as_df(self) -> pd.DataFrame:
        a = self.adata
        labels = a.obs.index.astype(np.int64).to_numpy()
        data = {}

        if not self._only_centroids:
            X = np.asarray(a.X)
            var_names = list(a.var_names)
            cols = self._cols if self._cols is not None else var_names
            for c in cols:
                if c in var_names:
                    data[c] = X[:, var_names.index(c)]
            # obs columns (e.g. track_id for live pops) — include those present/requested
            for c in a.obs.columns:
                if self._cols is None or c in self._cols:
                    data[c] = a.obs[c].to_numpy()

        if self._with_centroids and "spatial" in a.obsm:
            sp = np.asarray(a.obsm["spatial"])
            for i, name in enumerate(self.centroid_columns()):
                data[name] = sp[:, i]
            if "temporal" in a.obsm:
                tp = np.asarray(a.obsm["temporal"])
                for i, name in enumerate(self.temporal_columns()):
                    data[name] = tp[:, i]

        df = pd.DataFrame(data)
        df["label"] = labels
        if self._label_ids is not None:
            df = df[df["label"].isin(self._label_ids)].reset_index(drop=True)
        return df

    # ── write path (mirror of the Julia LabelProps writer) ─────────────────────────
    # You read a label-keyed DataFrame (`as_df`); you write a label-keyed DataFrame
    # (`add_obs(...).save()`). Same chain idiom as Julia so there is one way to touch
    # H5AD storage in either language (docs/DATAMODEL.md).
    def add_obs(self, data):
        """Stage obs columns to write. ``data`` is a DataFrame with a ``label`` column
        plus one column per obs field (aligned to the obs index by label; labels absent
        from ``data`` get NaN), or a dict ``{name: array}`` already in obs order. Repeated
        calls accumulate. Terminal verb is ``save()``."""
        if self._pending_obs is None:
            self._pending_obs = {}
        labels = self.adata.obs.index.astype(np.int64).to_numpy()
        if isinstance(data, pd.DataFrame):
            if "label" not in data.columns:
                raise ValueError("add_obs: DataFrame needs a 'label' column")
            rowof = {int(l): i for i, l in enumerate(labels)}
            for c in data.columns:
                if c == "label":
                    continue
                arr = np.full(len(labels), np.nan)
                for lab, v in zip(data["label"].astype(np.int64), data[c]):
                    r = rowof.get(int(lab))
                    if r is not None:
                        arr[r] = v
                self._pending_obs[c] = arr
        else:
            for k, v in dict(data).items():
                self._pending_obs[k] = np.asarray(v)
        return self

    def drop_obs(self, names):
        """Stage obs columns to delete (by name). Names absent from the file are ignored
        (idempotent). Mirror of the Julia `drop_obs`. Use to invalidate derived columns whose
        source changed — e.g. tracking drops stale ``live.cell.*`` / ``live.track.*`` measures
        when it writes new tracks. Combine with ``add_obs`` in one chain."""
        if self._pending_drop is None:
            self._pending_drop = []
        for nm in ([names] if isinstance(names, str) else names):
            if nm not in self._pending_drop:
                self._pending_drop.append(nm)
        return self

    def add_obsm(self, key, labels, values):
        """Stage a 2-D obsm matrix to write — e.g. a UMAP embedding ``add_obsm('X_umap', labels,
        xy)``. Aligned to the file's obs index **by label** (mirror of `add_obs`): ``values`` is
        ``(m, k)`` for the ``m`` ``labels``; obs rows whose label is absent get NaN. Repeated calls
        accumulate (later key wins). Terminal verb is ``save()``."""
        if self._pending_obsm is None:
            self._pending_obsm = {}
        vals = np.asarray(values, dtype=float)
        if vals.ndim == 1:
            vals = vals.reshape(-1, 1)
        self._pending_obsm[key] = (np.asarray(labels).astype(np.int64), vals)
        return self

    def save(self):
        """Terminal write: flush staged obs adds/drops + obsm matrices into the .h5ad via anndata.
        No-op if nothing is staged. Drops are applied before adds (so a column can be dropped and
        re-added in one chain)."""
        if not self._pending_obs and not self._pending_drop and not self._pending_obsm:
            return self
        labels = self.adata.obs.index.astype(np.int64).to_numpy()
        for c in (self._pending_drop or []):
            if c in self.adata.obs.columns:
                del self.adata.obs[c]
        for k, v in (self._pending_obs or {}).items():
            self.adata.obs[k] = v
        for key, (lab, vals) in (self._pending_obsm or {}).items():
            rowof = {int(l): i for i, l in enumerate(labels)}
            full = np.full((len(labels), vals.shape[1]), np.nan)
            for l, row in zip(lab, vals):
                r = rowof.get(int(l))
                if r is not None:
                    full[r] = row
            self.adata.obsm[key] = full
        self.adata.write_h5ad(self.filepath)
        self._pending_obs = None
        self._pending_drop = None
        self._pending_obsm = None
        return self

    def close(self):
        self.adata = None


class LabelPropsUtils:
    """Resolves and opens label-props views for an image's task dir. Keeps the old cecelia
    ``label_props_view(value_name=...)`` entry point so `pop_utils.pop_df` is unchanged."""

    def __init__(self, task_dir: str, value_name: str = "default"):
        self.task_dir = task_dir
        self.value_name = value_name

    def label_props_filepath(self, value_name: str = None) -> str:
        vn = value_name or self.value_name
        return os.path.join(self.task_dir, "labelProps", f"{vn}.h5ad")

    def label_props_view(self, value_name: str = None) -> LabelPropsView:
        return LabelPropsView(self.label_props_filepath(value_name))
