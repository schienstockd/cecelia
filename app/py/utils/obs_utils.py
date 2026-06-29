"""Write categorical / string obs columns into a labelProps ``.h5ad``.

Julia's ``LabelProps`` writer (app/src/label_props.jl) emits **float64** obs columns only — it
writes plain numeric arrays directly via HDF5.jl. Categorical columns (HMM states, cluster ids)
and string columns (HMM transitions like ``"1_2"``) need anndata's categorical encoding
(``categories`` + integer ``codes``), so they are written here through anndata — the same
"new encodings are Python's job" split used for the per-track table (docs/DATAMODEL.md,
docs/ARCHITECTURE.md). The Julia reader already decodes categoricals on read.

Invoked as a subprocess by a Julia task (see ``write_categorical_obs`` in label_props.jl).
"""

import anndata as ad
import numpy as np
import pandas as pd


def write_categorical_obs(params, log=None):
    """Write/replace categorical obs columns in an existing ``.h5ad``, aligned by label.

    params:
      - ``filepath``: target ``.h5ad`` (must exist).
      - ``columns``: list of ``{name, labels, values}`` — ``values`` aligned to ``labels``;
        each becomes a categorical obs column (string categories). Labels not present in the
        column, or with a null value, are left unset (NaN / category code -1).
      - ``drop`` (optional): obs column names to delete first (idempotent).
    """
    def _log(m):
        (log.log(m) if log is not None else print(m, flush=True))

    filepath = params["filepath"]
    columns = params.get("columns", []) or []
    drop = params.get("drop", []) or []

    adata = ad.read_h5ad(filepath)
    obs_labels = adata.obs.index.astype(np.int64).to_numpy()
    rowof = {int(l): i for i, l in enumerate(obs_labels)}

    for c in drop:
        if c in adata.obs.columns:
            del adata.obs[c]
            _log(f"[INFO] dropped obs column {c}")

    for col in columns:
        name = col["name"]
        labels = col.get("labels", [])
        values = col.get("values", [])
        arr = np.full(len(obs_labels), None, dtype=object)
        for lab, v in zip(labels, values):
            if v is None:
                continue
            r = rowof.get(int(lab))
            if r is not None:
                arr[r] = str(v)
        adata.obs[name] = pd.Categorical(arr)
        n_set = int(np.sum(arr != np.array(None)))
        n_cat = len(adata.obs[name].cat.categories)
        _log(f"[INFO] wrote categorical obs '{name}': {n_set}/{len(arr)} labelled, {n_cat} categories")

    adata.write_h5ad(filepath)
    _log(f"[INFO] saved {filepath}")
