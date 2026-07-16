"""Write categorical / string obs columns into a labelProps ``.h5ad``.

Julia's ``LabelProps`` writer (app/src/label_props.jl) emits **float64** obs columns only — it
writes plain numeric arrays directly via HDF5.jl. Categorical columns (HMM states, cluster ids)
and string columns (HMM transitions like ``"1_2"``) need anndata's categorical encoding
(``categories`` + integer ``codes``), so they are written here through anndata — the same
"new encodings are Python's job" split used for the per-track table (docs/DATAMODEL.md,
docs/ARCHITECTURE.md). The Julia reader already decodes categoricals on read.

Invoked as a subprocess by a Julia task (see ``write_categorical_obs`` in label_props.jl).
"""

from cecelia.utils.label_props_utils import LabelPropsView


def write_categorical_obs(params, log=None):
    """Write/replace categorical obs columns in an existing ``.h5ad``, aligned by label.

    Thin driver over ``LabelPropsView.add_categorical_obs`` — the one Python h5ad-open path
    (docs/DATAMODEL.md). This module owns the param contract + logging; the view owns the
    encoding and file I/O.

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

    view = LabelPropsView(filepath)
    if drop:
        view.drop_obs(drop)
        _log(f"[INFO] dropping obs columns: {', '.join(drop)}")

    for col in columns:
        name = col["name"]
        labels = col.get("labels", [])
        values = col.get("values", [])
        view.add_categorical_obs(name, labels, values)
        n_set = sum(1 for v in values if v is not None)
        n_cat = len({str(v) for v in values if v is not None})
        _log(f"[INFO] wrote categorical obs '{name}': {n_set} labelled, {n_cat} categories")

    view.save()
    _log(f"[INFO] saved {filepath}")
