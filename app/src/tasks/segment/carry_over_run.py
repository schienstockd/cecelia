"""
Obs carry-over runner for the `segment.correct_measures` composite (P2 Decision 4b).

Two phases, dispatched on `phase` param:

  - `snapshot`: read `labelProps/{vn}.h5ad`, serialise every obs column (numeric + categorical)
    keyed by label id to a JSON blob under `task_run_dir()`.
  - `restore`: read the snapshot, read the freshly-re-measured h5ad, add back every column NOT
    present in the fresh frame (measureLabels' own morphology always wins), routing numerics to
    `LabelPropsView.add_obs` and categoricals to `add_categorical_obs`.

WHY JSON, NOT AN H5AD COPY. `write_json_atomic` is the mandated durable writer for structured
state. An h5ad copy would carry the feature matrix `X` unnecessarily and require a direct
`ad.read_h5ad` at restore — this stays obs-only, uses the sanctioned atomic helper, and is
inspectable when a composite goes sideways.

NAN SEMANTICS. `add_obs` fills missing labels with `np.nan` (`label_props_utils.py:286`), and every
obs column it writes is float64 — matching the codebase's existing behaviour (see the track_id
writer in tracking_utils.py). Int columns become float64 with NaN for absent rows; this is what
happens today with tracking's own writes. Categoricals go through `add_categorical_obs`, which
encodes as a pandas `Categorical` (missing = code -1) — the only path that produces non-float obs.
"""

import json
import os

import numpy as np
import pandas as pd

from cecelia.utils import script_utils
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils.label_props_utils import LabelPropsUtils, LabelPropsView


def _is_categorical(series: pd.Series) -> bool:
    """A column snapshots as categorical iff its dtype needs `add_categorical_obs` on restore.
    That's the pandas Categorical dtype, plus object/string dtypes (HMM state strings, cluster
    ids written as `"1"` etc. — encoded as categorical on write per `add_categorical_obs`).

    Pandas' nullable string extension dtype (`pd.StringDtype`) is NOT `object` — it's an extension
    dtype with its own class. Treat it as categorical too: without this, F6 sent a `StringDtype`
    column through `_serialise_numeric` → `np.asarray(dtype=float)` → TypeError.
    """
    dtype = series.dtype
    if isinstance(dtype, pd.CategoricalDtype):
        return True
    if dtype == object:
        return True
    if isinstance(dtype, pd.StringDtype):
        return True
    return False


def _serialise_numeric(series: pd.Series) -> list:
    """Numpy → JSON-safe list. NaN → None (json can't encode NaN; None round-trips back to NaN
    inside `add_obs`, which fills missing labels with NaN itself).

    Extension numeric dtypes (`pd.Int64Dtype`, `pd.BooleanDtype`, `pd.Float64Dtype`, etc.) use
    `.to_numpy(dtype=float, na_value=np.nan)` — `np.asarray(dtype=float)` raises `TypeError` on
    `pd.NA`, which crashed the snapshotter for any h5ad carrying a nullable-int `track_id` or a
    nullable-bool gate col (F6). Non-extension numpy dtypes take the fast path unchanged.
    """
    dtype = series.dtype
    if pd.api.types.is_extension_array_dtype(dtype):
        # Extension arrays expose `to_numpy(na_value=…)`; use it so pd.NA becomes np.nan cleanly.
        arr = series.to_numpy(dtype=float, na_value=np.nan)
    else:
        arr = np.asarray(series, dtype=float)
    return [None if np.isnan(x) else float(x) for x in arr]


def _serialise_categorical(series: pd.Series) -> list:
    """String or None per row. Missing (NaN, None, or the sentinel float(nan)) → None."""
    out = []
    for v in series:
        if v is None:
            out.append(None)
        elif isinstance(v, float) and np.isnan(v):
            out.append(None)
        else:
            out.append(str(v))
    return out


def run_snapshot(params: dict, log) -> None:
    task_dir      = params['taskDir']
    value_name    = params['valueName']
    snapshot_file = params['snapshotFile']
    result_file   = params['resultFile']

    view = LabelPropsUtils(task_dir, value_name).label_props_view()
    labels = view.labels().astype(int).tolist()
    obs = view.adata.obs

    numeric_cols: dict = {}
    categorical_cols: dict = {}
    for col in obs.columns:
        series = obs[col]
        if _is_categorical(series):
            categorical_cols[col] = _serialise_categorical(series)
        else:
            numeric_cols[col] = _serialise_numeric(series)

    payload = {
        'valueName':   value_name,
        'labels':      labels,
        'numeric':     numeric_cols,
        'categorical': categorical_cols,
    }
    write_json_atomic(snapshot_file, payload)

    result = {
        'nRowsSnapshotted':  len(labels),
        'nNumericCols':      len(numeric_cols),
        'nCategoricalCols':  len(categorical_cols),
    }
    write_json_atomic(result_file, result)
    log.log(f'>> snapshot: {len(labels)} row(s), '
            f'{len(numeric_cols)} numeric + {len(categorical_cols)} categorical col(s)')


def run_restore(params: dict, log) -> None:
    task_dir      = params['taskDir']
    value_name    = params['valueName']
    snapshot_file = params['snapshotFile']
    result_file   = params['resultFile']

    if not os.path.exists(snapshot_file):
        write_json_atomic(result_file,
                          {'nRowsCarried': 0, 'nColsCarried': 0, 'nColsSkipped': 0})
        log.log(f'>> restore: snapshot missing at {snapshot_file} — no-op')
        return

    with open(snapshot_file, 'r', encoding='utf-8') as f:
        snap = json.load(f)

    labels = [int(x) for x in snap.get('labels', [])]
    numeric_snap: dict     = snap.get('numeric', {}) or {}
    categorical_snap: dict = snap.get('categorical', {}) or {}

    view = LabelPropsUtils(task_dir, value_name).label_props_view()
    fresh_cols = set(view.adata.obs.columns)

    # Only carry columns NOT present in the fresh frame — measureLabels' own morphology (area,
    # mean_intensity_*, centroid_*, etc.) is authoritative for the re-measured labels. Carrying
    # them here would overwrite fresh numbers with pre-correction stale ones.
    numeric_to_carry     = {c: v for c, v in numeric_snap.items()     if c not in fresh_cols}
    categorical_to_carry = {c: v for c, v in categorical_snap.items() if c not in fresh_cols}
    n_carried = len(numeric_to_carry) + len(categorical_to_carry)
    n_skipped = (len(numeric_snap) - len(numeric_to_carry)) + \
                (len(categorical_snap) - len(categorical_to_carry))

    if n_carried == 0:
        write_json_atomic(result_file,
                          {'nRowsCarried': 0, 'nColsCarried': 0, 'nColsSkipped': n_skipped})
        log.log(f'>> restore: nothing to carry (all {n_skipped} col(s) regenerated by measureLabels)')
        return

    # Build one DataFrame for numerics — `add_obs` aligns by 'label' and silently drops labels
    # missing from the fresh h5ad (dropped ids in a `label.remove` op; sacrificed ids in a
    # `label.merge`). Sacrificed ids inheriting nothing is correct: the pixel semantics is
    # `src → into`, so the survivor `into` inherits its OWN pre-op obs by label equality.
    if numeric_to_carry:
        df = pd.DataFrame({'label': labels})
        for col, vals in numeric_to_carry.items():
            df[col] = pd.array([np.nan if v is None else float(v) for v in vals], dtype=float)
        view.add_obs(df)

    for col, vals in categorical_to_carry.items():
        view.add_categorical_obs(col, labels, vals)

    view.save()

    # Count of rows that actually received a carried column — the intersection of the snapshot's
    # labels with the fresh h5ad's labels. Cheaper as a set op than re-reading post-save.
    fresh_labels = set(int(x) for x in LabelPropsUtils(task_dir, value_name)
                                       .label_props_view().labels().tolist())
    n_rows_carried = len(set(labels) & fresh_labels)

    write_json_atomic(result_file, {
        'nRowsCarried':  n_rows_carried,
        'nColsCarried':  n_carried,
        'nColsSkipped':  n_skipped,
    })
    log.log(f'>> restore: {n_carried} col(s) onto {n_rows_carried} row(s); skipped {n_skipped}')


def run(params: dict) -> None:
    log = script_utils.get_logfile_utils(params)
    phase = params.get('phase')
    if phase == 'snapshot':
        run_snapshot(params, log)
        return
    if phase == 'restore':
        run_restore(params, log)
        return
    raise ValueError(f'carry_over_run: unknown phase {phase!r}')


if __name__ == '__main__':
    import sys
    params_path = sys.argv[1]
    with open(params_path, 'r', encoding='utf-8') as f:
        params = json.load(f)
    run(params)
