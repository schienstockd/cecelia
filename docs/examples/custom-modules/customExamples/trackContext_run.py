"""Example custom-module Python compute — standardise the Julia-written track-mean column.

Reads `<inColumn>` from the .h5ad via the sanctioned LabelPropsView, computes a z-score (or a robust
median/MAD version), and writes it back as `<outColumn>`. Launched by the `customExamples.trackContext`
Julia task through `run_py`; `cecelia.*` resolves via PYTHONPATH (set by run_py — no sys.path hacks).
See docs/CUSTOM_MODULES.md.

Parameter contract (JSON written by the Julia task):
  labelPropsPath - absolute path to the segmentation's labelProps .h5ad
  inColumn       - obs column to read (the Julia-written track mean)
  outColumn      - obs column to write (the standardised values)
  method         - "zscore" | "robust"
"""
import numpy as np

import cecelia.utils.script_utils as script_utils
from cecelia.utils.label_props_utils import LabelPropsView


def run(params):
    log     = script_utils.get_logfile_utils(params)
    path    = params['labelPropsPath']
    in_col  = params['inColumn']
    out_col = params['outColumn']
    method  = params.get('method', 'zscore')

    log.progress(0, 1)
    view = LabelPropsView(path)
    df = view.view_cols([in_col]).as_df()          # label + in_col
    x  = df[in_col].to_numpy(dtype=float)
    finite = np.isfinite(x)

    z = np.full_like(x, np.nan)
    if finite.sum() > 1:
        if method == 'robust':
            med = np.median(x[finite])
            mad = np.median(np.abs(x[finite] - med)) or 1.0
            z[finite] = (x[finite] - med) / (1.4826 * mad)
        else:  # zscore
            mu, sd = x[finite].mean(), x[finite].std()
            z[finite] = (x[finite] - mu) / sd if sd > 0 else 0.0

    out = df[['label']].copy()
    out[out_col] = z
    view.add_obs(out).save()                        # write-back via the same view chain
    log.log(f"[INFO] Python wrote {out_col} ({int(finite.sum())} finite cells, method={method})")
    log.progress(1, 1)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
