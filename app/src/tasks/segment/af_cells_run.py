"""
segment.afCells (image-scope) — Python runner.

Per-cell autofluorescence correction for a cyclic multiplex panel (`cecelia.utils.cell_af_utils`):
reads a label set's per-channel intensities, corrects each channel from the other markers on its dye,
and writes the result as FEATURE columns `af_{measure}_intensity_{i}` beside the measured ones, so
gating, clustering and plots pick them by channel name (`af_CD4`). The measured columns are untouched.

Params: propsPath (the label set's .h5ad), groups (one dye key per channel, null = no group),
exclude (0-based channels to leave as measured), qcOutPath.
"""

import numpy as np

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
from cecelia.utils.label_props_utils import LabelPropsView, _CHANNEL_RE
from cecelia.utils.cell_af_utils import correct_cell_af, same_dye_correlation
from cecelia.utils.atomic_io import write_json_atomic

PREFIX = "af_"


def run(params):
    log = script_utils.get_logfile_utils(params)
    props = params["propsPath"]
    groups = list(script_utils.get_param(params, "groups", default=[]))
    exclude = [int(i) for i in script_utils.get_param(params, "exclude", default=[])]
    qc_path = params.get("qcOutPath")

    view = LabelPropsView(props)
    measure = view.adata.uns.get("intensity_measure", "mean") or "mean"
    # the measured columns: the reader's channel columns with no channel-type prefix (not nuc_, not af_)
    cols = {int(m.group(2)): v for v in view.channel_columns()
            if (m := _CHANNEL_RE.match(v)) and m.group(1) is None}
    if not cols:
        log.log("[ERROR] afCells: the cell table has no measured intensity columns")
        return
    n_ch = max(cols) + 1
    groups = (groups + [None] * n_ch)[:n_ch]
    X = np.zeros((view.adata.n_obs, n_ch))
    df = view.view_cols([cols[i] for i in sorted(cols)]).as_df()
    for i in sorted(cols):
        X[:, i] = df[cols[i]].to_numpy(dtype=np.float64)
    missing = [i for i in range(n_ch) if i not in cols]
    if missing:
        exclude = exclude + missing

    log.progress(1, 3)
    corrected, info = correct_cell_af(X, groups, exclude)
    chans = info["channels"]
    for g, idx in info["groups"].items():
        log.log(f">> afCells: dye {g}: {len(idx)} markers corrected")
    if not chans:
        log.log("[WARN] afCells: no dye carries 3 or more markers — nothing to correct")

    log.progress(2, 3)
    before = same_dye_correlation(X, groups, exclude)
    Xc = X.copy()
    if chans:
        Xc[:, chans] = corrected
    after = same_dye_correlation(Xc, groups, exclude)

    out = {f"{PREFIX}{measure}_intensity_{ch}": corrected[:, k] for k, ch in enumerate(chans)}
    # always clear the earlier run's columns first: a channel now excluded must not keep a stale value
    LabelPropsView(props).set_vars(out, drop_prefix=PREFIX).save()
    log.log(f">> afCells: wrote {len(out)} corrected channel(s) as {PREFIX}{measure}_intensity_*")

    if qc_path is not None:
        write_json_atomic(qc_path, {
            "nCells": int(X.shape[0]),
            "nCorrected": len(chans),
            "nGroups": len(info["groups"]),
            "sameDyeBefore": None if before is None else before[0],
            "sameDyeAfter": None if after is None else after[0],
            "otherDyeBefore": None if before is None else before[1],
            "otherDyeAfter": None if after is None else after[1],
        })
    log.progress(3, 3)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
