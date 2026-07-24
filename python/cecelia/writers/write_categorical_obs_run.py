"""Categorical/string obs-column writer entry point.

Called as a subprocess by the label-props layer (`label_props.jl::write_categorical_obs`) to write
categorical cell labels — HMM states / transitions today, cluster ids later. Julia computes the
values and passes them here; this script writes them into the segmentation's labelProps ``.h5ad``
via anndata's categorical encoding (Julia writes numeric obs directly, but not categoricals). It is
a data-layer writer (the h5ad write-side counterpart to the readers), NOT a scheduler task — so it
lives in the IO library under ``python/cecelia/writers/``. Task runners, by contrast, live beside
their Julia ``.jl`` under ``app/src/tasks/<cat>/``. See ``cecelia.utils.obs_utils.write_categorical_obs``
for the parameter contract.
"""

import sys
import os
# `cecelia.*` resolves via run_py's PYTHONPATH=python/ (this is launched through run_py) and the
# editable cecelia install — no sys.path manipulation needed.
import cecelia.utils.script_utils as script_utils
from cecelia.utils.obs_utils import write_categorical_obs


def run(params: dict):
    log = script_utils.get_logfile_utils(params)
    write_categorical_obs(params, log)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
