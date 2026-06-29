"""Categorical/string obs-column writer entry point.

Called as a subprocess by Julia tasks that produce categorical cell labels — HMM states /
transitions today, cluster ids later. Julia computes the values and passes them here; this
script writes them into the segmentation's labelProps ``.h5ad`` via anndata's categorical
encoding (Julia writes numeric obs directly, but not categoricals). See
``py.utils.obs_utils.write_categorical_obs`` for the parameter contract.
"""

import sys
import os
# app/py/tasks/labels/write_categorical_obs_run.py → 4 levels up to app/
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__))))))

import py.utils.script_utils as script_utils
from py.utils.obs_utils import write_categorical_obs


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
