"""
Per-track table writer task entry point.

Called by the Julia `tracking.track_measures` handler as a subprocess. Julia computes the
per-track measures (celltrackR port) and passes them here; this script writes the companion
per-track h5ad `{value_name}__tracks.h5ad` (one row per track, measures in X/var, lineage in
obs). New-file creation is Python's job (docs/DATAMODEL.md). See
`py.utils.tracking_utils.write_track_props` for the parameter contract.
"""

import sys
import os
# `py.*` resolves via PYTHONPATH=app/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import py.utils.script_utils as script_utils
from py.utils.tracking_utils import write_track_props


def run(params: dict):
    log = script_utils.get_logfile_utils(params)
    write_track_props(params, log)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
