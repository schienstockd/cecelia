"""
Delete ONE track set (a `track_source`) from a segmentation's label-props H5AD.

Called by the per-run delete (`delete_analysis_run!` in `app/src/analysis_runs.jl`) as a subprocess.
It is the tracking write-back with an EMPTY run: `merge_track_lineage` deletes the source's rows,
compacts the surviving track ids and invalidates the track-derived `live.*` columns — exactly what a
re-run of that source does before it writes its new tracks. One implementation of the rules, not two.

Parameter contract (JSON written by Julia):
  propsPath    - absolute path to labelProps/{valueName}.h5ad
  trackSource  - the source to delete: a population uid, or "whole_seg"
"""

import pandas as pd

import cecelia.utils.script_utils as script_utils
from cecelia.utils.tracking_utils import merge_track_lineage

# The columns `merge_track_lineage` reads off a run's track table — empty, so nothing is written.
_EMPTY_RUN_COLS = ("track_id", "parent", "root", "state", "generation", "t", "label_id", "cell_id")


def run(params: dict):
    log = script_utils.get_logfile_utils(params)
    source = str(params["trackSource"])
    log.log(f">> Delete track set track_source='{source}'")
    merge_track_lineage(params["propsPath"], pd.DataFrame(columns=list(_EMPTY_RUN_COLS)), source,
                        live_track_sources=None, force_track_source=False, log=log)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
