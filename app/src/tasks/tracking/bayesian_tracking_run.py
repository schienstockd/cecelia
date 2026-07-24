"""
Bayesian (btrack) tracking task entry point.

Called by the Julia `tracking.bayesian_tracking` handler as a subprocess. Reads centroids
from the segmentation's label-props H5AD, runs btrack, and writes the lineage columns
(track_id, track_parent, track_root, track_state, track_generation, cell_id) back into
the same H5AD obs. See `cecelia.utils.tracking_utils` for the convention.

Parameter contract (JSON written by Julia):
  taskDir              - metadata directory ({proj}/1/{uid}/)
  valueName            - segmentation label set name (default 'default')
  labelIds             - list of label IDs to track (gated population), or null (whole seg)
  maxSearchRadius, maxLost, trackBranching, minTimepoints, accuracy, probToAssign,
  noiseInital, noiseProcessing, noiseMeasurements, distThresh, timeThresh,
  segmentationMissRate, lambdaLink, lambdaBranch, lambdaTime, lambdaDist, thetaTime,
  thetaDist            - btrack model parameters
"""

import sys
import os
# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
from cecelia.utils.tracking_utils import BayesianTrackingUtils


def run(params: dict):
    log = script_utils.get_logfile_utils(params)
    # The vendored btrack config ships beside this runner (app/src/tasks/tracking/cell_config.json);
    # hand its path to the tracking helper so the helper never reaches into cecelia's package-data.
    params.setdefault("btrackConfig",
                      os.path.join(os.path.dirname(os.path.abspath(__file__)), "cell_config.json"))
    BayesianTrackingUtils(params, log).track_objects()


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
