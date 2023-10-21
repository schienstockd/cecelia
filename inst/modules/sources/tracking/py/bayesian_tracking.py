# add CCIA modules
import sys
import os
sys.path.append("./")

from py.bayesian_tracking_utils import BayesianTrackingUtils

import py.script_utils as script_utils
import py.ome_xml_utils as ome_xml_utils

# track cells
def run(params):
  # define params
  params = {
    'ccia': params['ccia'],
    'task_dir': params['taskDir'],
    'im_res': params['imRes'],
    'max_search_radius': params['maxSearchRadius'],
    'max_lost': params['maxLost'],
    'accuracy': params['accuracy'],
    'prob_to_assign': params['probToAssign'],
    'noise_inital': params['noiseInital'],
    'noise_processing': params['noiseProcessing'],
    'noise_measurements': params['noiseMeasurements'],
    'min_timepoints': params['minTimepoints'],
    'lambda_link': params['lambdaLink'],
    'lambda_time': params['lambdaTime'],
    'lambda_dist': params['lambdaDist'],
    'theta_time': params['thetaTime'],
    'theta_dist': params['thetaDist'],
    'dist_thresh': params['distThresh'],
    'time_thresh': params['timeThresh'],
    'segmentation_miss_rate': params['segmentationMissRate'],
    'filters': params['filters']
  }

  # run tracking
  tracking_utils = BayesianTrackingUtils(params)
  tracking_utils.track_objects()

def main():
  # get params
  params = script_utils.script_params(
    flatten_dict_except = {
  	  'filters': ['measure']
  	  }
  )

  # run
  run(params)

if __name__ == '__main__':
  main()
