import os
import numpy as np

# https://github.com/quantumjot/BayesianTracker
import btrack

# utils
from py.tracking_utils import TrackingUtils

import py.script_utils as script_utils
import py.config_utils as cfg

class BayesianTrackingUtils(TrackingUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)
    
    self.max_search_radius = params['max_search_radius']
    self.accuracy = script_utils.get_param(params, 'accuracy', default = 0.9)
    self.prob_to_assign = script_utils.get_param(params, 'prob_to_assign', default = 0.9)
    self.max_lost = params['max_lost']
    
    # https://github.com/quantumjot/BayesianTracker/blob/f035ed7ce5c6f8bbfa134c6d3c5a89e7ea88f911/CONFIGURATION.md
    # sigma values, that is, uncertainty of measurements for
    # P - Initials noise or/ Initial covariance estimate
    # G - Noise for coordinates and velocity ie/ during processing
    #     or/ Estimated error in process
    # R - Noise in coordinates or/ Estimated error in measurements
    self.noise_inital = script_utils.get_param(params, 'noise_inital', default = 150)
    self.noise_processing = script_utils.get_param(params, 'noise_processing', default = 15)
    self.noise_measurements = script_utils.get_param(params, 'noise_measurements', default = 5)
    
    # set options for hypothesis model
    self.lambda_link = script_utils.get_param(params, 'lambda_link', default = None)
    self.lambda_time = script_utils.get_param(params, 'lambda_time', default = None)
    self.lambda_dist = script_utils.get_param(params, 'lambda_dist', default = None)
    self.theta_time = script_utils.get_param(params, 'theta_time', default = None)
    self.theta_dist = script_utils.get_param(params, 'theta_dist', default = None)
    self.dist_thresh = script_utils.get_param(params, 'dist_thresh', default = None)
    self.time_thresh = script_utils.get_param(params, 'time_thresh', default = None)
    self.segmentation_miss_rate = script_utils.get_param(params, 'segmentation_miss_rate', default = None)
    
  """
  Track objects from centroids
  """
  def track_objects_from_centroids(self, centroid_df):
    # how do I make objects?
    # https://github.com/quantumjot/BayesianTracker/blob/master/examples/segmentation_to_btrack_to_napari.ipynb
    
    # rename columns
    centroid_df = centroid_df.rename(columns = {
        'centroid_x': 'x',
        'centroid_y': 'y',
        'centroid_z': 'z',
        'centroid_t': 't',
        'label': 'ID'
    })
    
    # add label back
    centroid_df['label_id'] = centroid_df['ID'] 
    
    # add state
    centroid_df['state'] = 1
    
    # initialise a tracker session using a context manager
    with btrack.BayesianTracker() as tracker:
      # configure the tracker using a config file
      # https://github.com/quantumjot/BayesianTracker/blob/8ce43045c234f52eb9fca9a1d8bf3770a632ca57/CONFIGURATION.md
      # https://github.com/jni/pia-tracking/blob/master/btrack_notes.md
      
      # load base config
      model_config = btrack.config.load_config(os.path.join(
        self.ccia_path,
        cfg.data['python']['btrack']['default']['dir'],
        cfg.data['python']['btrack']['default']['name']
        ))
        
      # reverse to get probability not to assign a track
      # assuming that 0.0001 is the highest value
      # and 0.1 is the lowest value
      prob_not_assign = (0.1 - (1/10000)) * (1 - self.prob_to_assign)
      
      # change config values
      model_config.motion_model.max_lost = self.max_lost
      model_config.motion_model.prob_not_assign = prob_not_assign
      
      # highest accuracy that worked without crashing the kernel was '10'
      model_config.motion_model.accuracy = self.accuracy * 10
      
      # change sigma values
      model_config.motion_model.P = model_config.motion_model.P * self.noise_inital
      model_config.motion_model.G = model_config.motion_model.G * self.noise_processing
      model_config.motion_model.R = model_config.motion_model.R * self.noise_measurements
      
      # change hypothesis model
      model_config.hypothesis_model.lambda_link = self.lambda_link
      model_config.hypothesis_model.lambda_time = self.lambda_time
      model_config.hypothesis_model.lambda_dist = self.lambda_dist
      model_config.hypothesis_model.theta_time = self.theta_time
      model_config.hypothesis_model.theta_dist = self.theta_dist
      model_config.hypothesis_model.dist_thresh = self.dist_thresh
      model_config.hypothesis_model.time_thresh = self.time_thresh
      model_config.hypothesis_model.segmentation_miss_rate = self.segmentation_miss_rate
      
      # set config
      tracker.configure(model_config)
      
      tracker.max_search_radius = self.max_search_radius
  
      # append the objects to be tracked
      tracker.append(centroid_df)
  
      # track them (in interactive mode)
      tracker.track_interactive(step_size=100)
  
      # generate hypotheses and run the global optimizer
      tracker.optimize()
      
      tracks = tracker.tracks
      
    # contact track and label ids
    tracks_concat = [x['label_id'] for x in tracks]
    
    # repeat track ids for all label ids
    track_ids = [[i+1]*len(x) for i, x in enumerate(tracks_concat)]
    track_ids = np.concatenate(track_ids)
    
    # push together as array
    track_label_ids = np.concatenate(tracks_concat)
    
    # return
    return np.column_stack((track_ids, track_label_ids))
