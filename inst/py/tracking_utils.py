import numpy as np
import pandas as pd
import os

# utils
from py.label_props_utils import LabelPropsUtils

import py.config_utils as cfg
import py.script_utils as script_utils
import py.measure_utils as measure_utils

class TrackingUtils:
  def __init__(self, params):
    # init params
    self.task_dir = params['task_dir']
    self.im_res = params['im_res']
    
    # general parameters
    self.min_timepoints = script_utils.get_param(params, 'min_timepoints', default = 5)
    
    # filtering parameters
    self.filters = script_utils.get_param(params, 'filters', default = None)
    
    # logging
    self.logfile_utils = script_utils.get_logfile_utils(params)
    
    # get labels name and channel info
    self.value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
    self.labels_props_filename = cfg.value_dir(self.value_name, 'labelProps')
    self.channel_names = script_utils.get_ccia_param(params, 'channel_names', default = None)
    
    self.logfile_utils.log(f'> Use {self.labels_props_filename}')
    
    # get label prop utils
    self.label_props_utils = LabelPropsUtils(self.task_dir, self.labels_props_filename)
    
  """
  Track objects
  """
  def track_objects(self):
    # read centroids and labels
    if self.filters is None:
      centroid_df = self.label_props_utils.label_props_view(value_name = self.value_name)\
        .view_centroid_cols()\
        .view_label_col()\
        .as_df()
    else:
      self.logfile_utils.log(">> Filter tracks by")
      self.logfile_utils.log(f'{self.filters}')
      
      # filter values
      label_view = self.label_props_utils.label_props_view(value_name = self.value_name)
      
      # change channel names
      if self.channel_names is not None:
        label_view.change_channel_names(self.channel_names)
      
      self.logfile_utils.log(len(self.channel_names))
      self.logfile_utils.log(self.channel_names)
      
      # go through and filter
      if len(self.filters) > 0:
        for i, x in self.filters.items():
          if len(x['measure']) > 0:
            # apply filter
            label_view = label_view.filter_rows(
              # get python type for values
              filter_vals = script_utils.py_type_from_string(x['values']),
              filter_by = x['measure'],
              filter_type = x['type'] if 'type' in x.keys() else 'abs',
              filter_fun = x['fun']
              )
          
      centroid_df = label_view.view_centroid_cols()\
        .view_label_col()\
        .as_df()
        
    # convert centroid df to physical units
    measure_utils.convert_pixel_to_physical(
      centroid_df, im_res = self.im_res
      )
    
    self.logfile_utils.log(">> Start tracking objects")
      
    # track objects
    tracks_array = self.track_objects_from_centroids(centroid_df)
    
    # convert to dataframe
    track_df = pd.DataFrame(
        tracks_array,
        columns = ['track_id', 'label_id']
        )
    
    # drop na and sort by label id
    track_df.dropna(axis = 0, inplace = True)
    track_df.sort_values('label_id', inplace = True)
    
    # get number of tracks
    num_tracks = len(track_df.track_id.unique())
    self.logfile_utils.log(f'> found {num_tracks} tracks')
    
    # filter tracks by minimum timepoints
    # https://stackoverflow.com/a/58536543/13766165
    track_df = track_df[
      track_df.groupby("track_id")['track_id'].transform('size') >= self.min_timepoints
      ]
      
    # get number of tracks
    num_tracks = len(track_df.track_id.unique())
    self.logfile_utils.log(f'> after filtering {num_tracks}')
    
    # add cell numbering for tracks
    # https://stackoverflow.com/a/57787917/13766165
    track_df['cell_id'] = track_df.groupby('track_id')['label_id'].rank(method="first", ascending=True)
    
    self.logfile_utils.log(">> Save back to label properties")
    
    # get labels
    labels_ids = self.label_props_utils.label_props_view()\
      .view_label_col()\
      .values_obs()
      
    # merge track ids to labels
    merged_track_ids = labels_ids.join(track_df.set_index('label_id'), on = 'label')
    
    # get track ids as dict
    track_dict = {
      'track_id': merged_track_ids['track_id'],
      'cell_id': merged_track_ids['cell_id']
    }
    
    self.logfile_utils.log(
      "> Save to " + str(self.label_props_utils.label_props_view()\
        .adata_filepath()))
    
    # add to obs and save
    self.label_props_utils.label_props_view()\
        .add_obs(track_dict)\
        .save()

  """
  Track objects from centroids
  """
  def track_objects_from_centroids(self, centroid_df):
    pass
