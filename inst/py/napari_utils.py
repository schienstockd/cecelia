import napari
from napari.components._viewer_constants import CanvasPosition

import tifffile
import zarr
import numpy as np
import os
import json
import re
import math
import pandas as pd
import matplotlib.pyplot as plt
import datetime

from pathlib import Path

import py.math_helpers as math_helpers
import py.type_helpers as type_helpers

# segmentation functions
from py.dim_utils import DimUtils
from py.pop_utils import PopUtils
from py.shape_utils import ShapeUtils
from py.label_props_utils import LabelPropsUtils

import py.colour_utils as colour_utils
import py.zarr_utils as zarr_utils
import py.slice_utils as slice_utils
import py.ome_xml_utils as ome_xml_utils
import py.tiff_utils as tiff_utils
import py.morpho_utils as morpho_utils
import py.points_utils as points_utils
import py.colormap_utils as cmap_utils

import py.config_utils as cfg

class NapariUtils:
  def __init__(self):
    # napari viewer
    self._viewer = None

    # image data
    self._im_path = None
    self._as_dask = None
    self._im_data = None
    self._dim_utils = None

    # labels
    self._im_labels = None
    self._label_props_utils = None
    self._im_label_ids = None

    self._omexml = None
    self._zarr_group_info = None
    self._time_interval = None

    # viewpoint cube
    self._im_cube_data = None
    self._im_cube_seg = None
    self._im_cube_coords = None

    # filepaths
    self._task_dir = None
    self._viewer_input_file = None
    self._viewer_output_file = None

    # settings
    self._viewer_settings = dict()
    self._use_channel_axis = False
    self._im_scale = None
    
    # populations
    self._pop_utils = PopUtils()
    
    # shapes
    self._shape_utils = ShapeUtils()
    
    # layers to refer to
    self._labels_layer = None
    self._points_layer = None
    self._preview_layers = None

  """
  Getters
  """
  @property
  def task_dir(self):
    return self._task_dir
  
  @property
  def viewer(self):
    return self._viewer
  
  @property
  def im_path(self):
    return self._im_path
  
  @property
  def as_dask(self):
    return self._as_dask
  
  @property
  def dim_utils(self):
    return self._dim_utils
  
  @property
  def im_labels(self):
    return self._im_labels
  
  @property
  def label_props_utils(self):
    return self._label_props_utils
  
  @property
  def zarr_group_info(self):
    return self._zarr_group_info
  
  @property
  def time_interval(self):
    return self._time_interval
  
  @property
  def viewer_input_file(self):
    return self._viewer_input_file
  
  @property
  def viewer_output_file(self):
    return self._viewer_output_file
  
  @property
  def use_channel_axis(self):
    return self._use_channel_axis
  
  @property
  def pop_utils(self):
    return self._pop_utils
  
  @property
  def shape_utils(self):
    return self._shape_utils
  
  @property
  def labels_layer(self):
    return self._labels_layer
  
  @property
  def points_layer(self):
    return self._points_layer
  
  @property
  def preview_layers(self):
    return self._preview_layers

  """
  Setters
  """
  @task_dir.setter
  def task_dir(self, x):
    self._task_dir = x
    
    # update utils dependent on task directory
    self.label_props_utils = LabelPropsUtils(x)
    
  @viewer.setter
  def viewer(self, x):
    self._viewer = x
  
  @im_path.setter
  def im_path(self, x):
    self._im_path = x
  
  @as_dask.setter
  def as_dask(self, x):
    self._as_dask = x
  
  @dim_utils.setter
  def dim_utils(self, x):
    self._dim_utils = x
  
  @im_labels.setter
  def im_labels(self, x):
    self._im_labels = x
  
  @label_props_utils.setter
  def label_props_utils(self, x):
    self._label_props_utils = x
  
  @zarr_group_info.setter
  def zarr_group_info(self, x):
    self._zarr_group_info = x
  
  @time_interval.setter
  def time_interval(self, x):
    self._time_interval = x
  
  @viewer_input_file.setter
  def viewer_input_file(self, x):
    self._viewer_input_file = x
  
  @viewer_output_file.setter
  def viewer_output_file(self, x):
    self._viewer_output_file = x
  
  @use_channel_axis.setter
  def use_channel_axis(self, x):
    self._use_channel_axis = x
  
  @pop_utils.setter
  def pop_utils(self, x):
    self._pop_utils = x
  
  @shape_utils.setter
  def shape_utils(self, x):
    self._shape_utils = x
  
  @labels_layer.setter
  def labels_layer(self, x):
    self._labels_layer = x
  
  @points_layer.setter
  def points_layer(self, x):
    self._points_layer = x
  
  @preview_layers.setter
  def preview_layers(self, x):
    self._preview_layers = x

  """
  Open Viewer
  """
  def open_viewer(self):
    self.viewer = napari.Viewer()

  """
  Close Viewer
  """
  def close_viewer(self, set_none = True):
    if self.viewer is not None:
      self.viewer.close()
      
    if set_none is True:
      self.viewer = None

  """
  Open image in Napari
  """
  def open_image(self, im_path,
    use_channel_axis = False, channel_names = None,
    channel_colormaps = None, as_dask = True, show_3D = False,
    multiscales = None, compute_dask = False,
    contrast_limits = None, visible = True, squeeze = False,
    downsample_z = False, as_mip = False, slices = None):
      self.im_path = im_path
      self.as_dask = as_dask
      
      # open as Zarr
      self.im_data, self.zarr_group_info = zarr_utils.open_as_zarr(
        self.im_path, multiscales = multiscales, as_dask = as_dask)
        
      # get OME-XML
      self.omexml = ome_xml_utils.parse_meta(self.im_path)
      
      # set dimensions
      self.dim_utils = DimUtils(self.omexml, use_channel_axis)
      self.dim_utils.calc_image_dimensions(self.im_data[0].shape)
      
      print(self.dim_utils.im_dim)
      
      # slice data?
      # TODO this only works when multiscales is set to '1'
      # you would need to cycle through scales and set slices .. ?
      if multiscales == 1 and slices is not None:
        self.im_data[0] = self.im_data[0][slices]

      # force the image as 16 bit?
      if self.dim_utils.is_32_bit():
        self.im_data = [x.astype(np.uint16) for x in self.im_data]

      # check that viewer is open
      if self.viewer is None:
        self.open_viewer()

      # this has to be set to the channel when used
      # if channel_names is not None and len(channel_names) == 1:
      #   use_channel_axis = False
      #   channel_names = None
      self.use_channel_axis = use_channel_axis
      
      # get channel axis
      channel_axis = self.dim_utils.dim_idx('C', squeeze = squeeze)
      
      if squeeze is True:
        # squeeze data
        self.im_data = [np.squeeze(x) for x in self.im_data]
        
        # set scale
        self.im_scale = self.dim_utils.im_scale(
          dims = self.dim_utils.trimmed_default_dim_order(
            ignore_channel = use_channel_axis, squeeze = True))
      else:
        self.im_scale = self.dim_utils.im_scale()
      
        # remove channel
        if use_channel_axis is True:
          self.im_scale.pop(channel_axis)
      
      # TODO this is hard coded for SLIDE-SEQ
      if contrast_limits is None and self.im_data[0].dtype == np.float16:
        contrast_limits = [0, 1]
      
      # downsample to prevent replication of first frame
      if downsample_z is True:
        slices = [slice(None) for _ in range(len(self.im_data[0].shape))]
        slices[self.dim_utils.dim_idx('Z')] = slice(
          0, self.dim_utils.dim_val('Z'), 2
        )
        
        self.im_data = [x[tuple(slices)] for x in self.im_data]
        
      # create MIP for viewing
      if as_mip is True:
        # fortify
        # TODO do I have to do that .. ?
        self.im_data = [zarr_utils.fortify(x) for x in self.im_data]
        
        # create mip
        self.im_data = [np.amax(x, axis = self.dim_utils.dim_idx('Z')) for x in self.im_data]
        
        # reset scale
        self.im_scale.pop(self.dim_utils.dim_idx('Z'))
      
      # open in viewer
      # 3D images will be shown with lowest resolution
      # https://forum.image.sc/t/viewing-a-volume-as-2d-pyramids/38383/2
      self.viewer.add_image(
        [x.compute() for x in self.im_data] if compute_dask is True else self.im_data,
        channel_axis = channel_axis if use_channel_axis else None,
        name = channel_names if use_channel_axis else None,
        colormap = channel_colormaps if use_channel_axis else None,
        contrast_limits = contrast_limits,
        scale = self.im_scale, visible = visible)
      
      # define dimension order for napari
      self.viewer.dims.order = self.dim_utils.default_dim_order(ignore_channel = True)
      
      # toggle 3D view?
      if show_3D is True:
        self.viewer.dims.ndisplay = 3
      
      # go through channels and adjust contrast once
      if contrast_limits is None and not self.dim_utils.is_32_bit():
        for x in self.viewer.layers:
          if x.visible > 0: x.reset_contrast_limits() 

      # show scalebar
      self.viewer.scale_bar.unit = 'um'
      self.viewer.scale_bar.visible = True
      self.viewer.scale_bar.ticks = False

  """
  Add timestamp
  https://forum.image.sc/t/napari-how-add-a-text-label-time-always-in-the-same-spot-in-viewer/52932/5
  """
  def add_timestamp(self, time_interval = 1):
    # set timescale
    self.time_interval = time_interval
    
    def update_slider(event):
      time = self.viewer.dims.current_step[0]
      self.viewer.text_overlay.text = str(datetime.timedelta(seconds = time * self.time_interval * 60))
    
    # TODO this should be adjustable
    self.viewer.text_overlay.visible = True
    self.viewer.text_overlay.font_size = 12
    self.viewer.text_overlay.color = 'white'
    self.viewer.text_overlay.position = CanvasPosition.TOP_LEFT
    
    # TODO dummy call?
    update_slider(None)
    
    self.viewer.dims.events.current_step.connect(update_slider)

  """
  Reset scale for labels
  """
  def reset_labels_scale(self, layers_startswith = 'Result of'):
    # adjust scales for images
    # TODO this is a helper function for APOC
    for x in self.viewer.layers:
      if isinstance(x, napari.layers.labels.labels.Labels) and x.name.startswith(layers_startswith):
          x.scale = self.dim_utils.im_scale(dims = ['X', 'Y'])

  """
  Save labels
  """
  def save_labels(self, filepath, layer_name = 'Labels', exclude_names = None,
                  notify_module_id = None):
    if exclude_names is not None:
      # save all but the defined
      labels_to_save = [
        x for x in self.viewer.layers \
        if isinstance(x, napari.layers.labels.labels.Labels) and re.match(exclude_names, x.name) is None
        ]
        
      # get label names
      label_names_to_save = [
        '{y}.zarr'.format(
          y = re.search('(?<= ).+$', x.name).group(0) if re.match('\(.+\) [a-zA-Z]+ [^ ]+', x.name) else x.name
          ) for x in labels_to_save]
        
      # use path as directory to save files
      save_in_dir = True
    else:
      labels_to_save = [self.viewer.layers[layer_name]]
      save_in_dir = False
      
    # save labels back as zarr
    if save_in_dir is False:
      for x in labels_to_save:
        zarr.save(filepath, x.data.astype(np.uint16))
    else:
      for i, x in enumerate(labels_to_save):
        zarr.save(
          os.path.join(filepath, label_names_to_save[i]),
          x.data.astype(np.uint16)
          )
      
    # notify module of saved labels?
    if notify_module_id is not None:
      out_cmd = dict()
      out_cmd[notify_module_id] = [x for x in label_names_to_save]
      
      self.write_to_output(out_cmd)
                  
  """
  Show labels for multiple value names
  """
  def show_labels_all(self, value_names, label_suffixes = dict(), **kwargs):
    # get distinct colours for points
    points_colours = colour_utils.distinct_colours(len(value_names), as_hex = True)
    
    # call show labels for all in list
    for i, x in enumerate(value_names):
      # get suffixes for value name
      value_name_suffixes = label_suffixes[x] if x in label_suffixes else []
      
      self.show_labels(x, label_suffixes = value_name_suffixes, **kwargs)
                    
  """
  Show labels
  """
  def show_labels(self, value_name, show_labels = True, show_points = True,
                  show_label_ids = True, as_np_array = False, show_tracks = True,
                  points_colour = 'white', cache = True, label_suffixes = [],
                  show_branching = False, branching_property = 'type',
                  slices = None, split_tracks = None,
                  tracks_blending = 'additive', binarise_labels = False):
    # set label ids
    label_ids = self.label_ids(value_name = value_name)

    # show label properties on image
    # https://forum.image.sc/t/add-semantic-labels-in-napari/33484/4
    # label_mapping = {x: x for i, x in enumerate(label_ids)}
    # label_cluster = {x: i for i, x in enumerate(label_ids)}

    # metadata = {
    #     'label_mapping': label_mapping,
    #     'label_cluster': label_cluster
    # }

    # add label ids
    properties = dict()
    
    if label_ids is not None:
      properties['label_id'] = label_ids

    if show_labels is True:
      labels_paths = {
        'base': os.path.join(self.task_dir, cfg.value_dir(value_name, 'labels'))
      }
      
      # add label suffixes
      if len(label_suffixes) > 0:
        labels_stem = Path(labels_paths['base']).stem
        labels_suffix = Path(labels_paths['base']).suffix
        labels_parent = Path(labels_paths['base']).parent
        
        # go through suffixes
        for x in label_suffixes:
          labels_paths[x] = os.path.join(self.task_dir, labels_parent, f'{labels_stem}_{x}{labels_suffix}')
      
      # open labels
      print(labels_paths)
      
      for i, x in labels_paths.items():
        if os.path.exists(x):
          if self.as_dask is True:
            self.im_labels, group_info = zarr_utils.open_labels_as_dask(
              x, multiscales = len(self.im_data))
          else:
            self.im_labels, group_info = zarr_utils.open_labels_as_zarr(
              x, multiscales = len(self.im_data))
              
          # slice data?
          # TODO this only works when multiscales is set to '1'
          # you would need to cycle through scales and set slices .. ?
          if len(self.im_data) == 1 and slices is not None:
            print(self.im_labels[0].shape)
            self.im_labels[0] = self.im_labels[0][slices]
          
          # TODO Do we need to convert to numpy array to edit labels?
          if as_np_array is True or binarise_labels is True:
            if self.as_dask is True:
              # self.im_labels[0] = self.im_labels[0].compute()
              self.im_labels = [y.compute() for y in self.im_labels]
            else:
              # self.im_labels[0] = self.im_labels[0][:]
              self.im_labels = [y[:] for y in self.im_labels]
            
            # binarise labels
            # TODO this is hard coded
            if binarise_labels is True and not x.endswith('.branch.zarr'):
              self.im_labels = [y > 0 for y in self.im_labels]
          
          print(self.im_labels[0].shape)
          
          # remove layer if shown
          labels_layer = 'Labels' if value_name is None else f'({value_name}) {i} Labels'
          self.remove_layer_by_name(labels_layer)
          
          # show nuclei as contours
          contour = 0
          opacity = 0.7
          # if i in ['nuc', 'halo']:
          if i in ['nuc', 'halo']:
            contour = 0
            opacity = 1.0
          
          # hide cytoplams by default  
          visible = True
          if i in ['cyto']:
            visible = False
            
          # # create MIP for viewing
          # if self.im_scale is not None and len(self.im_scale) < len(
          #   self.dim_utils.default_dim_order(ignore_channel = True)):
          #   # fortify
          #   # TODO do I have to do that .. ?
          #   self.im_labels = [zarr_utils.fortify(x) for x in self.im_labels]
          #   
          #   # create mip
          #   self.im_labels = [np.amax(x, axis = self.dim_utils.dim_idx('Z', ignore_channel = True)) for x in self.im_labels]
          
          # check that shape corresponds to scale
          # TODO this can happen when there is no time axis for 4D images
          labels_scale = self.im_scale.copy()
          if len(self.im_labels[0].shape) < len(self.im_scale) - 1:
            labels_scale.pop(self.dim_utils.dim_idx('T', ignore_channel = True))
            labels_scale.pop(self.dim_utils.dim_idx('Z', ignore_channel = True, ignore_time = True))
          elif len(self.im_labels[0].shape) < len(self.im_scale):
            labels_scale.pop(self.dim_utils.dim_idx('T', ignore_channel = True))
          
          # show labels
          # make sure it is labels and not float
          if self.im_labels[0].dtype in [bool, np.uint32]:
            labels_layer = self.viewer.add_labels(
                self.im_labels, properties = properties,
                # metadata = metadata,
                name = labels_layer,
                scale = labels_scale,
                cache = cache,
                opacity = opacity,
                visible = visible
                # rendering = "translucent"
            )
          else:
            labels_layer = self.viewer.add_image(
                self.im_labels,
                # metadata = metadata,
                name = labels_layer,
                scale = labels_scale,
                cache = cache,
                opacity = opacity,
                visible = visible
                # rendering = "translucent"
            )
          
          labels_layer.contour = contour
          
          # add branching?
          if show_branching is True and value_name.endswith('.branch'):
            # if branching_property == 'type':
            #   percentile = 100
            # else:
            #   percentile = 99.5
              
            percentile = 100
            
            chnl_scale = 255
            branching_property_col = f'branch-{branching_property}'
            
            # get branching property
            paths_table = self.label_props_utils\
              .label_props_view(value_name = value_name)\
              .view_cols(['label', branching_property_col])\
              .values_obs()
            
            #branching_values = paths_table['branch-distance'].values
            branching_values = paths_table[branching_property_col].values
            
            # get max value from intensity
            max_chnl_val = np.percentile(branching_values, percentile)
            
            # how to select a map.. ?
            labels_cm = plt.cm.viridis(np.linspace(0, 1, num = chnl_scale))
            
            # insert background
            labels_cm = np.insert(labels_cm, 0, 0, axis = 0)
            
            layer_chnl_colours = np.array(branching_values/max_chnl_val * chnl_scale - 1).astype(int)
            layer_chnl_colours[layer_chnl_colours >= chnl_scale] = chnl_scale - 1
            layer_chnl_colours[layer_chnl_colours <= 0] = 1
            layer_chnl_colours = list(layer_chnl_colours)
            
            # insert background
            layer_chnl_colours.insert(0, 0)
            
            # convert to dict
            labels_layer.color = {x: labels_cm[layer_chnl_colours[i]] for i, x in enumerate(label_ids)}
    
    # show points
    if show_points is True:
      labels_view = self.label_props_utils.label_props_view(value_name = value_name)
      
      if labels_view is not None:
        # add points for cell selection
        centroid_cols = labels_view.centroid_columns(self.dim_utils.im_dim_order)
        label_df = labels_view.view_centroid_cols(self.dim_utils.im_dim_order).view_label_col().as_df()
        
        # get coordinates
        label_points = points_utils.prep_points(
          label_df, self.dim_utils, centroid_cols,
          im_scale = self.im_scale
        )
        
        properties = {
            'label_id': label_df.label.tolist()
        }
        
        # remove layer if shown
        points_layer = 'Points' if value_name is None else f'({value_name}) Points'
        self.remove_layer_by_name(points_layer)
        
        points_text = None
        
        if show_label_ids is True:
          points_text = {
            'string': '{label_id}',
            'size': 20,
            'color': 'white'
            # 'translation': np.array([-30, 0]),
          }
          
        self.points_layer = self.viewer.add_points(
          label_points, size = 6,
          properties = properties,
          name = points_layer,
          visible = True,
          # n_dimensional = True,
          scale = self.im_scale,
          face_color = points_colour,
          edge_color = 'black',
          text = points_text
          )
        
        labels_view.close()
    
    # show tracks
    if show_tracks is True:
      labels_view = self.label_props_utils.label_props_view(value_name = value_name)
      
      if labels_view is not None:
        if labels_view.has_cols(['track_id'], dat_type = 'obs'):
          # get tracks
          tracks = labels_view.view_centroid_cols(self.dim_utils.im_dim_order)\
            .view_obs_cols(['track_id'])\
            .as_df()\
            .dropna()\
            .to_numpy()
            
          labels_view.close()
          
          # TODO make sure the array shape is correct
          # this might be because the image is 2D
          if len(self.im_scale) > tracks.shape[1] - 1:
            tracks = np.insert(tracks, 2, 0, axis = 1)
          
          # prepare properties
          prop_df = self.label_props_utils.label_props_view(value_name = value_name).exclude_centroid_cols()\
            .exclude_obs_cols(['label'])\
            .as_df()\
            .dropna(subset=['track_id'])\
            .replace(False, 0)\
            .replace(True, 1)
            # ValueError: Cannot setitem on a Categorical with a new category, set the categories first
            # .fillna(0)\
          
          # convert categories to numeric
          # https://stackoverflow.com/a/36107995/13766165
          cat_columns = prop_df.select_dtypes(['category']).columns
          prop_df[cat_columns] = prop_df[cat_columns].apply(
            lambda x: pd.to_numeric(x, errors = 'coerce'))
            
          if split_tracks is not None:
            # go through props and split tracks
            for i, x in split_tracks.items():
              for j, y in x.items():
                # remove layer if shown
                tracks_layer = f'Tracks {j}' if value_name is None else f'({value_name}) Tracks {j}'
                self.remove_layer_by_name(tracks_layer)
                
                # get binary mask
                bin_mask = prop_df[i].isin(y['values']).values
                
                # get cmap
                cmap = cmap_utils.cmap_single(['#000000'] + [y['colour']])
                
                # value list
                prop_list = prop_df[bin_mask].fillna(-1).to_dict(orient = 'list')
                
                # make sure that clustering starts at one
                # otherwise 0 will not be shown as colour
                if i.find('.clusters.') > 0:
                  prop_list[i] = np.array(prop_list[i]) + 1
                
                # add to napari
                if len(np.unique(bin_mask)) > 1:
                  self.viewer.add_tracks(
                      tracks[bin_mask, ::],
                      properties = prop_list,
                      name = tracks_layer,
                      scale = self.im_scale,
                      color_by = i,
                      colormaps_dict = {i: cmap},
                      tail_width = 4,
                      blending = tracks_blending
                  )
            
          else:
            # remove layer if shown
            tracks_layer = 'Tracks' if value_name is None else f'({value_name}) Tracks'
            self.remove_layer_by_name(tracks_layer)
            
            # add to napari
            self.viewer.add_tracks(
                tracks,
                properties = prop_df.fillna(-1).to_dict(orient = 'list'),
                name = tracks_layer,
                scale = self.im_scale,
                tail_width = 4,
                blending = tracks_blending
            )
    
    # define dimension order for napari
    self.viewer.dims.order = self.dim_utils.default_dim_order(ignore_channel = True)

  """
  Show channel intensity
  """
  def show_channel_intensity(
    self, channel_id, value_name = 'default', chnl_scale = 255,
    percentile = cfg.data['images']['normalise']['percentile'],
    cmap = 'viridis', intensity_measure = 'mean'):
    chnl = f'{intensity_measure}_intensity_{channel_id}'

    label_view = self.label_props_utils.label_props_view(value_name = value_name)
    chnl_values = label_view.view_cols(['label', chnl]).values_vars()
    label_ids = label_view.values_obs()['label']
    label_view.close()
    
    # get max value from intensity
    max_chnl_val = np.percentile(chnl_values, percentile)
    
    # how to select a map.. ?
    labels_cm = plt.cm.viridis(np.linspace(0, 1, num = chnl_scale))
    
    # insert background
    labels_cm = np.insert(labels_cm, 0, 0, axis = 0)

    layer_chnl_colours = np.array(chnl_values/max_chnl_val * chnl_scale - 1).astype(np.int)
    layer_chnl_colours[layer_chnl_colours >= chnl_scale] = chnl_scale - 1
    layer_chnl_colours[layer_chnl_colours <= 0] = 1
    layer_chnl_colours = list(layer_chnl_colours)

    # insert background
    layer_chnl_colours.insert(0, 0)

    # convert to dict
    layer_chnl_coloursDict = {x: labels_cm[layer_chnl_colours[i]] for i, x in enumerate(label_ids)}
    
    # push to napari
    self.viewer.layers[f'({value_name}) base Labels'].color = layer_chnl_coloursDict

  """
  Show surfaces
  """
  def show_surfaces(self, value_name = 'default', layer_name = 'surfaces',
                    filter_measure = 'NONE', filter_vals = ['NONE'], as_convex_hull = True):
    surface_df = self.label_props_utils.label_props_view(value_name = value_name)\
      .view_cols(['label', 'centroid_t']\
        + [f'bbox_min_{x}' for x in ['z', 'y', 'x']])\
      .as_df()

    # get meshes for tracks
    meshes = morpho_utils.df_to_meshes(
        self.task_dir,
        surface_df, value_name,
        as_convex_hull = as_convex_hull,
        dim_utils = self.dim_utils,
        filter_measure = filter_measure,
        filter_vals = filter_vals
    )
    
    # get surface
    surface = morpho_utils.to_napari_surface_object(
        meshes, surface_df, self.dim_utils
    )
    
    # add to napari
    self.viewer.add_surface(
        surface,
        name = layer_name,
        # already scaled
        # scale = self.im_scale,
        colormap = 'PiYG'
    )

  """
  Highlight tracks
  """
  def highlight_tracks(self, value_name, track_ids,
                       name = "*Tracks", color_by = "track_id",
                       show_surfaces = False, as_convex_hull = True):
    # get tracks
    tracks = self.label_props_utils.label_props_view(value_name = value_name)\
      .filter_by_obs(track_ids, filter_by = 'track_id')\
      .view_centroid_cols(self.dim_utils.im_dim_order)\
      .view_obs_cols(['track_id'])\
      .as_df()\
      .dropna()\
      .to_numpy()
    
    # prepare properties
    properties =  self.label_props_utils.label_props_view(value_name = value_name)\
      .filter_by_obs(track_ids, filter_by = 'track_id')\
      .exclude_centroid_cols()\
      .exclude_obs_cols(['label'])\
      .as_df()\
      .dropna(subset=['track_id'])\
      .replace(np.nan, 0)\
      .replace(False, 0)\
      .replace(True, 1)\
      .to_dict(orient = 'list')
        
    # remove layer if shown
    tracks_layer = name if value_name is None else f'({value_name}) {name}'
    self.remove_layer_by_name(tracks_layer)
    
    # add to napari
    self.viewer.add_tracks(
        tracks,
        properties = properties,
        name = tracks_layer,
        scale = self.im_scale,
        color_by = color_by
    )
    
    # show surfaces
    if show_surfaces is True:
      surface_df = self.label_props_utils.label_props_view(value_name = value_name)\
        .filter_by_obs(track_ids, filter_by = 'track_id')\
        .view_cols(['label', 'centroid_t', 'track_id']\
          + [f'bbox_min_{x}' for x in ['z', 'y', 'x']])\
        .as_df()
      
      # get meshes for tracks
      meshes = morpho_utils.tracks_to_meshes(
        self.task_dir, track_ids,
        surface_df, self.dim_utils,
        value_name = value_name,
        as_convex_hull = as_convex_hull
      )
      
      # get surface
      surface = morpho_utils.to_napari_surface_object(
        meshes, surface_df, self.dim_utils
      )
      
      # remove layer if shown
      surface_layer = f'*{name}' if value_name is None else f'({value_name}) *{name}'
      self.remove_layer_by_name(surface_layer)
      
      # add to napari
      self.viewer.add_surface(
        surface, 
        name = surface_layer,
        # already scaled
        # scale = self.im_scale,
        colormap = 'PiYG'
      )

  """
  Highlight labels
  """
  def highlight_labels(self, value_name, label_ids):
    labels_view = self.label_props_utils.label_props_view(value_name = value_name)
    
    # get labels
    centroid_cols = labels_view.centroid_columns(self.dim_utils.im_dim_order)
    label_df = labels_view.filter_by_obs(label_ids)\
      .view_centroid_cols(self.dim_utils.im_dim_order)\
      .view_label_col()\
      .as_df()
    
    # prepare for points
    label_points = points_utils.prep_points(
      label_df, self.dim_utils, centroid_cols,
      im_scale = self.im_scale
    )
    
    properties = {
      'label_id': label_ids
    }

    # remove highlights if present
    if 'selection' in self.viewer.layers:
      self.viewer.layers.remove('selection')
    
    # highlight labels
    self.viewer.add_points(
        label_points, size = 6,
        properties = properties,
        face_color = 'magenta',
        edge_color = 'black',
        name = 'selection',
        n_dimensional = False if self.dim_utils.is_timeseries() else True,
        scale = self.im_scale
    )

  """
  add preview
  """
  def show_preview(self, data, channel_names = None,
                   as_points = False, size = None,
                   as_labels = False,
                   use_channel_axis = True, use_scale = True,
                   multiscale = True, contrast_limits = None,
                   cache = True, remove_previous = True):
    # add 'preview' to names
    if channel_names is not None:
      if type(channel_names) is list:
        channel_names = [f'(preview) {x}' for x in channel_names]
      else:
        channel_names = f'(preview) {channel_names}'
    
    layer_props = dict()
    
    # init layers
    if self.preview_layers is None:
      self.preview_layers = []
    
    # remove preview layers
    # TODO a bit cleaner
    if self.preview_layers is not None and remove_previous is True:
      for x in self.preview_layers:
        if x in self.viewer.layers:
          if isinstance(channel_names, list):
            remove_layer = True if x.name in channel_names else False
          else:
            remove_layer = True if x.name == channel_names else False
          
          # remember layer props
          layer_props[x.name] = {
            'visible': x.visible,
            'colormap': x.colormap
            }
          
          if remove_layer is True:
            self.viewer.layers.remove(x)
      # reset layers
      self.preview_layers = []
    
    # check on napari
    if as_points is True:
      new_layers = self.viewer.add_points(
        data,
        size = size,
        name = channel_names,
        scale = self.im_scale if use_scale is True else None
        )
    elif as_labels is True:
      new_layers = self.viewer.add_labels(
        data,
        name = channel_names,
        scale = self.im_scale if use_scale is True else None,
        rendering = "translucent"
        )
    else:
      new_layers = self.viewer.add_image(
        data,
        # if adding a dask array
        multiscale = multiscale,
        contrast_limits = contrast_limits,
        channel_axis = self.dim_utils.dim_idx("C") if use_channel_axis is True else None,
        name = channel_names,
        scale = self.im_scale if use_scale is True else None,
        cache = cache
        )
    
      # reset contrast
      # for x in self.preview_layers:
      #   x.reset_contrast_limits()
    
    if use_channel_axis is True:
      self.viewer.dims.order = self.dim_utils.default_dim_order(ignore_channel = True)
    
    # add layers
    if isinstance(new_layers, list):
      self.preview_layers += new_layers
    else:
      self.preview_layers += [new_layers]
    
    # toggle layer props
    for i, x in layer_props.items():
      self.viewer.layers[i].visible = x['visible']
      self.viewer.layers[i].colormap = x['colormap']

  """
  Show shapes
  """
  def show_shapes(self, shape_type = 'region', remove_previous = True,
                  value_name = 'default'):
    # TODO this is very naive
    # get data for shapes
    shapes = self.shape_utils.shapes(self.task_dir, shape_type = shape_type, value_name = value_name)
    
    # hide shapes if they are not shown
    if remove_previous is True:
      shapelayers_to_remove = [x for x in self.viewer.layers if isinstance(x, napari.layers.shapes.shapes.Shapes)]
      
      for x in shapelayers_to_remove:
        self.viewer.layers.remove(x.name)
    
    # then go through all shapes and show
    for i, x in shapes.items():
      # TODO add shapes with individual colours here
      # this also assumes polygon
      self.viewer.add_shapes(
        x, name = i,
        opacity = 0.4,
        shape_type = 'polygon',
        scale = self.im_scale)
        
      # move to the bottom of the stack
      image_layers = [x for x in self.viewer.layers if isinstance(x, napari.layers.image.image.Image)]
      
      self.viewer.layers.move(
        self.viewer.layers.index(i),
        self.viewer.layers.index(image_layers[-1]) + 1
      )
    
  """
  Save shapes
  """
  def save_shapes(self, shape_type = 'region', value_name = 'default'):
    # TODO this is very naive
    # get shape layers
    shape_layers = [x for x in self.viewer.layers if isinstance(x, napari.layers.shapes.shapes.Shapes)]
    
    # create value directory
    shapes_dir = os.path.join(self.task_dir, cfg.data['dirs']['tasks']['shapes'], value_name)
    
    if not os.path.exists(shapes_dir):
      os.mkdir(shapes_dir)
    else:
      # remove all previous shapes
      for x in os.listdir(shapes_dir):
        if x.endswith(".csv"):
            os.remove(os.path.join(shapes_dir, x))

    # go through and save 
    for x in shape_layers:
      x.save(os.path.join(shapes_dir, x.name))

  """
  Get pop layer name
  """
  def pop_layer_name(self, pop_type, pop_item):
    pop_name = pop_item['name'][0]
    pop_parent = pop_item['parent'][0]
    parent_path = pop_parent.split('/')[-1]
    
    # get name
    return f'({pop_type}) {parent_path}/{pop_name}'

  """
  Set point size of pops
  """
  def set_pop_points_size(self, pop_type, point_size = 6):
    # get data for populations
    pop_map = self.pop_utils.pop_map(self.task_dir, pop_type)
    
    # go through and change point size
    for i, pop in pop_map.items():
      pop_layer = self.viewer.layers[self.pop_layer_name(pop_type, pop)]
      
      # edit size
      pop_layer.size = np.full(pop_layer.size.shape, point_size)

  """
  Show population mapping
  
  This could also be done by listening to changes of the CSV
  population files - is that worth doing .. ?
    https://forum.image.sc/t/watchdog-in-napari/56692
  """
  def show_pop_mapping(self, pop_type, remove_previous = True,
                       value_name = None, filtered_from_value_name = False,
                       points_size = 6, pops = None):
    # get data for populations
    pop_map = self.pop_utils.pop_map(self.task_dir, pop_type)
    pop_data = self.pop_utils.pop_data(self.task_dir, pop_type)
    
    # hide populations if they are not shown
    # TODO at the moment - this is how populations are renamed
    if remove_previous is True:
      # pop_layer_names = [f'({pop_type}) {x["name"][0]}' for i, x in pop_map.items()]
      poplayers_to_remove = [i for i in [x.name for x in self.viewer.layers] if i.startswith(f'({pop_type})')]
        # and i not in pop_layer_names]
      
      for x in poplayers_to_remove:
        self.viewer.layers.remove(x)
    
    # filter on pops
    if pops is not None and len(pops) > 0:
      pop_map = {i: x for i, x in pop_map.items() if x['path'][0] in pops}
    
    # show populations as individual points layers
    # use pop map - in case there are still old
    # populations in pop_data dict
    for i, pop in pop_map.items():
      # check if population has cells
      if i in pop_data.keys():
        x = pop_data[i]
        
        pop_colour = pop['colour'][0]
        pop_show = pop['show'][0]
        pop_path = pop['path'][0]
        
        pop_value_name = pop['valueName'][0] if value_name is None else value_name
        filter_measure = pop['filterMeasure'][0] if 'filterMeasure' in pop else None
        
        # create name
        pop_layer_name = self.pop_layer_name(pop_type, pop)
        
        show_pop = True
        
        # if a population is filterd from a dataset,
        # then take only the relevant populations
        if filtered_from_value_name is True:
          # TODO this is a bit hardcoded and is only required
          # because the function can be called multiple times
          # in the case of 'live' images with multiple value names
          if filter_measure is not None and pop_type in cfg.data['populations']['multifile']:
            show_pop = pop_path.startswith(f'{pop_value_name}/')
        
        # show population
        if show_pop is True:
          # is population already shown?
          popLayer = None
          if pop_layer_name in self.viewer.layers:
            popLayer = self.viewer.layers[pop_layer_name]
          
          labels_view = self.label_props_utils.label_props_view(value_name = pop_value_name)
          
          # add points for cell selection
          centroid_cols = labels_view.centroid_columns(self.dim_utils.im_dim_order)
          label_df = labels_view.filter_by_obs(x)\
            .view_centroid_cols(self.dim_utils.im_dim_order)\
            .view_label_col()\
            .as_df()
        
          # get coordinates
          label_points = points_utils.prep_points(
            label_df, self.dim_utils, centroid_cols,
            im_scale = self.im_scale
          )
          
          # show on viewer
          # if popLayer is None:
          # TODO always add points - to preserve the order of populations?
          self.viewer.add_points(
              label_points,
              face_color = pop_colour,
              edge_color = 'black',
              name = pop_layer_name,
              visible = pop_show,
              n_dimensional = False if self.dim_utils.is_timeseries() else True,
              scale = self.im_scale,
              size = points_size,
              blending = 'translucent_no_depth')
      # else:
      #   # replace data
      #   popLayer.data = selected_points
      #   popLayer.face_color = pop_colour
      #   popLayer.visible = pop_show
        
    # showing populations as labels takes a long time at the moment
    # # get colours
    # cluster_colours = dict()
    # 
    # for i, x in pop_map.items():
    #   curColour = x['colour']
    # 
    #   # replace black or not shown with with none
    #   if x['colour'] == "#000000":
    #     curColour = None
    #   if x['show'] is False:
    #     curColour = None
    # 
    #   cluster_colours[i] = curColour
    # 
    # # insert background
    # cluster_colours[keywordBG] = None
    # # prepare background
    # clusterDF = adata.obs.copy() # ! copy !
    # backgroundRow = {'label': 0, keyword: keywordBG}
    # clusterDF = clusterDF.append(
    #   backgroundRow, ignore_index = True)
    # 
    # # convert to dict
    # layerMapColoursDict = {x: cluster_colours[
    #   clusterDF[clusterDF.label == x][keyword].values[0]
    #   ] for i, x in enumerate(self.label_ids())}
    # 
    # # push to napari
    # self.viewer.layers['labels'].color = layerMapColoursDict
    
  """
  Show cell neighbours
  """
  def show_cell_neighbours(
    self, pop_type, remove_previous = True, value_name = 'default', color = 'orange'):
    # TODO you should pre-compute some of this
      
    # load properties
    labels_view = self.label_props_utils.label_props_view(value_name = f'{value_name}.sq')
    adata = labels_view.as_adata()
    labels_view.close()
    
    # hide previous neighbours
    if remove_previous is True:
      layers_to_remove = [i for i in [x.name for x in self.viewer.layers] if i.startswith(f'({pop_type}) Neighbours')]
      
      for x in layers_to_remove:
        self.viewer.layers.remove(x)
    
    # get indices of connectivities
    idx = np.vstack(adata.obsp['spatial_connectivities'].nonzero()).T
    
    # convert to DF
    df = pd.DataFrame(idx)
    df.columns = ['A', 'B']
    
    # create vector array
    vector_array = np.zeros([
      len(df.index),  # number of vectors
      2,              # this is start and direction of vector
      len(self.dim_utils.default_dim_order(ignore_channel = True))
    ])
    
    # get the label positions
    label_pos = pd.DataFrame(adata.obsm['spatial']).reset_index()
    label_pos.columns = ['index'] + list(adata.uns['spatial_cols'])
    
    adata.file.close()
    
    dim_order = self.dim_utils.trimmed_default_dim_order(ignore_channel = True, squeeze = True)
    
    source_cols = {f'centroid_{x.lower()}': f'{x.lower()}_source' for x in dim_order}
    target_cols = {f'centroid_{x.lower()}': f'{x.lower()}_target' for x in dim_order}
    
    # patch on the positions of the vectors
    neighbour_df = pd.merge(
      pd.merge(
        df, label_pos, how = 'left',
        left_on = ['A'], right_on = ['index']
      ).rename(columns = source_cols),
      label_pos, how = 'left',
      left_on = ['B'], right_on = ['index']
    ).rename(columns = target_cols)
    
    for x in dim_order:
      # drop cols
      neighbour_df.drop(f'index_{x.lower()}', axis = 1)
      
      # get target vectors
      neighbour_df[f'dist_{x.lower()}'] = neighbour_df[f'{x.lower()}_target'] - neighbour_df[f'{x.lower()}_source']
      
      # add to vector array
      centroid_idx = self.dim_utils.dim_idx(x, ignore_channel = True)
      
      vector_array[:, 0, centroid_idx] = neighbour_df[f'{x.lower()}_source'].to_list()
      vector_array[:, 1, centroid_idx] = neighbour_df[f'dist_{x.lower()}'].to_list()
    
    # add to napari
    self.viewer.add_vectors(
      vector_array,
      scale = self.im_scale,
      edge_color = color,
      edge_width = 2,
      name = f'({pop_type}) Neighbours'
    )
    
    # move to the bottom of the stack
    pop_type_layers = [i for i in [x.name for x in self.viewer.layers] if i.startswith(f'({pop_type})')]
    
    self.viewer.layers.move(
      self.viewer.layers.index(pop_type_layers[-1]),
      self.viewer.layers.index(pop_type_layers[0])
    )
    
  """
  Show layer
  """
  def show_layer(self, layer_name):
    # check if layer is in list
    if layer_name in self.viewer.layers:
      self.viewer.layers[layer_name].visible = True
      
  """
  Hide layer
  """
  def hide_layer(self, layer_name):
    # check if layer is in list
    if layer_name in self.viewer.layers:
      self.viewer.layers[layer_name].visible = False
      
  """
  Remove layer
  """
  def hide_layer(self, layer_name):
    # check if layer is in list
    if layer_name in self.viewer.layers:
      self.viewer.layers[layer_name].visible = False
      
  """
  Remove layer
  """
  def remove_layer_by_name(self, layer_name):
    # check if layer is in list
    if layer_name in self.viewer.layers:
      self.viewer.layers.remove(layer_name)

  """
  return label IDs
  """
  def label_ids(self, value_name = None):
    label_ids = None
    
    labels_view = self.label_props_utils.label_props_view(value_name = value_name)
    
    if labels_view is not None:
      label_ids = labels_view.view_label_col().values_obs().label.tolist()
      labels_view.close()
        
      # add '0' for background
      label_ids.insert(0, 0)

    return label_ids

  """
  Remove segmentation layer
  """
  def remove_segmentation_layer(self, layer_name = "Segmentation"):
    self.remove_layer_by_name(layer_name)

  """
  Write command to output
  """
  def write_to_output(self, cmd_dict):
    with open(self.viewer_output_file, 'w+') as f:
      json.dump(cmd_dict, f)

  """
  Create bindings for segmentation
  """
  def create_segmentation_module(self):
    """
    Test binding
    """
    @self.viewer.bind_key('k', overwrite = True)
    def segment_viewpoint(event = None):
      # send command to shiny
      self.write_to_output({"segment": "viewpoint"})

  """
  Push selected cells to output file
  """
  def selected_points_to_output(self, moduleID):
      # get selected points
      selected_points_idx = np.array(list(self.points_layer.selected_data))
      selected_points_ids = self.points_layer.properties['label_id'][selected_points_idx]
      selected_points_dict = dict()
      selected_points_dict[moduleID] = selected_points_ids.tolist()
      
      # send to output
      self.write_to_output(selected_points_dict)

  """
  Create bindings for cell mapping
  """
  def create_clust_populations_module(self):
    @self.viewer.bind_key('k', overwrite = True)
    def save_selected_points(event = None):
      self.selected_points_to_output('clustPopulationsSelectPoints')
      
  """
  Create bindings for cell mapping
  """
  def create_spatial_analysis_module(self):
    @self.viewer.bind_key('k', overwrite = True)
    def save_selected_points(event = None):
      self.selected_points_to_output('spatialAnalysisSelectPoints')
      
  """
  Create bindings for cell mapping
  """
  def create_pixel_classification_module(self):
    pass
      
  """
  Create bindings for cell mapping
  """
  def create_signal_analysis_module(self):
    @self.viewer.bind_key('k', overwrite = True)
    def save_selected_points(event = None):
      self.selected_points_to_output('signalAnalysisSelectPoints')
      
  """
  Create bindings for cell mapping
  """
  def create_tracking_images_module(self):
    @self.viewer.bind_key('k', overwrite = True)
    def save_selected_points(event = None):
      self.selected_points_to_output('trackingImagesSelectPoints')
      
  """
  Create bindings for cell mapping
  
  https://napari.org/guides/stable/connecting_events.html?highlight=mouse
  """
  def create_gate_populations_module(self):
    # @self.points_layer.mouse_move_callbacks.append
    # def selected_pointsToOut(layer, event):
    #   selected_points = set()
    # 
    #   while len(selected_points) == 0:
    #     selected_points = layer.selected_data
    #     yield
    # 
    #   print(selected_points)
    @self.viewer.bind_key('k', overwrite = True)
    def save_selected_points(event = None):
      self.selected_points_to_output('gatePopulationsSelectPoints')
      
