import numpy as np
import math
import zarr
import os
import json
import shutil
from pathlib import Path

# utils
import py.script_utils as script_utils
import py.zarr_utils as zarr_utils
import py.slice_utils as slice_utils
import py.measure_utils as measure_utils
from py.label_props_utils import LabelPropsUtils
import py.config_utils as cfg

# to expand labels
from skimage.segmentation import expand_labels, clear_border
from skimage.morphology import remove_small_objects, erosion, disk, ball
from skimage import filters, measure

class SegmentationUtils:
  def __init__(self, params):
    # init params
    self.dim_utils = params['dim_utils']
    self.subtract_edges = script_utils.get_param(params, 'subtract_edges', default = False)
    self.segment = script_utils.get_param(params, 'segment', default = True)
    self.measure = script_utils.get_param(params, 'measure', default = False)
    self.update_measures = script_utils.get_param(params, 'update_measures', default = False)
    self.save_measures = script_utils.get_param(params, 'save_measures', default = True)
    self.save_meshes = script_utils.get_param(params, 'save_meshes', default = False)
    self.find_contours = script_utils.get_param(params, 'find_contours', default = False)
    self.use_gpu = script_utils.get_param(params, 'use_gpu', default = False)
    self.label_expansion = script_utils.get_param(params, 'label_expansion', default = 0)
    self.label_erosion = script_utils.get_param(params, 'label_erosion', default = 0)
    self.halo_size = script_utils.get_param(params, 'halo_size', default = 0)
    self.halo_whole_cell = script_utils.get_param(params, 'halo_whole_cell', default = False)
    self.rank_labels = script_utils.get_param(params, 'rank_labels', default = False)
    self.label_suffixes = script_utils.get_param(params, 'label_suffixes', default = [])
    
    self.cell_size_min = script_utils.get_param(params, 'remove_small_objects', default = 20)
    self.cell_size_min = script_utils.get_param(params, 'cell_size_min', default = self.cell_size_min)
    # self.cell_size_max = script_utils.get_param(params, 'cell_size_max', default = 10**5)
    self.cell_size_max = script_utils.get_param(params, 'cell_size_max', default = 0)
    
    # thresholding
    self.threshold = script_utils.get_param(params, 'threshold', default = 0)
    self.rel_threshold = script_utils.get_param(params, 'rel_threshold', default = 0)
    
    # clear labels touching the border
    # in 2P many cells are touching the border
    # TODO in other scenarios - this makes sense?
    self.clear_touching_border = script_utils.get_param(params, 'clear_touching_border', default = True)
    self.clear_depth = script_utils.get_param(params, 'clear_depth', default = True)

    # tiling parameters
    self.block_size = script_utils.get_param(params, 'block_size', default = None)
    self.overlap = script_utils.get_param(params, 'overlap', default = 0)
    self.block_size_z = script_utils.get_param(params, 'block_size_z', default = None)
    self.overlap_z = script_utils.get_param(params, 'overlap_z', default = None)
    
    # TODO does that make sense .. ?
    self.context = script_utils.get_param(params, 'context', default = round(self.overlap * 3/4))
    
    self.timepoints = None
    if self.dim_utils.is_timeseries() is True:
      self.timepoints = script_utils.get_param(params, 'timepoints',
        default = list(range(self.dim_utils.dim_val('T'))))

    # process as zarr?
    self.process_as_zarr = script_utils.get_param(params, 'process_as_zarr', default = False)
    
    # get labels name
    self.value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
    self.ccia_path = script_utils.get_ccia_param(params, 'ccia_path', default = 'default')
    self.labels_filename = cfg.value_dir(self.value_name, 'labels')
    self.labels_props_filename = cfg.value_dir(self.value_name, 'labelProps')
    
    self.task_dir = params['task_dir']
    self.im_path = params['im_path']
    
    # extended parameters?
    self.extended_measures = script_utils.get_param(params, 'extended_measures', default = False)
    
    # generate label paths
    labels_path = Path(self.labels_filename)
    labels_stem = labels_path.stem
    labels_suffix = labels_path.suffix
    labels_parent = labels_path.parent
    
    # add labels for halo
    if self.halo_size > 0:
      self.label_suffixes += ['halo']
    
    self.labels_paths = {
      'base': os.path.join(self.task_dir, self.labels_filename)
    }
    
    for x in self.label_suffixes:
      self.labels_paths[x] = os.path.join(self.task_dir, labels_parent, f'{labels_stem}_{x}{labels_suffix}')
        
    # logging
    self.logfile_utils = script_utils.get_logfile_utils(params)  
      
    # keep segmentation labels matched?
    self.match_labels = False
    if all([x in self.label_suffixes for x in ['nuc', 'cyto']]):
      self.match_labels = True
      
    self.logfile_utils.log(f'>> Match segmentation: {self.match_labels}')
        
  """
  Predict objects
  """
  def predict(self, im_dat, nscales = 1):
    if self.process_as_zarr is True:
      if self.segment is True:
        labels = self.predict_from_zarr(im_dat)
      else:
        labels = dict()
        
        for i, x in self.labels_paths.items():
          labels[i], group_info = zarr_utils.open_labels_as_zarr(x, len(im_dat))
        
        # get first level
        labels = {i: x[0] for i, x in labels.items()}
      
      print(self.labels_props_filename)
      
      # run measurements
      # this will be the last step and
      # should be done on the whole image with Zarr arrays
      if self.measure is True:
        self.logfile_utils.log('Measure labels')
        
        props = measure_utils.measure_from_zarr(
          labels, im_dat, self.dim_utils, self.logfile_utils,
          block_size = self.block_size, overlap = self.overlap,
          context = self.context,
          clear_touching_border = self.clear_touching_border,
          clear_depth = self.clear_depth,
          timepoints = self.timepoints,
          task_dir = self.task_dir,
          value_name = self.value_name,
          save_meshes = self.save_meshes,
          extended_measures = self.extended_measures
        )
        
        # get spatial and temporal columns from props
        centroid_spatial = [x for x in props.columns if x in [f'centroid_{i}' for i in ['x', 'y', 'z']]]
        centroid_temporal = [x for x in props.columns if x == 'centroid_t']
        
        obsm = dict()
        uns = dict()
        
        # split spatial and temporal information into obsm
        # this will then allow processing with squidpy
        if len(centroid_spatial) > 0:
          uns['spatial_cols'] = centroid_spatial
          obsm['spatial'] = props[centroid_spatial].to_numpy()
          
        if len(centroid_temporal) > 0:
          uns['temporal_cols'] = centroid_temporal
          obsm['temporal'] = props[centroid_temporal].to_numpy()
        
        # save props
        if self.save_measures is True:
          LabelPropsUtils(self.task_dir, self.labels_props_filename)\
            .label_props(
              props[[x for x in props.columns if x not in centroid_spatial + centroid_temporal]],
              save = True,
              update_existing = self.update_measures,
              obsm = obsm, uns = uns
              )

      # find shapes of identified labels
      if self.find_contours is True:
        self.logfile_utils.log('Find contours')

        label_contours = measure_utils.countours_2D_from_zarr(
          labels, im_dat, self.dim_utils, self.logfile_utils,
          block_size = self.block_size, overlap = self.overlap,
          context = self.context
        )

        # save contours
        label_contours.to_csv(
          os.path.join(self.task_dir, 'label_contours.csv'),
          index = False)
      
      # generate multiscales for labels
      # TODO is there a more elegant way to do this .. ?
      if nscales > 1:
        for i, x in self.labels_paths.items():
          multiscales_file_path = x + '.multiscales'

          zarr_utils.create_multiscales(
            labels[i], multiscales_file_path,
            dim_utils = self.dim_utils,
            nscales = nscales,
            keyword = 'labels',
            ignore_channel = True
          )

          # remove previous labels and rename multiscales
          shutil.rmtree(x)
          os.rename(multiscales_file_path, x)
      
    return labels

  """
  Predict from Zarr
  """
  def predict_from_zarr(self, im_dat):
    # init shape and chunks
    zarr_shape = list(im_dat.shape)
    zarr_chunks = list(zarr_utils.chunks(im_dat))

    zarr_shape.pop(self.dim_utils.dim_idx('C'))
    zarr_chunks.pop(self.dim_utils.dim_idx('C'))
    
    # create empty zarr file
    labels = dict()
    for i, x in self.labels_paths.items():
      # remove directory if present
      if os.path.exists(x):
        shutil.rmtree(x)
      
      labels[i] = zarr.open(
        x,
        mode = 'w',
        shape = tuple(zarr_shape),
        chunks = tuple(zarr_chunks),
        dtype = np.uint32)

    # get slices
    slices = slice_utils.create_slices(
      labels[list(labels.keys())[0]].shape, self.dim_utils, self.block_size, self.overlap,
      block_size_z = self.block_size_z, overlap_z = self.overlap_z,
      timepoints = self.timepoints)
      
    # cur_max_labels = {i: 0 for i in labels.keys()}
    cur_max_labels = 0
    
    # clear borders?
    if self.dim_utils.is_timeseries() is True:
      clear_borders = len(slices) > self.dim_utils.dim_val('T')
    else:
      clear_borders = len(slices) > 1
    
    # go through slices
    for i, cur_slices in enumerate(slices):
      self.logfile_utils.log('>> Slice: ' + str(i + 1) + '/' + str(len(slices)))
      self.logfile_utils.log(cur_slices)
      self.logfile_utils.log(str(cur_max_labels))
      
      # add channel back for slice prediction
      if len(cur_slices) < len(im_dat.shape):
        dat_slices = list(cur_slices)
        dat_slices.insert(self.dim_utils.dim_idx('C'), slice(None))
        dat_slices = tuple(dat_slices)
      
      # call segmentation implementation
      alg_labels = self.predict_slice(im_dat, dat_slices)
      
      if alg_labels is not None:
        for j in alg_labels.keys():
          if alg_labels[j] is not None:
            # erode labels
            if self.label_erosion > 0:
              alg_labels[j] = erosion(alg_labels[j], selem = ball(self.label_erosion) \
                if self.dim_utils.is_3D() else disk(self.label_erosion))
            
            # expand labels
            if self.label_expansion > 0:
              alg_labels[j] = expand_labels(alg_labels[j], self.label_expansion)
            
            # remove border labels
            alg_labels[j] = measure_utils.clear_border_labels(
              alg_labels[j], self.dim_utils, context = self.context,
              clear_borders = clear_borders,
              clear_touching_border = self.clear_touching_border,
              clear_depth = self.clear_depth)
              
            # remove small/big objects
            if self.cell_size_min > 0:
              alg_labels[j] = remove_small_objects(alg_labels[j], self.cell_size_min)
            if self.cell_size_max > 0:
              alg_labels[j] = measure_utils.remove_big_objects(alg_labels[j], self.cell_size_max)
            
            # post process labels
            if self.subtract_edges is True:
              alg_labels[j] = measure_utils.subtract_edges_from_labels(alg_labels[j])
      
            # add dummy dimension if time is used
            # otherwise cur_labels and alg_labels cannot
            # be merged together
            if self.dim_utils.is_timeseries() is True:
              alg_labels[j] = np.expand_dims(
                alg_labels[j], axis = self.dim_utils.dim_idx('T'))
            
            # rank label ids
            # TODO not run run when labels have to be matched .. ?
            if self.match_labels is False and self.rank_labels is True:
              alg_labels[j] = measure_utils.rank_labels(alg_labels[j])
            
        # match labels, then go on
        if self.match_labels is True:
          # TODO there must be a better way of doing this
          # get all label ids
          label_ids = {
            i: set(np.unique(x)) for i, x in alg_labels.items()
          }
          
          matched_labels = None
          
          # find intersections
          for y in label_ids.values():
            if matched_labels is None:
              matched_labels = y
            else:
              matched_labels = set(matched_labels).intersection(y)
              
          # set all values not in set as '0'
          # TODO not very nice
          for j, y in label_ids.items():
            for k in [z for z in list(y) if z not in list(matched_labels)]:
              alg_labels[j][alg_labels[j] == k] = 0
        
        # run post processing steps
        alg_labels = self.post_processing(alg_labels)
        next_max_labels = list()
        
        for j, y in alg_labels.items():
          if y is not None:
            # increase numbering
            # y[y > 0] = y[y > 0] + cur_max_labels[i]
            y[y > 0] = y[y > 0] + cur_max_labels
            
            # merge with exisiting labels
            labels[j][cur_slices] = np.maximum(labels[j][cur_slices], y)
            
            y_max_label = y.max()
            
            if y_max_label > 0:
              next_max_labels.append(y_max_label)
          
        # set current maximum from base
        # TODO is this a fair assumption? - No
        # if alg_labels['base'] is not None and alg_labels['base'].max() > 0:
        #   cur_max_labels = alg_labels['base'].max()
        if len(next_max_labels) > 0:
          cur_max_labels = max(next_max_labels)
      
        self.logfile_utils.log(f'>> Max label {cur_max_labels}')
      
    return labels

  """
  Run post processing steps
  """
  def post_processing(self, labels):
    # get base labels
    base_labels = np.squeeze(labels['base'])
    
    # calculate halo 
    if self.halo_size > 0:
      # expand base labels abd subtract base labels from expanded labels
      if self.halo_whole_cell is True:
        labels['halo'] = expand_labels(base_labels, self.halo_size)
      else:
        labels['halo'] = expand_labels(base_labels, self.halo_size) - base_labels
      
    # add cyto segmentation from base and nucleus?
    if 'nuc' in labels.keys():
      # subtract nuclei from whole cell
      labels['cyto'] = np.copy(base_labels)
      labels['cyto'][labels['nuc'] > 0] = 0
    
    return labels
    
  """
  Predict slice
  """
  def predict_slice(self, im_dat, im_slices):
    pass
