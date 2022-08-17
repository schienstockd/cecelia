import numpy as np
import math
import zarr
import os
import json
import shutil

# find local peaks with a specific radius
# skimage.feature.peak_local_max
# https://scikit-image.org/docs/dev/api/skimage.feature.html#skimage.feature.peak_local_max
# https://scikit-image.org/docs/dev/api/skimage.feature.html#skimage.feature.blob_log
# 
# For segmentation
# https://jni.github.io/i2k-skimage-napari/lectures/2_segmentation_and_regionprops.html

import numpy as np

from py.dim_utils import DimUtils

import py.script_utils as script_utils
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
import py.measure_utils as measure_utils
import py.slice_utils as slice_utils

class SeedUtils:
  def __init__(self, params):
    # init params
    self.dim_utils = params['dim_utils']
    self.seed_channel = params['seed_channel']
    self.cell_radius = params['cell_radius']
    self.z_spread = params['z_spread'] if "z_spread" in params else 1
    self.slices = params['slices'] if "slices" in params else None
    
    # logging
    self.logfile_utils = script_utils.get_logfile_utils(params)
    
    # tiling parameters
    self.timepoints = params['timepoints'] if 'timepoints' in params else None
    self.block_size = params['block_size'] if 'block_size' in params else None
    self.overlap = params['overlap'] if 'overlap' in params else 0
    self.block_size_z = params['block_size_z'] if 'block_size_z' in params else None
    self.overlap_z = params['overlap_z'] if 'overlap_z' in params else None
    
    # TODO does that make sense .. ?
    self.context = params['context'] if 'context' in params else round(self.overlap * 3/4)
    
    self.timepoints = None
    if self.dim_utils.is_timeseries() is True:
      self.timepoints = params['timepoints'] if 'timepoints' in params else list(range(self.dim_utils.dim_val("T")))
    
    self.task_dir = params['task_dir']
    self.im_path = params['im_path']
    
    # get OME-XML
    self.omexml = ome_xml_utils.parse_meta(self.im_path)
    
  """
  Detect seeds
  """
  def detect_seeds(self, im_dat):
    # get blobs as list
    seeds_list = list()
    
    # drop channel
    seed_detection_shape = list(im_dat.shape)
    seed_detection_shape.pop(self.dim_utils.dim_idx("C"))
    seed_detection_shape = tuple(seed_detection_shape)
    
    # get slices
    if self.slices is None:
      self.slices = slice_utils.create_slices(
        seed_detection_shape, self.dim_utils, self.block_size, self.overlap,
        block_size_z = self.block_size_z, overlap_z = self.overlap_z,
        timepoints = self.timepoints)
      
    # go through slices
    for i, cur_slices in enumerate(self.slices):
      # only one channel
      cur_slices = list(cur_slices)
      cur_slices[self.dim_utils.dim_idx("C")] = slice(
        self.seed_channel, self.seed_channel + 1, 1)
      cur_slices = tuple(cur_slices)
      
      self.logfile_utils.log(">> Slice: " + str(i + 1) + "/" + str(len(self.slices)))
      self.logfile_utils.log(cur_slices)
      
      # get seeds for slices
      seeds = self.detect_seeds_in_slice(im_dat, cur_slices)
      
      self.logfile_utils.log("> Detected seeds")
      self.logfile_utils.log(len(seeds))
      
      # add to list
      seeds_list.append(seeds)
    
    # combine
    return np.vstack(seeds_list)

  """
  Detect seeds in slice
  """
  def detect_seeds_in_slice(self, im_dat, im_slices):
    pass
