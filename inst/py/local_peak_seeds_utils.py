import os
import numpy as np

# utils
from py.seed_utils import SeedUtils
import py.zarr_utils as zarr_utils

import skimage.feature
import skimage.morphology

from scipy import ndimage

class LocalPeakSeedsUtils(SeedUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)

    # init params
    self.cell_min_distance = params['cell_min_distance']
    self.seed_threshold_rel = params['seed_threshold_rel'] if 'seed_threshold_rel' in params else None
    self.seed_threshold_abs = params['seed_threshold_abs'] if 'seed_threshold_abs' in params else None
    
    # create ball shape to detect seeds
    if self.dim_utils.is_3D():
      ball_shape = skimage.morphology.ball(self.cell_radius)
      
      # squash the ball according to image scale
      ball_scale = [
        # TODO use when ome_type 0.21 is released  
        # 1/self.omexml.images[0].pixels.physical_size_z,
        # 1/self.omexml.images[0].pixels.physical_size_y,
        # 1/self.omexml.images[0].pixels.physical_size_x
        1/self.omexml.image().Pixels.get_PhysicalSizeX(),
        1/self.omexml.image().Pixels.get_PhysicalSizeY(),
        1/self.omexml.image().Pixels.get_PhysicalSizeZ()
      ]
      
      # account for z spread
      # this should be the PSF.. but the user needs to estimate this
      ball_scale[0] *= self.z_spread
    else:
      ball_shape = skimage.morphology.disk(self.cell_radius)
      
      ball_scale = [
        # TODO use when ome_type 0.21 is released  
        # 1/self.omexml.images[0].pixels.physical_size_y,
        # 1/self.omexml.images[0].pixels.physical_size_x
        1/self.omexml.image().Pixels.get_PhysicalSizeY(),
        1/self.omexml.image().Pixels.get_PhysicalSizeZ()
      ]
    
    # reshape to image pixel size
    self.ball_shape = ndimage.zoom(ball_shape, tuple(ball_scale))
    
  """
  Detect seeds in slice
  """
  def detect_seeds_in_slice(self, im_dat, im_slices):
    # detect seed with local peak
    seeds = skimage.feature.peak_local_max(
      np.squeeze(zarr_utils.fortify(im_dat[im_slices])),
      footprint = self.ball_shape,
      min_distance = self.cell_min_distance,
      threshold_rel = self.seed_threshold_rel,
      threshold_abs = self.seed_threshold_abs,
      exclude_border = False
    )
    
    # add time dimension to seeds
    if self.dim_utils.is_timeseries():
      seeds_time = np.zeros((
        seeds.shape[0],
        seeds.shape[1] + 1
      ))

      for i in range(seeds.shape[0]):
        seeds_time[i] = np.insert(
          seeds[i, :], 0,
          im_slices[self.dim_utils.dim_idx("T")].start
          )
      
      seeds = seeds_time
    
    return seeds
