import os
import numpy as np

# utils
from py.segmentation_utils import SegmentationUtils

# For segmentation
# https://jni.github.io/i2k-skimage-napari/lectures/2_segmentation_and_regionprops.html

import numpy as np

from py.local_peak_seeds_utils import LocalPeakSeedsUtils
from py.dim_utils import DimUtils
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
import py.measure_utils as measure_utils

import skimage.feature
import skimage.filters
import skimage.morphology
import skimage.segmentation

from scipy import ndimage as ndi

class MorphoWatershedUtils(SegmentationUtils):
  def __init__(self, params):
    # call super
    super().__init__(params)

    # init params
    self.seed_channel = params['seed_channel']
    self.cell_radius = params['cell_radius']
    self.gaussian_filter = params['gaussian_filter'] if 'gaussian_filter' in params else 1
    self.maximum_filter = params['maximum_filter'] if 'maximum_filter' in params else 1
    self.median_filter = params['median_filter'] if 'median_filter' in params else 1
    self.minimum_filter = params['minimum_filter'] if 'minimum_filter' in params else 1
    
    # determine seeds for every slice?
    self.seeds = params['seeds'] if 'seeds' in params else None
    self.calc_seeds = True if self.seeds is None else False
    
    self.params = params

  """
  Predict slice
  """
  def predict_slice(self, im_dat, dat_slices):
    # add channel to slices for data
    dat_slices = list(dat_slices)
    dat_slices[self.dim_utils.dim_idx("C")] = slice(
      self.seed_channel, self.seed_channel + 1, 1
    )
    dat_slices = tuple(dat_slices)
    
    # enhance blobs
    im_to_segment = np.squeeze(zarr_utils.fortify(im_dat[dat_slices]))
    
    # filter noise
    im_to_segment = skimage.filters.gaussian(
      im_to_segment, sigma = self.gaussian_filter)
    
    # make objects bigger
    im_to_segment = ndi.maximum_filter(
      im_to_segment, size = self.maximum_filter)
    
    # get edges
    im_edges = skimage.filters.scharr(im_to_segment)
    
    # filter edges
    im_edges = skimage.filters.median(im_edges)
    im_edges = ndi.minimum_filter(im_edges, size = self.minimum_filter)
    
    # TODO would that work?
    # im_edges = ndi.gaussian_laplace(im_to_segment, sigma = 3)
    
    # generate seeds if None
    if self.calc_seeds is True:
      # add slices
      params = self.params.copy()
      params['slices'] = [dat_slices]
      
      # run seed detection
      seed_utils = LocalPeakSeedsUtils(params)
      self.seeds = seed_utils.detect_seeds(im_dat)
    
    # get markers
    markers = self.seeds[
      self.seeds[:, self.dim_utils.dim_idx("T")] == dat_slices[self.dim_utils.dim_idx("T")].start]
    
    # add markers to im
    markers_im = np.zeros_like(im_to_segment, dtype = int)
    
    for i, x in enumerate(markers):
      if self.dim_utils.is_3D():
        markers_im[
          int(x[1]), int(x[2]), int(x[3])
        ] = i + 1
      else:
        markers_im[
          int(x[1]), int(x[2])
        ] = i + 1
        
    # get ball shape
    if self.dim_utils.is_3D():
      ball_shape = skimage.morphology.ball(round(self.cell_radius))
    else:
      ball_shape = skimage.morphology.disk(round(self.cell_radius))
        
    # dilate markers
    markers_im = skimage.morphology.dilation(
      markers_im, ball_shape
    )
    
    # get labels
    labels = skimage.segmentation.watershed(
    #labes = ndi.watershed_ift(
    #labes = skimage.segmentation.random_walker(
      im_edges, markers_im
    )
    
    return {
      'base': labels
      }
