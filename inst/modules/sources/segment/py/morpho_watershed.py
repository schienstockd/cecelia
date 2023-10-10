# add CCIA modules
import sys
import os
sys.path.append("./")

# find local peaks with a specific radius
# skimage.feature.peak_local_max
# https://scikit-image.org/docs/dev/api/skimage.feature.html#skimage.feature.peak_local_max
# https://scikit-image.org/docs/dev/api/skimage.feature.html#skimage.feature.blob_log
#
# For segmentation
# https://jni.github.io/i2k-skimage-napari/lectures/2_segmentation_and_regionprops.html

import numpy as np

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
import py.script_utils as script_utils

from py.dim_utils import DimUtils
from py.morpho_watershed_utils import MorphoWatershedUtils

# detect seeds and run watershed
def run(params):
  # load image
  if 'imData' not in params.keys():
    im_dat, zarr_group_info = zarr_utils.open_as_zarr(
      params['imPath'], as_dask = True)
  else:
    im_dat = params['imData']

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(params['imPath'])

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  # define params
  params = {
    'ccia': params['ccia'],
    'task_dir': params['taskDir'],
    'im_path': params['imPath'],
    'dim_utils': dim_utils,
    'seed_channel': params['seedChannel'],
    'cell_radius': params['cellRadius'],
    'cell_size_max': params['cellSizeMax'],
    'seed_threshold_rel': params['seedThresholdRel'],
    'seed_threshold_abs': params['seedThresholdAbs'],
    'z_spread': params['zSpread'],
    'cell_min_distance': params['cellMinDistance'],
    'gaussian_filter': params['gaussianFilter'],
    'maximum_filter': params['maximumFilter'],
    'median_filter': params['medianFilter'],
    'minimum_filter': params['minimumFilter'],
    'timepoints': params['timepoints'],
    'process_as_zarr': True,
    'clear_touching_border': True,
    'clear_depth': False,
    'measure': True
  }

  # run segmentation
  morpho_utils = MorphoWatershedUtils(params)
  labels = morpho_utils.predict(im_dat)

def main():
  # get params
  params = script_utils.script_params(
  	flatten_except = ['timepoints']
  )

  # run
  run(params)

if __name__ == "__main__":
  main()
