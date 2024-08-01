# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.correction_utils as correction_utils
import py.script_utils as script_utils

from scipy.ndimage import binary_fill_holes
# from skimage.morphology import closing, disk
from skimage.morphology import disk
import zarr
import numpy as np

# crop
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  im_path = script_utils.get_param(params, 'imPath', default = None)
  im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default = None)
  to_common_area = script_utils.get_param(params, 'toCommonArea', default = False)
  im_channels = script_utils.get_param(params, 'imChannels', default = None)
  
  # load image
  # im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = True)
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)
  
  # crop image to common area
  # get masks for channels, combine and crop to image
  crop_masks = list()
  t_idx = dim_utils.dim_idx('T')
  x_idx = dim_utils.dim_idx('X')
  y_idx = dim_utils.dim_idx('Y')
  
  for i in im_channels:
    im_slices = dim_utils.create_channel_slices(i)
    
    # get minimum across time
    im_min = np.squeeze(np.min(im_dat[0][im_slices], axis = t_idx))
    
    crop_masks.append(binary_fill_holes(im_min > np.percentile(im_min, 0.01), structure = disk(10)))
    # crop_masks.append(closing(im_min > np.percentile(im_min, 0.01), footprint = disk(10)))
    
  # combine
  crop_mask = np.max(np.stack(crop_masks), axis = 0)
  
  # go through channels and crop
  corrected_image = zarr.zeros_like(im_dat[0])
  
  for i in range(dim_utils.dim_val('C')):
    im_slices = dim_utils.create_channel_slices(i)
    corrected_image[im_slices] = im_dat[0][im_slices] * crop_mask
  
  logfile_utils.log('>> crop edges')
  
  # crop zero edges
  # TODO this assumes 2D!
  # https://stackoverflow.com/a/39466129
  # true_points = np.argwhere(np.squeeze(crop_mask))
  true_points = np.argwhere(crop_mask)
  top_left = true_points.min(axis = 0)
  bottom_right = true_points.max(axis = 0)
  
  crop_slices = [slice(None) for _ in range(len(corrected_image.shape))]
  crop_slices[y_idx] = slice(top_left[0], bottom_right[0] + 1, 1)
  crop_slices[x_idx] = slice(top_left[1], bottom_right[1] + 1, 1)
  
  corrected_image = corrected_image[tuple(crop_slices)]
  
  logfile_utils.log(corrected_image.shape)
  
  # save everything into first multilevel
  corrected_path = os.path.join(im_correction_path, '0')

  logfile_utils.log('>> save back')

  # save back
  zarr_utils.create_multiscales(
    corrected_image, im_correction_path,
    nscales = len(im_dat), dim_utils = dim_utils,
    im_chunks = im_dat[0].chunks)

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    im_correction_path, im_path,
    changed_shape = corrected_image.shape,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params(flatten_except = ['imChannels'])

  # run AF and drift correction
  run(params)

if __name__ == '__main__':
  main()
