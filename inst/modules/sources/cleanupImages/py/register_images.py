# add CCIA modules
import sys
import os
sys.path.append("./")

import py.script_utils as script_utils

# config
import py.config_utils as cfg
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.sitkibex as sitkibex
import SimpleITK as sitk

import math
import shutil
import zarr
import numpy as np

# register images and apply transformations
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # init params
  fixed_im_path = script_utils.get_param(params, 'fixedImPath', default = '')
  zero_root_dir = script_utils.get_param(params, 'zeroRootDir', default = '')
  im_source_name = script_utils.get_param(params, 'imSourceName', default = 'ccidImage.zarr')
  uids = script_utils.get_param(params, 'uIDs', default = [])
  im_reg_path = script_utils.get_param(params, 'imRegPath', default = '')
  reg_channels = script_utils.get_param(params, 'regChannels', default = [])
  do_fft_initialization = script_utils.get_param(params, 'doFftInitialization', default = False)
  do_affine_2d = script_utils.get_param(params, 'doAffine2d', default = True)
  do_affine_3d = script_utils.get_param(params, 'doAffine3d', default = False)
  ignore_spacing = script_utils.get_param(params, 'ignoreSpacing', default = True)
  sigma = script_utils.get_param(params, 'sigma', default = 1.0)
  auto_mask = script_utils.get_param(params, 'autoMask', default = False)
  samples_per_parameter = script_utils.get_param(params, 'samplesPerParameter', default = 5000)
  expand = script_utils.get_param(params, 'expand', default = None)
  expand = expand if expand > 0 else None

  # get image information
  im_paths = [os.path.join(zero_root_dir, x, im_source_name) for x in uids]
  
  input_arrays = [zarr_utils.open_as_zarr(x, as_dask = True) for x in im_paths]
  input_arrays = [x[0] for x in input_arrays]

  # create dim utils for images
  dim_utils = [
    DimUtils(ome_xml_utils.parse_meta(x), use_channel_axis = True) for x in im_paths
  ]

  for i, x in enumerate(dim_utils):
    x.calc_image_dimensions(input_arrays[i][0].shape)
  
  ### Registration with ITK from here ###
  # All credit goes to sitkibex - this is just a port into Cecelia
  # https://github.com/niaid/imaris_extensions
  # https://github.com/niaid/sitk-ibex
  # https://zenodo.org/record/4632320#.Y5pbnOJBzRY
  
  # get new image dimensions
  # reg_im_shape = list(min([x[0].shape for x in input_arrays]))
  reg_im_shape = list(input_arrays[0][0].shape)
  
  # add channels
  # exclude channel for registration
  reg_im_shape[dim_utils[0].dim_idx('C')] = sum([x.dim_val('C') - 1 for x in dim_utils]) + 1
  
  # remove previous folder
  if im_reg_path is not None and os.path.exists(im_reg_path) is True:
    shutil.rmtree(im_reg_path)
  
  # create array
  reg_zarr = zarr.create(
    reg_im_shape,
    dtype = input_arrays[0][0].dtype,
    chunks = input_arrays[0][0].chunksize,
    store = im_reg_path
  )
  
  # get slicing
  slices = [[slice(None) for _ in range(len(input_arrays[0][0].shape))]] * len(input_arrays)
  
  for i, x in enumerate(slices):
    slices[i][dim_utils[i].dim_idx('T')] = 0
  
  # get transforms
  reg_tx = list()
  
  # get fixed image
  slices[0][dim_utils[0].dim_idx('C')] = reg_channels[0]
  fixed_im = sitk.GetImageFromArray(np.squeeze(zarr_utils.fortify(input_arrays[0][0][tuple(slices[0])])))
  
  # go through arrays
  for i, x in enumerate(input_arrays[1:]):
    logfile_utils.log(f'>> Register {i}')
    
    # set slicing
    slices[i + 1][dim_utils[i].dim_idx('C')] = reg_channels[i + 1]
    
    # apply
    # TODO can you do this somehow in the low res image and then scale up .. ?
    reg_tx.append(sitkibex.registration(
      fixed_im,
      sitk.GetImageFromArray(np.squeeze(zarr_utils.fortify(x[0][tuple(slices[i + 1])]))),
      do_fft_initialization = do_fft_initialization,
      do_affine2d = do_affine_2d,
      do_affine3d = do_affine_3d,
      ignore_spacing = ignore_spacing,
      sigma = sigma,
      auto_mask = auto_mask,
      samples_per_parameter = samples_per_parameter,
      expand = expand))
      
  # apply transforms
  reg_slices = [slice(None) for _ in range(len(reg_zarr.shape))]
  im_slices = [slice(None) for _ in range(len(x[0].shape))]
  reg_slices[dim_utils[0].dim_idx('T')] = 0
  reg_slices[dim_utils[0].dim_idx('C')] = slice(0, dim_utils[0].dim_val('C'), 1)
  im_slices[dim_utils[0].dim_idx('T')] = 0
  
  # push first image
  reg_zarr[tuple(reg_slices)] = input_arrays[0][0][tuple(im_slices)]
  
  channel_sum = dim_utils[0].dim_val('C')
  
  # go through arrays
  for i, x in enumerate(input_arrays[1:]):
    # count channels to exclude registration channel
    k = 0
    
    logfile_utils.log(f'>> Save {i}')
    
    # go through channels
    for j in range(dim_utils[i + 1].dim_val('C')):
      if j != reg_channels[0]:
        # set slicing
        reg_slices[dim_utils[0].dim_idx('C')] = channel_sum + k
        im_slices[dim_utils[i + 1].dim_idx('C')] = j
        
        logfile_utils.log(f'> Channel {j}')
        
        # push to zarr
        reg_zarr[tuple(reg_slices)] = sitk.GetArrayFromImage(sitkibex.resample(
          fixed_image = fixed_im,
          moving_image = sitk.GetImageFromArray(np.squeeze(zarr_utils.fortify(x[0][tuple(im_slices)]))),
          transform = reg_tx[i]))
          
        k += 1
    
    channel_sum += (dim_utils[i + 1].dim_val('C') - 1)

  logfile_utils.log('>> save back')
  
  # save back
  zarr_utils.create_multiscales(
    reg_zarr, im_reg_path,
    dim_utils = dim_utils[0], nscales = len(input_arrays[0]))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    fixed_im_path, im_reg_path,
    changed_shape = reg_im_shape,
    dim_utils = dim_utils[0]
  )

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['uIDs', 'regChannels']
  )

  # run AF and drift correction
  run(params)

if __name__ == "__main__":
  main()
