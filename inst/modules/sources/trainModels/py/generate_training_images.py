# add CCIA modules
import sys
import os
sys.path.append('./')

import zarr
import numpy as np
import random
import tifffile

import py.script_utils as script_utils
import py.zarr_utils as zarr_utils
import py.slice_utils as slice_utils
from py.dim_utils import DimUtils
import py.ome_xml_utils as ome_xml_utils

# generate training images
def run(params):
  # there should an image path and a number of directories
  # to save the training images to
  # these directories should be generated beforehand in R
  # as images belonging to a training set

  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # load base image
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(params['imPath'])
  omexml = ome_xml_utils.parse_meta(params['imPath'])

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  # create chunks
  zarr_chunks = np.ones(len(dim_utils.im_dim))
  zarr_chunks[dim_utils.dim_idx('X')] = params['crop']['X'] if params['crop']['X'] < dim_utils.dim_val('X') else dim_utils.dim_val('X')
  zarr_chunks[dim_utils.dim_idx('Y')] = params['crop']['Y'] if params['crop']['Y'] < dim_utils.dim_val('Y') else dim_utils.dim_val('Y')

  # create shape
  zarr_shape = zarr_chunks.copy()
  zarr_shape[dim_utils.dim_idx('C')] = len(params['channels'])
  
  if dim_utils.is_3D():
    zarr_shape[dim_utils.dim_idx('Z')] = params['crop']['Z'] if params['crop']['Z'] > 0 else dim_utils.dim_val('Z')

  if dim_utils.is_timeseries():
    zarr_shape[dim_utils.dim_idx('T')] = params['crop']['T'] if params['crop']['T'] > 0 else dim_utils.dim_val('T')
  
  # convert to tuple
  zarr_chunks = tuple(zarr_chunks)
  zarr_shape = tuple(zarr_shape)

  # go through images
  for x in params['trainingImagePaths']:
    # create zarr
    # crop_zarr = zarr.open_array(
    #   x, mode = 'w', shape = zarr_shape,
    #   chunks = zarr_chunks, fill_value = 0,
    #   dtype = im_dat[0].dtype
    # )

    # get random slices for channels and copy in
    # at the moment, this will only be one channel
    # I don't know how that works with multiple ...
    for y in params['channels']:
      slices = [slice(None) for z in range(len(zarr_shape))]

      for i, crop_val in params['crop'].items():
        # get image dimension index
        dim_idx = dim_utils.dim_idx(i)

        if dim_idx is not None:
          # get image dimension value
          dim_val = dim_utils.dim_val(i)
          
          # make sure crop value is not bigger than dim value          
          if crop_val > dim_val:
            crop_val = dim_val
          
          if crop_val > 0:
            # get random range between this and value and zero
            start_pos = random.randint(0, dim_val - crop_val)
            end_pos = start_pos + crop_val

            slices[dim_idx] = slice(start_pos, end_pos, 1)

      # set channel
      slices[dim_utils.dim_idx('C')] = slice(y, y + 1, 1)

      # convert to tuple
      slices = tuple(slices)

      # get slice
      slice_im = im_dat[0][slices]
      slice_shape = zarr_shape

      # do maximum projection
      if params['maximumProjection'] is True:
        slice_im = np.amax(slice_im, dim_utils.dim_idx('Z'))

        # add z dimension back
        slice_im = np.expand_dims(
          slice_im, axis = dim_utils.dim_idx('Z'))

        # edit shape
        slice_shape = list(slice_shape)
        slice_shape[dim_utils.dim_idx('Z')] = 1
        slice_shape = tuple(slice_shape)
      
      # save as multiscales back
      logfile_utils.log(x)
      logfile_utils.log(slice_im.shape)
      logfile_utils.log(zarr_chunks)
      zarr_utils.create_multiscales(slice_im, x, im_chunks = zarr_chunks)

      # add metadata
      ome_xml_utils.save_meta_in_zarr(
        x, params['imPath'],
        changed_shape = [int(x) for x in slice_shape],
        dim_utils = dim_utils
      )

def main():
  # get params
  params = script_utils.script_params(
  	flatten_except = ['trainingImagePaths', 'channels']
  )

  # run main function
  run(params)

if __name__ == '__main__':
  main()
