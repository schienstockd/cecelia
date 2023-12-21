# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils

import zarr
import numpy as np
from tqdm import tqdm
import cv2

# correct AF and drift correct
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  im_path = script_utils.get_param(params, 'imPath', default = None)
  im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default = None)
  time_delta = script_utils.get_param(params, 'timeDelta', default = 1)
  im_channels = script_utils.get_param(params, 'imChannels', default = None)
  create_new_channels = script_utils.get_param(params, 'createNewChannels', default = False)
  
  # load image
  # im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = True)
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  logfile_utils.log('>> correct image by time delta')
  
  # get all channels by default
  if im_channels is None:
    im_channels = list(range(dim_utils.dim_val('C')))
    
  logfile_utils.log(f'> use {im_channels}')
  
  # remove previous folder
  # if im_correction_path is not None:
  #   shutil.rmtree(im_correction_path)
  
  # get im shape
  im_shape = im_dat[0].shape
  if create_new_channels is True:
    logfile_utils.log(f'> add {len(im_channels)} new channels')
    
    im_shape = list(im_shape)
    im_shape[dim_utils.dim_idx('C')] = dim_utils.dim_val('C') + len(im_channels)
    im_shape = tuple(im_shape)
  
  # prepare image taking into account any channels that need adding
  sum_zarr = zarr.create(
    # im_dat[0].shape,
    im_shape,
    dtype = im_dat[0].dtype,
    # chunks = im_dat[0].chunksize
    chunks = im_dat[0].chunks
    # store = im_correction_path
  )
  
  # fill in image channels if others are being added
  # TODO is there a better way to do this .. ?
  if create_new_channels is True:
    for i in range(dim_utils.dim_val('C')):
      im_slice = dim_utils.create_channel_slices(i)
      sum_zarr[im_slice] = im_dat[0][im_slice]
      
  # go through timepoints and channels
  for i in tqdm(range(dim_utils.dim_val('T'))):
    # for j in range(dim_utils.dim_val('C')):
    for j, k in enumerate(im_channels):
      # build average image over time for warping 
      d_start = i
      d_end = i + time_delta
  
      if d_start < 0:
        d_start = 0
        #d_end = time_delta * 2
  
      if d_start >= dim_utils.dim_val('T') - (time_delta * 2):
        #d_start = dim_utils.dim_val('T') - (time_delta * 2)
        d_end = dim_utils.dim_val('T') - 1
        
      if d_end >= dim_utils.dim_val('T'):
        d_end = dim_utils.dim_val('T') - 1
  
      # set slices
      start_slices = [slice(None) for _ in range(len(im_dat[0].shape))]
      start_slices[dim_utils.dim_idx('C')] = k
      end_slices = [x for x in start_slices]
      
      start_slices[dim_utils.dim_idx('T')] = slice(d_start, d_start + 1, 1)
      end_slices[dim_utils.dim_idx('T')] = slice(d_end, d_end + 1, 1)
  
      sum_slices = [slice(None) for _ in range(len(sum_zarr.shape))]
      sum_slices[dim_utils.dim_idx('T')] = i
      sum_slices[dim_utils.dim_idx('C')] = k if create_new_channels is False else dim_utils.dim_val('C') + j
      
      # set z slices
      if dim_utils.is_3D() is False and dim_utils.dim_idx('Z') is not None:
        sum_slices[dim_utils.dim_idx('Z')] = 0
        
      start_slices = tuple(start_slices)
      end_slices = tuple(end_slices)
      sum_slices = tuple(sum_slices)
  
      logfile_utils.log(start_slices)
      logfile_utils.log(end_slices)
  
      # TODO this is very slow with im_dat as dask
      # so it is currently limited to images that
      # can fit into memory with zarr
      sum_zarr[sum_slices] = np.squeeze(cv2.absdiff(
        im_dat[0][start_slices], im_dat[0][end_slices]))

  logfile_utils.log('>> save back')
  
  # save back
  zarr_utils.create_multiscales(
    sum_zarr, im_correction_path,
    dim_utils = dim_utils, nscales = len(im_dat))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    im_correction_path, im_path,
    changed_shape = sum_zarr.shape,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params(flatten_except = ['imChannels'])

  # run AF and drift correction
  run(params)

if __name__ == '__main__':
  main()
