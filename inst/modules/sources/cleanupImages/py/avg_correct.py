# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils
import math
import numpy as np
import dask.array as da

# correct AF and drift correct
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  avg_channels = script_utils.get_param(params, 'avgChannels', default = dict())
  im_path = script_utils.get_param(params, 'imPath', default = None)
  im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default = None)
  tile_xy = script_utils.get_param(params, 'tileXY', default = None)
  tile_offset = script_utils.get_param(params, 'tileOffset', default = None)

  # load image
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(
    im_path, as_dask = True)
    # im_path, as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)
  
  logfile_utils.log('>> avg image')
  logfile_utils.log(avg_channels)
  
  im_avg_list = list()
  
  # get indices
  x_idx = dim_utils.dim_idx('X')
  y_idx = dim_utils.dim_idx('Y')
  c_idx = dim_utils.dim_idx('C')
  z_idx = dim_utils.dim_idx('Z')
  
  x_val = dim_utils.dim_val('X')
  y_val = dim_utils.dim_val('Y')
  
  for cur_channel in avg_channels:
    logfile_utils.log(f'>> Process {cur_channel}')
    
    # get slices
    slices = [slice(None) for _ in range(len(im_dat[0].shape))]
    slices[c_idx] = cur_channel
    
    cur_im = zarr_utils.fortify(im_dat[0][tuple(slices)])
    
    slices = [slice(None) for _ in range(len(cur_im.shape))]
    tile_list = list()
    
    # top left
    logfile_utils.log('> top left')
    
    for k, z in enumerate(range(int(tile_xy/tile_offset))):
      tile_list.append(np.zeros(cur_im.shape, dtype = cur_im.dtype))
      logfile_utils.log(k)
      
      for i, x in enumerate(range(0 + (z * tile_offset) - int(tile_xy/2), x_val + (z * tile_offset), tile_xy)):
        for j, y in enumerate(range(0 + (z * tile_offset) - int(tile_xy/2), y_val + (z * tile_offset), tile_xy)):
          slices[dim_utils.dim_idx('X', ignore_channel = True)] = slice(x, x + tile_xy, 1)
          slices[dim_utils.dim_idx('Y', ignore_channel = True)] = slice(y, y + tile_xy, 1)
          
          tile_list[-1][tuple(slices)] = np.average(cur_im[tuple(slices)])
    
    # bottom right
    logfile_utils.log('> bottom right')
    
    for k, z in enumerate(range(int(tile_xy/tile_offset))):
      tile_list.append(np.zeros(cur_im.shape, dtype = cur_im.dtype))
      logfile_utils.log(k)
      
      for i, x in enumerate(range(x_val - (z * tile_offset), 0 - (z * tile_offset) - int(tile_xy/2), -tile_xy)):
        for j, y in enumerate(range(y_val - (z * tile_offset), 0 - (z * tile_offset) - int(tile_xy/2), -tile_xy)):
          slices[dim_utils.dim_idx('X', ignore_channel = True)] = slice(x - tile_xy, x, 1)
          slices[dim_utils.dim_idx('Y', ignore_channel = True)] = slice(y - tile_xy, y, 1)
          
          tile_list[-1][tuple(slices)] = np.average(cur_im[tuple(slices)])
    
    im_avg_list.append(
      np.expand_dims(np.average(np.stack(tile_list), axis = 0),
      axis = dim_utils.dim_idx('C')))
  
  logfile_utils.log('>> save back')
  
  output_dask = da.concatenate([im_dat[0]] + im_avg_list, axis = dim_utils.dim_idx('C'))

  # adjust chunksize and data type
  output_dask = output_dask.rechunk(chunks = im_dat[0].chunksize)
  output_dask = output_dask.astype(im_dat[0].dtype)
  
  # save back
  zarr_utils.create_multiscales(
    output_dask, im_correction_path,
    dim_utils = dim_utils, nscales = len(im_dat))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    im_correction_path, im_path,
    changed_shape = output_dask.shape,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == '__main__':
  main()
