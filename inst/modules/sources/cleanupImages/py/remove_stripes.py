# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils
import py.zarr_utils as zarr_utils
from copy import copy

import zarr
import numpy as np
from tqdm import tqdm

# correct stripes in image plane
def remove_stripes(im, stripe_perc = 20, axis = 1):
  # get row value
  row_perc = np.percentile(im, stripe_perc, axis = axis)
  
  # correct
  corrected = (im.T - row_perc).T
  corrected[corrected < 0] = 0
  
  return corrected.astype(im.dtype)

# correct AF and drift correct
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  im_path = script_utils.get_param(params, 'imPath', default = None)
  im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default = None)
  stripe_perc = script_utils.get_param(params, 'stripePerc', default = 20)
  im_channels = script_utils.get_param(params, 'imChannels', default = [])
  
  # load image
  # im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = True)
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  logfile_utils.log('>> remove stripes from image')
  
  t_val = dim_utils.dim_val('T')
  c_val = dim_utils.dim_val('C')
  z_val = dim_utils.dim_val('Z')
  
  t_idx = dim_utils.dim_idx('T')
  c_idx = dim_utils.dim_idx('C')
  z_idx = dim_utils.dim_idx('Z')
  
  is_3D = dim_utils.is_3D()
  
  im_to_process = zarr.create(
    im_dat[0].shape,
    dtype = im_dat[0].dtype,
    chunks = im_dat[0].chunks
  )
  
  # copy data from image
  im_to_process[:] = im_dat[0]
  
  slices = [slice(None) for _ in range(len(im_to_process.shape))]
  
  logfile_utils.log('> Go through image')
  
  # go through all channels, timepoints and Z
  for c in im_channels:
    slices[c_idx] = c
    logfile_utils.log(f'> C {c}')
    
    if dim_utils.is_timeseries():
      for t in range(t_val):
        slices[t_idx] = t
        logfile_utils.log(f'> T {t}')
        
        if is_3D:
          for z in range(z_val):
            slices[z_idx] = z
    
            im_to_process[tuple(slices)] = remove_stripes(zarr_utils.fortify(im_to_process[tuple(slices)]))
        else:
          im_to_process[tuple(slices)] = remove_stripes(zarr_utils.fortify(im_to_process[tuple(slices)]))
    else:
        if is_3D:
          for z in range(z_val):
            slices[z_idx] = z
    
            im_to_process[tuple(slices)] = remove_stripes(zarr_utils.fortify(im_to_process[tuple(slices)]))
        else:
          im_to_process[tuple(slices)] = remove_stripes(zarr_utils.fortify(im_to_process[tuple(slices)]))

  logfile_utils.log('>> save back')
  
  # save back
  zarr_utils.create_multiscales(
    im_to_process, im_correction_path,
    dim_utils = dim_utils, nscales = len(im_dat))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    im_correction_path, im_path,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params(flatten_except = ['imChannels'])

  # run AF and drift correction
  run(params)

if __name__ == '__main__':
  main()
