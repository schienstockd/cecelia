# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils

from cellpose import denoise
import torch
import ome_types
import numpy as np
import math

# correct with Cellpose
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  im_path = script_utils.get_param(params, 'imPath', default = None)
  models = script_utils.get_param(params, 'models', default = dict())
  use_gpu = script_utils.get_param(params, 'useGPU', default = False)
  im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default = None)

  # load image
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(
    im_path, as_dask = True)
    # im_path, as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  logfile_utils.log('>> correct image')
  logfile_utils.log(models)
  
  # get GPU
  gpu_device = None
  
  # TODO is there a better way?
  if use_gpu and torch.backends.mps.is_available():
    # self.gpu_device = 'mps'
    gpu_device = torch.device('mps')

  # correct with cellpose
  # get slices
  # TODO this will process all slices and channels individually
  # slices = dim_utils.create_channel_slices()
  
  # go through slices and copy into array
  output_image = im_dat[0].copy()
  
  # adjust diameter for image resolution
  # cell_diameter /= self.dim_utils.omexml.images[0].pixels.physical_size_x
  scaling_factor = dim_utils.im_physical_size('x')
  
  if dim_utils.omexml.images[0].pixels.physical_size_x_unit == ome_types.model.UnitsLength.MILLIMETER:
    scaling_factor *= 1000
    
  # get image rescale factor
  im_rescale_factor = np.iinfo(im_dat[0].dtype).max
  
  # go through parameters
  for i, x in models.items():
    logfile_utils.log(f'> Process {i}')
    logfile_utils.log(x['model'][0])
    
    dn = denoise.DenoiseModel(
      model_type = x['model'][0], gpu = use_gpu, device = gpu_device)
    
    # get slices for channels
    slices = list()
    for j in x['modelChannels']:
      slices.append(dim_utils.create_channel_slices(channel = j))
    
    if dim_utils.is_3D():
      slices = dim_utils.expand_slices([list(y) for y in slices], dim = 'Z')
    
    if dim_utils.is_timeseries():
      slices = dim_utils.expand_slices([list(y) for y in slices], dim = 'T')
    
    # get max for images to rescale
    im_max = 0
    
    for y in slices:
      logfile_utils.log(y)
      
      # TODO this is not ideal
      output_image[y] = (dn.eval(
        [im_dat[0][y]], channels = [0, 0], diameter = x['modelDiameter'][0]/scaling_factor)[0][..., 0]/10) * im_rescale_factor
    
    # TODO you need to somehow scale these after correction?
    
  logfile_utils.log('>> save back')
  
  # save back
  zarr_utils.create_multiscales(
    output_image, im_correction_path,
    dim_utils = dim_utils, nscales = len(im_dat))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    im_correction_path, im_path,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params(
  	flatten_except = ['models']
  )

  # run
  run(params)

if __name__ == '__main__':
  main()
