# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils

# from cellpose import denoise
# import torch

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
  dn = denoise.DenoiseModel(
    model_type = denoise_model, gpu = use_gpu, device = gpu_device)
  
  # get slices
  # TODO this will process all slices and channels individually
  # slices = dim_utils.create_channel_slices()
  
  # go through slices and copy into array
  output_image = im_dat[0].copy()
  
  # go through parameters
  for i, x in models.items():
    
    # get slices for channels
    slices = list()
    for j in x['modelChannels']:
      slices.append(dim_utils.create_channel_slices(channel = j))
    
    if dim_utils.is_3D():
      slices = dim_utils.expand_slices([list(x) for x in slices], dim = 'Z')
  
    for x in slices:
      output_image[x] = dn.eval([im_dat[0][x]], channels=[0, 0], diameter=diameter)[0][..., 0]

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
