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

# correct with Cellpose
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  im_path = script_utils.get_param(params, 'imPath', default = None)
  cp_params = script_utils.get_param(params, 'cpParams', default = dict())
  denoise_model = script_utils.get_param(params, 'denoiseModel', default = None)
  use_gpu = script_utils.get_param(params, 'useGPU', default = False)

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
  logfile_utils.log(cp_params)
  
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
  slices = dim_utils.create_channel_slices()
  
  if dim_utils.is_3D():
    slices = dim_utils.expand_slices(list(slices), dim = 'Z')
  
  # create new image sink
  # output_image = list()
  output_image = [input_image[x] for x in slices]

  # denoise
  for i, x in enumerate(output_image):
    if filter_fun == 'gaussian':
      # apply gaussian
      output_image[i] = dask_image.ndfilters.gaussian_filter(
        x, sigma = filter_values
      )

  # combine dask arrays back
  # https://docs.dask.org/en/latest/array-stack.html
  output_dask = da.concatenate(output_image, axis = dim_utils.dim_idx('C'))

  # adjust chunksize and data type
  output_dask = output_dask.rechunk(chunks = input_image.chunksize)
  output_dask = output_dask.astype(input_image.dtype)
  
  # go through slices
  for cur_slice in slices:
    logfile_utils.log(f'> slice {cur_slice}')
    
    slices = [slice(None) for _ in range(len(im.shape))]
    im_list = list()
    
    for i in range(im.shape[z_axis]):
      slices[0] = slice(i, i + 1, 1)
  
      im_list.append(np.squeeze(dn.eval(
        [im[tuple(slices)]], channels = channels, diameter = cell_diameter)[0]))
    
    # compile back
    im = np.stack(im_list, axis = z_axis)
  else:
    logfile_utils.log(f'> Denoise 2D {denoise_name}')
    im = dn.eval([im], channels = channels, diameter = cell_diameter)[0]

  logfile_utils.log('>> save back')
  
  # save back
  zarr_utils.create_multiscales(
    corrected_image, params['imCorrectionPath'],
    dim_utils = dim_utils, nscales = len(im_dat))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    params['imCorrectionPath'], params['imPath'],
    changed_shape = corrected_image.shape,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params(
  	flatten_dict_except = {
  	  'afCombinations': ['divisionChannels']
  	  }
  )

  # run
  run(params)

if __name__ == '__main__':
  main()
