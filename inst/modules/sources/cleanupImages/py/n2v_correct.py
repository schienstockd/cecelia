# add CCIA modules
import sys
import os
sys.path.append('./')
import numpy as np

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils

from n2v.models import N2V

# correct AF and drift correct
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  im_path = script_utils.get_param(params, 'imPath', default = None)
  im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default = None)
  model_dir = script_utils.get_param(params, 'modelDir', default = None)
  model_mapping = script_utils.get_param(params, 'modelMapping', default = {})

  # load image
  # im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = True)
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  logfile_utils.log('> run correction')
  
  # get numpy array
  im = np.squeeze(np.array(zarr_utils.fortify(im_dat[0])))
  max_val = np.iinfo(im.dtype).max
  
  # build slices
  t_val = dim_utils.dim_val('T')
  c_val = dim_utils.dim_val('C')
  t_idx = dim_utils.dim_idx('T', squeeze = True)
  c_idx = dim_utils.dim_idx('C', squeeze = True)
  is_3D = dim_utils.is_3D()
  slices = [slice(None) for _ in range(len(im.shape))]
  
  # TODO you have to create a new prediction image with time and float32
  # otherwise the float32 result will be pushed into uint16
  # pred = np.copy(im)
  
  # go through model mapping
  for i, x in model_mapping.items():
    logfile_utils.log(f'> load model {i} from {model_dir}')
    model = N2V(config = None, name = i, basedir = model_dir)
    
    for j in x['modelChannels']:
      # set slices
      # TODO not sure that catches single channel images
      if c_val > 1:
        slices[c_idx] = j
      
      if dim_utils.is_timeseries():
        # save predictions in list
        pred = list()
        
        # go through timepoints
        for t in range(t_val):
          slices[t_idx] = t
          
          if is_3D:
            pred.append(model.predict(im[tuple(slices)], axes = 'ZYX', n_tiles = (2, 4, 4)))
          else:
            pred.append(model.predict(im[tuple(slices)], axes = 'YX', n_tiles = (4, 4)))
            
        # concat
        pred = np.stack(pred, axis = 0)
      else:
        if is_3D:
          pred = model.predict(im[tuple(slices)], axes = 'ZYX', n_tiles = (2, 4, 4))
        else:
          pred = model.predict(im[tuple(slices)], axes = 'YX', n_tiles = (4, 4))
          
      # pred_max = pred.max()
      # pred_min = pred.min()
      pred_min = np.percentile(pred, 100 - x['normPerc'][0])
      pred_max = np.percentile(pred, x['normPerc'][0])
      
      # TODO is there a better way to do this?
      if t_idx is not None:
        slices[t_idx] = slice(None)
      
      pred = (pred - pred_min) / (pred_max - pred_min)
      pred[pred > 1] = 1
      pred[pred < 0] = 0
      
      # push normalised back
      im[tuple(slices)] = pred * (max_val- 1)
  
  logfile_utils.log('>> save back')
  
  # generate chunks
  im_chunks = [1] * len(im.shape)
  x_idx = dim_utils.dim_idx('X', squeeze = True)
  y_idx = dim_utils.dim_idx('Y', squeeze = True)
  
  x_chunks = im.shape[x_idx]
  y_chunks = im.shape[y_idx]
  
  max_chunk = 512
  im_chunks[x_idx] = x_chunks if x_chunks < max_chunk else max_chunk
  im_chunks[y_idx] = y_chunks if y_chunks < max_chunk else max_chunk
  
  logfile_utils.log((x_chunks,y_chunks))
  logfile_utils.log(im.shape)
  logfile_utils.log(im.dtype)
  logfile_utils.log(im_chunks)
  
  # save back
  zarr_utils.create_multiscales(
    im, im_correction_path, im_chunks = im_chunks,
    dim_utils = None, nscales = len(im_dat),
    x_idx = x_idx, y_idx = y_idx)

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    im_correction_path, im_path,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params(flatten_except = ['modelMapping'])

  # run AF and drift correction
  run(params)

if __name__ == '__main__':
  main()
