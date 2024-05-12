# add CCIA modules
import sys
sys.path.append('./')

import os
import numpy as np
import glob
import re

import py.script_utils as script_utils

import tifffile
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
from ome_types import model, from_xml

# segment image
def run(params):
  # get params
  im_path = script_utils.get_param(params, 'imPath')
  zarr_path = script_utils.get_param(params, 'zarrPath')
  seq_rexp = script_utils.get_param(params, 'seqREXP', default = None)
  is_stacked = script_utils.get_param(params, 'isStacked', default = False)
  stack_im = script_utils.get_param(params, 'stackImage', default = False)
  skip_tiles = script_utils.get_param(params, 'skipTiles', default = 0)
  nscales = script_utils.get_param(params, 'nscales', default = 1)
  physical_stack_scale = script_utils.get_param(params, 'physicalStackScale', default = 1)
  stack_dim = script_utils.get_param(params, 'stackDim', default = 'Z')
  
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # read in OME XML from first image sequence
  logfile_utils.log(f'>> open image {im_path}')
  
  # or - read by regular expression
  # if seq_rexp is not None:
  if seq_rexp is not None and seq_rexp.strip() != "":
    # >>> image_sequence = TiffSequence(
    # ...     'temp_C0*.tif', pattern=r'_(C)(\d+)(T)(\d+)'
    # ... )
    # https://stackoverflow.com/a/56985941
    im = tifffile.TiffSequence(
      os.path.join(os.path.dirname(im_path), '*.tif*'), pattern = r'%s' %seq_rexp)
    
    logfile_utils.log(f'> sequence shape {im.shape}')
  if stack_im is True:
    # go through images
    im = list()
    im_list = glob.glob(os.path.join(os.path.dirname(im_path), '*.tif*'))
    
    for i, x in enumerate(im_list):
      im.append(tifffile.TiffFile(x).asarray())
      
    im = np.stack(im, axis=0)
    
    # TODO this is only needed for one use-case and should be deleted afterwards
    logfile_utils.log(f'> channels')
    logfile_utils.log("\n".join([re.search('(?<=ch[0-9]{3}_).*(?=\\.tif)', x, re.IGNORECASE).group() for x in im_list]))
  else:  
    im = tifffile.TiffFile(im_path)
  
  if is_stacked is True:
    logfile_utils.log(f'>> stack images with {skip_tiles}')
    
    # TODO this assumes that the image can fit into memory
    x_array = im.asarray()
    
    omexml = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml)
    dim_utils.calc_image_dimensions(x_array.shape)
    
    # split y axis
    # TODO this is specific to Thorlabs 3P output
    split_size = int(dim_utils.dim_val('Y')/dim_utils.dim_val('X'))
    
    # split array along Y
    x_split = np.array_split(x_array, split_size, axis = dim_utils.dim_idx('Y'))
    
    # get or create stack axis
    stack_dim_idx = dim_utils.dim_idx(stack_dim)
    stack_array = False
    
    # how to handle new dimension
    if stack_dim == 'C':
      # concat arrays
      x_new = np.concatenate(x_split, axis = stack_dim_idx)
    else:
      # check whether to create this dimension
      if stack_dim_idx is None:
        stack_dim_idx = dim_utils.default_order.index(stack_dim)
        stack_array = True
    
      # concat or stack
      if stack_array is True:
        x_new = np.stack(x_split[::(skip_tiles + 1)], axis = stack_dim_idx)
      else:
        # TODO need to test this
        x_new = np.concatenate(x_split[::(skip_tiles + 1)], axis = stack_dim_idx)
    
    logfile_utils.log(f'>> Save back {x_new.shape}')
    
    # generate chunks
    # TODO this is specific to Thorlabs
    im_chunks = [1] * len(x_new.shape)
    im_chunks[dim_utils.dim_idx('X')] = x_new.shape[dim_utils.dim_idx('X')]
    im_chunks[dim_utils.dim_idx('Y')] = x_new.shape[dim_utils.dim_idx('Y')]
    
    # save back
    zarr_utils.create_multiscales(x_new, zarr_path, nscales = nscales, im_chunks = im_chunks)
    
    # create shape dict for new image
    shape_dict = {'Y': int(dim_utils.dim_val('Y')/split_size)}
    shape_dict[stack_dim] = x_new.shape[stack_dim_idx]
    
    # change image dimensions in xml
    omexml_new = ome_xml_utils.set_im_size_with_dict(omexml, shape_dict)
    
    if not stack_dim in ('C', 'T'):
      scale_dict = dict()
      scale_dict[stack_dim] = physical_stack_scale
      
      omexml_new = ome_xml_utils.set_physical_size_with_dict(omexml_new, scale_dict)
      
    # add metadata
    ome_xml_utils.write_ome_xml(zarr_path, omexml_new)
  else:
    # save back
    logfile_utils.log(f'>> save as zarr {zarr_path}')
    
    if hasattr(im, 'asarray'):
      im = im.asarray()
      
    # generate chunks
    # TODO this is guessing
    im_chunks = [1] * len(im.shape)
    im_chunks[-1] = min(im.shape[-1], 1024)
    im_chunks[-2] = min(im.shape[-2], 1024)
    x_idx = len(im.shape) - 1
    y_idx = x_idx - 1
    
    zarr_utils.create_multiscales(
      im, zarr_path, nscales = nscales, im_chunks = im_chunks,
      x_idx = x_idx, y_idx = y_idx)
    
    # add metadata
    if hasattr(im, 'ome_metadata'):
      im_metadata = im.ome_metadata
    elif os.path.isfile(os.path.join(os.path.dirname(im_path), 'ome.xml')):
      im_metadata = from_xml(os.path.join(os.path.dirname(im_path), 'ome.xml'))
    else:
      len_dims = len(im.dims)
      len_shape = len(im.shape) - len_dims
      
      # create metadata
      # TODO anything else?
      pixels = model.pixels.Pixels(
        size_c = im.dims.index('c') if 'c' in im.dims and im.dims.index('c') else 0,
        size_x = im.shape[-1],
        size_y = im.shape[-2],
        size_t = im.dims.index('t') if 't' in im.dims and im.dims.index('t') else 0,
        size_z = im.shape[0] if len_shape > 2 else 0,
        physical_size_x = 1,
        physical_size_y = 1,
        physical_size_x_unit = model.UnitsLength.MICROMETER,
        physical_size_y_unit = model.UnitsLength.MICROMETER,
        type = 'uint16',
        dimension_order = model.Pixels_DimensionOrder.XYZCT,
        metadata_only = True
        )
      
      for x in channel_names:
        pixels.channels.append(model.channel.Channel(name = x))
        
      # add metadata
      im_metadata = model.OME(images=[model.Image(pixels=pixels)])
        
    ome_xml_utils.write_ome_xml(zarr_path, im_metadata)

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == '__main__':
  main()
