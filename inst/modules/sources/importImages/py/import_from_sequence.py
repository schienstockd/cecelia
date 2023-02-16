# add CCIA modules
import sys
sys.path.append("./")

import numpy as np

import py.script_utils as script_utils

import tifffile
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils

# segment image
def run(params):
  # get params
  im_path = script_utils.get_param(params, 'imPath')
  zarr_path = script_utils.get_param(params, 'zarrPath')
  is_stacked = script_utils.get_param(params, 'isStacked', default = False)
  skip_tiles = script_utils.get_param(params, 'skipTiles', default = 0)
  nscales = script_utils.get_param(params, 'nscales', default = 1)
  physical_z_scale = script_utils.get_param(params, 'physicalZScale', default = 1)
  
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # read in OME XML from first image sequence
  logfile_utils.log(f'>> open image {im_path}')
  im = tifffile.TiffFile(im_path)
  
  if is_stacked is True:
    logfile_utils.log(f'>> stack images')
    
    # TODO this assumes that the image can fit into memory
    x = im.asarray()
    
    omexml = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml)
    dim_utils.calc_image_dimensions(x.shape)
    
    # split y axis
    # TODO this is specific to Thorlabs 3P output
    split_size = int(dim_utils.dim_val('Y')/dim_utils.dim_val('X'))
    
    # split array along Y
    x_split = np.array_split(x, split_size, axis = dim_utils.dim_idx('Y'))
    
    # get or create z axis
    z_dim = dim_utils.dim_idx('Z')
    stack_array = False
    
    if z_dim is None:
        z_dim = dim_utils.default_order.index('Z')
        stack_array = True
        
    # concat or stack
    if stack_array is True:
        x_new = np.stack(x_split[::(skip_tiles + 1)], axis = z_dim)
    else:
        # TODO need to test this
        x_new = np.concatenate(x_split[::(skip_tiles + 1)], axis = z_dim)
        
    # save back
    zarr_utils.create_multiscales(x_new, zarr_path, nscales = nscales)
    
    # change image dimensions in xml
    omexml_new = ome_xml_utils.set_im_size_with_dict(
      omexml, {
        'Z': x_new.shape[z_dim],
        'Y': int(dim_utils.dim_val('Y')/split_size)
        })
    omexml_new = ome_xml_utils.set_physical_size_with_dict(
      omexml_new, {'Z': physical_z_scale})
      
    # add metadata
    ome_xml_utils.write_ome_xml(zarr_path, omexml_new)
  else:
    # save back
    logfile_utils.log(f'>> save as zarr {zarr_path}')
    zarr_utils.create_multiscales(im.asarray(), zarr_path, nscales = nscales)
    
    # add metadata
    ome_xml_utils.write_ome_xml(zarr_path, im.ome_metadata)

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == "__main__":
  main()
