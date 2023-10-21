# add CCIA modules
import sys
import os
sys.path.append("./")

import zarr
import math
import numpy as np

import py.zarr_utils as zarr_utils
import py.correction_utils as correction_utils
from py.dim_utils import DimUtils
import py.ome_xml_utils as ome_xml_utils

import py.script_utils as script_utils

# generate training image
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  task_dir = script_utils.get_param(params, 'taskDir')
  zero_root_dir = script_utils.get_param(params, 'zeroRootDir')
  im_source_name = script_utils.get_param(params, 'imSourceName')
  im_path = script_utils.get_param(params, 'imPath')
  uids = script_utils.get_param(params, 'uIDs', default = [])
  crop = script_utils.get_param(params, 'crop', default = 0)
  num_cols = script_utils.get_param(params, 'numCols', default = 4)
  normalise_image = script_utils.get_param(params, 'normaliseImage', default = True)
  norm_percentile = script_utils.get_param(params, 'normPercentile', default = 99.98)
  
  logfile_utils.log(f'>> Normalise images at {norm_percentile} ({normalise_image})')
  
  # get image arrays
  # im_paths = [os.path.join(zero_root_dir, x, 'ccidImage.ome.zarr') for x in uids]
  im_paths = [os.path.join(zero_root_dir, x, im_source_name) for x in uids]
  im_arrays = [zarr_utils.open_as_zarr(
    x, as_dask = True
    )[0][0] for x in im_paths]
    
  # get dim utils for images
  im_dim_utils = list()

  for i, x in enumerate(uids):
    # get OME-XML
    omexml = ome_xml_utils.parse_meta(im_paths[i])

    # set dimensions
    im_dim_utils.append(DimUtils(omexml, True))
    im_dim_utils[-1].calc_image_dimensions(im_arrays[i].shape)
    
  # create a new zarr image and fill in the loaded images
  num_rows = math.ceil(len(im_arrays)/num_cols)
  
  # get min shape for images to crop them
  # TODO this will only work for 2D for now
  min_x = min([x.dim_val('X') for x in im_dim_utils])
  min_y = min([x.dim_val('Y') for x in im_dim_utils])
  min_shape = min([min_x, min_y])
  
  logfile_utils.log(f'>> Shape {min_shape} with crop {crop}')
  
  # set minimum of crop or shape
  crop = crop if crop > 0 and crop <= min_shape else min_shape
  
  logfile_utils.log(f'>> Use {crop}')

  # create combined image
  # go through images and make random squares
  im_combined = zarr.create(
    mode = 'w',
    shape = tuple(
        [im_dim_utils[0].dim_val('C')] + [
            num_rows * crop, num_cols * crop
        ]
    ),
    chunks = tuple((1, crop, crop)),
    dtype = im_arrays[0].dtype)
    
  # copy in images
  for i, x in enumerate(im_arrays):
    max_x = im_dim_utils[i].dim_val('X')
    max_y = im_dim_utils[i].dim_val('Y')
    
    # get x and y coords
    x_slice = slice(
      math.floor((max_x - crop)/2),
      max_x - math.ceil((max_x - crop)/2),
      1
    )
    
    y_slice = slice(
      math.floor((max_y - crop)/2),
      max_y - math.ceil((max_y - crop)/2),
      1
    )
    
    # get slices
    slices = [slice(None) for _ in range(len(x.shape))]
    slices[im_dim_utils[i].dim_idx('X')] = x_slice
    slices[im_dim_utils[i].dim_idx('Y')] = y_slice
    
    # get row and col number
    row_i = math.floor(i/num_cols)
    col_i = i % num_cols
    
    combined_slices = [
      slice(None),
      slice(row_i * crop, (row_i + 1) * crop, 1),
      slice(col_i * crop, (col_i + 1) * crop, 1)
    ]
    
    # normalise image for each channel
    im_dtype = x.dtype
    im = zarr_utils.fortify(x[tuple(slices)]).astype(np.uint32)
    
    if normalise_image is True:
      im = correction_utils.normalise_channels(
        im,
        dim_utils = im_dim_utils[i],
        norm_percentile = norm_percentile,
        dtype = x.dtype
      )
    
    # copy slices
    im_combined[tuple(combined_slices)] = np.squeeze(im.astype(im_dtype))
  
  # save as multiscales back
  zarr_utils.create_multiscales(
    im_combined, im_path,
    x_idx = 2, y_idx = 1
    )

  # add metadata with changed shape
  changed_shape = {
    'C': im_combined.shape[0],
    'Y': im_combined.shape[1],
    'X': im_combined.shape[2]
  }
  
  ome_xml_utils.save_meta_in_zarr(
    im_path, im_paths[0],
    new_shape = changed_shape
  )

def main():
  # get params
  params = script_utils.script_params(
  	flatten_except = ['uIDs']
  )

  # run main function
  run(params)

if __name__ == '__main__':
  main()
