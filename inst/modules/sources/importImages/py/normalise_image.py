# add CCIA modules
import sys
sys.path.append("./")

import tifffile
import numpy as np
import os
from pathlib import Path

import py.zarr_utils as zarr_utils
import py.tiff_utils as tiff_utils
import py.script_utils as script_utils

def run(params):
  # get filepaths
  im_path_in = script_utils.get_param(params, 'imPathIn')
  im_path_out = script_utils.get_param(params, 'imPathOut')
  
  # the following will create a new image and remove the old one
  # load the image
  im_dat, _ = zarr_utils.open_image_as_zarr(im_path_in, as_dask = True)

  # # get min and max from lowest dimension
  # low_res = zarr_utils.fortify(im_dat[len(im_dat) - 1])
  # 
  # # get min and max
  # im_min = low_res[low_res > 0].min()
  # im_max = low_res.max()
  # del(low_res)
  
  # TODO I have to fortify for indexing?
  im_out = zarr_utils.fortify(im_dat[0])
  
  # get min and max
  im_min = im_out[im_out > 0].min()
  im_max = im_out.max()
  
  # add minimum to image
  im_out[im_out == 0] += im_min

  # write back as tif
  tifffile.imwrite(
    im_path_out, 
    (((im_out - im_min) / (im_max - im_min)) * 2**16).astype(np.uint16)
    # imagej = True
  )
 
  # add metadata from original
  tifffile.tiffcomment(
      im_path_out,
      tifffile.tiffcomment(im_path_in)
  )

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == '__main__':
  main()
