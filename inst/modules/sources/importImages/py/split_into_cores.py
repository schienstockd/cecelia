# add CCIA modules
import sys
sys.path.append("./")

import tifffile
import numpy as np
import os
import re
import subprocess
from pathlib import Path

import dask.array as da
import tifffile

from skimage.measure import regionprops

import py.zarr_utils as zarr_utils
import py.tiff_utils as tiff_utils
import py.opal_utils as opal_utils

# config
import py.config_utils as cfg

import py.script_utils as script_utils

def run(params):
  # get filepaths
  ori_path = script_utils.get_param(params, 'oriPath')
  normalise_image = script_utils.get_param(params, 'normaliseImage', default = False)
  median_filter = script_utils.get_param(params, 'medianFilter', default = 5)
  otsu_adjust = script_utils.get_param(params, 'otsuAdjust', default = 1)
  closing_filter = script_utils.get_param(params, 'closingFilter', default = 5)
  small_objects_size = script_utils.get_param(params, 'smallObjectsSize', default = 1000)
  label_expansion = script_utils.get_param(params, 'labelExpansion', default = 5)
  
  # load the image
  im_dat, _ = zarr_utils.open_image_as_zarr(ori_path, as_dask = True)
  
  im_min, im_max = zarr_utils.get_minmax_from_low_res(im_dat)
  
  # get labels for cores
  label_im, _ = opal_utils.extract_cores_labels(
    im_dat,
    im_min = im_min,
    im_max = im_max,
    median_filter = median_filter,
    otsu_adjust = otsu_adjust,
    closing_filter = closing_filter,
    small_objects_size = small_objects_size,
    label_expansion = label_expansion
    )
  
  # get pixel resolution for new images
  showinf_path = os.path.join(cfg.data['dirs']['bftools'], 'showinf')
  
  im_info = subprocess.check_output([showinf_path, '-nopix', ori_path])
  im_info = im_info.decode().split('\n')
  
  # get resolution
  im_res_size = {
      re.findall('^[X|Y]', x)[0].lower(): float(re.findall('\d+\.\d+', x)[0]) \
      for x in im_info if re.match('^[X|Y]Resolution', x)
  }
  
  im_res_unit = {
      re.findall('^ResolutionUnit', x)[0]: re.findall('(?<=ResolutionUnit: ).*', x)[0] \
      for x in im_info if re.match('^ResolutionUnit', x)
  }
  
  im_res_unit = {i: im_res_unit['ResolutionUnit'] for i in im_res_size.keys()}
  
  # for building path names for for cores
  ori_path_info = Path(ori_path)
  
  # get core props
  props = regionprops(label_im)
  
  # go through bounding boxes and save images
  for i, x in enumerate(props):
    cbbox = [y * (2**(len(im_dat) - 1)) for y in x.bbox]
    
    # TODO this assumes XY non 3D image
    im = im_dat[0][:, cbbox[0]:cbbox[2], cbbox[1]:cbbox[3]]
    
    # normalise
    if normalise_image is True:
      im_new = im.map_blocks(zarr_utils.apply_min, im_min = im_min)
      im_new = (((im_new - im_min) / (im_max - im_min)) * 2**16)
      im = zarr_utils.fortify(im_new.astype(np.uint16))
    else:
      im = zarr_utils.fortify(im_new)
    
    # save back
    tiff_utils.save_as_tiff(
      im_path = os.path.join(ori_path_info.parent, f'{ori_path_info.stem}_core{i:03d}{ori_path_info.suffix}'),
      im_data = im,
      physical_sizes = im_res_size,
      physical_units = im_res_unit,
      convert_sizes = False
    )

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == "__main__":
  main()
