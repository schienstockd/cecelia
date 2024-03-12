# add CCIA modules
import sys
sys.path.append('./')

import os
import numpy as np

import py.script_utils as script_utils

import tifffile
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils

# segment image
def run(params):
  # get params
  ref_path = script_utils.get_param(params, 'refPath')
  im_path_in = script_utils.get_param(params, 'imPathIn')
  im_path_out = script_utils.get_param(params, 'imPathOut')
  
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # load image
  logfile_utils.log(f'>> open reference image {ref_path}')
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(ref_path, as_dask = True)
  
  # get OME-XML
  omexml = ome_xml_utils.parse_meta(ref_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)
  
  # read in OME XML from first image sequence
  logfile_utils.log(f'>> open image {im_path_in}')
  
  im = tifffile.TiffFile(im_path_in).asarray()
  
  # TODO generate image chunks - this is not great
  # match image dimensions
  # get all dimensions that are not zero and then take the chunks
  # if that does not fit - then the image might be a timecourse
  # and the segmentation covers all timepoints
  zarr_chunks = zarr_utils.chunks(im_dat[0])
  im_shape = list(im_dat[0].shape)
  
  if dim_utils.is_timeseries():
    zarr_chunks.pop(dim_utils.dim_idx('T'))
    im_shape.pop(dim_utils.dim_idx('T'))
    
  nonzero_idx = [i for i, e in enumerate(im_shape) if e > 1]
  im_chunks = [zarr_chunks[i] for i in nonzero_idx]
    
  # check that this matches the size of the image
  # if len(im.shape) < len(zarr_chunks):
  #   # im_chunks.pop(0)
  #   im = np.stack([im for _ in range(im_dat[0].shape[nonzero_idx[0]])], axis=0)
    
  # TODO this is very manual now
  while len(im.shape) < len(zarr_chunks) - 1:
    im = np.expand_dims(im, axis = 0)
    im_chunks.insert(0, 1)
  
  logfile_utils.log(im.shape)
  logfile_utils.log(im_chunks)
  logfile_utils.log(zarr_chunks)
  
  # save back
  logfile_utils.log(f'>> save as zarr {im_path_out}')
  zarr_utils.create_multiscales(
    im,
    im_path_out,
    dim_utils = dim_utils,
    nscales = len(im_dat),
    # reference_zarr = im_dat[0],
    x_idx = len(im.shape) - 1,
    y_idx = len(im.shape) - 2,
    im_chunks = im_chunks,
    keyword = 'labels'
    # ignore_channel = True
    )
  
def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == '__main__':
  main()
