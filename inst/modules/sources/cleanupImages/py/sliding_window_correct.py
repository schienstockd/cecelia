# add CCIA modules
import sys
import os
sys.path.append("./")

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
import py.script_utils as script_utils

import zarr
import numpy as np
from tqdm import tqdm

# correct AF and drift correct
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  im_path = script_utils.get_param(params, 'imPath', default = None)
  im_correction_path = script_utils.get_param(params, 'imCorrectionPath', default = None)
  sliding_window = script_utils.get_param(params, 'slidingWindow', default = 1)

  # load image
  # im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = True)
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = False)

  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)

  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)

  logfile_utils.log('>> correct image by sliding window')
  
  # remove previous folder
  # if im_correction_path is not None:
  #   shutil.rmtree(im_correction_path)
  
  # prepare image
  sum_zarr = zarr.create(
    im_dat[0].shape,
    dtype = im_dat[0].dtype,
    # chunks = im_dat[0].chunksize
    chunks = im_dat[0].chunks
    # store = im_correction_path
  )
  
  # go through timepoints and channels
  for i in tqdm(range(dim_utils.dim_val('T'))):
    for j in range(dim_utils.dim_val('C')):
      # build average image over time for warping 
      w_start = i - sliding_window
      w_end = i + sliding_window
  
      if w_start < 0:
        w_start = 0
        #w_end = sliding_window * 2
  
      if w_start >= dim_utils.dim_val('T') - (sliding_window * 2):
        #w_start = dim_utils.dim_val('T') - (sliding_window * 2)
        w_end = dim_utils.dim_val('T')
  
      # set slices
      im_slices = [slice(None) for _ in range(len(im_dat[0].shape))]
      im_slices[dim_utils.dim_idx('T')] = slice(w_start, w_end, 1)
      im_slices[dim_utils.dim_idx('C')] = j
      im_slices = tuple(im_slices)
  
      sum_slices = [slice(None) for _ in range(len(sum_zarr.shape))]
      sum_slices[dim_utils.dim_idx('T')] = i
      sum_slices[dim_utils.dim_idx('C')] = j
      sum_slices = tuple(sum_slices)
  
      # TODO this is very slow
      sum_zarr[sum_slices] = np.squeeze(np.median(
        im_dat[0][im_slices],
        axis = dim_utils.dim_idx('T', ignore_channel = True),
        keepdims = True))

  logfile_utils.log('>> save back')
  
  # save back
  zarr_utils.create_multiscales(
    sum_zarr, im_correction_path,
    dim_utils = dim_utils, nscales = len(im_dat))

  # add metadata
  ome_xml_utils.save_meta_in_zarr(
    im_correction_path, im_path,
    dim_utils = dim_utils
  )

def main():
  # get params
  params = script_utils.script_params()

  # run AF and drift correction
  run(params)

if __name__ == "__main__":
  main()
