# add CCIA modules
import sys
import os
sys.path.append("./")

import numpy as np
import pandas as pd
import math
import zarr
import bioformats
import shutil
import random

from scipy.sparse import coo_array
import skimage.filters
import skimage.morphology

import py.ome_xml_utils as ome_xml_utils
import py.script_utils as script_utils
import py.zarr_utils as zarr_utils
import py.config_utils as cfg

from py.dim_utils import DimUtils

# segment image
def run(params):
  task_dir = script_utils.get_param(params, 'taskDir')
  value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
  
  im_path_in = script_utils.get_param(params, 'imPathIn')
  ts_zarr_path = script_utils.get_param(params, 'imPathOut')
  # nscales = script_utils.get_param(params, 'pyramidScale')
  sum_value = script_utils.get_param(params, 'sumValue')
  filter_value = script_utils.get_param(params, 'filterValue')
  min_filter_value = script_utils.get_param(params, 'minFilterValue')
  base_dir = os.path.dirname(im_path_in)
  
  # define filepaths
  transcripts_path = os.path.join(base_dir, 'transcripts.csv.gz')
  # ts_zarr_path = os.path.join(base_dir, 'ts.zarr')
  
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  logfile_utils.log(f'>> open dataset {im_path_in}')
  
  # get data and meta data
  im_data, _ = zarr_utils.open_as_zarr(im_path_in)
  omexml = ome_xml_utils.parse_meta(im_path_in)
  dim_utils = DimUtils(omexml)
  dim_utils.calc_image_dimensions(im_data[0].shape)
  pixel_sizes = dim_utils.im_physical_sizes()
  
  # get scales
  nscales = len(im_data)
  
  logfile_utils.log(f'>> Read transcript data')
  
  # read transcript data
  ts_data = pd.read_csv(
    transcripts_path, compression = 'gzip', error_bad_lines = False)
  max_values = ts_data['qv'].max()
  
  ## convert to zarr image
  # build up image dimensions
  dim_cols = ['y_location', 'x_location']
  x_min, y_min = ts_data[dim_cols].min(axis = 0)
  x_max, y_max = ts_data[dim_cols].max(axis = 0)
  
  im_dim = [
    0, 0,
    dim_utils.im_dim[0],
    dim_utils.im_dim[1]
  ]
  
  channel_names = ts_data['feature_name'].unique()

  # filter blanks
  channel_names = [x for x in channel_names if x.startswith(('BLANK', 'NegControl')) is False]
  
  # DEBUG only use defined ones for now
  # random sample
  # channel_names = random.sample(channel_names, 3)
  
  # channel_names = [
  #     # # B
  #     # 'BANK1', 'CD79A', 'MS4A1',
  #     # # T
  #     # 'CCL5', 'CD4', 'CD8A','CXCR4',
  #     # # 'IL7R', # failed - have to check again
  #     # 'CYTIP', 'LTB', 'TRAC',
  #     # # Mphage
  #     # 'APOC1', 'C15orf48', 'C1QA', 'C1QC', 'CD14',
  #     # 'CD163', 'CD68', 'FGL2', 'ITGAX', 'MMP12',
  #     # # DC
  #     # 'CCR7', 'CD83', 'IL3RA', 'LILRA4', 'PLD4',
  #     # # Stroma
  #     # 'ALDH1 A3', 'GJB2', 'LUM', 'MMP2', 'POSTN', 'SFRP4'
  #     
  #     # FOR BRAIN
  #     # 'Slc17a6', 'Nxph3'
  # ]
  
  num_channels = len(channel_names)
  
  # create zarr
  zarr_shape = (
    # TODO this will not always be the case
    num_channels + 1, # plus DAPI
    im_dim[2] - im_dim[0],
    im_dim[3] - im_dim[1],
    #im_dim[5] - im_dim[2]
  )
    
  seq_image = zarr.open(
    ts_zarr_path,
    mode = 'w',
    shape = zarr_shape,
    # chunks = (1, 512, 512),
    chunks = tuple([1] + list(im_data[0].chunks)),
    # dtype = np.float16
    dtype = np.uint16
  )
  
  # remove previous image
  if os.path.isdir(ts_zarr_path) is True:
    shutil.rmtree(ts_zarr_path)
  
  # copy in DAPI
  logfile_utils.log(f'>> Copy image channels')
  im_data, _ = zarr_utils.open_as_zarr(im_path_in)
  # seq_image[0, :, :] = im_data[0]/np.max(im_data[0])
  seq_image[0, :, :] = im_data[0]
  
  # go through and create images
  for i, x in enumerate(channel_names):
    logfile_utils.log(f'> Process {x}')
    
    y1 = ts_data.loc[ts_data['feature_name'] == x]
    
    # create sparse matrix
    row = y1[dim_cols[0]].values / pixel_sizes['y']
    col = y1[dim_cols[1]].values / pixel_sizes['x']
    
    # TODO rows and cols might be negative
    value_idx = np.intersect1d(np.where(row >= 0), np.where(col >= 0))
    row = row[value_idx]
    col = col[value_idx]
    
    # data = y1['qv'].values / max_values
    # data = np.repeat(1, len(y1.index))
    data = np.repeat(1, len(row))

    y2 = coo_array((data, (row, col)), shape = zarr_shape[1:3]).toarray().astype(np.uint8)

    # TODO use Dask?
    # sum
    seq_image[i + 1, :, :] = skimage.filters.rank.sum(
        y2, skimage.morphology.disk(sum_value))
        
    # enhance donuts
    seq_image[i + 1, :, :] = (skimage.filters.gaussian(seq_image[i + 1, :, :],
      filter_value, preserve_range = True) * (2**8-1)).astype(np.uint16)
    if min_filter_value > 0:
      seq_image[i + 1, :, :] = skimage.filters.rank.minimum(seq_image[i + 1, :, :],
        skimage.morphology.disk(min_filter_value))
    # seq_image[i + 1, :, :] = skimage.filters.median(seq_image[i + 1, :, :],
    #   skimage.morphology.disk(filter_value))
    
  # generate multiscales 
  # TODO is there a more elegant way to do this .. ?
  if nscales > 1:
    multiscales_file_path = ts_zarr_path + ".multiscales"
  
    zarr_utils.create_multiscales(
      seq_image, multiscales_file_path,
      x_idx = 1, y_idx = 2,
      nscales = nscales
    )
    
    # remove previous and rename multiscales
    if os.path.isdir(ts_zarr_path) is True:
      shutil.rmtree(ts_zarr_path)
    os.rename(multiscales_file_path, ts_zarr_path)
  
  # build metadata
  o = bioformats.omexml.OMEXML()
  
  # TODO anything else?
  o.image().Pixels.channel_count = zarr_shape[0]
  o.image().Pixels.set_SizeC(zarr_shape[0])
  o.image().Pixels.set_SizeX(zarr_shape[2])
  o.image().Pixels.set_SizeY(zarr_shape[1])
  o.image().Pixels.set_PhysicalSizeX(pixel_sizes['x'])
  o.image().Pixels.set_PhysicalSizeY(pixel_sizes['y'])
  o.image().Pixels.set_PhysicalSizeXUnit('um')
  o.image().Pixels.set_PhysicalSizeYUnit('um')
  o.image().Pixels.set_PixelType('uint16')
  
  #for i, x in enumerate(channel_names[0:num_channels]):
  for i, x in enumerate(channel_names):
    o.image().Pixels.Channel(i).Name = x
  
  # add metadata
  ome_xml_utils.write_ome_xml(ts_zarr_path, o)

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == '__main__':
  main()
