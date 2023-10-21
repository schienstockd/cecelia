# add CCIA modules
import sys
import os
sys.path.append("./")

import numpy as np
import pandas as pd
import anndata as ad
import math
import zarr
import bioformats
import shutil

from scipy.interpolate import griddata

import py.ome_xml_utils as ome_xml_utils
import py.script_utils as script_utils
import py.zarr_utils as zarr_utils
import py.config_utils as cfg

from py.label_props_utils import LabelPropsUtils

# segment image
def run(params):
  task_dir = script_utils.get_param(params, 'taskDir')
  value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
  
  adata_filepath = script_utils.get_param(params, 'adataFilepath')
  zarr_filepath = script_utils.get_param(params, 'zarrFilepath')
  nscales = script_utils.get_param(params, 'pyramidScale')
  
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  logfile_utils.log(f'>> open dataset {adata_filepath}')
  
  # read anndata
  adata = ad.read_h5ad(adata_filepath)
  
  # build up image dimensions
  x_min, y_min = adata.obsm['spatial'].min(axis = 0)
  x_max, y_max = adata.obsm['spatial'].max(axis = 0)
  
  im_dim = [
      math.floor(x_min), math.floor(y_min),
      math.ceil(x_max), math.ceil(y_max)
  ]
  
  num_channels = len(adata.obsm['q05_cell_abundance_w_sf'].columns)
  
  # create zarr
  zarr_shape = (
    num_channels,
    im_dim[2] - im_dim[0],
    im_dim[3] - im_dim[1]
  )
  
  logfile_utils.log(f'>> create zarr {zarr_shape}')
  
  seq_image = zarr.open(
    zarr_filepath,
    mode = 'w',
    shape = zarr_shape,
    chunks = (1, 512, 512),
    dtype = np.float16
  )

  # go through columns and interpolate
  points = adata.obsm['spatial'] - im_dim[0:2]
  grid_x, grid_y = np.mgrid[0:zarr_shape[1], 0:zarr_shape[2]]

  logfile_utils.log(f'>> prepare grid')

  for i, x in enumerate(adata.obsm['q05_cell_abundance_w_sf'].columns):
    logfile_utils.log(f'> {x}')

    seq_image[i, :, :] = griddata(
      points,
      adata.obsm['q05_cell_abundance_w_sf'][x].values,
      (grid_x, grid_y),
      method = 'nearest'
      )
      
  logfile_utils.log(f'>> save back multiscales')
      
  # generate multiscales 
  # TODO is there a more elegant way to do this .. ?
  if nscales > 1:
    multiscales_file_path = zarr_filepath + ".multiscales"
    
    zarr_utils.create_multiscales(
      seq_image, multiscales_file_path,
      x_idx = 1, y_idx = 2,
      nscales = nscales
    )
    
    # remove previous and rename multiscales
    shutil.rmtree(zarr_filepath)
    os.rename(multiscales_file_path, zarr_filepath)
    
  # build metadata
  o = bioformats.omexml.OMEXML()
  
  # create channel name
  channel_names = [x.replace('q05cell_abundance_w_sf_', '')\
    for x in list(adata.obsm['q05_cell_abundance_w_sf'].columns)]
    
  # TODO anything else?
  o.image().Pixels.channel_count = zarr_shape[0]
  o.image().Pixels.set_SizeC(zarr_shape[0])
  o.image().Pixels.set_SizeX(zarr_shape[2])
  o.image().Pixels.set_SizeY(zarr_shape[1])
  o.image().Pixels.set_PhysicalSizeX(1)
  o.image().Pixels.set_PhysicalSizeY(1)
  o.image().Pixels.set_PhysicalSizeXUnit('um')
  o.image().Pixels.set_PhysicalSizeYUnit('um')
  o.image().Pixels.set_PixelType('uint16')

  for i, x in enumerate(channel_names):
    o.image().Pixels.Channel(i).Name = x
    
  # add metadata
  ome_xml_utils.write_ome_xml(zarr_filepath, o)
  
  logfile_utils.log(f'>> save label properties')
  
  # build label properties from individual spots
  obsm = dict()
  uns = dict()
  
  uns['spatial_cols'] = ['centroid_y', 'centroid_x']
  obsm['spatial'] = adata.obsm['spatial']
  
  # adjust spatial
  obsm['spatial'] = obsm['spatial'] - [x_min, y_min]
  
  # add running label
  x_vars = adata.obsm['q05_cell_abundance_w_sf']
  
  # rename columns
  x_vars.columns = [f'mean_intensity_{i}' for i in range(0, len(x_vars.columns))]
  
  # add label
  x_vars['label'] = list(range(0, len(x_vars)))
  
  # save props
  LabelPropsUtils(task_dir, cfg.value_dir(value_name, 'labelProps'))\
    .label_props(
      x_vars,
      save = True,
      obsm = obsm,
      uns = uns
      )

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == '__main__':
  main()
