# add CCIA modules
import sys
import os
sys.path.append("./")

import numpy as np
import pandas as pd
import scanpy as sc
import squidpy as sq

from py.label_props_utils import LabelPropsUtils
from py.pop_utils import PopUtils

import py.script_utils as script_utils
import py.config_utils as cfg

# segment image
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # prepare params
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')
  
  # get properties
  value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
  pop_type = script_utils.get_param(params, 'popType', default = None)
  
  pops = script_utils.get_param(params, 'pops', default = list())
  neighbour_method = script_utils.get_param(params, 'neighbourMethod', default = 'grid')
  neighbour_radius = script_utils.get_param(params, 'neighbourRadius', default = '0')
  n_rings = script_utils.get_param(params, 'nRings', default = '1')
  
  logfile_utils.log(f'>> Find neighbours for {pops} via {neighbour_method}')
  
  # get the requested population from here
  pop_utils = PopUtils()
  pop_map = pop_utils.pop_map(task_dir, pop_type, pops = pops)
  pop_data = pop_utils.pop_data(task_dir, pop_type, pops = pops)
  
  # get data for neigbour detection
  if len(pops) > 0:
    adata = LabelPropsUtils(task_dir, value_name = value_name).label_props_view(read_only = False)\
      .filter_by_obs([x for y in [x for x in pop_data.values()] for x in y])\
      .view_label_col()\
      .as_adata()
  else:
    adata = LabelPropsUtils(task_dir, value_name = value_name).label_props_view(read_only = False)\
      .view_label_col()\
      .as_adata()
      
  # add population information
  adata.obs['pop'] = 'NONE'
  
  for i, x in pop_data.items():
    adata.obs.loc[adata.obs['label'].isin(x), 'pop'] = pop_map[i]['name'][0]
      
  # make categorical
  adata.obs['pop'] = adata.obs['pop'].astype('category')
  
  logfile_utils.log(f'>> run neighbour detection')
  
  if neighbour_method == 'delaunay':
    sq.gr.spatial_neighbors(
      adata,
      coord_type = 'generic',
      delaunay = True
    )
    
    # filter on distance
    adata.obsp['spatial_distances'][adata.obsp['spatial_distances'] > neighbour_radius] = 0
    
    # apply to connectivities
    adata.obsp['spatial_connectivities'][adata.obsp['spatial_connectivities'].nonzero()] = 0
    adata.obsp['spatial_connectivities'][adata.obsp['spatial_distances'].nonzero()] = 1
  elif neighbour_method == 'grid':
    sq.gr.spatial_neighbors(
      adata,
      coord_type = 'grid',
      n_rings = n_rings
    )
  else:
    sq.gr.spatial_neighbors(
      adata,
      coord_type = 'generic',
      radius = neighbour_radius
    )

  logfile_utils.log(f'>> save back')
  
  # save data
  adata.write_h5ad(os.path.join(
    task_dir, 'labelProps', f'{value_name}.sq.h5ad'))
    
  # close
  adata.file.close()
  
def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['pops']
  )
  
  # run
  run(params)

if __name__ == "__main__":
  main()
