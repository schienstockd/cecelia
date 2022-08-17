# add CCIA modules
import sys
import os
import numpy as np
sys.path.append("./")

import scanpy as sc

# utils
import py.script_utils as script_utils
from py.label_props_utils import LabelPropsUtils

# cluster cell meshes
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # prepare params
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')

  # get properties
  value_name = script_utils.get_ccia_param(params, 'value_name', default = None)
  init_root = script_utils.get_param(params, 'initRoot', default = '0')
  clusters_col = script_utils.get_param(params, 'clustersCol', default = 'clusters')

  logfile_utils.log(f'> load {value_name}.sc')

  # get data for pseudotime
  label_view = LabelPropsUtils(task_dir, value_name = f'{value_name}.sc').label_props_view(read_only = False)
  adata = label_view.as_adata()
  
  # gte diffmaps
  sc.tl.diffmap(adata)
  
  # define root and get pseudotime
  # https://scanpy.readthedocs.io/en/stable/generated/scanpy.tl.dpt.html
  # We recommend, however, to only use dpt() for computing pseudotime
  # (n_branchings=0) and to detect branchings via paga()
  adata.uns['iroot'] = np.flatnonzero(adata.obs[clusters_col] == init_root)[0]
  sc.tl.dpt(adata, n_branchings = 0, n_dcs = 10)
  
  logfile_utils.log(f'>> Write results back')

  # save back
  label_view.adata = adata
  label_view.save()
  label_view.close()

def main():
  # get params
  params = script_utils.script_params()

  # run
  run(params)

if __name__ == "__main__":
  main()
