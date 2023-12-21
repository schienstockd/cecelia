# add CCIA modules
import sys
import os
sys.path.append('./')

# utils
from py.label_props_utils import LabelPropsUtils
import py.scanpy_utils as scanpy_utils

import py.script_utils as script_utils
import py.config_utils as cfg

# cluster cell meshes
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # prepare params
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')

  # get properties
  value_name = script_utils.get_ccia_param(params, 'value_name', default = None)
  object_measures = script_utils.get_param(params, 'objectMeasures', default = list())
  logical_measures = script_utils.get_param(params, 'logicalMeasures', default = list())
  track_measures = script_utils.get_param(params, 'trackMeasures', default = list())
  n_measures = script_utils.get_param(params, 'nMeasures', default = list())
  sum_measures = script_utils.get_param(params, 'sumMeasures', default = list())
  calc_measures = script_utils.get_param(params, 'calcMeasures', default = list())
  resolution = script_utils.get_param(params, 'resolution', default = 1)
  percentile = script_utils.get_param(params, 'percentile', default = 0.998)
  paga_threshold = script_utils.get_param(params, 'pagaThreshold', default = 0.1)
  use_paga = script_utils.get_param(params, 'usePaga', default = True)

  # TODO somehow an empty track measures is a dict
  if len(track_measures) == 0:
    track_measures = list()

  # get data for clustering
  adata = LabelPropsUtils(task_dir, value_name = value_name).label_props_view(read_only = False)\
    .as_adata()\
    .to_memory()

  # add mean and sd to object measures
  object_measures_adj = [f'{x}.mean' for x in object_measures]
  object_measures_adj += [f'{x}.sd' for x in object_measures]
  object_measures_adj += [f'{x}.median' for x in object_measures]
  # object_measures_adj += [f'{x}.sum' for x in object_measures]
  object_measures_adj += [f'{x}.qUp' for x in object_measures]
  object_measures_adj += [f'{x}.qLow' for x in object_measures]

  # selected columns for logicals
  logical_measures_adj = list()
  for x in logical_measures:
    logical_measures_adj += [y for y in list(adata.var_names) if y.startswith(f'{x}.')\
      and not y.endswith('.n') and not y.endswith('.NA')]

  # add n to n measures
  n_measures_adj = list()
  for x in n_measures:
    n_measures_adj += [y for y in list(adata.var_names) if y.startswith(f'{x}.')\
      and not y.endswith('.NA') and not y.endswith('.NA.n')]

  # add sum to sum measures
  sum_measures_adj = list()
  for x in sum_measures:
    sum_measures_adj += [y for y in list(adata.var_names) if y.startswith(f'{x}.')\
      and y.endswith('.sum')]

  # set columns to cluster
  cols_for_clust = track_measures + logical_measures_adj\
                     + n_measures_adj + sum_measures_adj\
                     + object_measures_adj + calc_measures

  # only select cluster columns
  adata = adata[:, cols_for_clust]

  logfile_utils.log(f'>> Find clusters for {cols_for_clust}')

  # find populations for tracks
  scanpy_utils.find_populations(
      adata, resolution = resolution,
      percentile = percentile,
      paga_threshold = paga_threshold,
      use_paga = use_paga
  )

  logfile_utils.log(f'>> Write results back')

  # save back
  adata.write_h5ad(os.path.join(
    task_dir, 'labelProps', f'{value_name}.sc.h5ad'))

  # close
  adata.file.close()

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = [
      'popsToCluster', 'trackMeasures',
      'objectMeasures', 'logicalMeasures',
      'nMeasures', 'sumMeasures'
      ]
  )

  # run
  run(params)

if __name__ == '__main__':
  main()
