# add CCIA modules
import sys
import os
import numpy as np
import pandas as pd
sys.path.append('./')

from py.label_props_utils import LabelPropsUtils
from py.pop_utils import PopUtils

import py.scanpy_utils as scanpy_utils

import py.script_utils as script_utils
import py.config_utils as cfg

# segment image
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # prepare params
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')

  # get properties
  value_name = script_utils.get_ccia_param(params, 'value_name', default = None)
  resolution = script_utils.get_param(params, 'resolution', default = 1)
  merge_umap = script_utils.get_param(params, 'mergeUmap', default = True)
  cluster_channels = script_utils.get_param(params, 'clusterChannels', default = {})
  object_measures = script_utils.get_param(params, 'objectMeasures', default = [])
  pop_type = script_utils.get_param(params, 'popType', default = None)
  pops_to_cluster = script_utils.get_param(params, 'popsToCluster', default = [])
  keep_pops = script_utils.get_param(params, 'keepPops', default = False)
  normalise_axis = script_utils.get_param(params, 'normaliseAxis', default = 'channels')
  normalise_to_median = script_utils.get_param(params, 'normaliseToMedian', default = False)
  max_fraction = script_utils.get_param(params, 'maxFraction', default = 0)
  normalise_percentile = script_utils.get_param(params, 'normalisePercentile', default = 100)
  normalise_percentile_bottom = script_utils.get_param(params, 'normalisePercentileBottom', default = 0)
  normalise_individually = script_utils.get_param(params, 'normaliseIndividually', default = False)
  correct_batch = script_utils.get_param(params, 'correctBatch', default = False)
  intensity_measure = script_utils.get_param(params, 'intensityMeasure', default = 'mean')
  transformation = script_utils.get_param(params, 'transformation', default = 'NONE')
  log_base = script_utils.get_param(params, 'logBase', default = 10)
  paga_threshold = script_utils.get_param(params, 'pagaThreshold', default = 0.1)
  use_paga = script_utils.get_param(params, 'usePaga', default = True)
  use_gpu = script_utils.get_param(params, 'useGPU', default = False)
  uIDs = script_utils.get_param(params, 'uIDs', default = [])
  
  logfile_utils.log(f'>> Find clusters at {resolution}')
  logfile_utils.log(f'>> Normalise by {normalise_percentile} with {normalise_axis} for individual images ({normalise_individually})')
  logfile_utils.log(f'>> Transform by {transformation}')
  logfile_utils.log(f'>> Normalise to median {normalise_to_median}')
  logfile_utils.log(f'>> Create UMAP {merge_umap}')

  image_task_dirs = dict()

  # remember cluster channel mapping
  cluster_channels_map = dict()

  # get label properties from uIDs if running on set
  if len(uIDs) > 0:
    logfile_utils.log(f'>> Running clustering on set for {uIDs}')
    image_dfs = []
    
    max_clusters = 0

    # make task dirs
    task_dir_split = task_dir.split(os.sep)
    
    # get dataframes
    for x in uIDs:
      logfile_utils.log(f'> Get data for {x} with {normalise_axis}')
      
      # prepare cluster channels
      # should be a dict of channel types with lists
      column_names = []
      channel_columns = []
      
      for j, y in cluster_channels[x].items():
        # get column names
        channel_columns += [f'{j}_{intensity_measure}_intensity_{z}' if j != 'base' else f'{intensity_measure}_intensity_{z}' for z in y['channels']]
      
      # set cluster map
      cluster_channels_map[x] = {y: f'intensity-{j}' for j, y in enumerate(channel_columns)}
      
      # make sure object measures is a list
      if len(object_measures) == 0:
        object_measures = list()
      
      # add object measurements to column names
      column_names = channel_columns + object_measures
      
      logfile_utils.log(f'> {column_names}')
      
      # create task directory
      task_dir_split[-1] = x
      image_task_dirs[x] = os.path.join(*([os.sep] + task_dir_split))

      image_df, image_clusters = get_adata(
        image_task_dirs[x], value_name, column_names, logfile_utils,
        pops_to_cluster = pops_to_cluster, pop_type = pop_type,
        keep_pops = keep_pops, as_df = True,
        transformation = transformation if normalise_individually is True else 'NONE',
        log_base = log_base,
        normalise_axis = normalise_axis if normalise_individually is True else 'NONE',
        normalise_to_median = normalise_to_median if normalise_individually is True else False,
        max_fraction = max_fraction,
        normalise_percentile = normalise_percentile if normalise_individually is True else 0,
        normalise_percentile_bottom = normalise_percentile_bottom if normalise_individually is True else 0
        )
        
      # rename channel columns
      image_df.rename(columns = cluster_channels_map[x], inplace = True)
      
      # append
      image_dfs.append(image_df)
      
      # set max clusters
      max_clusters = max(image_clusters, max_clusters)

      # set uID
      image_dfs[-1]['uID'] = x

    # concat together
    image_df = pd.concat(image_dfs, axis = 0, ignore_index = True)
    
    # convert to adata
    adata = LabelPropsUtils(task_dir)\
      .label_props(image_df, obs_cols = ['uID', 'label'])\
      .as_adata()
  # else:
  #   # run clustering on single image
  #   adata, max_clusters = get_adata(
  #     task_dir, value_name, column_names, logfile_utils,
  #     pops_to_cluster = pops_to_cluster, pop_type = pop_type,
  #     keep_pops = keep_pops)
  
  logfile_utils.log(f'>> increase cluster numbering by {max_clusters}')
  logfile_utils.log(f'>> find populations')
  
  # use GPU version?
  if use_gpu is True:
    import py.rapid_sc_utils as rsc_utils 
    
    # find populations
    rsc_utils.find_populations(
      adata, resolution,
      transformation = 'NONE' if len(uIDs) > 0 and normalise_individually is True else transformation,
      log_base = log_base,
      axis = normalise_axis,
      to_median = False if len(uIDs) > 0 and normalise_individually is True else normalise_to_median,
      max_fraction = max_fraction,
      percentile = 0 if len(uIDs) > 0 and normalise_individually is True else normalise_percentile,
      percentile_bottom = 0 if len(uIDs) > 0 and normalise_individually is True else normalise_percentile_bottom,
      correct_batch = correct_batch,
      create_umap = merge_umap,
      paga_threshold = paga_threshold,
      use_paga = use_paga
      )
  else:
    # find populations
    scanpy_utils.find_populations(
      adata, resolution,
      transformation = 'NONE' if len(uIDs) > 0 and normalise_individually is True else transformation,
      log_base = log_base,
      axis = normalise_axis,
      to_median = False if len(uIDs) > 0 and normalise_individually is True else normalise_to_median,
      max_fraction = max_fraction,
      percentile = 0 if len(uIDs) > 0 and normalise_individually is True else normalise_percentile,
      percentile_bottom = 0 if len(uIDs) > 0 and normalise_individually is True else normalise_percentile_bottom,
      correct_batch = correct_batch,
      create_umap = merge_umap,
      paga_threshold = paga_threshold,
      use_paga = use_paga
      )
      
  logfile_utils.log(f'>> save back')

  # save data
  if len(uIDs) > 0:
    for i, x in image_task_dirs.items():
      logfile_utils.log(f'> {i} to {x}')

      # filter by uID
      image_adata = adata[adata.obs['uID'] == i]
      
      # rename columns back
      channel_map_inv = {y: j for j, y in cluster_channels_map[i].items()}
      image_adata.var_names = [channel_map_inv[y] if y in channel_map_inv.keys() else y for y in image_adata.var_names]

      # remove uID
      image_adata.obs.drop('uID', axis = 1, inplace = True)

      # save back
      if keep_pops is True and pop_type == 'clust' and len(pops_to_cluster) > 0:
         merge_new_adata(image_adata, x, value_name, logfile_utils, merge_umap = merge_umap, max_clusters = max_clusters).write_h5ad(os.path.join(
          x, 'labelProps', f'{value_name}.clust.h5ad'))
      else:
        image_adata.write_h5ad(os.path.join(
          x, 'labelProps', f'{value_name}.clust.h5ad'))
          
  # else:
  #   if keep_pops is True and pop_type == 'clust' and len(pops_to_cluster) > 0:
  #     merge_new_adata(adata, task_dir, value_name, logfile_utils, merge_umap = merge_umap, max_clusters = max_clusters).write_h5ad(os.path.join(
  #       task_dir, 'labelProps', f'{value_name}.clust.h5ad'))
  #   else:
  #     adata.write_h5ad(os.path.join(
  #       task_dir, 'labelProps', f'{value_name}.clust.h5ad'))

  # close
  adata.file.close()

# merge new adata clusters with previous ones
def merge_new_adata(new_adata, task_dir, value_name, logfile_utils, merge_umap = True, max_clusters = 0):
  # load previous adata
  label_view = LabelPropsUtils(task_dir = task_dir)\
    .label_props_view(value_name = f'{value_name}.clust')
  
  # remove columns from previous data
  # if used in the new data
  non_used_cols = [
    i for i, x in enumerate(label_view.col_names()) if x not in new_adata.var_names.tolist()
    ]
  used_cols = {
    i: x for i, x in enumerate(label_view.col_names()) if x in new_adata.var_names.tolist()
    }
  
  # define new column names
  new_var_names = [label_view.col_names()[i] for i in non_used_cols] + new_adata.var_names.tolist()
  
  # remember old values
  # TODO this has to be cleaner
  prev_x = label_view.adata[
    ~label_view.adata.obs['label'].isin(new_adata.obs['label'])].X[:, list(used_cols.keys())]
  
  # merge X
  # extend x for new columns with '0'
  label_view.x_values(
      np.append(
          label_view.adata.X[:, non_used_cols],
          np.zeros((label_view.adata.X[:].shape[0], new_adata.X.shape[1])),
          axis = 1
      ), var_names = new_var_names
  )
  
  # copy in new values
  label_view.adata.X[
    label_view.adata.obs['label'].isin(new_adata.obs['label']),
    len(non_used_cols):
    ] = new_adata.X
    
  # copy in old values
  # TODO loop through, is there a better way?
  for i, x in enumerate(used_cols.values()):
    label_view.adata.X[
      ~label_view.adata.obs['label'].isin(new_adata.obs['label']),
      new_var_names.index(x)] = prev_x[:, i]
    
  # set values not used for clustering to '0'
  label_view.adata.X[
    label_view.adata.obs['label'].isin(new_adata.obs['label']),
    :len(non_used_cols)
    ] = 0
    
  # TODO you need to get this from the whole dataset
  # # adjust UMAP by shifting values to the right
  # # get maxi for umap from other populations not used
  # max_x_umap = np.max(
  #   label_view.adata[
  #     ~label_view.adata.obs['label'].isin(new_adata.obs['label'])
  #     ].obsm['X_umap'],
  #   axis = 0)[0]
  # 
  # # add max + buffer to new umap
  # label_view.adata.obsm['X_umap'][
  #   label_view.adata.obs['label'].isin(new_adata.obs['label']), 0
  # ] += max_x_umap + 5
  
  # copy clusters
  prev_adata = label_view.as_adata()
  label_view.close()
    
  # merge clusters with previous obs
  # TODO is there a more elegant way?
  prev_adata.obs['clusters'] = prev_adata.obs['clusters'].astype(np.uint16)
  new_adata.obs['clusters'] = new_adata.obs['clusters'].astype(np.uint16)
  
  # increase cluster numbering
  # clusters start at '0'
  # max_clusters = prev_adata.obs.loc[
  #   ~prev_adata.obs['label'].isin(new_adata.obs['label']), 'clusters'].max() + 1
  
  new_adata.obs['clusters'] += max_clusters + 1
  
  # merge
  prev_adata.obs['clusters'] = prev_adata.obs['clusters'].astype('str')
  new_adata.obs['clusters'] = new_adata.obs['clusters'].astype('str')
  
  # add cluster values
  prev_adata.obs.loc[
    prev_adata.obs['label'].isin(new_adata.obs['label']),
    'clusters'
    ] = list(new_adata.obs['clusters'])
    
  # merge UMAP values
  if merge_umap is True:
    # check that UMAP exists
    if 'X_umap' not in prev_adata.obsm.keys():
      prev_adata.obsm['X_umap'] = np.zeros(
        [len(prev_adata.obs.index), 2], dtype = np.float32)
    
    prev_adata.obsm['X_umap'][
      prev_adata.obs['label'].isin(new_adata.obs['label']), :
    ] = new_adata.obsm['X_umap']
    
  return prev_adata

# get adata from image
def get_adata(task_dir, value_name, column_names, logfile_utils,
              pops_to_cluster = None, pop_type = None,
              keep_pops = False, as_df = False,
              normalise_axis = 'channels', normalise_to_median = False, max_fraction = 0,
              normalise_percentile = 100, normalise_percentile_bottom = 0,
              transformation = 'NONE', log_base = 10):
  label_view = LabelPropsUtils(task_dir, value_name = value_name).label_props_view(read_only = False)
  
  # get population for clustering
  pop_labels = []
  if pops_to_cluster is not None and len(pops_to_cluster) > 0:
    # get the requested population from here
    pop_utils = PopUtils()
    pop_map = pop_utils.pop_map(task_dir, pop_type, pops = pops_to_cluster)
    pop_data = pop_utils.pop_data(task_dir, pop_type, pops = pops_to_cluster)
    
    pop_labels += [y for x in pop_data.values() for y in x]
    
  # filter labels
  if len(pop_labels) > 0:
    label_view.filter_by_obs(pop_labels)
    
  # get max clusters
  max_clusters = 0
  
  if keep_pops is True:
    clust_label_view = LabelPropsUtils(task_dir = task_dir)\
      .label_props_view(value_name = f'{value_name}.clust')
      
    if 'clusters' in clust_label_view.values_obs().keys():
      max_clusters = clust_label_view.values_obs().loc[
        ~clust_label_view.values_obs()['label'].isin(pop_labels), 'clusters'].astype(np.int32).max()
    
    clust_label_view.close()
  
  # get data for clustering
  # label_view.view_vars_cols(column_names)\
  label_view.view_cols(column_names)\
    .view_label_col()\
    .exclude_spatial_temporal()
  
  # transform columns?
  scanpy_utils.apply_transform(label_view.adata, transformation = transformation, log_base = log_base)

  # normalise columns?
  scanpy_utils.normalise_adata(
    label_view.adata,
    axis = normalise_axis,
    to_median = normalise_to_median,
    max_fraction = max_fraction,
    percentile = normalise_percentile,
    percentile_bottom = normalise_percentile_bottom)

  if as_df is True:
    adata = label_view.as_df()
  else:
    adata = label_view.as_adata()

  label_view.close()

  return adata, max_clusters

def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['clusterChannels', 'objectMeasures', 'uIDs', 'popsToCluster']
  )

  # run
  run(params)

if __name__ == '__main__':
  main()
