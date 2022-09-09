import scanpy as sc
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import os

# utils
import py.colour_utils as colour_utils
import py.config_utils as cfg

dat_filename = "adata.h5ad"

"""
Normalise data
"""
def normalise_adata(
  adata, axis = 'channels', to_median = False, max_fraction = 0,
  percentile = cfg.data['images']['normalise']['percentile'],
  percentile_bottom = 0):
  if axis == 'cells':
    exclude_highly_expressed = False
    if max_fraction > 0:
      exclude_highly_expressed = True
      
    sc.pp.normalize_total(
      adata, target_sum = 1, exclude_highly_expressed = exclude_highly_expressed,
      max_fraction = max_fraction, inplace = True)
  elif axis == 'channels':
    if percentile > 0:
      normalise_per_channel(
        adata, percentile = percentile, percentile_bottom = percentile_bottom,
        to_median = to_median, inplace = True)

"""
TODO
Normalise per median channel
"""
def normalise_per_median_channel(adata, inplace = True):
  if inplace is not True:
    adata = adata.copy()
  
  # get medin values for channels
  median_values = np.median(adata.X, axis = 0)
  
  # calculate relative values
  adata.X = adata.X / median_values
  
  if inplace is not True:
    return adata

"""
Normalise per channel
"""
def normalise_per_channel(
  adata, percentile, percentile_bottom = 0, to_median = False, inplace = True):
    if inplace is not True:
      adata = adata.copy()
    
    # normalise to median first?
    if to_median is True:
      channel_median = np.median(adata.X, axis = 0)
      
      adata.X = adata.X / channel_median
    
    # get min/max values for channels
    max_percentile = np.percentile(adata.X, percentile, axis = 0)
    min_percentile = np.percentile(adata.X, percentile_bottom, axis = 0)
    
    # get total min and max for backup
    max_total = np.max(adata.X, axis = 0)
    min_total = np.min(adata.X, axis = 0)
    
    # for values that are very low
    # TODO this is for the specific case of rare HMM transitions
    # not very elegant, but otherwise:
    # RuntimeWarning: invalid value encountered in true_divide
    # RuntimeWarning: divide by zero encountered in true_divide
    failed_percentiles = max_percentile == min_percentile
    
    if any(failed_percentiles == True):
      max_percentile[failed_percentiles] = max_total[failed_percentiles]
      min_percentile[failed_percentiles] = min_total[failed_percentiles]
      
    # if there are only zeros - set max to '1'
    failed_percentiles = max_percentile == min_percentile
    
    if any(failed_percentiles == True):
      max_percentile[failed_percentiles] = 1
      min_percentile[failed_percentiles] = 0

    # calculate relative values
    adata.X = (adata.X - min_percentile) / (max_percentile - min_percentile)
    
    # adjust
    adata.X[adata.X < 0] = 0
    adata.X[adata.X > 1] = 1
    
    if inplace is not True:
      return adata

"""
Apply transformation
"""
def apply_transform(adata, transformation = 'NONE', log_base = 10):
  if transformation != 'NONE':
    if transformation == 'log':
      use_log_base = None
      
      if log_base > 0:
        use_log_base = log_base
      
      # call transform
      sc.pp.log1p(adata, base = use_log_base)
    if transformation == 'reversedLog':
      # get max from columns
      col_max = np.max(adata.X, axis = 0)
      
      # reverse axis and take log
      if log_base == 0:
        adata.X = np.log((-adata.X + col_max) + 1)
      elif log_base == 2:
        adata.X = np.log2((-adata.X + col_max) + 1)
      elif log_base == 10:
        adata.X = np.log10((-adata.X + col_max) + 1)
          
      # reverse back
      adata.X = -adata.X + np.max(adata.X, axis = 0)

"""
Find populations
"""
def find_populations(
  adata, resolution = 1, clusterMethod = 'leiden',
  axis = 'channels', to_median = False, max_fraction = 0,
  percentile = cfg.data['images']['normalise']['percentile'],
  percentile_bottom = 0, create_umap = True,
  paga_threshold = 0.1, use_paga = False, transformation = 'NONE', log_base = 0):
  # transform data
  apply_transform(adata, transformation = transformation, log_base = log_base)
      
  # normalize
  normalise_adata(
    adata, axis = axis,
    to_median = to_median,
    max_fraction = max_fraction,
    percentile = percentile,
    percentile_bottom = percentile_bottom)
  
  # run umap and leiden
  # sc.pp.pca(adata)
  
  # Use the indicated representation. 'X' or any key for .obsm is valid.
  # If None, the representation is chosen automatically:
  #   For .n_vars < 50, .X is used, otherwise ‘X_pca’ is used.
  #   If ‘X_pca’ is not present, it’s computed with default parameters.
  # reset NaN
  # adata.fillna(0, inplace = True)
  adata.X[np.isnan(adata.X)] = 0
  sc.pp.neighbors(adata, use_rep = 'X')
  
  if clusterMethod == "leiden":
    sc.tl.leiden(
      adata, resolution = resolution,
      key_added = "clusters")
  elif clusterMethod == "louvain":
    sc.tl.louvain(
      adata, resolution = resolution,
      key_added = "clusters")
  
  if create_umap is True:
    if use_paga is True:
      # calculate PAGA
      sc.tl.paga(adata, groups = 'clusters')
      # ValueError: Plot PAGA first, so that adata.uns['paga'] with key 'pos'.
      sc.pl.paga(adata, plot = False, show = False, threshold = paga_threshold)
      
      # then UMAP based on paga
      sc.tl.umap(adata, init_pos = 'paga')
    else:
      sc.tl.umap(adata)
  
"""
Save data
"""
def save_data(adata, task_dir):
  # TODO .. would zarr be better?
  adata.write_h5ad(os.path.join(task_dir, dat_filename))
  
"""
Load data
"""
def load_data(task_dir):
  return sc.read(os.path.join(task_dir, dat_filename))

"""
Prepare colours to show clusters
"""
def prep_cluster_colours(adata):
  # prepare colours
  cluster_colours = colour_utils.distinct_colours(
      len(adata.obs.clusters.unique()) + 1, as_dict = True)
      
  return cluster_colours
