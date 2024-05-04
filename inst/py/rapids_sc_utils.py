import scanpy as sc
import py.scanpy_utils as scanpy_utils
import numpy as np

import cupy as cp
import rapids_singlecell as rsc
import py.config_utils as cfg
import rmm
from rmm.allocators.cupy import rmm_cupy_allocator

rmm.reinitialize(
    managed_memory=False,  # Allows oversubscription
    pool_allocator=False,  # default is False
    devices=0,  # GPU device IDs to register. By default registers only GPU 0.
)
cp.cuda.set_allocator(rmm_cupy_allocator)

"""
Find populations
"""
def find_populations(
  adata, resolution = 1, clusterMethod = 'leiden',
  axis = 'channels', to_median = False, max_fraction = 0,
  percentile = cfg.data['images']['normalise']['percentile'],
  percentile_bottom = 0, create_umap = True,
  paga_threshold = 0.1, use_paga = False, transformation = 'NONE', log_base = 0,
  correct_batch = None):
  # transform data
  scanpy_utils.apply_transform(adata, transformation = transformation, log_base = log_base)
      
  # normalize
  scanpy_utils.normalise_adata(
    adata, axis = axis,
    to_median = to_median,
    max_fraction = max_fraction,
    percentile = percentile,
    percentile_bottom = percentile_bottom)
  
  # run batch effect
  if correct_batch is not None:
    if correct_batch == 'combat':
      sc.pp.combat(adata, key = 'uID')
    if correct_batch == 'harmony':
      # apply PCA before harmony
      rsc.pp.pca(adata)
      rsc.pp.harmony_integrate(adata, key = 'uID')
  
  # Use the indicated representation. 'X' or any key for .obsm is valid.
  # If None, the representation is chosen automatically:
  #   For .n_vars < 50, .X is used, otherwise ‘X_pca’ is used.
  #   If ‘X_pca’ is not present, it’s computed with default parameters.
  # reset NaN
  # adata.fillna(0, inplace = True)
  adata.X[np.isnan(adata.X)] = 0
  rsc.pp.neighbors(adata, use_rep = 'X')
  # rsc.pp.neighbors(adata)
  
  if clusterMethod == "leiden":
    rsc.tl.leiden(
      adata, resolution = resolution,
      key_added = "clusters")
  elif clusterMethod == "louvain":
    rsc.tl.louvain(
      adata, resolution = resolution,
      key_added = "clusters")
  
  if create_umap is True:
    if use_paga is True:
      # calculate PAGA
      sc.tl.paga(adata, groups = 'clusters')
      # ValueError: Plot PAGA first, so that adata.uns['paga'] with key 'pos'.
      sc.pl.paga(adata, plot = False, show = False, threshold = paga_threshold)
      
      # then UMAP based on paga
      rsc.tl.umap(adata, init_pos = 'paga')
    else:
      rsc.tl.umap(adata)
