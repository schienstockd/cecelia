import pandas as pd
import numpy as np
import anndata as ad
import os

# utils
from py.label_props_view import LabelPropsView

label_ext = "labelProps.h5ad"

class LabelPropsUtils(LabelPropsView):
  def __init__(self, task_dir, labels_file = None, value_name = None):
    super().__init__(task_dir, labels_file = labels_file,
                     value_name = value_name)
  
  """
  Create adata from label properties DF
  """
  def label_props(self, label_props_df, save = False, obs_cols = ['label'], update_existing = False,
                  obsm = dict(), uns = dict(), split_columns = False):
    # split off columns
    if split_columns is True:
      # get spatial and temporal columns from props
      centroid_spatial = [x for x in label_props_df.columns if x in [f'centroid_{i}' for i in ['x', 'y', 'z']]]
      centroid_temporal = [x for x in label_props.columns if x == 'centroid_t']
      
      # split spatial and temporal information into obsm
      # this will then allow processing with squidpy
      if len(centroid_spatial) > 0:
        uns['spatial_cols'] = centroid_spatial
        obsm['spatial'] = label_props_df[centroid_spatial].to_numpy()
        
      if len(centroid_temporal) > 0:
        uns['temporal_cols'] = centroid_temporal
        obsm['temporal'] = label_props_df[centroid_temporal].to_numpy()
            
    # get positive and negative location positions
    obs_loc = [x in obs_cols for x in label_props_df.columns]
    other_loc = [x not in obs_cols for x in label_props_df.columns]
    
    # convert to adata
    if update_existing is True:
      # load data
      self.load()
      
      # remember others
      obsm = self.adata.obsm
      uns = self.adata.uns
      
      # create dataset
      self.adata = super().create_new_dataset(
        label_props_df.loc[:, other_loc].to_numpy().astype(np.float32),
          var = pd.DataFrame(index = label_props_df.columns[other_loc]),
          obs = self.adata.obs
      )
    else:
      self.adata = super().create_new_dataset(
        label_props_df.loc[:, other_loc].to_numpy().astype(np.float32),
          var = pd.DataFrame(index = label_props_df.columns[other_loc]),
          obs = label_props_df.loc[:, obs_loc]
      )
      
    # set obsm
    if len(obsm) > 0:
      self.adata.obsm = obsm
      
    # set uns
    if len(uns) > 0:
      self.adata.uns = uns
    
    # save back?
    if save is True:
      self.save(self.adata_filepath())
      
    return self
      
  """
  Get view of label properties
  """
  def label_props_view(self, labels_file = None, value_name = None, read_only = True):
    # load and return view
    return LabelPropsView(
      task_dir = self.task_dir,
      labels_file = self.labels_file if labels_file is None else labels_file,
      value_name = self.value_name if value_name is None else value_name
      ).load(read_only = read_only)
  
