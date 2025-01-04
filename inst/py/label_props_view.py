import pandas as pd
import numpy as np
import anndata as ad
import os
import re
import shutil
import time
# import igraph as ig

# utils
import py.config_utils as cfg

class LabelPropsView:
  def __init__(self, task_dir, labels_file = None, value_name = None):
    self._task_dir = task_dir
    self._value_name = None
    
    # get labels file
    if value_name is not None:
      self._value_name = value_name
      labels_file = cfg.value_dir(value_name, 'labelProps')
    
    self._labels_file = labels_file
    
    if labels_file is not None:
      self._labels_path = os.path.join(task_dir, labels_file)
    
    self._channel_names = None
    self._adata = None
    self._intensity_measure = None
    
    # by default, add centroids to df
    self._add_centroids_to_df = True
    self._add_x_to_df = True
    self._centroids_order = None
  
  # close connections
  # TODO this does not get called in R via reticulate
  def __del__(self):
    self.close()
  
  """
  Getters
  """  
  @property
  def task_dir(self):
    return self._task_dir

  @property
  def adata(self):
    return self._adata
  
  @property
  def channel_names(self):
    return self._channel_names
  
  @property
  def labels_file(self):
    return self._labels_file
  
  @property
  def value_name(self):
    return self._value_name
  
  @property
  def labels_path(self):
    return self._labels_path
  
  @property
  def add_centroids_to_df(self):
    return self._add_centroids_to_df
  
  @property
  def add_x_to_df(self):
    return self._add_x_to_df
  
  @property
  def centroids_order(self):
    return self._centroids_order
  
  @property
  def intensity_measure(self):
    return self._intensity_measure
  
  """
  Setters
  """
  @task_dir.setter
  def task_dir(self, x):
    self._task_dir = x
  
  @adata.setter
  def adata(self, x):
    self._adata = x
  
  @channel_names.setter
  def channel_names(self, x):
    self._channel_names = x
    
  @labels_file.setter
  def labels_file(self, x):
    self._labels_file = x
    
  @value_name.setter
  def value_name(self, x):
    self._value_name = x
    
  @labels_path.setter
  def labels_path(self, x):
    self._labels_path = x
    
  @add_centroids_to_df.setter
  def add_centroids_to_df(self, x):
    self._add_centroids_to_df = x
    
  @add_x_to_df.setter
  def add_x_to_df(self, x):
    self._add_x_to_df = x
    
  @centroids_order.setter
  def centroids_order(self, x):
    self._centroids_order = x
    
  @intensity_measure.setter
  def intensity_measure(self, x):
    self._intensity_measure = x

  """
  Add data to obs
  """
  def add_obs(self, obs_to_add):
    # if remove_previous is True:
    #   self.del_obs(obs_to_add.keys())
    
    # go through names of dict
    for i, x in obs_to_add.items():
      self.adata.obs[i] = x
    
    return self
  
  """
  Delete data from obs
  """
  def del_obs(self, obs_to_del):
    # make sure cols are in obs
    obs_to_del = [x for x in obs_to_del if x in self.col_names('obs')]
    
    # drop columns
    self.adata.obs.drop(obs_to_del, axis = 1, inplace = True)
    
    return self
  
  """
  Change var names
  """
  def change_var_names(self, var_names):
    # change var names
    self.adata.var_names = var_names
    
    return self
  
  """
  Change channel names
  """
  def change_channel_names(self, channel_names):
    channel_columns = self.channel_columns()
    
    # do they have the same length as channels?
    if (len(channel_names) == len(channel_columns)):
      # get extra channel types
      channel_types = self.channel_types()
      
      # change over in var
      var_names = self.col_names()
      self.channel_names = channel_names
      
      # get channel columns
      channel_types_columns = {
        x: [f'{x}_{y}' for y in channel_columns] for x in channel_types
      }
      
      # go through channel names and change columns
      for i, x in enumerate(channel_names):
        # TODO is there a better way to do this .. ?
        var_names = list(map(lambda k: re.sub(
          f'^{channel_columns[i]}$', x, k), var_names))
          
        # are there other channel types to add?
        for j, y in channel_types_columns.items():
          var_names = list(map(lambda k: re.sub(
          f'^{y[i]}$', f'{j}_{x}', k), var_names))
      
      # set names
      self.adata.var_names = var_names

    return self
  
  """
  Read adata
  """
  def load(self, read_only = True):
    if os.path.exists(self.adata_filepath()):
      # close previous connection
      self.close()
      self.adata = None
      
      # try to read until resource is available
      # TODO is there a better way to do this .. ?
      # https://stackoverflow.com/q/63444603/13766165
      # OSError: Unable to open file (unable to lock file, errno = 35, error message = 'Resource temporarily unavailable')
      counter = 0
      while self.adata is None and counter < 5:
        try:
          self.adata = ad.read_h5ad(
            self.adata_filepath(), backed = 'r' if read_only is True else 'r+')
        except OSError as e:
          print(f'>> {self.adata_filepath()} locked - retry in 2s')
          print(f'>> {e}')
          time.sleep(2)
          
          counter += 1
      
      # set intensity measure value
      if 'intensity_measure' in self.adata.uns.keys() and self.adata.uns['intensity_measure'] != None:
        self.intensity_measure = self.adata.uns['intensity_measure']
      else:
        self.intensity_measure = 'mean'
      
      return self
        
  """
  return(adata file path
  """
  def adata_filepath(self):
    return os.path.join(
      self.task_dir,
      cfg.data['files']['labelProps'] if self.labels_file is None else self.labels_file
      )
  
  """
  Close adata
  """
  def close(self):
    # close file
    if self.adata is not None:
      self.adata.file.close()
    
    # set to None
    self.adata = None
  
  """
  Save adata
  """
  def save(self, filename = None, close = True, revert_channel_names = True):
    if filename is None:
      # does not work if creating a new dataset
      # which has not yet been saved
      filename = self.adata.filename
      
      if filename is None:
        filename = self.labels_path
    
    # TODO - that needs to be cleaner
    move_file_back = False
    
    # remove file
    # Error in py_call_impl(callable, dots$args, dots$keywords) :
    # OSError: Unable to create file (file exists)
    if os.path.exists(filename):
      ori_filename = filename
      filename = str(filename) + '.tmp'
      
      move_file_back = True
      
    # change channel names back to mean intensity
    if revert_channel_names is True:
      numeric_channels = self.channel_columns(as_numeric = True)
      
      if len(numeric_channels) > 0:
        self.change_channel_names(numeric_channels)
    
    # save file
    # remove if exists otherwise there will be:
    # Error: OSError: Unable to open file (bad object header version number)
    if os.path.exists(filename):
      os.remove(filename) 
    self.adata.write_h5ad(filename)
    
    # remove old one and rename
    if move_file_back is True:
      shutil.move(filename, ori_filename)
      
    # close connection
    if close is True:
      self.close()

  """
  Get column names
  """
  def col_names(self, data_type = 'vars'):
    ret_val = None
    
    # get var names
    if data_type == 'vars':
      ret_val = self.adata.var_names.tolist()
    
    # get obs names
    elif data_type == 'obs':
      ret_val = self.adata.obs_keys()
    
    return ret_val
  
  """
  Check whether column names are in adata
  """
  def has_cols(self, dat_cols, dat_type = 'vars'):
    return dat_cols is not None and \
        set(dat_cols).issubset(self.col_names(dat_type))
        
  """
  Get cols for data type from list
  """
  def cols_type(self, dat_cols, dat_type = 'vars'):
    return dat_cols is not None and \
        set(dat_cols).issubset(self.col_names(dat_type))
    
  """
  Get centroid columns
  """
  def centroid_columns(self, order = None):
    if self.has_spatial_obsm():
      cols = list()
    
      # spatial
      if 'spatial_cols' in self.adata.uns.keys():
        cols += self.adata.uns['spatial_cols'].tolist()
      
      # temporal
      if 'temporal_cols' in self.adata.uns.keys():
        cols += self.adata.uns['temporal_cols'].tolist()
    else:
      # TODO this is the old way
      cols = [i for i in self.col_names('vars') if i.startswith('centroid')]
      
    # sort cols?
    if order is not None:
      # convert 
      cols_order = [f'centroid_{x.lower()}' for x in order]
      
      cols = [x for x in cols_order if x in cols]
      
    return cols
  
  """
  Get channel types
  """
  def channel_types(self):
    channel_names = self.channel_names
    
    # get channel types from mean intensity
    if channel_names is None:
      types = [re.match(f'^[a-z]+(?=_{self.intensity_measure}_intensity)', x) for x in self.col_names('vars')]
    else:
      # take first channel name
      types = [re.match(f'^[a-z]+(?=_{channel_names[0]})', x) for x in self.col_names('vars')]
    
    # return matches
    return set([x.group() for x in types if x is not None])
  
  """
  Get channel columns
  """
  def channel_columns(self, as_numeric = False, prefix = None):
    channel_names = self.channel_names
    
    # fall back to mean if median not present
    if not self.has_cols(f'{self.intensity_measure}_intensity_0'):
      intensity_type = 'mean'
    
    if channel_names is None:
      channel_names = [i for i in self.col_names('vars') if i.startswith(f'{self.intensity_measure}_intensity_')]
      
    # change names to numeric
    if as_numeric is True:
      channel_names = [f'{self.intensity_measure}_intensity_{i}' for i in range(len(channel_names))]
      
    # add prefix to channels?
    if prefix is not None:
      channel_names = [f'{prefix}_{x}' for x in channel_names]
    
    return channel_names
  
  """
  Load to memory
  """
  def to_memory(self):
    self.adata = self.adata.to_memory()
    
    return self
    
  """
  Check that dataset is in memory
  """
  def force_in_memory(self):
    if self.adata.isbacked is True:
        self.to_memory()
        
    return self
  
  """
  Set or return x values
  """
  def x_values(self, x = None, var_names = None):
    if x is None:
      return self.adata.X
    else:
      # create new dataset
      y = self.create_new_dataset(
        x,
        var = pd.DataFrame(index = var_names),
        obs = self.adata.obs
      )
      
      # set obsm
      if len(self.adata.obsm) > 0:
        y.obsm = self.adata.obsm
        
      # set obsp
      if len(self.adata.obsp) > 0:
        y.obsp = self.adata.obsp
        
      # set uns
      if len(self.adata.uns) > 0:
        y.uns = self.adata.uns
      
      # close dataset
      self.close()
      self.adata = y
      
      # set labels path
      
      return self
  
  """
  Create new adata
  """
  def create_new_dataset(self, x_values, var, obs = None):
    return ad.AnnData(x_values, var = var, obs = obs, dtype = np.float32)
  
  """
  Return adata
  """
  def as_adata(self):
    return self.adata
  
  # """
  # Return as igraph
  # """
  # def as_igraph(self):
  #   if 'spatial_connectivities' in self.adata.obsp.keys():
  #     # get indices of connectivities and convert to DF
  #     # TODO this is unweighted
  #     # g = ig.Graph.Weighted_Adjacency(
  #     return ig.Graph.Adjacency(
  #       self.adata.obsp['spatial_connectivities'].todense(),
  #       loops = False, mode = 'max'
  #     )
  
  """
  Return spatial neighbours DF
  """
  def as_spatial_connectivities(self):
    if 'spatial_connectivities' in self.adata.obsp.keys():
      if self.adata.obsp['spatial_connectivities'].count_nonzero() > 0:
        # convert nonzero entried to DF
        df_connectivities = pd.DataFrame(
          np.vstack(self.adata.obsp['spatial_connectivities'].nonzero()).T
        )
        df_connectivities.columns = ['from', 'to']
        df_distances = pd.DataFrame(self.adata.obsp['spatial_distances'][
          self.adata.obsp['spatial_connectivities'].nonzero()].T)
          
        # bind columns
        df = pd.concat([df_connectivities, df_distances], axis = 1)
        df.columns = ['from', 'to', 'dist']
      
        return df
  
  """
  Return adata as dataframe w/o obs
  """
  def as_df(self, include_x = True, add_obs = True, close = True):
    # convert adata to df
    adata_df = None
    
    if self.add_x_to_df is True and include_x is True:
      adata_df = self.adata.to_df()
    
    # add obs
    if add_obs is True:
      # drop index
      # https://stackoverflow.com/a/51710480/13766165
      # self.adata.obs.reset_index(drop = True, inplace = True)
      
      if adata_df is not None:
        # adata_df.reset_index(drop = True, inplace = True)
        # dfs_to_concat = [self.adata.obs, adata_df.reset_index(drop = True)]
        dfs_to_concat = [self.adata.obs, adata_df]
      else:
        dfs_to_concat = [self.adata.obs]
      
      # add observation measurements
      # TODO this is very specific
      if len(self.adata.obsm) > 0:
        dfs = dict()
        
        if 'X_umap' in self.adata.obsm.keys():
          dfs['X_UMAP'] = pd.DataFrame(self.adata.obsm['X_umap'])
          dfs['X_UMAP'].columns = ['UMAP_1', 'UMAP_2']
        if 'X_diffmap' in self.adata.obsm.keys():
          # take first three columns
          dfs['X_diffmap'] = pd.DataFrame(self.adata.obsm['X_diffmap']).iloc[:, 1:4]
          dfs['X_diffmap'].columns = ['DC_1', 'DC_2', 'DC_3']
          
        if self.add_centroids_to_df is True and self.has_spatial_obsm() is True:
          df_spatial = None
          df_temporal = None
          
          if 'spatial' in self.adata.obsm.keys():
            df_spatial = pd.DataFrame(self.adata.obsm['spatial'])
            df_spatial.columns = self.adata.uns['spatial_cols']
          if 'temporal' in self.adata.obsm.keys():
            df_temporal = pd.DataFrame(self.adata.obsm['temporal'])
            df_temporal.columns = self.adata.uns['temporal_cols']
          
          # concat and change order
          dfs['centroids'] = pd.concat([df_spatial, df_temporal], axis = 1)
          
          if self.centroids_order is not None:
            dfs['centroids'] = dfs['centroids'][self.centroids_order]
          
        # drop index
        # https://stackoverflow.com/a/51710480/13766165
        for i, x in dfs.items():
          # TODO not sure why dropping index does not work here
          # dfs_to_concat.append(x.reset_index(drop = True))
          x.index = self.adata.obs.index
          dfs_to_concat.append(x)
      
      adata_df = pd.concat(dfs_to_concat, axis = 1)
    
    # close connection
    if close is True:
      self.close()
    
    return adata_df
  
  """
  View columns
  """
  def view_cols(self, cols):
    # get col types
    vars_cols = [x for x in cols if x in self.col_names('vars')]
    obs_cols = [x for x in cols if x in self.col_names('obs')]
    
    # set view
    if len(vars_cols) > 0:
      self.view_vars_cols(vars_cols)
      
    if len(obs_cols) > 0:
      self.view_obs_cols(obs_cols)
      
    return self
  
  """
  View vars columns
  """
  def view_vars_cols(self, cols):
    if self.has_cols(cols):
      self.adata = self.adata[:, cols]
    
    return self
  
  """
  View obs columns
  """
  def view_obs_cols(self, cols):
    if self.has_cols(cols, dat_type = 'obs'):
      # has to be in memory
      self.force_in_memory()
      
      self.adata.obs = self.adata.obs[cols]
    
    return self
  
  """
  Exclude vars columns
  """
  def exclude_vars_cols(self, cols):
    if self.has_cols(cols):
      # get indicies
      col_idx = self.adata.var_names[~self.adata.var_names.isin(cols)]
      
      # filter data
      self.adata = self.adata[:, col_idx]
    
    return self
  
  """
  Exclude spatial temporal information
  """
  def exclude_spatial_temporal(self):
    # remove spatial and temporal information if set
    for x in ['spatial', 'temporal']:
      if x in self.adata.obsm.keys():
        self.adata.obsm.pop(x, None)
      if x in self.adata.uns.keys():
        self.adata.uns.pop(f'{x}_cols', None)
        
    return self
  
  """
  Exclude obs columns
  """
  def exclude_obs_cols(self, cols):
    if self.has_cols(cols, dat_type = 'obs'):
      # has to be in memory
      self.force_in_memory()
      
      self.adata.obs = self.adata.obs.drop(cols, axis=1)
    
    return self
  
  """
  Return vars values
  """
  def values_vars(self):
    return self.adata.X
  
  """
  Return obs values
  """
  def values_obs(self):
    return self.adata.obs
  
  """
  Return uns values
  """
  def values_uns(self):
    return self.adata.uns
  
  """
  Check where centroids are stored
  """
  def has_spatial_obsm(self):
    return 'spatial' in self.adata.obsm.keys()
  
  """
  View centroid values
  """
  def view_centroid_cols(self, order = None):
    if self.has_spatial_obsm():
      # add centroid cols to df
      self.add_centroids_to_df = True
      self.centroids_order = self.centroid_columns(order)
      
      # hide everything else
      self.add_x_to_df = False
    else:
      # TODO Deprecated this is the old way
      # view only centroid columns
      self.view_vars_cols(self.centroid_columns(order))
    
    return self
  
  """
  Exclude centroid values
  """
  def exclude_centroid_cols(self):
    if self.has_spatial_obsm():
      # add centroid cols to df
      self.add_centroids_to_df = False
      self.add_x_to_df = True
    else:
      # TODO Deprecated this is the old way
      # exclude centroid columns
      self.exclude_vars_cols(self.centroid_columns())
    
    return self
  
  """
  View channel values
  """
  def view_channel_cols(self):
    # view only channel columns
    self.view_vars_cols(self.channel_columns())
    self.exclude_centroid_cols()
    
    return self
  
  """
  View label values
  """
  def view_label_col(self):
    # view only label columns
    self.view_obs_cols(['label'])
    
    return self
  
  """
  Clear obs cols
  """  
  def clear_obs_cols(self):
    self.view_obs_cols([])
    
    return self
  
  """
  Apply filters
  """
  def filter_rows(self, filter_vals, filter_by = 'label', filter_fun = 'eq', filter_type = 'abs'):
    # correct for list
    if type(filter_by) is not list: 
      filter_by = [filter_by]
      
    if type(filter_vals) is not list: 
      filter_vals = [filter_vals]
      
    # check whether the value is obs or vars
    if self.has_cols(filter_by, dat_type = 'obs'):
      # get percentile for value
      if filter_type == 'perc':
        filter_vals = [np.percentile(self.values_obs()[filter_by[0]], filter_vals)]
      
      self.filter_by_obs(
        filter_vals = filter_vals, filter_by = filter_by, filter_fun = filter_fun
      )
    elif self.has_cols(filter_by, dat_type = 'vars'):
      # get percentile for value
      if filter_type == 'perc':
        filter_vals = [np.percentile(self.values_vars()[
          :, self.col_names('vars').index(filter_by[0])
        ], filter_vals)]
        
      self.filter_by_vars(
        filter_vals = filter_vals, filter_by = filter_by, filter_fun = filter_fun
      )
      
    return self
  
  """
  Filter vars values
  """
  def filter_by_vars(self, filter_vals, filter_by = 'area', filter_fun = 'eq'):
    self.force_in_memory()
    
    # correct for list
    if len(filter_by) == 1: 
      filter_by = filter_by[0]
    
    # filter for equal
    if filter_fun == 'eq':
      filter_idx = np.isin(self.adata[:, filter_by].X, filter_vals)
    
    # filter for not equal
    if filter_fun == 'neq':
      filter_idx = ~np.isin(self.adata[:, filter_by].X, filter_vals)
        
    # greater than
    elif filter_fun == 'gt':
      filter_idx = self.adata[:, filter_by].X > filter_vals
        
    # greater than or equal
    elif filter_fun == 'gte':
      filter_idx = self.adata[:, filter_by].X >= filter_vals
        
    # less than
    elif filter_fun == 'lt':
      filter_idx = self.adata[:, filter_by].X < filter_vals
        
    # less than or equal
    elif filter_fun == 'lte':
      filter_idx = self.adata[:, filter_by].X <= filter_vals
    
    # apply filter
    self.adata = self.adata[filter_idx]
    
    return self
  
  """
  Filter obs values
  """
  def filter_by_obs(self, filter_vals, filter_by = 'label', filter_fun = 'eq'):
    self.force_in_memory()
    
    # correct for list
    if len(filter_by) == 1: 
      filter_by = filter_by[0]
    
    # filter for equal
    if filter_fun == 'eq':
      filter_idx = self.adata.obs[filter_by].isin(filter_vals)
        
    # filter for not equal
    if filter_fun == 'neq':
      filter_idx = ~self.adata.obs[filter_by].isin(filter_vals)
        
    # greater than
    elif filter_fun == 'gt':
      filter_idx = self.adata.obs[filter_by] > filter_vals
        
    # greater than or equal
    elif filter_fun == 'gte':
      filter_idx = self.adata.obs[filter_by] >= filter_vals
        
    # less than
    elif filter_fun == 'lt':
      filter_idx = self.adata.obs[filter_by] < filter_vals
        
    # less than or equal
    elif filter_fun == 'lte':
      filter_idx = self.adata.obs[filter_by] <= filter_vals
        
    # apply filter
    self.adata = self.adata[filter_idx]
    
    return self
