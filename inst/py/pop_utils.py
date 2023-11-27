import numpy as np
import os
import json
import pandas as pd
from functools import reduce

from py.label_props_utils import LabelPropsUtils
import py.config_utils as cfg

class PopUtils:
  def __init__(self):
    # population directory and type
    self.__pop_dir = None
    self.__pop_type = None
    
    # population data
    self.__pop_data = dict()
    self.__pop_data_mtimes = dict()
    
    # population mapping
    self.__pop_map = None
    self.__pop_map_mtime = None
    
  """
  pop map filepath
  """
  def pop_map_filepath(self):
    return os.path.join(self.__pop_dir, self.__pop_type + ".json")
  
  """
  pop data filepath
  """
  def pop_data_filepath(self, pop_id):
    return os.path.join(
      self.__pop_dir, self.__pop_type, pop_id + ".csv")
  
  """
  pop map changed?
  """
  def changed_pop_map(self):
    file_changed = False
    
    pop_map_file = self.pop_map_filepath()
    
    # check modification time
    pop_map_mtime = os.path.getmtime(pop_map_file)
    
    if self.__pop_map_mtime != pop_map_mtime:
      self.__pop_map_mtime = pop_map_mtime
      
      file_changed = True
      
    return file_changed
  
  """
  pop data changed?
  """
  def changed_pop_data(self, pop_id):
    file_changed = False
    
    pop_dat_file = self.pop_data_filepath(pop_id)
    
    # init modification time
    if pop_id not in self.__pop_data_mtimes.keys():
      self.__pop_data_mtimes[pop_id] = 0
    
    # check modification time
    if os.path.exists(pop_dat_file):
      pop_dat_mtime = os.path.getmtime(pop_dat_file)
      
      if self.__pop_data_mtimes[pop_id] != pop_dat_mtime:
        self.__pop_data_mtimes[pop_id] = pop_dat_mtime
      
        file_changed = True
      
    return file_changed
  
  """
  check whether config has changed
  """
  def changed_pop_conf(self, pop_dir, pop_type):
    conf_changed = False
    
    # check whether directory changed
    if self.__pop_dir != pop_dir:
      self.__pop_dir = pop_dir
      conf_changed = True
      
    # check whether type changed
    if self.__pop_type != pop_type:
      self.__pop_type = pop_type
      conf_changed = True
      
    # reset data modification times
    if conf_changed is True:
      self.__pop_data = dict()
      self.__pop_data_mtimes = dict()
    
    return conf_changed
  
  """
  return pop map
  """
  def pop_map(self, task_dir, pop_type, pops = list()):
    # get population directory
    pop_dir = self.pop_dir(task_dir)
    
    # check config
    reload_pop_map = self.changed_pop_conf(pop_dir, pop_type)
    
    # get population map
    if self.changed_pop_map():
      reload_pop_map = True
      
    if reload_pop_map is True:
      # load pop map
      with open(self.pop_map_filepath(), 'r') as f:
        self.__pop_map = json.load(f)
    
    # return only populations matching the paths
    if len(pops) > 0:
      return {i: x for i, x in self.__pop_map.items() if x['path'][0] in pops}
    else:
      return self.__pop_map
    
  """
  return pop data
  """
  def pop_data(self, task_dir, pop_type, pops = list()):
    # get pop map
    pop_map = self.pop_map(task_dir, pop_type, pops = pops)
    
    # get population directory
    pop_dir = self.pop_dir(task_dir)
    
    # check config
    reload_all = self.changed_pop_conf(pop_dir, pop_type)
    reload_pop_data = list()
    
    # check which populations have changed
    if reload_all is False:
      # go through populations
      for i in pop_map.keys():
        # has the population changed?
        if self.changed_pop_data(i):
          reload_pop_data.append(i)
    else:
      reload_pop_data = pop_map.keys()
    
    # load populations
    for i in reload_pop_data:
      if os.path.exists(self.pop_data_filepath(i)):
        pop_df = pd.read_csv(self.pop_data_filepath(i)).iloc[:,0]
        self.__pop_data[i] = list(pop_df)
    
    # return only populations matching the paths
    if len(pops) > 0:
      return {
        i: x for i, x in self.__pop_data.items() \
          if i in pop_map.keys() and pop_map[i]['path'][0] in pops
      }
    else:
      return self.__pop_data
  
  """
  return population dir
  """
  def pop_dir(self, task_dir):
    return os.path.join(
      task_dir,
      cfg.data['dirs']['tasks']['populations']
      )
  
  """
  return population dataframe
  """
  def pop_df(self, task_dir, label_props_utils, pop_type, pops = list(), cols = list(),
             unique_labels = True, value_name = None, invert = False):
    # TODO add root population to get DF for classifications
    if pop_type == 'clsf':
      root_pops = list(set([x.split('/', 1)[0] for x in pops]))
      pops = pops.copy() + root_pops
               
    # get data for populations
    pop_map = self.pop_map(task_dir, pop_type, pops = pops)
    pop_data = self.pop_data(task_dir, pop_type)
    
    # get population dfs
    pop_dfs = list()
    
    # add base population
    # TODO too specific
    base_pop = [x for x in pops if '/' not in x] if pop_type not in ['clust'] else []
    
    if len(base_pop) > 0:
      base_pop = base_pop[0]
      label_view = label_props_utils.label_props_view(value_name = base_pop)
      
      if len(cols) > 0:
        label_view.view_cols(cols = cols)
      
      # create population df
      pop_df = label_view.as_df()
      label_view.close()
      
      # add value name and population
      pop_df['value_name'] = base_pop
      pop_df['pop'] = base_pop
      
      # add to list
      pop_dfs.append(pop_df)
    
    for i, pop in pop_map.items():
      # check if population has cells
      if i in pop_data.keys():
        x = pop_data[i]
        
        pop_value_name = pop['valueName'][0] if value_name is None else value_name
        pop_path = pop['path'][0]
        
        # get dataframe for population
        label_view = label_props_utils.label_props_view(value_name = pop_value_name)
        
        if len(cols) > 0:
          label_view.view_cols(cols = cols)
          
        # create population df
        pop_df = label_view.filter_by_obs(
          x, filter_fun = 'eq' if invert is False else 'neq').as_df()
        label_view.close()
        
        # add value name and population
        pop_df['value_name'] = pop_value_name
        pop_df['pop'] = pop_path
        
        # add to list
        pop_dfs.append(pop_df)
        
    # return concat dataframe
    # TODO this will be redundant
    # find a way to merge tables together in
    # a pythonic way
    if len(pop_dfs) == 1 or unique_labels is False:
      return pd.concat(pop_dfs)
    elif len(pop_dfs) > 0:
      # merge dataframes
      # https://stackoverflow.com/a/38089112/13766165
      def reduce_dfs(x, y):
        # merge
        merged_df = pd.merge(
            x, y[['value_name', 'label', 'pop']],
            how = 'left', on = ['value_name', 'label'])
            
        # correct pop names
        idx_na_y = pd.isna(merged_df['pop_y'])
        
        merged_df.loc[idx_na_y, 'pop_y'] = merged_df.loc[idx_na_y, 'pop_x']
        merged_df['pop'] = merged_df['pop_y']
        merged_df.drop(['pop_x', 'pop_y'], axis = 1, inplace = True)
        
        return merged_df
            
      # return
      return reduce(lambda x, y: reduce_dfs(x, y), pop_dfs)
    else:
      return None
        
