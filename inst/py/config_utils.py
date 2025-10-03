import yaml
import os

from copy import deepcopy

# merge dict of dicts
# adapted from https://stackoverflow.com/a/26853961
def dict_of_dicts_merge(x, y):
  z = {}
  
  # get overlapping keys
  overlapping_keys = x.keys() & y.keys()
  
  # go through keys that overlap
  for key in overlapping_keys:
    if isinstance(x[key], dict):
      z[key] = dict_of_dicts_merge(x[key], y[key])
    else:
      # overwrite x with y
      z[key] = y[key]
    
  # go through non overlapping keys
  for key in x.keys() - overlapping_keys:
    z[key] = deepcopy(x[key])
  for key in y.keys() - overlapping_keys:
    z[key] = deepcopy(y[key])
    
  return z

# load config
with open("config.yml", "r") as yamlfile:
  data = yaml.load(yamlfile, Loader=yaml.FullLoader)
    
  # load default
  data = data['default']
  
  # check if there is a custom config
  if os.path.exists("custom.yml"):
    with open("custom.yml", "r") as custom_yamlfile:
      custom_data = yaml.load(custom_yamlfile, Loader=yaml.FullLoader)
      
      # load default
      custom_data = custom_data['default']
      
      # copy everything
      # this is a shallow copy
      # data = {**data, **custom_data}
      data = dict_of_dicts_merge(data, custom_data)
  
# return function to convert value names
# to the processing paths
def value_dir(value_name, task_name, dir_only = False,
              value_as_dir = False, file_name = 'default'):
  if value_as_dir is True:
    if dir_only is True:
      return os.path.join(data['dirs']['tasks'][task_name], value_name)
    else:
      return os.path.join(
        data['dirs']['tasks'][task_name],
        value_name, str(file_name) + data['files']['ext'][task_name]
        )
  else:
    return os.path.join(
      data['dirs']['tasks'][task_name],
      value_name + data['files']['ext'][task_name]
      )
