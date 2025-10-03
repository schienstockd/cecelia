import argparse
import json
import os

from py.logfile_utils import LogfileUtils

"""
get param
"""
def get_param(params, key, default = None):
  return params[key] if key in params else default
 
"""
get param
"""
def get_ccia_param(params, key, default = None):
  return get_param(get_ccia_params(params), key, default = default)

"""
get ccia params
"""
def get_ccia_params(params):
  if 'ccia' in params:
    return params['ccia']
  else:
    return list()

"""
return python type from string
"""
def py_type_from_string(string):
  ret_val = string
  
  # check whether the string matches any data type
  if string.lower() in ['true', 't']:
    ret_val = True
  elif string.lower() in ['false', 'f']:
    ret_val = False
    # https://www.geeksforgeeks.org/python-check-for-float-string/
  elif string.replace('.', '', 1).isdigit():
    ret_val = float(string)
  elif string.isnumeric():
    ret_val = int(string)

  return ret_val

"""
get logfile utils
"""
def get_logfile_utils(params):
  # get ccia params
  ccia_params = get_ccia_params(params)
  
  if "logfile" in ccia_params:
    logfile_utils = LogfileUtils(
      ccia_params["logfile"], create_file = not ccia_params["append_log"])
  else:
    logfile_utils = LogfileUtils()
  
  return logfile_utils

"""
get params for script
"""
def script_params(flatten_params = True, flatten_except = list(),
                  flatten_dict_except = dict()):
  # define command line options
  CLI = argparse.ArgumentParser()
  CLI.add_argument(
    "--params",  
    type = str,
    default = None
  )
  
  # parse arguments
  args = CLI.parse_args()
  
  params = None
  
  # get params
  if args.params is not None:
    if os.path.exists(args.params):
      # read params
      with open(args.params, 'r') as f:
        params = json.load(f)
        
      # delete file
      os.remove(args.params)

  # add ccia attributes
  flatten_dict_except['ccia'] = ['channel_names']
  
  # flatten?
  if flatten_params is True:
    params = flatten_params_except(params, flatten_except, flatten_dict_except)
  
  return params

"""
Flatten params
"""
def flatten_params(params, to_flatten, flatten_dict_except):
  params = params.copy()
  
  # go through flatten elements
  for x in to_flatten:
    if type(params[x]) is dict and len(params[x]) > 0 and type(list(params[x].values())[0]) is dict:
      # check which items to flatten in dict
      ignore_in_dict = list()
      if x in flatten_dict_except.keys():
        ignore_in_dict = flatten_dict_except[x]
      
      # TODO how to to this in one comprehension statement.. ?
      flattened_params = params[x].copy()
      
      for j, y in params[x].items():
        flattened_params[j].update({k:z[0] for k, z in y.items() if k not in ignore_in_dict})
    elif type(params[x]) is dict and len(params[x]) > 0 and type(list(params[x].values())[0]) is not dict:
      # TODO more elegant?
      if x in flatten_dict_except.keys():
        params[x] = {j: y[0] if j not in flatten_dict_except[x] else y for j, y in params[x].items()}
      else:
        params[x] = {j: y[0] for j, y in params[x].items()}
    elif type(params[x]) is list and len(params[x]) > 0 and type(params[x][0]) is dict:
      params[x] = [{k: z[0] for k, z in y.items()} for y in params[x]]
    elif len(params[x]) > 0:
      params[x] = params[x][0]
    
  return params

"""
Flatten params except
"""
def flatten_params_except(params, flatten_except = list(),
                          flatten_dict_except = dict()):
  return flatten_params(
    params, [x for x in params.keys() if x not in flatten_except],
    flatten_dict_except
  )
