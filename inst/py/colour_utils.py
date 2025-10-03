import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

"""
Get distinct colours
"""
def distinct_colours(num_colours, as_dict = False, as_list = False,
                     as_hex = True):
  # https://stackoverflow.com/a/48309072/13766165
  # get default matplotlib styles back
  sns.reset_orig()  
  
  # a list of RGB tuples
  clrs = sns.color_palette('husl', n_colors = num_colours)  
  
  # convert to hex?
  if as_hex is True:
    clrs = [matplotlib.colors.to_hex(x) for x in clrs]
  
  # convert to dict
  if as_dict is True:
    clrs = {int(i): x for i, x in enumerate(clrs)}
    
  # convert to list
  if as_list is True:
    clrs = [x for x in clrs]
  
  return clrs
