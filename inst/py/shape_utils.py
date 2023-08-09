import numpy as np
import os
import glob
import json

from napari_builtins.io import csv_to_layer_data
from pathlib import Path
import py.config_utils as cfg

class ShapeUtils:
  def __init__(self):
    # shape directory and type
    self.__shape_dir = None
    self.__shape_type = None
    self.__shape_value_name = None
    
  """
  return shapes dir
  """
  def shapes_dir(self, task_dir, value_name = 'default'):
    return os.path.join(
      task_dir,
      cfg.data['dirs']['tasks']['shapes'],
      value_name
      )  
  
  """
  return shapes
  """
  def shapes(self, task_dir, shape_type = 'region', value_name = 'default'):
    # TODO this is very naive
    # get shapes directory
    shapes_dir = self.shapes_dir(task_dir, value_name = value_name)
    
    # go through directory and return shape data
    shape_files = glob.glob(os.path.join(shapes_dir, '*{}'.format(cfg.data['files']['ext']['shapes'])))
    
    shape_data = dict()
    for x in shape_files:
      shape_data[Path(x).name.replace('.csv', '')], *_ = csv_to_layer_data(x)
    
    return shape_data
