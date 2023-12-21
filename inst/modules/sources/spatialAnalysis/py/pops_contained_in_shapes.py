# add CCIA modules
import sys
import os
sys.path.append('./')

import numpy as np
import pandas as pd

from py.pop_utils import PopUtils
from py.label_props_utils import LabelPropsUtils
from py.shape_utils import ShapeUtils
from py.dim_utils import DimUtils
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils

import py.script_utils as script_utils
import py.config_utils as cfg

from shapely.geometry import Point
from shapely.geometry.polygon import Polygon

# segment image
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)

  # prepare params
  task_dir = script_utils.get_param(params, 'taskDir', default = 'default')
  im_path = script_utils.get_param(params, 'imPath', default = None)
  
  # get properties
  value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
  pop_type = script_utils.get_param(params, 'popType', default = None)
  
  pops = script_utils.get_param(params, 'pops', default = [])
  shapes = script_utils.get_param(params, 'shapes', default = [])
  
  logfile_utils.log(f'>> Check whether {pops} contained in {shapes}')
  
  # load image
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = True)
  
  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)
  
  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)
  
  # get the requested populations from here
  pop_utils = PopUtils()
  pop_map = pop_utils.pop_map(task_dir, pop_type, pops = pops)
  pop_data = pop_utils.pop_data(task_dir, pop_type, pops = pops)
  
  label_props_utils = LabelPropsUtils(task_dir, value_name = value_name)
  
  # get data for shape detection
  label_view = label_props_utils.label_props_view(read_only = False)\
    .filter_by_obs([x for y in [x for x in pop_data.values()] for x in y])\
    .view_label_col()
  centroid_cols = label_view.centroid_columns()
  pop_df = label_view.as_df()
  
  # merge pops
  pop_df['pop'] = 'NONE'
  
  for i, x in pop_data.items():
      pop_df.loc[pop_df['label'].isin(x), 'pop'] = pop_map[i]['name'][0]
  
  # make categorical
  pop_df['pop'] = pop_df['pop'].astype('category')
  
  # get shapes
  shape_utils = ShapeUtils()
  shapes = shape_utils.shapes(task_dir, value_name = value_name)
  
  # adjust centroid order
  centroid_cols = [f'centroid_{x.lower()}' for x in dim_utils.im_dim_order if f'centroid_{x.lower()}' in centroid_cols]
  
  # make points from pops and match dimension order
  points = [x for x in pop_df[['label'] + centroid_cols].values.tolist()]
  points = {int(x[0]): Point(x[1:]) for x in points}
  
  # get used axis idx
  used_idx = dim_utils.used_dim_idx(ignore_channel = True)
  
  # make polygons
  polygon_shapes = dict()
  
  for i, x in shapes.items():
      polygon_shapes[i] = [Polygon(y[:, used_idx]) for y in x]
      
  # check points in polygons
  points_in_shape = {i: [] for i in polygon_shapes.keys()}

  # test whether points are in region
  for i, x in points.items():
      # go through shapes
      for j, y in polygon_shapes.items():
          if any([k.contains(x) for k in y]):
              points_in_shape[j].append(i) 
              
  # convert to dataframe
  shapes_df = pd.DataFrame(
    pop_df['label'].tolist(),
    columns = ['label_id']
    )
  
  shapes_cols = {i: f'{pop_type}.region.contained.in.{i}' for i in points_in_shape.keys()}
  
  for i, x in points_in_shape.items():
      # add shapes column
      shapes_df[shapes_cols[i]] = False
      shapes_df.loc[shapes_df['label_id'].isin(x), shapes_cols[i]] = True
  
  # drop na and sort by label id
  shapes_df.dropna(axis = 0, inplace = True)
  shapes_df.sort_values('label_id', inplace = True)
  
  # push back to labels
  labels_ids = LabelPropsUtils(task_dir, value_name = value_name).label_props_view()\
    .view_label_col()\
    .values_obs()
  
  # merge regions to labels
  merged_shapes_ids = labels_ids.join(shapes_df.set_index('label_id'), on = 'label')
  
  # set NaN to False
  merged_shapes_ids.replace(np.NaN, False, inplace = True)
  
  # get shapes ids as dict
  shapes_dict = {
      x: merged_shapes_ids[x] for x in shapes_cols.values()
  }
  
  # add to obs and save
  LabelPropsUtils(task_dir, value_name = value_name).label_props_view()\
      .add_obs(shapes_dict)\
      .save()
  
def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['pops']
  )
  
  # run
  run(params)

if __name__ == '__main__':
  main()
