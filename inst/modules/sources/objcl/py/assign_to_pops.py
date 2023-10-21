# add CCIA modules
import sys
import os
sys.path.append("./")

import numpy as np
import pandas as pd

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
from py.label_props_utils import LabelPropsUtils
from py.pop_utils import PopUtils

import py.config_utils as cfg
import py.script_utils as script_utils

# generate training image
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  task_dir = script_utils.get_param(params, 'taskDir')
  im_path = script_utils.get_param(params, 'imPath')
  
  min_clsf_value = script_utils.get_param(params, 'minClsfValue', default = 0)
  assign_method = script_utils.get_param(params, 'assignMethod', default = 'area')
  pops = script_utils.get_param(params, 'pops', default = [])
  pop_type = script_utils.get_param(params, 'popType', default = None)
  # TODO this will exclude live image analysis for now
  value_name = script_utils.get_param(params, 'valueName', default = None)
  clsf_pops = script_utils.get_param(params, 'clsfPops', default = None)
  pop_label_paths = script_utils.get_param(params, 'popLabelPaths', default = dict())
  clsf_label_paths = script_utils.get_param(params, 'clsfLabelPaths', default = dict())
  
  logfile_utils.log(f'>> Start assigning {clsf_pops} to {pops}')
  
  # get image information
  im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask = False)
  
  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)
  
  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)
  
  logfile_utils.log(pop_label_paths)
  
  # open pop label images
  pop_labels = {
    i: zarr_utils.open_labels_as_zarr(x)[0][0] for i, x in pop_label_paths.items()
  }
  
  # open clsf label images
  clsf_labels = {
    i: zarr_utils.open_labels_as_zarr(x)[0][0] for i, x in clsf_label_paths.items()
  }
  
  # get classifier props
  # init pop utils
  pop_utils = PopUtils()
  
  # get props
  clsf_df = pop_utils.pop_df(
    task_dir,
    LabelPropsUtils(task_dir),
    'clsf',
    cols = ['clsf'],
    pops = clsf_pops
  )
  
  pop_df = pop_utils.pop_df(
    task_dir,
    LabelPropsUtils(task_dir),
    pop_type,
    cols = ['label', 'value_name'],
    pops = pops
  )
  
  clsf_value_counts = dict()

  # fortify pop labels
  # TODO this will not work for big images
  # you will need to make slices
  y = zarr_utils.fortify(pop_labels[value_name])
  
  # get counts of pop and clsf combinations
  # go through classfier pops
  for i in clsf_pops:
    logfile_utils.log(f'> Match {i}')
    
    # get labels
    x = zarr_utils.fortify(
      clsf_labels[i.split('/', 1)[0]]
    )
    
    clsf_value_counts[i] = dict()
    
    # label ids
    label_ids = list(clsf_df.loc[clsf_df['pop'] == i, 'label'])
    
    # binarise and multiply
    label_counts = np.unique(
      np.isin(x, label_ids).astype(np.uint8) * y,
      return_counts = True)
    
    clsf_value_counts[i] = dict(zip(
      label_counts[0], label_counts[1]
    ))
    
    # exclude 0
    clsf_value_counts[i] = {i: x for i, x in clsf_value_counts[i].items() if i != 0}
  
  # get label props
  # TODO this only works for static at the moment
  label_props_utils = LabelPropsUtils(task_dir, value_name = value_name)
  
  # save back to labels
  labels_ids = label_props_utils.label_props_view()\
    .view_label_col()\
    .values_obs()
  
  # go through classifier pops
  clsf_value_dfs = list()
  
  value_cols = list()
  bool_cols = list()
  
  # create dataframe
  for i in clsf_pops:
    # define column
    value_cols.append(f'{pop_type}.cell.cl.value#clsf.{i}')
    bool_cols.append(f'{pop_type}.cell.cl.bool#clsf.{i}')
    
    # convert to dataframe
    clsf_value_dfs.append(pd.DataFrame.from_dict({
      'label_id': clsf_value_counts[i].keys(),
      value_cols[-1]: clsf_value_counts[i].values(),
      bool_cols[-1]: [x >= min_clsf_value for x in clsf_value_counts[i].values()]
    }))
  clsf_value_df = pd.concat(clsf_value_dfs)
  
  # merge to labels
  merged_ids = labels_ids.join(clsf_value_df.set_index('label_id'), on = 'label')
  
  # set NaN to False
  # TODO oes this not work when using cols as index?
  for x in value_cols:
    merged_ids[x].replace(np.NaN, -1, inplace = True)
  for x in bool_cols:
    merged_ids[x].replace(np.NaN, False, inplace = True)
    
  # convert column to dict
  value_dict = merged_ids[[x for x in merged_ids.columns if x != 'label']].to_dict('list')
  
  logfile_utils.log(f'> save back')
  
  # add to obs and save
  label_props_utils.label_props_view()\
    .add_obs(value_dict)\
    .save()

def main():
  # get params
  params = script_utils.script_params(
  	flatten_except = ['pops', 'clsfPops']
  )

  # run main function
  run(params)

if __name__ == '__main__':
  main()
