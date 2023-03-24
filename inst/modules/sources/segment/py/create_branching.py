# add CCIA modules
import sys
import os
sys.path.append("./")

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
from py.label_props_utils import LabelPropsUtils

import py.config_utils as cfg
import py.script_utils as script_utils

import skimage.morphology
import skan
import numpy as np

def run(params):
  task_dir = script_utils.get_param(params, 'taskDir')
  value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
  branching_name = script_utils.get_param(params, 'branchingName')
  
  im_path = script_utils.get_param(params, 'imPath')
  dilation_size = script_utils.get_param(params, 'dilationSize')
  
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  logfile_utils.log(f'>> open dataset {value_name}')
    
  # get data and meta data
  im_data, _ = zarr_utils.open_zarr(im_path)
  labels_data, _ = zarr_utils.open_labels_as_zarr(
    os.path.join(task_dir, cfg.value_dir(value_name, 'labels')))
  omexml = ome_xml_utils.parse_meta(im_path)
  dim_utils = DimUtils(omexml)
  dim_utils.calc_image_dimensions(im_data[0].shape)
  
  # https://skeleton-analysis.org/stable/examples/visualizing_3d_skeletons.html
  logfile_utils.log(f'> create skeleton')
  
  skeleton = skan.Skeleton(
    skimage.morphology.skeletonize(
      (np.squeeze(labels_data[0]) > 0).astype(np.uint8))
  )
  
  # dilate
  logfile_utils.log(f'> dilate {dilation_size}')  
  
  # TODO this means the whole image will be in memory
  binary_skeleton = skimage.morphology.skeletonize(
    (np.squeeze(labels_data[0]) > 0).astype(np.uint8))
  skeleton = skan.Skeleton(binary_skeleton)
  
  skeleton_labels = skimage.morphology.dilation(
    np.asarray(skeleton), skimage.morphology.ball(dilation_size))
    
  logfile_utils.log(f'> expand dimension?')  
  
  # check that shape is matching
  # TODO this has to be done better and more generic
  if len(skeleton_labels.shape) != labels_data[0].shape:
    logfile_utils.log(f'> {skeleton_labels.shape} v {labels_data[0].shape}') 
    
    skeleton_labels = np.expand_dims(skeleton_labels, axis = 0)
  
  logfile_utils.log(f'> save zarr')
  
  # save labels
  zarr_utils.create_multiscales(
    skeleton_labels,
    cfg.value_dir(branching_name, 'labels'),
    dim_utils = dim_utils,
    nscales = len(labels_data),
    keyword = 'labels',
    ignore_channel = True,
    reference_zarr = labels_data[0]
  )
  
  # create properties
  paths_table = skan.summarize(skeleton)
  paths_table['label'] = np.arange(skeleton.n_paths)
  
  # save skeleton and table
  logfile_utils.log(f'> save dataset {branching_name}')
  
  # save props
  LabelPropsUtils(task_dir, cfg.value_dir(branching_name, 'labelProps'))\
    .label_props(
      paths_table,
      save = True,
      obs_cols = [
        'skeleton-id', 'node-id-src', 'node-id-dst', 'branch-type'
        ]
      )
      
def main():
  # get params
  params = script_utils.script_params(
  )

  # run cellpose
  run(params)

if __name__ == "__main__":
  main()
