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
import zarr
import shutil

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
  im_data, _ = zarr_utils.open_zarr(im_path, as_dask = True)
  labels_data, _ = zarr_utils.open_labels_as_zarr(
    os.path.join(task_dir, cfg.value_dir(value_name, 'labels')))
  omexml = ome_xml_utils.parse_meta(im_path)
  dim_utils = DimUtils(omexml)
  dim_utils.calc_image_dimensions(im_data[0].shape)
  
  # # https://skeleton-analysis.org/stable/examples/visualizing_3d_skeletons.html
  # logfile_utils.log(f'> create skeleton')
  # 
  # # create skeleton
  # binary_skeleton = skimage.morphology.skeletonize(
  #     (np.squeeze(labels_data[0]) > 0).astype(np.uint8))
  # skeleton = skan.Skeleton(binary_skeleton)
  # del(binary_skeleton)
  # 
  # # dilate
  # logfile_utils.log(f'> dilate {dilation_size}')
  # 
  # if dilation_size > 0:
  #   if dim_utils.is_3D() is True:
  #     skeleton_labels = skimage.morphology.dilation(
  #       np.asarray(skeleton), skimage.morphology.ball(dilation_size))
  #   else:
  #     skeleton_labels = skimage.morphology.dilation(
  #       np.asarray(skeleton), skimage.morphology.disk(dilation_size))
  # 
  # logfile_utils.log(f'> expand dimension?')
  # 
  # # check that shape is matching
  # # TODO this has to be done better and more generic
  # if len(skeleton_labels.shape) != len(labels_data[0].shape):
  #   logfile_utils.log(f'> {skeleton_labels.shape} v {labels_data[0].shape}')
  # 
  #   skeleton_labels = np.expand_dims(skeleton_labels, axis = 0)
  
  logfile_utils.log(f'> save zarr')
  
  # save labels
  # TODO is there a more elegant way to do this .. ?
  nscales = len(labels_data)
  skeleton_path = os.path.join(task_dir, cfg.value_dir(branching_name, 'labels'))
  
  # save as zarr
  skeleton_store = zarr.open(
    skeleton_path,
    # mode = 'w',
    mode = 'r',
    shape = labels_data[0].shape,
    chunks = labels_data[0].chunks,
    dtype = np.uint32)
  
  # copy data
  # skeleton_store[:] = skeleton_labels
  
  logfile_utils.log(skeleton_store.shape)
  logfile_utils.log(dim_utils.dim_idx('X', ignore_channel = True, squeeze = True))
  logfile_utils.log(dim_utils.dim_idx('X', ignore_channel = False, squeeze = True))
  logfile_utils.log(dim_utils.dim_idx('X', ignore_channel = True, squeeze = False))
  logfile_utils.log(dim_utils.dim_idx('X', ignore_channel = False, squeeze = False))
  
  if nscales > 1:
    multiscales_file_path = skeleton_path + '.multiscales'

    zarr_utils.create_multiscales(
      skeleton_store, multiscales_file_path,
      dim_utils = dim_utils,
      nscales = nscales,
      keyword = 'labels',
      ignore_channel = True,
      squeeze = True
    )

    # remove previous labels and rename multiscales
    shutil.rmtree(skeleton_path)
    os.rename(multiscales_file_path, skeleton_path)
  
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
      obs_cols = ['skeleton-id', 'node-id-src', 'node-id-dst', 'branch-type']
      )
      
def main():
  # get params
  params = script_utils.script_params(
  )

  # run cellpose
  run(params)

if __name__ == "__main__":
  main()
