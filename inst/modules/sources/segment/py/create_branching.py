# add CCIA modules
import sys
import os
sys.path.append('./')

import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
from py.dim_utils import DimUtils
from py.label_props_utils import LabelPropsUtils

import py.config_utils as cfg
import py.script_utils as script_utils
import py.slice_utils as slice_utils
import py.measure_utils as measure_utils

import py.ILEE_CSK as ILEE_CSK

import skimage.morphology
import skimage.segmentation
import skan
import numpy as np
import zarr
import shutil
import pandas as pd
import math

def run(params):
  task_dir = script_utils.get_param(params, 'taskDir')
  value_name = script_utils.get_ccia_param(params, 'value_name', default = 'default')
  branching_name = script_utils.get_param(params, 'branchingName')
  
  im_path = script_utils.get_param(params, 'imPath')
  pre_dilation_size = script_utils.get_param(params, 'preDilationSize')
  post_dilation_size = script_utils.get_param(params, 'postDilationSize')
  label_channels = script_utils.get_param(params, 'labelChannels', default = [])
  integrate_time_mode = script_utils.get_param(params, 'integrateTimeMode', default = None)
  flatten_branching = script_utils.get_param(params, 'flattenBranching', default = False)
  use_borders = script_utils.get_param(params, 'useBorders', default = False)
  calc_extended = script_utils.get_param(params, 'calcExtended', default = False)
  calc_flattened = script_utils.get_param(params, 'calcFlattened', default = False)
  aniso_radius = script_utils.get_param(params, 'anisoRadius', default = 50)
  save_meshes = script_utils.get_param(params, 'saveMeshes', default = False)
  save_props = script_utils.get_param(params, 'saveProps', default = False)
  
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
  nscales = len(labels_data)
  
  # get chunks
  labels_chunks = labels_data[0].chunks
  
  # use first pyramid
  im_data = im_data[0]
  labels_data = labels_data[0]
  
  # https://skeleton-analysis.org/stable/examples/visualizing_3d_skeletons.html
  logfile_utils.log(f'> create skeleton')
  
  # make sure that there is a time dimension for labels
  integrate_time = True if (len(im_data.shape) - 1) > len(labels_data.shape) else False
  
  # flatten 3D image for branching
  if flatten_branching and dim_utils.is_3D():
    # im_data = np.max(im_data, axis = dim_utils.dim_idx(
    #   'Z', ignore_channel = True, ignore_time = integrate_time))
    z_idx = dim_utils.dim_idx('Z', ignore_channel = True, ignore_time = integrate_time)
    labels_data_interm = np.max(labels_data, axis = z_idx)
      
    # TODO this will propagate the 2D image into 3D - otherwise
    # the following steps will be a bit confusing
    labels_data = zarr_utils.fortify(labels_data)
    
    slices = [slice(None) for _ in range(len(labels_data.shape))]
    for z in range(labels_data.shape[z_idx]):
      slices[z_idx] = z
      labels_data[tuple(slices)] = labels_data_interm
    
    logfile_utils.log(f'> flattened image {labels_data.shape}')
    
  # use label boundaries
  if use_borders:
    labels_data = skimage.segmentation.find_boundaries(labels_data)
  
  # get slices
  slices = {
    'im': slice_utils.create_slices(im_data.shape, dim_utils, ignore_time = integrate_time),
    'labels': slice_utils.create_slices(labels_data.shape, dim_utils, ignore_time = integrate_time, drop_z = True)
  }
  
  # save labels
  # TODO is there a more elegant way to do this .. ?
  skeleton_path = os.path.join(task_dir, cfg.value_dir(branching_name, 'labels'))
  
  # save as zarr
  skeleton_store = zarr.open(
    skeleton_path,
    mode = 'w',
    shape = labels_data.shape,
    chunks = labels_chunks,
    dtype = np.uint32)
  paths_tables = list()
  props_tables = list()
  ext_props_tables = list()
  ext_props_aniso = list()
  max_label = 0
  
  # go through slices
  # for i, cur_slices in enumerate(slices):
  for i in range(len(slices['im'])):
    cur_im_slices = slices['im'][i]
    cur_labels_slices = slices['labels'][i]
    
    # logfile_utils.log('>> Slice: ' + str(i + 1) + '/' + str(len(slices)))
    # logfile_utils.log(cur_im_slices)
    # logfile_utils.log(cur_labels_slices)
  
    # get image to process
    im = np.squeeze(labels_data[cur_labels_slices])
    
    logfile_utils.log(f'> dilate pre {pre_dilation_size}')
    
    if pre_dilation_size > 0:
      if dim_utils.is_3D() is True:
        pre_selem = skimage.morphology.ball(pre_dilation_size)
      else:
        pre_selem = skimage.morphology.disk(pre_dilation_size)
          
      bin_im = skimage.morphology.binary_closing((im > 0).astype(np.uint8), pre_selem)
    else:
      bin_im = np.squeeze(labels_data[cur_labels_slices]) > 0
    
    # create skeleton
    logfile_utils.log(f'> skeletonize')
    binary_skeleton = skimage.morphology.skeletonize(bin_im)
    skeleton = skan.Skeleton(binary_skeleton)
    del(binary_skeleton)
    del(bin_im)
  
    # dilate
    logfile_utils.log(f'> dilate post {post_dilation_size}')
  
    skeleton_labels = np.asarray(skeleton)
  
    if post_dilation_size > 0:
      if dim_utils.is_3D() is True:
        post_selem = skimage.morphology.ball(post_dilation_size)
      else:
        post_selem = skimage.morphology.disk(post_dilation_size)
          
      skeleton_labels = skimage.morphology.dilation(skeleton_labels, post_selem)
    
    logfile_utils.log(f'> expand dimension?')
    
    # check that shape is matching
    # TODO this has to be done better and more generic
    # if len(skeleton_labels.shape) != len(labels_data[cur_labels_slices].shape):
    while len(skeleton_labels.shape) != len(labels_data[cur_labels_slices].shape):
      logfile_utils.log(f'> {skeleton_labels.shape} v {labels_data[cur_labels_slices].shape}')

      skeleton_labels = np.expand_dims(skeleton_labels, axis = 0)
      
    # copy data
    # skeleton_store[cur_labels_slices][:] = skeleton_labels
    skeleton_store[cur_labels_slices] = skeleton_labels
    
    # create properties
    paths_tables.append(skan.summarize(skeleton))
    paths_tables[i]['path-id'] = np.arange(skeleton.n_paths)
    paths_tables[i]['label'] = np.arange(skeleton.n_paths) + 1 + max_label
    max_label = paths_tables[i]['label'].max()
    
    if dim_utils.is_timeseries() and integrate_time is False:
      # paths_tables[i]['centroid_t'] = i
      paths_tables[i]['centroid_t'] = cur_labels_slices[dim_utils.dim_idx('T', ignore_channel = True)].start
    
    # calc measure props
    if save_props is True or save_meshes is True:
      logfile_utils.log(f'> save props {save_props} v save meshes {save_meshes}')
      
      props, _, _ = measure_utils.measure_from_zarr(
        {'base': skeleton_store}, im_data, dim_utils, logfile_utils,
        task_dir = task_dir, slices = [cur_labels_slices],
        value_name = f'{value_name}.branch',
        save_meshes = save_meshes,
        extended_measures = True if dim_utils.is_3D() is True else False,
        calc_intensities = save_props,
        integrate_time = integrate_time
        )
        
      # append table information
      props_tables.append(props)
    
    # save meshes
    # if save_meshes is True:
    #   logfile_utils.log(f'> save meshes')
    #   
    #   # TODO is that too much overhead to save meshes?
    #   props_tables.append(measure_utils.measure_from_zarr(
    #     {'base': skeleton_store}, None, dim_utils, logfile_utils,
    #     task_dir = task_dir, slices = [cur_labels_slices],
    #     value_name = f'{value_name}.branch',
    #     save_meshes = save_meshes,
    #     extended_measures = True,
    #     calc_intensities = False,
    #     integrate_time = integrate_time
    #   ))
      
    # calculate extended measurements
    if calc_extended is True:
      # create shape and add time
      channels_shape = list(im.shape)
      if dim_utils.is_timeseries() and integrate_time is True:
        channels_shape.insert(dim_utils.dim_idx('T'), dim_utils.dim_val('T'))
        
      # TODO should that be different.. ?
      channels_im = np.zeros(channels_shape, dtype = np.uint32)
      
      for i in label_channels:
        channels_im = np.maximum(
          channels_im, 
          np.squeeze(np.take(
          im_data[cur_im_slices], i, axis = dim_utils.dim_idx('C')))
          )
        
      # check whether to integrate time
      if dim_utils.is_timeseries() and integrate_time is True:
        logfile_utils.log('> Average time')
        t_idx = dim_utils.dim_idx('T', ignore_channel = True)
        
        # get mode
        if integrate_time_mode == 'max':
          channels_im = np.max(channels_im, axis = t_idx)
        else:
          channels_im = np.average(channels_im, axis = t_idx)
      
      # flatten 3D image for calculation
      if calc_flattened and dim_utils.is_3D():
        channels_im = np.max(channels_im, axis = dim_utils.dim_idx(
          'Z', ignore_channel = True, ignore_time = integrate_time))
        im = np.max(im, axis = dim_utils.dim_idx(
          'Z', ignore_channel = True, ignore_time = integrate_time))
        
        logfile_utils.log(f'> flattened image {im.shape}')
      
      # get anisotropy and summary
      if not dim_utils.is_3D() or calc_flattened:
        ilee_summary, ilee_anisotropy = ILEE_CSK.analyze_actin_2d_standard(
          np.squeeze(channels_im), im,
          pixel_size = dim_utils.im_physical_size('x'),
          aniso_radius = aniso_radius,
          aniso_box_size = math.floor(aniso_radius/2),
          return_box_data = True
        )
      else:
        ilee_summary, ilee_anisotropy = ILEE_CSK.analyze_actin_3d_standard(
          np.squeeze(channels_im), im,
          dim_utils.im_physical_size('x'),
          dim_utils.im_physical_size('z'),
          # TODO this takes a long time - not sure this is necessary for our case?
          # oversampling_for_bundle = True,
          oversampling_for_bundle = False,
          pixel_size = dim_utils.im_physical_size('x'),
          aniso_radius = aniso_radius,
          aniso_box_size = math.floor(aniso_radius/2),
          return_box_data = True
        )
      
      ext_props_tables.append(ilee_summary)
      ext_props_aniso.append(ilee_anisotropy)
      
  logfile_utils.log(f'> save zarr')
  
  if nscales > 1:
    multiscales_file_path = skeleton_path + '.multiscales'

    zarr_utils.create_multiscales(
      skeleton_store, multiscales_file_path,
      dim_utils = dim_utils,
      nscales = nscales,
      keyword = 'labels',
      ignore_channel = True,
      # squeeze = True # TODO not sure if needed
      squeeze = False,
      idx_adjust = -1 if integrate_time is True else 0 
    )

    # remove previous labels and rename multiscales
    shutil.rmtree(skeleton_path)
    os.rename(multiscales_file_path, skeleton_path)
  
  # save skeleton and table
  logfile_utils.log(f'> save dataset {branching_name}')
  
  # adjust paths table
  paths_table = pd.concat(paths_tables, axis = 0, ignore_index = True)
  
  if len(props_tables) > 0:
    props_table = pd.concat(props_tables, axis = 0, ignore_index = True)
    
    # add bbox for meshes
    paths_table = paths_table.merge(
      # props_table.loc[:, props_table.columns.str.startswith(('label', 'bbox'))],
      props_table.loc[:, ~props_table.columns.str.startswith('centroid')],
      how = 'left', on = 'label')
  
  # create props
  label_view = LabelPropsUtils(task_dir, cfg.value_dir(branching_name, 'labelProps'))\
    .label_props(
      # TODO this will always drop time
      paths_table.drop('centroid_t', axis = 1) if 'centroid_t' in paths_table.columns else paths_table,
      # save = True,
      obs_cols = ['label', 'path-id', 'skeleton-id', 'node-id-src', 'node-id-dst', 'branch-type']
      # uns = uns, obsm = obsm
      # uns = {'extended': ext_props_table} if ext_props_table is not None else dict()
      )
      
  # create positions
  # locations are ZYX
  min_pos_idx = [label_view.adata.var_names.get_loc(i)
    for i in label_view.adata.var_names if i.startswith('image-coord-src-')]
  max_pos_idx = [label_view.adata.var_names.get_loc(i)
    for i in label_view.adata.var_names if i.startswith('image-coord-dst-')]
  
  # spatial_coords = np.mean(
  spatial_coords = np.median(
    np.array([
      # adjust for image scale
      # label_view.adata.X[:, min_pos_idx] * dim_utils.im_scale(['z', 'x', 'y']),
      # label_view.adata.X[:, max_pos_idx] * dim_utils.im_scale(['z', 'x', 'y'])
      label_view.adata.X[:, min_pos_idx],
      label_view.adata.X[:, max_pos_idx]
      ]), axis = 0)
  
  # add time
  if dim_utils.is_timeseries() and integrate_time is False:
    spatial_coords = np.hstack((np.vstack(paths_table['centroid_t']), spatial_coords))
  
  # get centre for coords
  # https://stackoverflow.com/a/18461943
  label_view.adata.obsm = {'spatial': spatial_coords}
  
  if dim_utils.is_timeseries() and integrate_time is False:
    if dim_utils.is_3D():
      spatial_cols = np.array(['centroid_t', 'centroid_z', 'centroid_y', 'centroid_x'])
    else:
      spatial_cols = np.array(['centroid_t', 'centroid_y', 'centroid_x'])
  else:
    if dim_utils.is_3D():
      spatial_cols = np.array(['centroid_z', 'centroid_y', 'centroid_x'])
    else:
      spatial_cols = np.array(['centroid_y', 'centroid_x'])
  
  # add extended measures
  uns = dict()
  if len(ext_props_tables) > 0:
    uns = {
      # https://github.com/scverse/anndata/issues/504
      # float64 is probably not supported
      'ilee_summary': pd.concat(ext_props_tables, axis = 0, ignore_index = True).astype(np.float32),
      # 'ilee_anisotropy': ext_props_aniso}
      'ilee_coor_list': [x[0] for x in ext_props_aniso],
      'ilee_eigval': [x[1] for x in ext_props_aniso],
      'ilee_eigvec': [x[2] for x in ext_props_aniso],
      'ilee_box_total_length': [x[3] for x in ext_props_aniso],
      'ilee_box_anisotropy': [x[4] for x in ext_props_aniso]
      }
  uns['spatial_cols'] = spatial_cols
  
  # create column identifier
  label_view.adata.uns = uns
      # 'spatial_neighbors': {
      #     'connectivities_key': 'spatial_connectivities',
      #     'distances_key': 'spatial_distances'
      # }
      
  # save props
  label_view.save(label_view.adata_filepath())
  
def main():
  # get params
  params = script_utils.script_params(
    flatten_except = ['labelChannels']
  )

  # run cellpose
  run(params)

if __name__ == '__main__':
  main()
