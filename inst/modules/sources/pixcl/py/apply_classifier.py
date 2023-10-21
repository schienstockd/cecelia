# add CCIA modules
import sys
import os
sys.path.append("./")

import numpy as np

from apoc import PixelClassifier
import py.zarr_utils as zarr_utils
import py.ome_xml_utils as ome_xml_utils
import py.correction_utils as correction_utils
import py.measure_utils as measure_utils
from py.dim_utils import DimUtils
from py.label_props_utils import LabelPropsUtils

import scipy.ndimage
import skimage.morphology
import skimage.measure

import py.config_utils as cfg
import py.script_utils as script_utils

# generate training image
def run(params):
  # logging
  logfile_utils = script_utils.get_logfile_utils(params)
  
  # get params
  task_dir = script_utils.get_param(params, 'taskDir')
  im_path = script_utils.get_param(params, 'imPath')
  cl_mapping = script_utils.get_param(params, 'clMapping', default = [])
  normalise_image = script_utils.get_param(params, 'normaliseImage', default = True)
  norm_percentile = script_utils.get_param(params, 'normPercentile', default = 99.98)
  save_meshes = script_utils.get_param(params, 'saveMeshes', default = False)
  extended_measures = script_utils.get_param(params, 'extendedMeasures', default = False)
  min_object_size = script_utils.get_param(params, 'minObjectSize', default = 10)
  
  logfile_utils.log(f'>> Start classification {cl_mapping}')
  
  # get image information
  im_dat, zarr_group_info = zarr_utils.open_as_zarr(im_path, as_dask = False)
  
  # get OME-XML
  omexml = ome_xml_utils.parse_meta(im_path)
  
  # create dim utils for image
  dim_utils = DimUtils(omexml, use_channel_axis = True)
  dim_utils.calc_image_dimensions(im_dat[0].shape)
  
  # fortify
  im = zarr_utils.fortify(im_dat[0])
  
  # normalise channels
  if normalise_image is True:
    im = correction_utils.normalise_channels(
      im,
      dim_utils = dim_utils,
      norm_percentile = norm_percentile
    )
  
  # go through all classification mappers
  for i, x in cl_mapping.items():
    # get parameters
    cl_channels = x['clChannels']
    cl_path = x['clPath'][0]
    
    # load channels for classification
    images = list()
    
    # ensure that channels are in ascending order
    cl_channels.sort()
    for x in cl_channels:
      slices = [slice(None) for _ in range(len(dim_utils.im_dim))]
      slices[dim_utils.dim_idx('C')] = x
      
      images.append(
        np.squeeze(zarr_utils.fortify(im[tuple(slices)]))
      )
      
    # predict labels
    cl_segmenter = PixelClassifier(opencl_filename = cl_path)
    cl_result = cl_segmenter.predict(image = images)
    
    # # post-process results
    # TODO this does not make sense when there are multiple cl classes
    # if closing_filter > 0:
    #   cl_result = skimage.morphology.closing(cl_result, selem = skimage.morphology.disk(closing_filter))
    #   
    # if median_filter > 0:
    #   cl_result = scipy.ndimage.median_filter(cl_result, median_filter)
    
    # expand results for zarr creation
    cl_results_padded = np.zeros(
      dim_utils.dim_vals(ignore_channel = True),
      dtype = cl_result.dtype
    )
    
    # prepare zarr for labels
    cl_result_labels = dict()
    cl_result_labels['base'], _ = zarr_utils.create_zarr_from_ndarray(
      cl_results_padded, dim_utils, im_dat[0],
      copy_values = False, ignore_channel = True)
    
    # add fixed slices for '1'
    slices = tuple([slice(None) if x != 1 else 0 for x in dim_utils.dim_vals(ignore_channel = True)])
    
    # create labels from images
    # consider '1' as background
    # There should not be any 0 values
    cl_result_labels['base'][slices] = skimage.measure.label(cl_result, background = 1)
    
    # remove small objects?
    if min_object_size > 0:
      cl_result_labels['base'][slices] = skimage.morphology.remove_small_objects(
        cl_result_labels['base'][slices], min_object_size
      )
      
    # get value name
    value_name = os.path.basename(cl_path)
    labels_filename = cfg.value_dir(value_name, 'labels')
    labels_props_filename = cfg.value_dir(value_name, 'labelProps')
    
    # measure labels
    props = measure_utils.measure_from_zarr(
      cl_result_labels, im_dat[0], dim_utils, logfile_utils,
      block_size = -1,
      overlap = -1,
      context = 0,
      clear_touching_border = False,
      clear_depth = False,
      task_dir = task_dir,
      value_name = value_name,
      save_meshes = save_meshes,
      extended_measures = extended_measures
      )
      
    # assign classifier classes to labels
    # ie/ every label will have a unique id and the classifier class
    props['clsf'] = 0
    
    for x in np.unique(cl_result):
      # binarise and multiply
      cl_label_ids = np.unique(np.array(cl_result == x) * cl_result_labels['base'])
      
      # add to props
      props.loc[props['label'].isin(cl_label_ids), 'clsf'] = x
      
    # get spatial and temporal columns from props
    centroid_spatial = [x for x in props.columns if x in [f'centroid_{i}' for i in ['x', 'y', 'z']]]
    centroid_temporal = [x for x in props.columns if x == 'centroid_t']
    
    obsm = dict()
    uns = dict()
    
    # split spatial and temporal information into obsm
    # this will then allow processing with squidpy
    if len(centroid_spatial) > 0:
      uns['spatial_cols'] = centroid_spatial
      obsm['spatial'] = props[centroid_spatial].to_numpy()
      
    if len(centroid_temporal) > 0:
      uns['temporal_cols'] = centroid_temporal
      obsm['temporal'] = props[centroid_temporal].to_numpy()
    
    # save props
    LabelPropsUtils(task_dir, labels_props_filename)\
      .label_props(
        props[[x for x in props.columns if x not in centroid_spatial + centroid_temporal]],
        save = True,
        obsm = obsm, uns = uns
        )
    
    # save labels
    zarr_utils.create_multiscales(
      cl_result_labels['base'],
      os.path.join(task_dir, labels_filename),
      dim_utils = dim_utils,
      nscales = len(im_dat),
      ignore_channel = True,
      reference_zarr = im_dat[0],
      keyword = "labels"
    )

def main():
  # get params
  params = script_utils.script_params(
  	flatten_except = ['clMapping']
  )

  # run main function
  run(params)

if __name__ == '__main__':
  main()
