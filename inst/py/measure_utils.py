from skimage import filters, measure, morphology, transform
from skimage.segmentation import clear_border
import pandas as pd
import numpy as np
from scipy import stats
import math
import os
import glob

from time import sleep
from tqdm import tqdm

import py.slice_utils as slice_utils
import py.morpho_utils as morpho_utils
import py.zarr_utils as zarr_utils
from skimage.morphology import remove_small_objects, convex_hull_image

import py.script_utils as script_utils
import py.config_utils as cfg

"""
Convert coordinates to physical units
"""
def convert_pixel_to_physical(df, omexml = None, im_res = dict()):
  if omexml is not None:
    # TODO use when ome_type 0.21 is released 
    # im_res['x'] = omexml.images[0].pixels.physical_size_x
    # im_res['y'] = omexml.images[0].pixels.physical_size_y
    # im_res['z'] = omexml.images[0].pixels.physical_size_z
    im_res['x'] = omexml.image().Pixels.get_PhysicalSizeX()
    im_res['y'] = omexml.image().Pixels.get_PhysicalSizeY()
    im_res['z'] = omexml.image().Pixels.get_PhysicalSizeZ()
    
    # account for None
    im_res = [x if x is not None else 1 for x in im_res]
  
  if len(im_res) > 0:
    # convert xy and then z
    df['centroid_x'] *= im_res['x']
    df['centroid_y'] *= im_res['y']
    
    if 'centroid_z' in df.columns:
      df['centroid_z'] *= im_res['z']

"""
Get labels from slice
"""
def get_labels_from_slice(cur_slices, labels, im_dat, dim_utils,
                          context = 128, clear_borders = True,
                          clear_touching_border = True, clear_depth = True):
  # add channel to slices for data
  dat_slices = list(cur_slices)
  dat_slices.insert(dim_utils.dim_idx('C'), slice(None))
  dat_slices = tuple(dat_slices)
  cur_im_dat = im_dat[dat_slices]

  # swap channel axis to last position
  new_order = dim_utils.im_dim_order.copy()
  new_order.append(new_order.pop(new_order.index('C')))
  cur_im_dat = dim_utils.transpose_array_axis(
      cur_im_dat,
      dim_utils.im_dim_order,
      new_order)
  
  cur_labels = {
    i: x.oindex[cur_slices] for i, x in labels.items()
    }
  
  # squeeze dimensions for timeseries only?
  # if dim_utils.is_timeseries() is True:
  cur_labels = {i: np.squeeze(x) for i, x in cur_labels.items()}
  cur_im_dat = np.squeeze(cur_im_dat)
  
  # remove border labels
  cur_labels = {
    i: clear_border_labels(x, dim_utils,
        context = context, clear_borders = clear_borders,
        clear_touching_border = clear_touching_border,
        clear_depth = clear_depth
      ) for i, x in cur_labels.items()
    }
  
  return cur_labels, cur_im_dat

"""
Rank labels
"""
def rank_labels(labels, dtype = None):
  labels_ranked = stats.rankdata(labels, method = 'dense', axis = None)
  
  # ranks start at 1 ie/ 0 = 1
  labels_ranked = labels_ranked - 1
  labels_ranked = np.reshape(labels_ranked, labels.shape)
  
  # get dtype
  if dtype is None:
    dtype = labels.dtype
  
  labels_ranked.astype(dtype)
    
  return labels_ranked

"""
Adapted clear borders for image
"""
def clear_border_labels(labels, dim_utils, context = 1,
                        clear_borders = True, clear_depth = True,
                        clear_touching_border = True):
  ret_labels = labels
  
  # get depth axis
  depth_axis = dim_utils.dim_idx(
    "Z", ignore_channel = True,
    ignore_time = True, default_order = False)
  
  # set context
  context = context if True in (clear_borders, clear_touching_border) else 1
  
  # set touching border
  # this only applies to images that have no context
  clear_touching_border = clear_touching_border if context == 1 else True
  
  # remove border items
  if clear_touching_border is True:
    if dim_utils.is_3D():
      # clear borders
      ret_labels = clear_border_applied(
        labels, context = context,
        depth_axis = depth_axis,
        clear_depth = clear_depth)
    else:
      # squeeze
      labels = np.squeeze(labels)
      
      ret_labels = clear_border_applied(labels, context = context)
    
  return ret_labels

"""
Adapted clear borders
"""
def clear_border_applied(labels, context = 1, depth_axis = None, clear_depth = True):
  ret_labels = np.zeros_like(labels)
  
  if depth_axis is None:
    ret_labels = clear_border(labels, buffer_size = context)
  else:
    # adapt for depth and create a mask
    context_mask = np.ones_like(labels).astype(bool)
    
    # dimensions to include
    dim_range = list(range(labels.ndim))
    
    # get range without depth
    if depth_axis is not None and clear_depth is False:
      dim_range.pop(depth_axis)
    
    # go through axis and fill mask
    for x in dim_range:
      clear_context = context
      
      # only clear touching objects for z
      if x == depth_axis:
        clear_context = 1
      
      # prepare slices
      slices_min = [slice(None) for i in range(labels.ndim)]
      slices_max = [slice(None) for i in range(labels.ndim)]
      
      # clear borders
      slices_min[x] = slice(0, clear_context, 1)
      slices_max[x] = slice(labels.shape[x] - clear_context, labels.shape[x], 1)
      
      context_mask[tuple(slices_min)] = 0
      context_mask[tuple(slices_max)] = 0
      
    # apply mask
    ret_labels = clear_border(labels, mask = context_mask)
    
  return ret_labels
  
"""
Convert property to image scale
"""
def convert_to_im_scale(prop, dim_utils, ignore_z = False):
  # check whether 3D
  if dim_utils.is_3D() is True:
    pixel_size = (
      dim_utils.im_physical_size('x') *
      dim_utils.im_physical_size('y')
      )
    
    if ignore_z is True:
      pixel_size *= dim_utils.im_physical_size('x')
    else:
      pixel_size *= dim_utils.im_physical_size('z')
      
    scaled_prop = prop * pixel_size
  else:
    pixel_size = (
      dim_utils.im_physical_size('x') *
      dim_utils.im_physical_size('y')
      )
    
    scaled_prop = prop * pixel_size
      
  return scaled_prop

"""
Measure properties from Zarr labels and image
"""
def measure_from_zarr(labels, im_dat, dim_utils, logfile_utils, task_dir, value_name,
  block_size = None, overlap = None, context = None, gaussian_sigma = 1,
  clear_touching_border = True, clear_depth = True, timepoints = None, save_meshes = False,
  extended_measures = False):
  # define base labels
  base_labels = 'base'
  labels_mode = 'default'
  
  if len(labels.keys()) > 0:
    # cytoplasm and nuclei
    if all([x in labels.keys() for x in ['nuc', 'cyto']]):
      labels_mode = 'nuc_cyto'
      
  # get slices
  slices = slice_utils.create_slices(
    labels[base_labels].shape, dim_utils, block_size, overlap,
    timepoints = timepoints)
  
  props = list()
  
  # get centroid indicies with regards to image dimension
  centroid_idx = {x: dim_utils.dim_idx(
    x, ignore_channel = True, ignore_time = True, squeeze = True) for x in ('X', 'Y', 'Z')}
  centroid_idx = {i: x for i, x in centroid_idx.items() if x is not None}
  
  # reverse lookup for centroid and slice indicies
  slice_idx = {
    dim_utils.dim_idx(
      x, ignore_channel = True, ignore_time = True, squeeze = True
      ): dim_utils.dim_idx(x, ignore_channel = True, ignore_time = False
      ) for x in ('X', 'Y', 'Z')
    }
    
  # get image scale
  im_scale = dim_utils.im_scale(dims = ['X', 'Y', 'Z'])
  
  # create directory for meshes
  # TODO this does only work if there is only one label file
  if save_meshes is True:
    mesh_dir = os.path.join(
      task_dir,
      cfg.value_dir(value_name, 'mesh', value_as_dir = True, dir_only = True)
    )
    
    # create or remove existing meshes
    if os.path.exists(mesh_dir) is False:
      os.mkdir(mesh_dir)
    else:
      # https://stackoverflow.com/a/32834943/13766165
      for meshfile in glob.iglob(os.path.join(mesh_dir, '*' + cfg.data['files']['ext']['mesh'])):
        os.remove(meshfile)
  
  # go through slices
  for i, cur_slices in enumerate(slices):
    logfile_utils.log(">> Slice: " + str(i + 1) + "/" + str(len(slices)))
    logfile_utils.log(cur_slices)
    
    # clear borders only if image is chunked
    if dim_utils.is_timeseries() is True:
      clear_borders = True if len(slices) > dim_utils.dim_val('T') else False
    else:
      clear_borders = True if len(slices) > 1 else False
    
    cur_labels, cur_im_dat = get_labels_from_slice(
      cur_slices, labels, im_dat, dim_utils, context,
      clear_borders = clear_borders, clear_depth = clear_depth,
      clear_touching_border = clear_touching_border)
    
    # fortify image
    cur_im_dat = zarr_utils.fortify(cur_im_dat)
    
    # channel should be at last position
    channel_idx = cur_im_dat.ndim - 1
    
    # run gaussian
    if gaussian_sigma > 0:
      # go through channels and run gaussian
      # not very elegant.. but ok
      # https://stackoverflow.com/a/42657219/13766165
      for i in range(cur_im_dat.shape[channel_idx]):
        # construct index
        idx = [slice(None)] * cur_im_dat.ndim
        idx[channel_idx] = i
        
        cur_im_dat[tuple(idx)] = filters.gaussian(
          cur_im_dat[tuple(idx)], preserve_range = True, sigma = gaussian_sigma)
      # # channel_axis is introduced in 0.19
      # cur_im_dat = filters.gaussian(
      #   cur_im_dat, sigma = gaussian_sigma,
      #   preserve_range = True, channel_axis = dim_utils.dim_idx('C'))
    
    minimal_props_to_get = [
      'label',
      'mean_intensity',
      'area'
    ]
    
    # run 3D or 2D measurements
    if dim_utils.is_3D() is True:
      # run regionsprobs on the identified labels
      # https://scikit-image.org/docs/0.18.x/api/skimage.measure.html?highlight=regionprops#skimage.measure.regionprops
      # TODO have to adjust the names when upgrading
      props_to_get = [
        'label',
        'mean_intensity',
        'centroid',
        'bbox',
        # do not work for 3D
        # 'eccentricity',
        # 'orientation'
        # 'perimeter',
      ]
    
      # remove non-objects
      # hope 40 is enough..
      for i, x in cur_labels.items():
        cur_labels[i] = remove_small_objects(x, 40)
        
        # make sure that objects are not flat
        # cur_labels[i] = remove_flat_objects(x, min_span = 3)
    else:
      props_to_get = [
        'label',
        'mean_intensity',
        'centroid',
        'bbox',
        'bbox_area',
        'eccentricity',
        'orientation',
        'perimeter',
        'area',
        'convex_area',
        'equivalent_diameter',
        'extent',
        'feret_diameter_max',
        'major_axis_length',
        'minor_axis_length',
        'solidity'
      ]
      
      # remove non-objects
      for i, x in cur_labels.items():
        cur_labels[i] = remove_small_objects(x, 10)
    
    # get measurements
    props_table = {
      i: pd.DataFrame(measure.regionprops_table(
        x, cur_im_dat,
        properties = props_to_get if i == 'base' else minimal_props_to_get
      )) for i, x in cur_labels.items()
      }
      
    # edit column names
    # INFO this could be done on the whole DF afterwards
    # but I need to know the bbox for surface quantification
    for i in props_table.keys():
      props_table[i].columns = [x.replace('-', '_') for x in props_table[i]]
    
    # rename centroid columns with _x, _y, _z
    rename_cols = {
      'centroid_' + str(x): 'centroid_' + str(i).lower()
      for i, x in centroid_idx.items() if x is not None
    }
    props_table[base_labels].rename(columns = rename_cols, inplace = True)
    
    # rename area columns
    if dim_utils.is_3D() is True:
      rename_cols = {
        f'{x}_area': x.replace('_area', '') + '_volume'
        for x in [y.replace('_area', '')
          for y in list(props_table[base_labels].columns[props_table[base_labels].columns.str.endswith('_area')])
          ]
      }
      rename_cols['area'] = 'volume'
      props_table[base_labels].rename(columns = rename_cols, inplace = True)
    
    # rename bbox columns with _min_x, _max_x, ...
    # get min
    rename_cols = {
      'bbox_' + str(x): 'bbox_min_' + str(i).lower()
      for i, x in centroid_idx.items() if x is not None
    }
    
    # get max
    rename_cols.update({
      'bbox_' + str(x + len(centroid_idx)): 'bbox_max_' + str(i).lower()
      for i, x in centroid_idx.items() if x is not None
    })
    props_table[base_labels].rename(columns = rename_cols, inplace = True)
    
    # TODO this will not work for multilabel files
    if dim_utils.is_3D() is True:
      # this can take a long time for static images with lots of cells
      if extended_measures is True:
        # mesh generation and shape descriptors
        # adapted from
        # https://github.com/CellProfiler/CellProfiler/blob/master/cellprofiler/modules/measureobjectsizeshape.py
        shape_descriptors = list()
        ellipsoid_major_axis_length = np.zeros(len(props_table[base_labels]['label']))
        ellipsoid_minor_axis_length = np.zeros(len(props_table[base_labels]['label']))
        ellipsoid_interm_axis_length = np.zeros(len(props_table[base_labels]['label']))
    
        # get surfaces within expanded area
        if len(props_table[base_labels].index) > 0:
          for index, label in enumerate(props_table[base_labels]['label']):
            # get label volume
            volume = morpho_utils.label_volume_from_label_props(
              cur_labels[base_labels], label, dim_utils, props_table[base_labels]
              )
            
            # get volume mesh
            volume_mesh = morpho_utils.mesh_from_label_volume(
              volume, dim_utils.im_scale(['X', 'Y', 'Z'])
            )
            
            # save mesh
            if save_meshes is True:
              volume_mesh.export(os.path.join(
                  task_dir,
                  cfg.value_dir(value_name, 'mesh', file_name = label, value_as_dir = True)
                  ))
            
            # TODO don't need this anymore?
            # https://forum.image.sc/t/scaling-regionprops-for-non-isotropic-images/61971/5
            # get shape descriptors
            shape_descriptors.append(morpho_utils.mesh_shape_descriptors(volume_mesh))
            
            # get ellipsoid
            # TODO error in ellipsoid_fit
            # u = np.linalg.solve(D.T.dot(D), D.T.dot(d2))
            # numpy.linalg.LinAlgError: Singular matrix
            try:
              centre, evecs, radii = morpho_utils.ellipsoid_fit(
                volume_mesh.convex_hull.vertices,
                dims = {
                  x: dim_utils.dim_idx(x, ignore_channel = True, ignore_time = True) for x in ['X', 'Y', 'Z']
                }
              )
              
              # sort array
              radii = abs(radii)
              radii.sort()
              
              # get major/minor axis lengths
              ellipsoid_minor_axis_length[index] = radii[0]
              ellipsoid_interm_axis_length[index] = radii[1]
              ellipsoid_major_axis_length[index] = radii[2]
            except:
              ellipsoid_minor_axis_length[index] = np.nan
              ellipsoid_interm_axis_length[index] = np.nan
              ellipsoid_major_axis_length[index] = np.nan
      
          # go through shape descriptors and add to table
          if len(shape_descriptors) > 0:
            for i in shape_descriptors[0].keys():
              props_table[base_labels][i] = [x[i] for x in shape_descriptors]
          
          # set ellipsoid properties
          props_table[base_labels]['major_axis_length'] = ellipsoid_major_axis_length
          props_table[base_labels]['minor_axis_length'] = ellipsoid_minor_axis_length
          props_table[base_labels]['interm_axis_length'] = ellipsoid_interm_axis_length
          
          # # add ratio of major and minor axis
          props_table[base_labels]['ellipticity_oblate'] = props_table[base_labels]['minor_axis_length'] / props_table[base_labels]['major_axis_length']
          props_table[base_labels]['ellipticity_prolate'] = props_table[base_labels]['major_axis_length'] / props_table[base_labels]['minor_axis_length']
          props_table[base_labels]['ellipticity_interm_oblate'] = props_table[base_labels]['minor_axis_length'] / props_table[base_labels]['interm_axis_length']
          props_table[base_labels]['ellipticity_interm_prolate'] = props_table[base_labels]['interm_axis_length'] / props_table[base_labels]['minor_axis_length']
      
          # compactness
          # https://doi.org/10.1016/j.patcog.2007.06.029
          props_table[base_labels][f'surface_to_volume'] = props_table[base_labels]['surface_area'] / props_table[base_labels]['volume']
          props_table[base_labels]['compactness'] = (props_table[base_labels]['surface_area'] ** 1.5) / props_table[base_labels]['volume']
          
          # sphericity
          props_table[base_labels]['sphericity'] = ((math.pi**(1/3)) * ((6*props_table[base_labels]['volume'])**(2/3))) / props_table[base_labels]['surface_area']
    else:
      # add ratio of major and minor axis
      props_table[base_labels]['oblate'] = props_table[base_labels]['minor_axis_length'] / props_table[base_labels]['major_axis_length']
      props_table[base_labels]['prolate'] = props_table[base_labels]['major_axis_length'] / props_table[base_labels]['minor_axis_length']
  
      # compactness
      props_table[base_labels]['perimeter_to_area'] = (props_table[base_labels]['perimeter'] ** 2) / props_table[base_labels]['area']
      
      # TODO there should be more in 2D
      # calculate nucleus to cytoplasm area
      # TODO you have to make sure that the tables are matching up
      if labels_mode == 'nuc_cyto':
        props_table[base_labels]['nuc_area'] = props_table['nuc']['area']
        props_table[base_labels]['nc_ratio'] = props_table['nuc']['area']/props_table[base_labels]['area']
        
        # replace NaN
        props_table[base_labels].fillna({
          'nc_ratio': 0, 'area': 0, 'nuc_area': 0
        }, inplace = True)
      
      # TODO measurements from Mesmer
      # doi.org/10.1038/s41587-021-01094-0
      # Aspect ratio
      props_table[base_labels]['aspect_ratio'] = props_table[base_labels]['major_axis_length'] / props_table[base_labels]['equivalent_diameter']
      
      # Fill
      props_table[base_labels]['fill'] = \
        (props_table[base_labels]['convex_area'] - props_table[base_labels]['area']) / props_table[base_labels]['convex_area']
      
      # save meshes for 2D
      # TODO do not calculate anything from it?
      # get surfaces within expanded area
      if save_meshes is True:
        for index, label in enumerate(props_table[base_labels]['label']):
          # get label volume
          volume = morpho_utils.label_volume_from_label_props(
            cur_labels[base_labels], label, dim_utils, props_table[base_labels]
            )
          
          # get volume mesh
          volume_mesh = morpho_utils.mesh_from_label_volume(
            volume,  dim_utils.im_scale(['X', 'Y', 'Z'])
            )
          
          # save mesh
          if save_meshes is True:
            volume_mesh.export(os.path.join(
                task_dir,
                cfg.value_dir(value_name, 'mesh', file_name = label, value_as_dir = True)
                ))
      
      # get convex hull from here to process the following parameters
      if extended_measures is True:
        for i in tqdm(np.unique(cur_labels[base_labels])):
          cur_hull = convex_hull_image(cur_labels[base_labels] == i).astype(np.uint8)
          
          props_to_get = [
            'label',
            'centroid',
            'area'
          ]
          
          # get properties for convex hull
          cur_hull_props = pd.DataFrame(measure.regionprops_table(
              cur_hull, properties = props_to_get
            ))
            
          # edit column names
          cur_hull_props.columns = [x.replace('-', '_') for x in cur_hull_props]
          
          # rename centroid columns with _x, _y, _z
          rename_cols = {
            'centroid_' + str(x): 'centroid_' + str(i).lower()
            for i, x in centroid_idx.items() if x is not None
          }
          cur_hull_props.rename(columns = rename_cols, inplace = True)
            
          # Asymmetry
          # √((x2 – x1)2 + (y2 – y1)2)/√(area)
          props_table[base_labels]['asymmetry'] = np.sqrt(
            (props_table[base_labels]['centroid_x'] - cur_hull_props['centroid_x']) ** 2\
            + (props_table[base_labels]['centroid_y'] - cur_hull_props['centroid_y']) ** 2
          ) / np.sqrt(props_table[base_labels]['area'])
          
          # Concavities
          # TODO
    
    # add intensities for individual compartments
    intensity_cols = [i for i in props_table[base_labels].columns if i.startswith('mean_intensity_')]
    for j in [y for y in props_table.keys() if y in ['halo', 'nuc', 'cyto']]:
      for i, x in enumerate(intensity_cols):
        props_table[base_labels][f'{j}_mean_intensity_{i}'] = props_table[j][x]
      
    centroid_cols = props_table[base_labels].columns[props_table[base_labels].columns.str.startswith('centroid')]
    bbox_min_cols = props_table[base_labels].columns[props_table[base_labels].columns.str.startswith('bbox_min')]
    bbox_max_cols = props_table[base_labels].columns[props_table[base_labels].columns.str.startswith('bbox_max')]
    
    # add values from current slices to centroid and bbox
    # https://stackoverflow.com/a/27275479/13766165
    for i, centroid in enumerate(centroid_cols):
      slice_id = slice_idx[i]
      
      # check that there is a slice for that dimension
      if slice_id < len(cur_slices):
        props_table[base_labels][centroid] += cur_slices[slice_id].start
        
    for i, bbox in enumerate(bbox_min_cols):
      slice_id = slice_idx[i]
      
      # check that there is a slice for that dimension
      if slice_id < len(cur_slices):
        props_table[base_labels][bbox] += cur_slices[slice_id].start
        
    for i, bbox in enumerate(bbox_max_cols):
      slice_id = slice_idx[i]
      
      # check that there is a slice for that dimension
      if slice_id < len(cur_slices):
        props_table[base_labels][bbox] += cur_slices[slice_id].start
    
    # add centroid for time
    if dim_utils.is_timeseries() is True:
      time_val = cur_slices[dim_utils.dim_idx('T')].start
      
      # add to table
      # props_table[base_labels][f'centroid-{num_centroid_cols}'] = time_val
      props_table[base_labels][f'centroid_t'] = time_val
      
    # add to list
    props.append(props_table[base_labels])
  
  # concat
  props_df = pd.concat(props, ignore_index = True)
  
  # reshuffle columns
  # https://stackoverflow.com/a/24396554/13766165
  # https://stackoverflow.com/a/11067072/13766165
  if dim_utils.is_timeseries() is True:
    props_df_columns = list(props_df.columns)
    
    # sort t centroid
    props_df_columns.insert(
      props_df_columns.index('centroid_x'),
      props_df_columns.pop(props_df_columns.index('centroid_t'))
      )

    # reindex
    props_df = props_df.reindex(props_df_columns, axis = 1)

  # remove duplicates
  props_df.drop_duplicates('label', inplace = True)
  
  return props_df

"""
Find shapes from labels
"""
def countours_2D_from_zarr(labels, im_dat, dim_utils, logfile_utils,
  block_size = None, overlap = None, context = None):
  # get slices
  slices = slice_utils.create_slices(
    labels.shape, dim_utils, block_size, overlap)
  
  label_contours = list()
  
  # go through slices
  for i, cur_slices in enumerate(slices):
    logfile_utils.log(">> Slice: " + str(i + 1) + "/" + str(len(slices)))
    logfile_utils.log(cur_slices)
    
    cur_labels, cur_im_dat = get_labels_from_slice(cur_slices, labels, im_dat, dim_utils, context)
    
    # find contours for individual labels
    contours = measure.find_contours(cur_labels, level = 0)
    
    # go through contours and find label_ids
    label_ids = list()
    approx_contours = list()
    
    for contour in contours:
      # approximate contour
      # approx_contour = measure.approximate_polygon(contour, 0.02)
      # take only every nth coordinate
      approx_contour = contour[::4]
      
      # find centre
      mean_x = int(np.mean(approx_contour[:,0]))
      mean_y = int(np.mean(approx_contour[:,1]))
      label_id = cur_labels[(mean_x, mean_y)]
      
      # TODO that should mostly be the case.. ?
      if label_id > 0:
        # add values from current slices
        approx_contour[:, 0] += cur_slices[0].start
        approx_contour[:, 1] += cur_slices[1].start
        
        # add to list
        approx_contours.append(approx_contour)
        
        # add repeated labels
        label_ids.append([label_id] * len(approx_contour))
      
    # make dataframe
    cur_df = pd.DataFrame(np.concatenate(approx_contours))
    
    # add labels
    # https://stackoverflow.com/a/11860531/13766165
    cur_df['label'] = sum(label_ids, [])
    
    # add to list
    label_contours.append(cur_df)
      
  # concat
  contours_df = pd.concat(label_contours, ignore_index = True)
  
  # rename columns
  contours_df.rename(columns={0: 'POS_0', 1: 'POS_1'}, inplace = True)
  
  # remove duplicates
  contours_df.drop_duplicates(inplace = True)
  
  return contours_df

"""
Subtract edges from labels
"""
def subtract_edges_from_labels(labels):
  # find edges
  # edges = filters.sobel(labels)
  edges = filters.roberts(labels)
  # edges = filters.scharr(labels)
  # edges = filters.prewitt(labels)
  edges[edges > 0] = 1
  
  # invert
  edges = (-1 * edges) + 1
  
  # subtract edges
  labels_minus_edges = labels * edges
  
  return labels_minus_edges.astype(np.int32)

"""
Remove flat objects
TODO is there a faster way to do this?
"""
def remove_flat_objects(labels, min_span = 2):
  # get unique values
  label_ids = list(np.unique(labels))
  label_ids.pop(0)
  
  # get flat labels
  flat_labels = [
    x for x in label_ids \
    if not all([len(np.unique(y)) > min_span for y in (labels == x).nonzero()])
    ]
    
  # remove labels
  # TODO more pythonic way.. ?
  for x in flat_labels:
    labels[labels == x] = 0
  
  return labels

"""
Remove big labels

https://stackoverflow.com/a/42380337/13766165
"""
def remove_big_objects(labels, max_size = 10000):
  out = np.copy(labels)
  
  # get object sizes
  component_sizes = np.bincount(labels.ravel())
  
  # filter objects
  too_big = component_sizes >= max_size
  too_big_mask = too_big[labels]
  
  # reset labels
  out[too_big_mask] = 0
  
  return out
