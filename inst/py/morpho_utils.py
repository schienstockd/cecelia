import numpy as np
import math
import trimesh
import re
import os

from skimage import measure
from scipy.spatial.distance import pdist

import py.config_utils as cfg

# return shape descriptors for mesh
def mesh_shape_descriptors(volume_mesh):
  # get number of dimensions
  ndim = volume_mesh.vertices.shape[1]
  
  return {
    'surface_area': volume_mesh.area,
    'volume': volume_mesh.volume,
    'bbox_area': volume_mesh.bounding_box_oriented.area,
    'bbox_volume': volume_mesh.bounding_box_oriented.volume,
    'convex_hull_area': volume_mesh.convex_hull.area,
    'convex_hull_volume': volume_mesh.convex_hull.volume,
    'euler_number': volume_mesh.euler_number,
    'extent': volume_mesh.volume/volume_mesh.bounding_box_oriented.volume,
    'solidity': volume_mesh.volume/volume_mesh.convex_hull.volume,
    'integral_mean_curvature': volume_mesh.integral_mean_curvature,
    'feret_diameter_max': mesh_feret_diameter_max(volume_mesh),
    'equivalent_diameter': mesh_equivalent_diameter(volume_mesh, ndim = 3)
  }
    
# convert meshes to napari surface layer object
def to_napari_surface_object(meshes, df, dim_utils):
  # get time index
  t_idx = None
  if dim_utils.is_timeseries():
    t_idx = dim_utils.dim_idx('T', ignore_channel = True)
  
  # keep track of vertices
  vertices_count = 0
  
  # create list for values
  faces = list()
  vertices = list()
  values = list()
  
  # add vertices count to faces
  for i, x in meshes.items():
    # get label
    label_id = int(re.findall('(?<=#)\d+', i)[0])
    
    # get vertices and faces
    x_vertices = x.vertices.tolist()
    x_faces = np.array(x.faces.tolist())
    
    # add time to vertices
    if t_idx is not None:
      # get time and track id
      track_id = int(df.loc[df['label'] == label_id]['track_id'])
      centroid_t = int(df.loc[df['label'] == label_id]['centroid_t'])
      
      for y in x_vertices:
        y.insert(t_idx, centroid_t)
      
      # add track id
      values += [track_id] * len(x_vertices)
    else:
      values += [label_id] * len(x_vertices)
    
    # add vertices count to faces
    x_faces += vertices_count
    vertices_count += len(x_vertices)
    
    # add to vertices and faces
    vertices += x_vertices
    faces += x_faces.tolist()
  
  # return surface object
  return (
    np.array(vertices),
    np.array(faces),
    np.array(values)
  )
    
# convert DF to meshes
# go through by condition
# TODO this is only one condition at the moment
def df_to_meshes(task_dir, df, value_name, filter_measure = 'NONE', filter_vals = ['NONE'],
                 dim_utils = None, im_res = None, is_3D = False,
                 as_convex_hull = False, add_value_name_to_name = True):
  # get image resolution
  if im_res is None:
    im_res = dim_utils.im_scale(['z', 'y', 'x'], as_dict = True, upper = False)
  
  # create translation pos
  # meshes are always 3D!
  translate_pos = im_res.copy()
  if is_3D is False:
    translate_pos['z'] = 1
  
  # build meshes
  meshes = dict()
  for filter_val in filter_vals:
    print(f'>> get meshes for {filter_measure} {filter_val}')
    
    # make sure filter value is in DF otherwise fall back to None
    if filter_val != 'NONE' and filter_measure in df.columns:
      cur_df = df.loc[df[filter_measure] == filter_val,]
    else:
      cur_df = df
    
    for x in cur_df.iterrows():
      mesh_label = int(x[1]['label'])
      mesh_name = mesh_label
      
      if add_value_name_to_name is True:
        mesh_name = value_name + '#' + str(mesh_name)
      
      # get relative position in image to place mesh
      pos = [x[1][f'bbox_min_{j.lower()}'] * im_res[j] for j in im_res.keys()\
        if f'bbox_min_{j.lower()}' in x[1].keys()]
      
      mesh_path = os.path.join(
        task_dir,
        cfg.value_dir(
          value_name, 'mesh',
          file_name = mesh_label,
          value_as_dir = True
        )
      )
      
      if os.path.exists(mesh_path):
        # load mesh
        meshes[mesh_name] = load_mesh(mesh_path, pos = pos)
        
        # translate positions back to pixel values
        # meshes[mesh_name].vertices = meshes[mesh_name].vertices / [translate_pos[i] for i in ['z', 'x', 'y']]
        
        # get convex hull
        if as_convex_hull is True:
          meshes[mesh_name] = meshes[mesh_name].convex_hull
          
  # return meshes
  return meshes

# convert tracks to meshes
def tracks_to_meshes(task_dir, track_ids, df, dim_utils, value_name, as_convex_hull = True):
  return(df_to_meshes(
    task_dir = task_dir, filter_measure = 'track_id',
    filter_vals = track_ids,
    df = df,
    dim_utils = dim_utils, value_name = value_name,
    as_convex_hull = as_convex_hull
  ))

# return volume for label from image
def label_volume_from_label_props(labels, label, dim_utils, props_table, padding = 0):
  # create slices
  labels_slices = [slice(None) for x in range(len(labels.shape))]
  
  # get label props from table
  label_props = props_table[props_table['label'] == label]
  
  # expand bbox by '1'
  # for x in [x.lower() for x in dim_utils.spatial_axis()]:
  for x in [x.lower() for x in dim_utils.spatial_axis()]:
    dim_idx = dim_utils.dim_idx(
      x, ignore_channel = True, ignore_time = True, drop_z = True, drop_time = True)
    
    bbox_min = max(int(label_props[f'bbox_min_{x}'])-padding, 0)
    bbox_max = min(int(label_props[f'bbox_max_{x}'])+padding, labels.shape[dim_idx])
    
    labels_slices[dim_idx] = slice(bbox_min, bbox_max, 1)
  
  return labels[tuple(labels_slices)] == label

# load mesh from file and adjust vertices according to position
def load_mesh(path, pos = None):
  # load mesh from file
  mesh = trimesh.load(path)
  
  # adjust vertices
  if pos is not None:
    # add padding if 2D
    if len(pos) == 2:
      pos = [0] + pos
        
    mesh.vertices += pos
  
  return mesh

# generate mesh from labelled volume
def mesh_from_label_volume(volume, spacing = 1.0,
                           simplify = False, simplify_factor = 0.8,
                           # smoothing = True, lamb = 1, iterations = 10,
                           process = True):
  # make 2D surface to 3D
  if len(volume.shape) == 2:
      volume = np.expand_dims(volume, axis = 0)
                             
  # create volume - used skimage.measure.marching_cubes internally
  # pitch == spacing
  volume_mesh = trimesh.voxel.ops.matrix_to_marching_cubes(
      volume, pitch = spacing
  )
  
  # make sure it is watertight if not
  # TODO could not find a better solution
  if volume_mesh.fill_holes() is False:
    volume_mesh = volume_mesh.split(only_watertight = True)[0]
  
  # does that help ..?
  volume_mesh.merge_vertices(merge_tex = True, merge_norm = True)
  
  # process mesh
  if process is True:
      volume_mesh.process()
  
  # simplify object
  if simplify is True:
      volume_mesh = volume_mesh.simplify_quadratic_decimation(
          len(volume_mesh.faces) * simplify_factor)
  
  # apply smoothing
  # ! this will distort meshes in 2D
  # if smoothing is True:
  #     volume_mesh = trimesh.smoothing.filter_laplacian(
  #         volume_mesh, lamb = lamb, iterations = iterations)
  
  return volume_mesh

# generate multiple meshes from labelled volume
def meshes_from_label_volume(volume, labels, spacing, prefix = '', 
                             simplify = False, simplify_factor = 0.8,
                             smoothing = True, lamb = 0.5, iterations = 10):
  # go through labels and get meshes
  volume_meshes = {
      f'{prefix}{x}': mesh_from_label_volume(
          volume == x, spacing = spacing,
          simplify = simplify, simplify_factor = simplify_factor,
          smoothing = smoothing, lamb = lamb, iterations = iterations) for x in labels
  }
  
  return volume_meshes

# get feret diameter
# adapted from
# https://github.com/scikit-image/scikit-image/blob/v0.18.0/skimage/measure/_regionprops.py
def mesh_feret_diameter_max(mesh):
  # get coordinates from convex hull
  coordinates = mesh.convex_hull.vertices
  
  # get distances
  distances = pdist(coordinates, 'sqeuclidean')
  
  # return max
  return math.sqrt(np.max(distances))

# get equivalent diameter for 
# adapted from
# https://github.com/scikit-image/scikit-image/blob/v0.18.0/skimage/measure/_regionprops.py
def mesh_equivalent_diameter(mesh, ndim):
  return (2 * ndim * mesh.area / math.pi) ** (1 / ndim)

"""
Get ellipsoid of datapoints

# adapted from
https://github.com/marksemple/pyEllipsoid_Fit/blob/master/ellipsoid_fit.py
Fit an ellipsoid to a cloud of points using linear least squares
based on Yury Petrov MATLAB algorithm: "ellipsoid_fit.m"
"""
def ellipsoid_fit(point_data, dims, mode = ''):
  X = point_data[:, dims['X']]
  Y = point_data[:, dims['Y']]
  Z = point_data[:, dims['Z']]
  
  # AlGEBRAIC EQUATION FOR ELLIPSOID, from CARTESIAN DATA
  if mode == '':  # 9-DOF MODE
    D = np.array([X * X + Y * Y - 2 * Z * Z,
                  X * X + Z * Z - 2 * Y * Y,
                  2 * X * Y, 2 * X * Z, 2 * Y * Z,
                  2 * X, 2 * Y, 2 * Z,
                  1 + 0 * X]).T

  elif mode == 0:  # 6-DOF MODE (no rotation)
    D = np.array([X * X + Y * Y - 2 * Z * Z,
                  X * X + Z * Z - 2 * Y * Y,
                  2 * X, 2 * Y, 2 * Z,
                  1 + 0 * X]).T

  # THE RIGHT-HAND-SIDE OF THE LLSQ PROBLEM
  d2 = np.array([X * X + Y * Y + Z * Z]).T
  
  # SOLUTION TO NORMAL SYSTEM OF EQUATIONS
  u = np.linalg.solve(D.T.dot(D), D.T.dot(d2))
  # chi2 = (1 - (D.dot(u)) / d2) ^ 2

  # CONVERT BACK TO ALGEBRAIC FORM
  if mode == '':  # 9-DOF-MODE
    a = np.array([u[0] + 1 * u[1] - 1])
    b = np.array([u[0] - 2 * u[1] - 1])
    c = np.array([u[1] - 2 * u[0] - 1])
    v = np.concatenate([a, b, c, u[2:, :]], axis=0).flatten()

  elif mode == 0:  # 6-DOF-MODE
    a = u[0] + 1 * u[1] - 1
    b = u[0] - 2 * u[1] - 1
    c = u[1] - 2 * u[0] - 1
    zs = np.array([0, 0, 0])
    v = np.hstack((a, b, c, zs, u[2:, :].flatten()))

  else:
    pass

  # PUT IN ALGEBRAIC FORM FOR ELLIPSOID
  A = np.array([[v[0], v[3], v[4], v[6]],
                [v[3], v[1], v[5], v[7]],
                [v[4], v[5], v[2], v[8]],
                [v[6], v[7], v[8], v[9]]])

  # FIND CENTRE OF ELLIPSOID
  centre = np.linalg.solve(-A[0:3, 0:3], v[6:9])

  # FORM THE CORRESPONDING TRANSLATION MATRIX
  T = np.eye(4)
  T[3, 0:3] = centre

  # TRANSLATE TO THE CENTRE, ROTATE
  R = T.dot(A).dot(T.T)

  # SOLVE THE EIGENPROBLEM
  evals, evecs = np.linalg.eig(R[0:3, 0:3] / -R[3, 3])

  # SORT EIGENVECTORS
  # i = np.argsort(evals)
  # evals = evals[i]
  # evecs = evecs[:, i]
  # evals = evals[::-1]
  # evecs = evecs[::-1]

  # CALCULATE SCALE FACTORS AND SIGNS
  radii = np.sqrt(1 / abs(evals))
  sgns = np.sign(evals)
  radii *= sgns

  return centre, evecs, radii
