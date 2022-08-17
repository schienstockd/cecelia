import math
import numpy as np

"""
Convert coords to slices
"""
def convert_coords_to_slices(coords):
  idx = ()
  
  # go through coords
  for x in coords:
    idx += (slice(x[0], x[1], 1),)
      
  return idx

"""
Create slices from image dimensions
"""
def create_slices(im_dim, dim_utils, block_size = None, overlap = None,
                  block_size_z = None, overlap_z = None, timepoints = None):
  slices = None
  
  # 3D timeseries
  if False not in (dim_utils.is_3D(), dim_utils.is_timeseries()):
    gen_slices = create_slices_3D_time(
      im_dim, dim_utils, block_size, overlap,
      block_size_z = block_size_z, overlap_z = overlap_z,
      timepoints = timepoints)
      
  # 2D timeseries
  elif dim_utils.is_3D() is False and dim_utils.is_timeseries() is True:
    gen_slices = create_slices_2D_time(
      im_dim, dim_utils, block_size, overlap,
      timepoints = timepoints)
      
  # 3D static
  elif dim_utils.is_3D() is True and dim_utils.is_timeseries() is False:
    gen_slices = create_slices_3D(
      im_dim, dim_utils, block_size, overlap,
      block_size_z = block_size_z, overlap_z = overlap_z)
      
  # 2D static
  elif dim_utils.is_3D() is False and dim_utils.is_timeseries() is False:
    gen_slices = create_slices_2D(im_dim, dim_utils, block_size, overlap)
    
  slices = np.array([[slice(None) for _ in range(len(im_dim))]] * len(gen_slices['slices']))
  
  # add slices
  for i, x in enumerate(gen_slices['slices']):
    for j, y in enumerate(gen_slices['order']):
      slices[i, dim_utils.dim_idx(y, ignore_channel = True)] = x[j]
    
  # TODO do I need that?
  slices = [tuple(l.tolist()) for l in slices]
      
  return slices

"""
Create slices from image dimensions (3D time)
"""
def create_slices_3D_time(im_dim, dim_utils, block_size = None,
                          overlap = None, block_size_z = None, overlap_z = None,
                          timepoints = None):
  # create 3D frame slices
  frame_slices = create_slices_3D(
    im_dim, dim_utils, block_size, overlap,
    block_size_z = block_size_z, overlap_z = overlap_z)
    
  # combine time and frame slices
  return combine_time_frame_slices(frame_slices, dim_utils,
                                   timepoints = timepoints)

"""
Create slices from image dimensions (2D time)
"""
def create_slices_2D_time(im_dim, dim_utils, block_size = None, overlap = None,
                          timepoints = None):
  # create 2D frame slices
  frame_slices = create_slices_2D(im_dim, dim_utils, block_size, overlap)
    
  # combine time and frame slices
  return combine_time_frame_slices(frame_slices, dim_utils,
                                   timepoints = timepoints)
    
"""
Combine time and frame slices
"""
def combine_time_frame_slices(frame_slices, dim_utils, timepoints = None):
  # get time idx
  time_idx = dim_utils.dim_idx('T', ignore_channel = True)
  time_vals = list(range(dim_utils.dim_val('T'))) if timepoints is None else timepoints
  
  # get length of frame slices
  len_frame_slices = len(frame_slices['slices'])
  
  # replicate for timepoints
  # https://stackoverflow.com/a/38099836/13766165
  frame_slices['slices'] = [x for x in frame_slices['slices'] for _ in time_vals]
  
  # convert frame tuples to list
  frame_slices['slices'] = [list(x) for x in frame_slices['slices']]
  
  # combine with time slices
  for i, t in enumerate(range(time_vals[0], time_vals[-1] + 1)):
    for x in range(1, len_frame_slices + 1):
      frame_slices['slices'][(i + 1) * x - 1].insert(time_idx, slice(t, t + 1, 1))
  
  # convert back to tuples
  frame_slices['slices'] = [tuple(x) for x in frame_slices['slices']]
  
  # add to order
  frame_slices['order'].insert(time_idx, 'T')
  
  return frame_slices

"""
Create slices from image dimensions (3D)
"""
def create_slices_3D(im_dim, dim_utils, block_size = None, overlap = None,
                     block_size_z = None, overlap_z = None):
  # get idx
  z_idx = dim_utils.dim_idx('Z', ignore_channel = True)
  y_idx = dim_utils.dim_idx('Y', ignore_channel = True)
  x_idx = dim_utils.dim_idx('X', ignore_channel = True)
  
  # set block size to image stack if not set
  if block_size is None or block_size < 0:
    block_size = max(im_dim[x_idx], im_dim[y_idx])
    
  if block_size_z is None or block_size_z < 0:
    block_size_z = im_dim[z_idx]
  
  # set overlap
  if overlap is None or overlap < 0:
    overlap = 0
    
  if overlap_z is None or overlap_z < 0:
    overlap_z = overlap
  
  tiles_z = math.ceil(im_dim[z_idx] / block_size_z)
  tiles_m = math.ceil(im_dim[y_idx] / block_size)
  tiles_n = math.ceil(im_dim[x_idx] / block_size)
  Z = im_dim[z_idx]//tiles_z
  M = im_dim[y_idx]//tiles_m
  N = im_dim[x_idx]//tiles_n
  
  # is there a residual after slicing?
  res_z = im_dim[z_idx] - Z * tiles_m
  res_m = im_dim[y_idx] - M * tiles_m
  res_n = im_dim[x_idx] - N * tiles_n
  
  # attach residual to last slice
  if res_z > 0: Z += res_z
  if res_m > 0: M += res_m
  if res_n > 0: N += res_n
  
  slices = [(
    slice(
      i - overlap_z if i - overlap_z >= 0 else 0,
      i + Z + overlap_z if i + Z + overlap_z < im_dim[z_idx] else im_dim[z_idx],
      1),
    slice(
      x - overlap if x - overlap >= 0 else 0,
      x + M + overlap if x + M + overlap < im_dim[y_idx] else im_dim[y_idx],
      1),
    slice(
      y - overlap if y - overlap >= 0 else 0,
      y + N + overlap if y + N + overlap < im_dim[x_idx] else im_dim[x_idx],
      1)
    ) for i in range(0, im_dim[z_idx], Z) for x in range(0, im_dim[y_idx], M) for y in range(0, im_dim[x_idx], N)]
  
  return {
    'slices': slices,
    'order': ['Z', 'Y', 'X']
    }

"""
Create slices from image dimensions (2D)
"""
def create_slices_2D(im_dim, dim_utils, block_size = None, overlap = None):
  # get idx
  y_idx = dim_utils.dim_idx('Y', ignore_channel = True)
  x_idx = dim_utils.dim_idx('X', ignore_channel = True)
  
  # set block size to image stack if not set
  if block_size is None or block_size < 0:
    block_size = max(im_dim[x_idx], im_dim[y_idx])
    
  # set overlap
  if overlap is None or overlap < 0:
    overlap = 0
    
  # adjust block size to make equally spaced tiles
  tiles_m = math.ceil(im_dim[y_idx] / block_size)
  tiles_n = math.ceil(im_dim[x_idx] / block_size)
  M = im_dim[y_idx]//tiles_m
  N = im_dim[x_idx]//tiles_n
  
  # is there a residual after slicing?
  res_m = im_dim[y_idx] - M * tiles_m
  res_n = im_dim[x_idx] - N * tiles_n
  
  # attach residual to last slice
  if res_m > 0: M += res_m 
  if res_n > 0: N += res_n
  
  slices = [(
    slice(
      x - overlap if x - overlap >= 0 else 0,
      x + M + overlap if x + M + overlap < im_dim[y_idx] else im_dim[y_idx],
      1),
    slice(
      y - overlap if y - overlap >= 0 else 0,
      y + N + overlap if y + N + overlap < im_dim[x_idx] else im_dim[x_idx],
      1)
    ) for x in range(0, im_dim[y_idx], M) for y in range(0, im_dim[x_idx], N)]
  
  return {
    'slices': slices,
    'order': ['Y', 'X']
    }

"""
Create multiscale slices
"""
def create_slices_multiscales(im_dim, dim_utils = None,
                              x_idx = None, y_idx = None,
                              nscales = 1, ignore_channel = False):
  # TODO this will only create multiscales for X and Y
  # Would you want to have scaling in Z?
  
  # create slices
  slices = [[slice(None) for _ in range(len(im_dim))] for _ in range(nscales)]
  
  # X and Y have to be defined if no DimUtils given
  if dim_utils is not None:
    # get idx
    y_idx = dim_utils.dim_idx('Y', ignore_channel = ignore_channel)
    x_idx = dim_utils.dim_idx('X', ignore_channel = ignore_channel)
    
  # get max
  y_max = im_dim[y_idx]
  x_max = im_dim[x_idx]
  
  # create scaled slices
  for i, x in enumerate(slices):
    slices[i][x_idx] = slice(0, x_max, 2**(i+1))
    slices[i][y_idx] = slice(0, y_max, 2**(i+1))
  
  # convert to tuples
  
  return [tuple(x) for x in slices]
