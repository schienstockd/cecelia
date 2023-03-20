import math
import numpy as np

import py.math_helpers as math_helpers
import py.ome_xml_utils as ome_xml_utils

class DimUtils:
  def __init__(self, omexml, use_channel_axis = False):
    self.default_order = list("TZCYX")
    self.omexml = omexml
    self.use_channel_axis = use_channel_axis
    
    # image dimensions
    self.im_dim = None
    self.im_dim_order = None
  
  """
  return pixel dimensions
  """
  def pixels(self):
    # TODO use when ome_type 0.21 is released
    # return self.omexml.images[0].pixels
    return self.omexml.image().Pixels
  
  """
  calc image dimensions
  """
  def calc_image_dimensions(self, im_shape):
    # get internal dimensions
    im_dim_dict = ome_xml_utils.get_im_size_dict(self.omexml)
      
    # remove '1' dimensions if the length differs
    if len(im_dim_dict) != len(im_shape):
      for x in im_dim_dict.copy().items():
        if x[1] == 1:
          im_dim_dict.pop(x[0])
    
    # split into dimensions and order
    im_dim = list(im_dim_dict.values())
    im_dim_order = list(im_dim_dict.keys())
    
    # check whether the dimensions are reversed
    # Zarr will have the reversed order
    # https://github.com/glencoesoftware/bioformats2raw
    # Have to see whether that is always the case
    im_shape = list(im_shape)
    
    # if they don't match - reverse or reshuffle
    # reverse first in case X and Y are a square
    # print(im_shape)
    # print(im_dim)
    # print(im_dim_order)
    
    if im_shape != im_dim:
      im_shape.reverse() 
      
      if im_shape == im_dim:
        # print(">> reverse dimensions")
        
        im_dim.reverse()
        im_dim_order.reverse()
      else:
        # reserve back
        im_shape.reverse()
        
        # print(">> match dimensions by size")
        
        # match by dimension size
        # im_shape_order = [im_shape.index(x) for x in im_dim]
        # TODO is there a better way of doing this ..?
        im_shape_order = np.zeros(len(im_dim), dtype = np.int)
        
        for i, x in enumerate(im_dim):
          idx = im_shape.index(x)
          
          im_shape_order[i] = idx
          # remove value from im shape
          im_shape[idx] = 0
        
        im_dim = [im_dim[i] for i in im_shape_order]
        im_dim_order = [im_dim_order[i] for i in im_shape_order]
    
    # print(">> image dimension order")
    # print(im_dim_order)
    
    self.im_dim = im_dim
    self.im_dim_order = im_dim_order
  
  """
  Create slice array
  """
  def create_channel_slices(self, channel):
    # create slices to access data from store
    slice_list = [slice(None) for i in range(len(self.im_dim))]
    slice_list[self.dim_idx('C')] = slice(channel, channel + 1, 1)
    
    return tuple(slice_list)

  """
  Get pixel type
  """
  def pixel_type(self):
    return self.omexml.image().Pixels.PixelType

  """
  Is the image 32 bit?
  """
  def is_32_bit(self):
    return self.pixel_type() == 'uint32'
  
  """
  Get physical image size dict
  """
  def im_physical_sizes(self, default = 1):
    axis_sizes = {
      'x': self.omexml.image().Pixels.get_PhysicalSizeX(),
      'y': self.omexml.image().Pixels.get_PhysicalSizeY(),
      'z': self.omexml.image().Pixels.get_PhysicalSizeZ()
    }
    
    # account for None
    return {i: x if x is not None else default for i, x in axis_sizes.items()}
  
  """
  Get physical image size
  """
  def im_physical_size(self, axis, default = 1):
    axis_size = default
    
    if axis == 'x':
      axis_size = self.omexml.image().Pixels.get_PhysicalSizeX()
    elif axis == 'y':
      axis_size = self.omexml.image().Pixels.get_PhysicalSizeY()
    elif axis == 'z':
      axis_size = self.omexml.image().Pixels.get_PhysicalSizeZ()
    
    return axis_size if axis_size is not None else default
  
  """
  Get physical image units dict
  """
  def im_physical_units(self, default = 'um'):
    axis_units = {
      'x': self.omexml.image().Pixels.get_PhysicalSizeXUnit(),
      'y': self.omexml.image().Pixels.get_PhysicalSizeYUnit(),
      'z': self.omexml.image().Pixels.get_PhysicalSizeZUnit()
    }
    
    # account for None
    return {i: x.replace('µ', 'u') if x is not None else default for i, x in axis_units.items()}
  
  """
  Get physical image unit
  """
  def im_physical_unit(self, axis, default = 'um'):
    axis_unit = default
    
    if axis == 'x':
      axis_unit = self.omexml.image().Pixels.get_PhysicalSizeXUnit()
    elif axis == 'y':
      axis_unit = self.omexml.image().Pixels.get_PhysicalSizeYUnit()
    elif axis == 'z':
      axis_unit = self.omexml.image().Pixels.get_PhysicalSizeZUnit()
    
    return axis_unit.replace('µ', 'u') if axis_unit is not None else default
  
  """
  Get image scale
  """
  def im_scale(self, dims = list(), as_dict = False, upper = True):
    # TODO use when ome_type 0.21 is released
    # size_x = self.omexml.images[0].pixels.physical_size_x
    # size_y = self.omexml.images[0].pixels.physical_size_y
    # size_z = self.omexml.images[0].pixels.physical_size_z
    size_x = self.omexml.image().Pixels.get_PhysicalSizeX()
    size_y = self.omexml.image().Pixels.get_PhysicalSizeY()
    size_z = self.omexml.image().Pixels.get_PhysicalSizeZ()
    
    # construct scale
    im_scale = list(np.ones_like(self.im_dim))
    
    # add zscale
    im_scale[self.dim_idx("X")] = size_x
    im_scale[self.dim_idx("Y")] = size_y
    
    # add z scale
    if self.is_3D():
      # add zscale
      im_scale[self.dim_idx("Z")] = size_z
      
    # convert None to 1
    im_scale = [x if x is not None else 1 for x in im_scale]
    
    # return only certain dimension?
    if len(dims) > 0:
      # ensure upper
      dims = [x.upper() for x in dims]
      
      new_scale = list()
      
      # go through indices
      for x in [self.dim_idx(x) for x in self.im_dim_order if x in dims]:
        new_scale.append(im_scale[x])
      
      im_scale = new_scale
      
    # convert to dict
    if as_dict is True:
      if len(dims) > 0:
        im_scale = {
          x: im_scale[i] for i, x in enumerate(
            [x for x in self.im_dim_order if x in dims]
            )}
      else:
        im_scale = {x: im_scale[i] for i, x in enumerate(self.im_dim_order)}
    
    # ensure to lower
    if upper is False:
      im_scale = {i.lower(): x for i, x in im_scale.items()}
    
    return im_scale
  
  """
  Get used dim idx
  """
  def used_dim_idx(self, ignore_channel = False, ignore_time = False, cutoff = 1):
    # TODO this has to be cleaner
    # set ignore axis
    ignore_axis = []
    if ignore_channel is True:
      ignore_axis += ['C']
    if ignore_time is True:
      ignore_time += ['T']
    
    # return
    return [self.dim_idx(x, ignore_channel = ignore_channel, ignore_time = ignore_time) \
      for i, x in enumerate(self.im_dim_order) if self.im_dim[i] > cutoff and x not in ignore_axis]
  
  """
  Get pos of dimension
  """
  def dim_idx(self, dim = None, ignore_channel = False, ignore_time = False,
              ignore_z = False, default_order = False, squeeze = False, drop_z = False):
    ret_val = None
    
    if default_order is True:
      dim_order = self.trimmed_default_dim_order()
    else:
      dim_order = self.im_dim_order.copy()
    
    # ignore channel?
    # only relevant if images where split into channels
    if ignore_channel is True:
      if self.use_channel_axis is True:
        dim_order.pop(self.dim_idx('C'))
        
    # ignore channel?
    # if processing a timeseries but only use one frame
    if ignore_time is True:
      if self.is_timeseries(ignore_one = False) is True:
        dim_order.pop(self.dim_idx('T', ignore_channel = ignore_channel))
        
    # drop z if not 3D?
    # TODO this is not clean
    if (drop_z is True and self.is_3D() is False) or ignore_z is True:
      dim_order.pop(self.dim_idx(
        'Z', ignore_channel = ignore_channel, ignore_time = ignore_time
        ))
    
    # squeeze and remove dimensions with one?
    if squeeze is True:
      dim_vals = [self.dim_val(x) for x in dim_order]
      
      # pop values with '1'
      for i, x in enumerate(dim_vals):
        if x == 1:
          dim_order.pop(i)
    
    if dim is not None: 
      # just to make sure
      dim = dim.upper()
      
      if dim in dim_order:
        ret_val = dim_order.index(dim.upper())
    else:
      # return indices of all dimenions
      ret_val = [dim_order.index(x) for x in self.im_dim_order if x in dim_order]
      
    return ret_val
  
  """
  Get value of dimension
  """
  def dim_val(self, dim, scale = 0):
    dim_val = 1
    
    # is the dimension set?
    if self.dim_idx(dim) >= 0:
      dim_val = self.im_dim[self.dim_idx(dim)]
      
    # adjust for scale
    # TODO is that fair .. ?
    if dim in ['X', 'Y']:
      dim_val = math.floor(dim_val / 2**scale)
    
    return dim_val
  
  """
  Get dimension values
  """
  def dim_vals(self, default_order = False, ignore_channel = False, ignore_time = False):
    # get dim order
    if default_order is True:
      dim_order = self.trimmed_default_dim_order()
    else:
      dim_order = self.im_dim_order.copy()
    
    # ignore channel?
    if ignore_channel is True:
      if self.use_channel_axis is True:
        dim_order.pop(self.dim_idx('C'))
        
    # ignore channel?
    # if processing a timeseries but only use one frame
    if ignore_time is True:
      if self.is_timeseries(ignore_one = False) is True:
        dim_order.pop(self.dim_idx("T"))
    
    # return values
    return [self.dim_val(x) for x in dim_order]
  
  """
  get default dimension order
  """
  def default_dim_order(self, ignore_channel = False, ignore_z = False):
    # get dimension order of image
    # align with napari's TZ(C)YX
    # https://forum.image.sc/t/napari-fix-time-axis-to-roll-only-through-spatial-dimensions/52132
    # https://forum.image.sc/t/4d-vectors-supported-in-napari/44158/2
    default_order = tuple()
    
    # trim list to values present in data
    # default_list = self.trimmed_default_dim_order()
    default_list = self.dim_idx(
      ignore_channel = ignore_channel, ignore_z = ignore_z, default_order = True)
    
    # for x in default_list:
    #   if x in self.im_dim_order:
    #     default_order += (self.im_dim_order.index(x),)
            
    return(default_list)
  
  """
  Return spatial coord axis
  """
  def spatial_axis(self):
    if self.is_3D():
      return ['X', 'Y', 'Z']
    else:
      return ['X', 'Y']
      
  """
  Is image 3D
  """
  def is_3D(self, ignore_one = True):
    if ignore_one is True:
      return 'Z' in self.im_dim_order and self.dim_val('Z') > 1
    else:
      return 'Z' in self.im_dim_order
  
  """
  Is image timeseries
  """
  def is_timeseries(self, ignore_one = True):
    if ignore_one is True:
      return 'T' in self.im_dim_order and self.dim_val('T') > 1
    else:
      return 'T' in self.im_dim_order
  
  """
  Trim default dimension order
  """
  def trimmed_default_dim_order(self, im_dim_order = None, remove_dim = None,
                                ignore_channel = False, ignore_time = False, squeeze = False):
    # set dimension order to self if not set
    if im_dim_order is None:
      im_dim_order = self.im_dim_order
    
    ret_val = [x for x in self.default_order if x in im_dim_order]
    
    # remove dims
    if remove_dim is not None:
      ret_val.remove(remove_dim)
    if ignore_channel is True:
      ret_val.remove('C')
    if ignore_time is True:
      ret_val.remove('T')
      
    # squeeze?
    if squeeze is True:
      ret_val = [x for x in ret_val if self.dim_val(x) > 1]
    
    return ret_val
  
  """
  Transpose axis based on reference dimension order
  """
  def transpose_array_axis(self, array_to_transpose, array_dim_order, ref_dim_order):
    transposed_data = array_to_transpose.copy()
    
    # get order
    transpose_order = [array_dim_order.index(x) for x in ref_dim_order]
    
    # transpose
    transposed_data = array_to_transpose.transpose(transpose_order)

    return transposed_data
  
  """
  Transpose to be safe that other applications can read the image
  """
  def transpose_array_axis_to_save(self, array_data):
    return self.transpose_array_axis(
        array_data,
        self.im_dim_order,
        self.trimmed_default_dim_order()
      )
      
  """
  Transpose to be safe that the array complies
  with the shown image
  """
  def transpose_array_axis_to_show(self, array_data,
    ignore_channel_axis = True, ignore_time_axis = True, use_im_dim = True):
    ret_array = array_data
    
    # adjust for channel if not required
    # this is usually the case for segmentation
    # where all channels are combined
    array_order = self.trimmed_default_dim_order()
    ref_order = self.im_dim_order.copy()
    
    # check array order with array dim to remove axis
    if use_im_dim is True:
      array_order = np.array(self.im_dim_order)[
          [i for i, x in enumerate(self.im_dim) if x > 1]
      ].tolist()
      
      ref_order = [x for x in ref_order if x in array_order]
      
    # remove further axis
    if ignore_channel_axis is True:
      if 'C' in array_order: array_order.remove('C')
      if 'C' in ref_order: ref_order.remove('C')
      
    if ignore_time_axis is True:
      if "T" in array_order: array_order.remove("T")
      if "T" in ref_order: ref_order.remove("T")
    
    array_data = self.transpose_array_axis(array_data, array_order, ref_order)
    
    return ret_array
  
  """
  Get normalisation range for channel axis from lowest resolution
  """
  def get_norm_range_from_low_res(self, im_dat, channels = None, min_perc = 2, max_perc = 98):
    # get max from lowest res
    low_res = im_dat[len(im_dat) - 1]
    
    return self.get_norm_range(
      low_res, channels, min_perc = min_perc, max_perc = max_perc)
  
  """
  Get normalisation range for channel axis
  """
  def get_norm_range(self, im_dat, channels = None, min_perc = 2, max_perc = 98):
    if channels is not None:
      im_dat = np.take(im_dat, channels,
        axis = self.dim_idx('C'))
    
    norm_min = math.floor(np.percentile(im_dat.flatten(), min_perc))
    norm_max = math.ceil(np.percentile(im_dat.flatten(), max_perc))
    
    return (norm_min, norm_max)
  
  """
  Get dim id from centroid column
  """
  def get_idx_from_centroid_col(self, col, ignore_time = False, ignore_channel = False,
                                ignore_z = False):
    dim_id = col.split('_')[1].upper()
    
    return self.dim_idx(
      dim_id, ignore_channel = ignore_channel, ignore_time = ignore_time,
      ignore_z = ignore_z
      )
  
  """
  Get padded cube for position
  """
  def get_padded_cube(self, pos,
    xy_padding = 50, z_padding = 5, xy_snap = 50, z_snap = 5):
      # make a cube over channels and time
      padded_cube = np.zeros((len(self.im_dim), 2))
      padded_cube[:, 1] = self.im_dim
  
      # XY
      for i in (self.dim_idx("X"), self.dim_idx("Y")):
        padded_cube[i, :] = (pos[i] - xy_padding, pos[i] + xy_padding)
      
      # Z
      if self.dim_idx("Z") is not None:
        padded_cube[self.dim_idx("Z"),:] = (pos[self.dim_idx("Z")] - z_padding, pos[self.dim_idx("Z")] + z_padding)
  
      # are the limits ok?
      padded_cube[padded_cube < 0] = 0
  
      # max value for XYZ
      for i in (self.dim_idx("X"), self.dim_idx("Y"), self.dim_idx("Z")):
        if i is not None:
          dim_max = self.im_dim[i]
          padded_cube[i, padded_cube[i, :] > dim_max] = dim_max - 1
          
      # snap
      for i in (self.dim_idx("X"), self.dim_idx("Y")):
        padded_cube[i, 0] = math_helpers.round_down(padded_cube[i, 0], xy_snap)
        padded_cube[i, 1] = math_helpers.round_up(padded_cube[i, 1], xy_snap)
      
      if self.dim_idx("Z") is not None:
        padded_cube[self.dim_idx("Z"), 0] = math_helpers.round_down(
          padded_cube[self.dim_idx("Z"), 0], z_snap)
        padded_cube[self.dim_idx("Z"), 1] = math_helpers.round_up(
          padded_cube[self.dim_idx("Z"), 1], z_snap)
  
      return padded_cube.astype(int)
  
