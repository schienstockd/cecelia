"""
Image dimension utilities built on top of OME-XML metadata.

DimUtils wraps an ome_types OME object and provides a clean API for:
  - Resolving dimension order (TZCYX) from the zarr/OME metadata.
  - Mapping named axes ('T', 'Z', 'C', 'Y', 'X') to array indices.
  - Generating numpy slice tuples for per-channel or per-plane access.
  - Querying physical pixel sizes and computing display scales for Napari.
  - Building padded crop cubes around spatial coordinates.

The dimension order determined by calc_image_dimensions() is stored in
im_dim_order and im_dim and is used by all subsequent slice/index calls.
"""

import math
import numpy as np
from copy import copy

import cecelia.utils.math_helpers as math_helpers
import cecelia.utils.ome_xml_utils as ome_xml_utils


class DimUtils:
  def __init__(self, omexml, use_channel_axis = True):
    self.default_order = list("TZCYX")
    self.omexml = omexml
    self.use_channel_axis = use_channel_axis

    self.im_dim = None
    self.im_dim_order = None

  def pixels(self):
    return self.omexml.images[0].pixels

  def calc_image_dimensions(self, im_shape, im_dim_dict = None):
    if im_dim_dict is None:
      im_dim_dict = ome_xml_utils.get_im_size_dict(self.omexml)

    if len(im_dim_dict) != len(im_shape):
      for x in im_dim_dict.copy().items():
        if x[1] == 1:
          im_dim_dict.pop(x[0])

    im_dim = list(im_dim_dict.values())
    im_dim_order = list(im_dim_dict.keys())

    im_shape = list(im_shape)

    print(im_shape)
    print(im_dim)
    print(im_dim_order)

    if im_shape != im_dim:
      im_shape.reverse()

      if im_shape == im_dim:
        im_dim.reverse()
        im_dim_order.reverse()
      else:
        im_shape.reverse()

        im_shape_order = np.zeros(len(im_dim), dtype=int)

        for i, x in enumerate(im_dim):
          idx = im_shape.index(x)
          im_shape_order[i] = idx
          im_shape[idx] = 0

        im_dim = [im_dim[i] for i in im_shape_order]
        im_dim_order = [im_dim_order[i] for i in im_shape_order]

    self.im_dim = im_dim
    self.im_dim_order = im_dim_order

  def create_channel_slices(self, channel = None):
    c_val = self.dim_val('C')
    c_idx = self.dim_idx('C')

    if channel is None:
      slice_list = [copy(slice_list) for _ in range(c_val)]
      for i in range(c_val):
        slice_list[i][c_idx] = slice(i, i + 1, 1)
      slice_list = [tuple(x) for x in slice_list]
    else:
      slice_list = [slice(None) for i in range(len(self.im_dim))]
      slice_list[c_idx] = slice(channel, channel + 1, 1)
      slice_list = tuple(slice_list)

    return slice_list

  def expand_slices(self, slices, dim):
    dim_val = self.dim_val(dim)
    dim_idx = self.dim_idx(dim)

    slice_list = [copy(slices[i]) for i in range(len(slices)) for _ in range(dim_val)]

    for i in range(len(slices)):
      for j in range(dim_val):
        slice_list[(i * dim_val) + j][dim_idx] = slice(j, j + 1, 1)

    return [tuple(x) for x in slice_list]

  def pixel_type(self):
    return self.omexml.images[0].pixels.type

  def is_32_bit(self):
    return self.pixel_type() == 'uint32'

  def im_physical_sizes(self, default = 1):
    axis_sizes = {
      'x': self.omexml.images[0].pixels.physical_size_x,
      'y': self.omexml.images[0].pixels.physical_size_y,
      'z': self.omexml.images[0].pixels.physical_size_z
    }
    return {i: x if x is not None else default for i, x in axis_sizes.items()}

  def im_physical_size(self, axis, default = 1):
    axis_size = default
    if axis == 'x':
      axis_size = self.omexml.images[0].pixels.physical_size_x
    elif axis == 'y':
      axis_size = self.omexml.images[0].pixels.physical_size_y
    elif axis == 'z':
      axis_size = self.omexml.images[0].pixels.physical_size_z
    return axis_size if axis_size is not None else default

  def im_physical_units(self, default = 'um'):
    axis_units = {
      'x': self.omexml.images[0].pixels.physical_size_x_unit.value,
      'y': self.omexml.images[0].pixels.physical_size_y_unit.value,
      'z': self.omexml.images[0].pixels.physical_size_z_unit.value
    }
    return {i: x.replace('µ', 'u') if x is not None else default for i, x in axis_units.items()}

  def im_physical_unit(self, axis, default = 'um'):
    axis_unit = default
    if axis == 'x':
      axis_unit = self.omexml.images[0].pixels.physical_size_x_unit.value
    elif axis == 'y':
      axis_unit = self.omexml.images[0].pixels.physical_size_y_unit.value
    elif axis == 'z':
      axis_unit = self.omexml.images[0].pixels.physical_size_z_unit.value
    return axis_unit.replace('µ', 'u') if axis_unit is not None else default

  def im_scale(self, dims = list(), as_dict = False, upper = True):
    size_x = self.omexml.images[0].pixels.physical_size_x
    size_y = self.omexml.images[0].pixels.physical_size_y
    size_z = self.omexml.images[0].pixels.physical_size_z

    im_scale = list(np.ones_like(self.im_dim))
    im_scale[self.dim_idx('X')] = size_x
    im_scale[self.dim_idx('Y')] = size_y

    if self.is_3D():
      im_scale[self.dim_idx('Z')] = size_z

    im_scale = [x if x is not None else 1 for x in im_scale]

    if len(dims) > 0:
      dims = [x.upper() for x in dims]
      new_scale = list()
      for x in [self.dim_idx(x) for x in self.im_dim_order if x in dims]:
        new_scale.append(im_scale[x])
      im_scale = new_scale

    if as_dict is True:
      if len(dims) > 0:
        im_scale = {x: im_scale[i] for i, x in enumerate([x for x in self.im_dim_order if x in dims])}
      else:
        im_scale = {x: im_scale[i] for i, x in enumerate(self.im_dim_order)}

    if upper is False:
      im_scale = {i.lower(): x for i, x in im_scale.items()}

    return im_scale

  def used_dim_idx(self, ignore_channel = False, ignore_time = False, cutoff = 1):
    ignore_axis = []
    if ignore_channel is True:
      ignore_axis += ['C']
    if ignore_time is True:
      ignore_time += ['T']
    return [self.dim_idx(x, ignore_channel = ignore_channel, ignore_time = ignore_time) \
      for i, x in enumerate(self.im_dim_order) if self.im_dim[i] > cutoff and x not in ignore_axis]

  def dim_idx(self, dim = None, ignore_channel = False, ignore_time = False,
              ignore_z = False, default_order = False, squeeze = False, drop_z = False,
              drop_time = False):
    ret_val = None

    if default_order is True:
      dim_order = self.trimmed_default_dim_order()
    else:
      dim_order = self.im_dim_order.copy()

    if ignore_channel is True:
      if self.use_channel_axis is True:
        dim_order.pop(self.dim_idx('C'))

    if (drop_time is True and self.is_timeseries() is False) or (ignore_time is True and self.is_timeseries() is True):
      t_idx = self.dim_idx('T', ignore_channel = ignore_channel)
      if t_idx is not None: dim_order.pop(t_idx)

    if (drop_z is True and self.is_3D() is False) or (ignore_z is True and self.is_3D() is True):
      dim_order.pop(self.dim_idx(
        'Z', ignore_channel = ignore_channel, ignore_time = ignore_time
        ))

    if squeeze is True:
      dim_vals = [self.dim_val(x) for x in dim_order]
      for i, x in enumerate(dim_vals):
        if x == 1:
          dim_order.pop(i)

    if dim is not None:
      dim = dim.upper()
      if dim in dim_order:
        ret_val = dim_order.index(dim.upper())
    else:
      ret_val = [dim_order.index(x) for x in self.im_dim_order if x in dim_order]

    return ret_val

  def dim_val(self, dim, scale = 0, **kwargs):
    dim_val = 1
    if self.dim_idx(dim) >= 0:
      dim_val = self.im_dim[self.dim_idx(dim, **kwargs)]
    if dim in ['X', 'Y']:
      dim_val = math.floor(dim_val / 2**scale)
    return dim_val

  def dim_vals(self, default_order = False, ignore_channel = False, ignore_time = False):
    if default_order is True:
      dim_order = self.trimmed_default_dim_order()
    else:
      dim_order = self.im_dim_order.copy()

    if ignore_channel is True:
      if self.use_channel_axis is True:
        dim_order.pop(self.dim_idx('C'))

    if ignore_time is True:
      if self.is_timeseries(ignore_one = False) is True:
        dim_order.pop(self.dim_idx('T'))

    return [self.dim_val(x) for x in dim_order]

  def default_dim_order(self, ignore_channel = False, ignore_z = False):
    default_order = tuple()
    default_list = self.dim_idx(
      ignore_channel = ignore_channel, ignore_z = ignore_z, default_order = True)
    return(default_list)

  def spatial_axis(self):
    if self.is_3D():
      return ['Z', 'Y', 'X']
    else:
      return ['Y', 'X']

  def is_3D(self, ignore_one = True):
    if ignore_one is True:
      return 'Z' in self.im_dim_order and self.dim_val('Z') > 1
    else:
      return 'Z' in self.im_dim_order

  def is_timeseries(self, ignore_one = True):
    if ignore_one is True:
      return 'T' in self.im_dim_order and self.dim_val('T') > 1
    else:
      return 'T' in self.im_dim_order

  def trimmed_default_dim_order(self, im_dim_order = None, remove_dim = None,
                                ignore_channel = False, ignore_time = False, squeeze = False):
    if im_dim_order is None:
      im_dim_order = self.im_dim_order

    ret_val = [x for x in self.default_order if x in im_dim_order]

    if remove_dim is not None:
      ret_val.remove(remove_dim)
    if ignore_channel is True:
      ret_val.remove('C')
    if ignore_time is True:
      ret_val.remove('T')

    if squeeze is True:
      ret_val = [x for x in ret_val if self.dim_val(x) > 1]

    return ret_val

  def transpose_array_axis(self, array_to_transpose, array_dim_order, ref_dim_order, integrate_time = False):
    transposed_data = array_to_transpose.copy()

    if integrate_time is True:
      array_dim_order.remove('T')
      ref_dim_order.remove('T')

    transpose_order = [array_dim_order.index(x) for x in ref_dim_order]
    transposed_data = array_to_transpose.transpose(transpose_order)
    return transposed_data

  def transpose_array_axis_to_save(self, array_data):
    return self.transpose_array_axis(
        array_data,
        self.im_dim_order,
        self.trimmed_default_dim_order()
      )

  def get_norm_range_from_low_res(self, im_dat, channels = None, min_perc = 2, max_perc = 98):
    low_res = im_dat[len(im_dat) - 1]
    return self.get_norm_range(low_res, channels, min_perc = min_perc, max_perc = max_perc)

  def get_norm_range(self, im_dat, channels = None, min_perc = 2, max_perc = 98):
    if channels is not None:
      im_dat = np.take(im_dat, channels, axis = self.dim_idx('C'))
    norm_min = math.floor(np.percentile(im_dat.flatten(), min_perc))
    norm_max = math.ceil(np.percentile(im_dat.flatten(), max_perc))
    return (norm_min, norm_max)

  def get_idx_from_centroid_col(self, col, ignore_time = False, ignore_channel = False, ignore_z = False):
    dim_id = col.split('_')[1].upper()
    return self.dim_idx(dim_id, ignore_channel = ignore_channel, ignore_time = ignore_time, ignore_z = ignore_z)

  def get_padded_cube(self, pos, xy_padding = 50, z_padding = 5, xy_snap = 50, z_snap = 5):
    padded_cube = np.zeros((len(self.im_dim), 2))
    padded_cube[:, 1] = self.im_dim

    for i in (self.dim_idx("X"), self.dim_idx("Y")):
      padded_cube[i, :] = (pos[i] - xy_padding, pos[i] + xy_padding)

    if self.dim_idx("Z") is not None:
      padded_cube[self.dim_idx("Z"),:] = (pos[self.dim_idx("Z")] - z_padding, pos[self.dim_idx("Z")] + z_padding)

    padded_cube[padded_cube < 0] = 0

    for i in (self.dim_idx("X"), self.dim_idx("Y"), self.dim_idx("Z")):
      if i is not None:
        dim_max = self.im_dim[i]
        padded_cube[i, padded_cube[i, :] > dim_max] = dim_max - 1

    for i in (self.dim_idx("X"), self.dim_idx("Y")):
      padded_cube[i, 0] = math_helpers.round_down(padded_cube[i, 0], xy_snap)
      padded_cube[i, 1] = math_helpers.round_up(padded_cube[i, 1], xy_snap)

    if self.dim_idx("Z") is not None:
      padded_cube[self.dim_idx("Z"), 0] = math_helpers.round_down(padded_cube[self.dim_idx("Z"), 0], z_snap)
      padded_cube[self.dim_idx("Z"), 1] = math_helpers.round_up(padded_cube[self.dim_idx("Z"), 1], z_snap)

    return padded_cube.astype(int)
