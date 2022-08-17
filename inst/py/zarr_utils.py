import zarr
import tifffile
import dask.array as da
import dask
import os
import numpy as np
from copy import copy

import py.slice_utils as slice_utils

"""
Open path as zarr
"""
def open_as_zarr(im_path, multiscales = None, as_dask = False, mode = 'r'):
  # get extension
  im_ext = os.path.splitext(im_path)[1]
  
  # open zarr
  if im_ext == ".zarr":
    im_data, zarr_group_info = open_zarr(
      im_path, multiscales = multiscales, as_dask = as_dask, mode = mode,
      omezarr = im_path.endswith("ome.zarr"))
  else:
    # open image
    im_data, zarr_group_info = open_image_as_zarr(
      im_path, multiscales = multiscales, as_dask = as_dask)
  
  return im_data, zarr_group_info

"""
Open Zarr converted from bioformats2raw
"""
def open_zarr(zarr_path, mode = 'r', multiscales = None, as_dask = False,
              omezarr = False):
  # open zarr store
  # TODO I could not get ome-zarr-py to work ..
  # there should only be one image for OME
  # z = zarr.open(zarr_path, mode='r')[0]
  
  # convert to list
  zarr_data, zarr_group_info = zarr_data_to_list(
    zarr_path, multiscales = multiscales, mode = mode, omezarr = omezarr)
  
  # convert to dask
  if as_dask is True:
    zarr_data = zarr_data_to_dask(zarr_data)
  
  return zarr_data, zarr_group_info

"""
Open image as Zarr
"""
def open_image_as_zarr(filepath, multiscales = None, as_dask = False):
  # convert to Zarr
  # https://forum.image.sc/t/wholeslide-ome-tiff-in-napari-from-ipython/52872/5
  store = tifffile.imread(filepath, aszarr = True)
  
  # convert to list
  zarr_data, zarr_group_info = zarr_data_to_list(
    store, multiscales = multiscales)
  
  # convert to dask
  if as_dask is True:
    zarr_data = zarr_data_to_dask(zarr_data)

  return zarr_data, zarr_group_info

"""
Open image as dask copy
If the image is zarr, it will create a dask array
"""
def get_dask_copy(image_array):
  # copy image data to dask array
  if type(image_array) is zarr.core.Array:
    image_array = da.from_zarr(image_array)
      
  # copy dask
  return copy(image_array)

"""
Fortify object
"""
def fortify(im_array):
  if type(im_array) is zarr.core.Array:
    return im_array[:]
  elif type(im_array) is dask.array.core.Array:
    return im_array.compute()
  
  return im_array
  
"""
Return chunksize
"""
def chunks(im_array):
  if type(im_array) is zarr.core.Array:
    return im_array.chunks
  elif type(im_array) is dask.array.core.Array:
    return im_array.chunksize
  
  return None

"""
Open labels as Dask array
"""
def open_labels_as_dask(filepath, multiscales = None):
  zarr_data, zarr_group_info = open_labels_as_zarr(
    filepath, multiscales = multiscales)

  return zarr_data_to_dask(zarr_data), zarr_group_info

"""
Open labels as zarr array
"""
def open_labels_as_zarr(filepath, multiscales = None):
  return zarr_data_to_list(
    filepath, multiscales = multiscales, data_group = "labels")
  
"""
Open zarr data as dask
https://forum.image.sc/t/napari-labels-layer-axis-order/53523/10
"""
def zarr_data_to_dask(zarr_data):
  # convert to dask array
  # https://forum.image.sc/t/napari-labels-layer-axis-order/53523/10
  dask_data = [da.from_zarr(arr) for arr in zarr_data]
  
  return dask_data
  
"""
Load zarr group into list
"""
def zarr_data_to_list(zarr_store, multiscales = None, data_group = 'datasets',
                      mode = 'r', omezarr = False):
  # check that the directory does not have '.zarray' and '.zgroup'
  if type(zarr_store) == str:
    if os.path.exists(os.path.join(zarr_store, '.zarray')) and os.path.exists(os.path.join(zarr_store, '.group')):
      # remove .zarray
      os.unlink(os.path.join(zarr_store, '.zarray'))
  
  zgroup = zarr.open(zarr_store, mode = mode)
  
  # get first image in ome zarr bundle
  if omezarr is True:
    zgroup = zgroup[0]
  
  # set info
  # https://github.com/zarr-developers/zarr-python/blob/master/zarr/util.py
  # are there different pyramid levels?
  if 'multiscales' in zgroup.attrs and type(zgroup) != zarr.core.Array:
    zarr_group_info = [dict(zgroup[i].info.obj.info_items()) for i in range(len(zgroup))]
    
    # slice multiscales
    if multiscales is None:
      multiscale_slices = slice(None)
    else:
      if multiscales > len(zgroup.attrs['multiscales'][0][data_group]):
        multiscales = len(zgroup.attrs['multiscales'][0][data_group])
      
      multiscale_slices = slice(0, multiscales, 1)
    
    zarr_data = [
      zgroup[int(dataset['path'])]
      for dataset in zgroup.attrs['multiscales'][0][data_group][multiscale_slices]
    ]
  else:
    zarr_group_info = [dict(zgroup.info.obj.info_items())]
    zarr_data = [zgroup]
  
  return zarr_data, zarr_group_info

"""
Save as mutliscales
"""
def save_dask_as_zarr_multiscales(image_array, im_path, nscales = 1):
  # remove contents of directory
  shutil.rmtree(im_path)
  
  # create multiscales
  multiscales_zarr = zarr.open(im_path, mode = 'w')

  # add path information to attributes
  multiscales_zarr.attrs['multiscales'] = [{'datasets': [
    {'path': f'{x}'} for x in range(0, nscales)
  ]}]
  
  # go through scales
  for i in range(nscales):
    # create scale
    new_scale = image_array[::(2**i), ::(2**i)]

    # add to group
    new_scale.to_zarr(
      os.path.join(im_path, str(i)),
      overwrite = True,
      # dimension_separator = "/"
      )

"""
Create zarr from ndarray
"""
def create_zarr_from_ndarray(im_array, dim_utils, reference_zarr,
                             ignore_channel = False, copy_values = True):
  # ignore channel for chunks?
  im_chunks = chunks(reference_zarr)
  
  if ignore_channel is True:
    im_chunks = list(im_chunks)
    im_chunks.pop(dim_utils.dim_idx('C'))
    im_chunks = tuple(im_chunks)
  
  print(im_array.shape)
  print(im_chunks)
  
  # create zarr and copy    
  new_zarr = zarr.create(
    mode = 'w',
    shape = im_array.shape,
    chunks = im_chunks,
    dtype = im_array.dtype
    )
  
  if copy_values is True:
    new_zarr[:] = im_array
  
  return new_zarr, im_chunks

"""
Create multiscales for zarr
https://zarr.readthedocs.io/en/stable/tutorial.html
"""
def create_multiscales(im_array, filepath, dim_utils = None,
                       x_idx = None, y_idx = None,
                       nscales = 1, keyword = 'datasets',
                       ignore_channel = False, reference_zarr = None):
  # create multiscales
  multiscales_zarr = zarr.open(filepath, mode = 'w')
  
  # add path information to attributes
  multiscales_zarr.attrs['multiscales'] = [{keyword: [
    # {'path': f'labels/{x:02d}'} for x in range(0, nscales)
    {'path': f'{x}'} for x in range(0, nscales)
  ]}]
  
  # add first scales
  if isinstance(im_array, dask.array.core.Array):
    im_array.to_zarr(
      os.path.join(filepath, "0"),
      # dimension_separator = "/"
    )
    
    im_chunks = chunks(im_array)
  elif isinstance(im_array, zarr.core.Array):
    multiscales_zarr[0] = im_array
    
    im_chunks = chunks(im_array)
  else:
    multiscales_zarr[0], im_chunks = create_zarr_from_ndarray(
      im_array, dim_utils,
      reference_zarr = reference_zarr,
      ignore_channel = ignore_channel)
  
  if nscales > 1:
    # create slices for new scale
    slices = slice_utils.create_slices_multiscales(
      im_array.shape, dim_utils = dim_utils,
      x_idx = x_idx, y_idx = y_idx,
      nscales = nscales - 1, ignore_channel = ignore_channel
    )
    
    # go through slices to build scales
    for i, x in enumerate(slices):
      # add to group
      multiscales_zarr.create_dataset(
        # f'{i:02d}',
        i + 1,
        # TODO not sure why this stopped working
        # https://github.com/zarr-developers/zarr-python/issues/962
        data = im_array[x] if isinstance(im_array, dask.array.core.Array) is False else fortify(im_array[x]),
        chunks = im_chunks,
        # dimension_separator = "/"
        )

"""
Apply minimum to array
"""
def apply_min(x, im_min):
  x[x == 0] = im_min
  return x

"""
Get minimum from lowest resolution
"""
def get_minmax_from_low_res(im_dat):
  # get min and max from lowest dimension
  low_res = fortify(im_dat[len(im_dat) - 1])
  
  # get min and max
  im_min = low_res[low_res > 0].min()
  im_max = low_res.max()
  
  return im_min, im_max
