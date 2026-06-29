"""
Zarr I/O utilities for OME-ZARR and plain zarr stores.

Handles opening, reading, and writing zarr arrays with support for:
  - OME-ZARR multiscale groups (bioformats2raw output)
  - Plain TIFF files via tifffile's aszarr interface
  - Dask-backed lazy loading for large images
  - Multiscale pyramid creation with power-of-two downsampling

All paths that end in .ome.zarr are opened via the OME series group wrapper
(series index 0).  Other .zarr paths are opened directly.
"""

import zarr
import tifffile
import dask.array as da
import dask
import os
import shutil
import numpy as np
from copy import copy

import py.utils.slice_utils as slice_utils


def open_as_zarr(im_path, multiscales=None, as_dask=False, mode='r'):
    im_ext = os.path.splitext(im_path)[1]
    if im_ext == ".zarr":
        im_data, zarr_group_info = open_zarr(im_path, multiscales=multiscales, as_dask=as_dask, mode=mode)
    else:
        im_data, zarr_group_info = open_image_as_zarr(im_path, multiscales=multiscales, as_dask=as_dask)
    return im_data, zarr_group_info


def open_zarr(zarr_path, mode='r', multiscales=None, as_dask=False):
    zarr_data, zarr_group_info = zarr_data_to_list(
        zarr_path, multiscales=multiscales, mode=mode, omezarr=zarr_path.endswith('.ome.zarr'))
    if as_dask is True:
        zarr_data = zarr_data_to_dask(zarr_data)
    return zarr_data, zarr_group_info


def open_image_as_zarr(filepath, multiscales=None, as_dask=False):
    store = tifffile.imread(filepath, aszarr=True)
    zarr_data, zarr_group_info = zarr_data_to_list(store, multiscales=multiscales)
    if as_dask is True:
        zarr_data = zarr_data_to_dask(zarr_data)
    return zarr_data, zarr_group_info


def get_dask_copy(image_array):
    if isinstance(image_array, zarr.Array):
        image_array = da.from_zarr(image_array)
    return copy(image_array)


def fortify(im_array):
    if isinstance(im_array, zarr.Array):
        return im_array[:]
    elif isinstance(im_array, dask.array.core.Array):
        return im_array.compute()
    return im_array


def chunks(im_array):
    im_chunks = None
    if isinstance(im_array, zarr.Array):
        im_chunks = im_array.chunks
    elif isinstance(im_array, dask.array.core.Array):
        im_chunks = im_array.chunksize
    return [x if isinstance(x, int) else x[0] for x in im_chunks]


def open_labels_as_dask(filepath, multiscales=None):
    zarr_data, zarr_group_info = open_labels_as_zarr(filepath, multiscales=multiscales)
    return zarr_data_to_dask(zarr_data), zarr_group_info


def open_labels_as_zarr(filepath, multiscales=None):
    return zarr_data_to_list(filepath, multiscales=multiscales, data_group='labels')


def zarr_data_to_dask(zarr_data):
    return [da.from_zarr(arr) for arr in zarr_data]


def zarr_data_to_list(zarr_store, multiscales=None, data_group='datasets', mode='r', omezarr=False):
    if type(zarr_store) == str:
        if os.path.exists(os.path.join(zarr_store, '.zarray')) and os.path.exists(os.path.join(zarr_store, '.group')):
            os.unlink(os.path.join(zarr_store, '.zarray'))

    zgroup = zarr.open(zarr_store, mode=mode)

    if omezarr is True and 'multiscales' not in zgroup.attrs:
        # bioformats2raw wraps arrays in a series group at "0/".
        # Flat OME-ZARRs (written by create_multiscales) have multiscales at root.
        zgroup = zgroup["0"]

    if 'multiscales' in zgroup.attrs and not isinstance(zgroup, zarr.Array):
        zarr_group_info = None

        if multiscales is None:
            multiscale_slices = slice(None)
        else:
            if multiscales > len(zgroup.attrs['multiscales'][0][data_group]):
                multiscales = len(zgroup.attrs['multiscales'][0][data_group])
            multiscale_slices = slice(0, multiscales, 1)

        zarr_data = [
            zgroup[dataset['path']]
            for dataset in zgroup.attrs['multiscales'][0][data_group][multiscale_slices]
        ]
    else:
        zarr_group_info = [dict(zgroup.info.obj.info_items())]
        zarr_data = [zgroup]

    return zarr_data, zarr_group_info


def save_dask_as_zarr_multiscales(image_array, im_path, nscales=1):
    shutil.rmtree(im_path)
    multiscales_zarr = zarr.open(im_path, mode='w')
    multiscales_zarr.attrs['multiscales'] = [{'datasets': [
        {'path': f'{x}'} for x in range(0, nscales)
    ]}]
    for i in range(nscales):
        new_scale = image_array[::(2**i), ::(2**i)]
        new_scale.to_zarr(os.path.join(im_path, str(i)), overwrite=True)


def create_zarr_from_ndarray(im_array, dim_utils, reference_zarr=None, im_chunks=None,
                             store_path=None, ignore_channel=False, ignore_time=False,
                             copy_values=True, remove_previous=False):
    if im_chunks is None:
        im_chunks = chunks(reference_zarr)

    if ignore_channel is True:
        im_chunks = list(im_chunks)
        im_chunks.pop(dim_utils.dim_idx('C'))
        im_chunks = tuple(im_chunks)

    if ignore_time is True:
        im_chunks = list(im_chunks)
        im_chunks.pop(dim_utils.dim_idx('T', ignore_channel=ignore_channel))
        im_chunks = tuple(im_chunks)

    if len(im_array.shape) != len(im_chunks):
        im_chunks = list(im_chunks)
        im_chunks.pop(0)
        im_chunks = tuple(im_chunks)

    if remove_previous is True and os.path.exists(store_path):
        shutil.rmtree(store_path)

    new_zarr = zarr.open_array(
        store_path,
        mode='w',
        shape=im_array.shape,
        chunks=im_chunks,
        dtype=im_array.dtype,
        zarr_format=2,
    )

    if copy_values is True:
        new_zarr[:] = im_array

    return new_zarr, im_chunks


def create_multiscales(im_array, filepath, dim_utils=None, im_chunks=None,
                       x_idx=None, y_idx=None, nscales=1, keyword='datasets',
                       ignore_channel=False, reference_zarr=None, mode='w',
                       squeeze=False, idx_adjust=0):
    # Write zarr v2 format so napari and zarr_data_to_list can read .zattrs directly.
    multiscales_zarr = zarr.open_group(filepath, mode=mode, zarr_format=2)

    # Build datasets entries; include physical scale in coordinateTransformations when available.
    scale_base = None
    if dim_utils is not None and dim_utils.im_dim_order:
        raw = dim_utils.im_scale()
        scale_base = [float(s) if s is not None else 1.0 for s in raw]

    datasets = []
    for lvl in range(nscales):
        entry = {'path': str(lvl)}
        if scale_base is not None:
            # XY axes are downsampled by 2^level; all other axes keep base scale.
            level_scale = [
                s * (2 ** lvl) if dim_utils.im_dim_order[i] in ('X', 'Y') else s
                for i, s in enumerate(scale_base)
            ]
            entry['coordinateTransformations'] = [{'type': 'scale', 'scale': level_scale}]
        datasets.append(entry)

    ms_entry = {keyword: datasets}
    if dim_utils is not None and dim_utils.im_dim_order:
        ms_entry['axes'] = [{'name': ax.lower()} for ax in dim_utils.im_dim_order]
    multiscales_zarr.attrs['multiscales'] = [ms_entry]

    if isinstance(im_array, dask.array.core.Array):
        # Write into the group so the sub-array inherits zarr v2 format.
        dest = multiscales_zarr.create_array(
            "0", shape=im_array.shape, chunks=im_array.chunksize, dtype=im_array.dtype
        )
        da.store(im_array, dest, lock=False)
        im_chunks = chunks(im_array)
    elif isinstance(im_array, zarr.Array):
        dest = multiscales_zarr.create_array("0", shape=im_array.shape, chunks=im_array.chunks, dtype=im_array.dtype)
        dest[:] = im_array[:]
        im_chunks = chunks(im_array)
    else:
        _, im_chunks = create_zarr_from_ndarray(
            im_array, dim_utils,
            reference_zarr=reference_zarr,
            im_chunks=im_chunks,
            store_path=os.path.join(filepath, "0"),
            ignore_channel=ignore_channel)

    if nscales > 1:
        slices = slice_utils.create_slices_multiscales(
            im_array.shape, dim_utils=dim_utils,
            x_idx=x_idx, y_idx=y_idx,
            nscales=nscales - 1, ignore_channel=ignore_channel,
            squeeze=squeeze, idx_adjust=idx_adjust
        )

        for i, x in enumerate(slices):
            data_slice = fortify(im_array[x]) if isinstance(im_array, dask.array.core.Array) else im_array[x]
            multiscales_zarr.create_array(str(i + 1), data=data_slice, chunks=im_chunks)


def apply_min(x, im_min):
    x[x == 0] = im_min
    return x


def get_minmax_from_low_res(im_dat):
    low_res = fortify(im_dat[len(im_dat) - 1])
    im_min = low_res[low_res > 0].min()
    im_max = low_res.max()
    return im_min, im_max
