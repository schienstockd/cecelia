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

import cecelia.utils.slice_utils as slice_utils


def open_as_zarr(im_path, multiscales=None, as_dask=False, mode='r'):
    im_ext = os.path.splitext(im_path)[1]
    if im_ext == ".zarr":
        im_data, zarr_group_info = open_zarr(im_path, multiscales=multiscales, as_dask=as_dask, mode=mode)
    else:
        im_data, zarr_group_info = open_image_as_zarr(im_path, multiscales=multiscales, as_dask=as_dask)
    return im_data, zarr_group_info


def open_zarr(zarr_path, mode='r', multiscales=None, as_dask=False):
    zarr_data, zarr_group_info = zarr_data_to_list(zarr_path, multiscales=multiscales, mode=mode)
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


def plane_chunks(shape, dim_utils=None, xy_tile=512):
    """Per-plane chunking for an OME-ZARR consumed by napari: 1 along the non-spatial axes
    (T/C/Z) and `xy_tile`-capped along the two spatial axes (Y/X). napari slices per (t,c,z),
    so a chunk must NOT span the time/channel axes — `dask`'s `chunks='auto'` packs the whole
    time series into one ~128 MB chunk, which makes a single plane cost a full-timecourse read
    (slow first open, fast once OS-cached). Spatial axes come from `dim_utils` when available,
    else assumed to be the last two (bioformats2raw TCZYX order)."""
    n = len(shape)
    spatial = set()
    order = getattr(dim_utils, "im_dim_order", None) if dim_utils is not None else None
    if order and len(order) == n:
        spatial = {i for i, ax in enumerate(order) if str(ax).upper() in ("X", "Y")}
    if not spatial:
        spatial = {n - 2, n - 1}
    return tuple(min(int(s), xy_tile) if i in spatial else 1 for i, s in enumerate(shape))


# NOTE: labels are opened the SAME way as images — open_as_zarr / open_zarr (a flat multiscales
# store with numeric `datasets`; see segmentation_utils). The old `open_labels_as_zarr/_as_dask`
# (data_group='labels') were a verbatim port from the R version, had NO callers here, and would
# KeyError on current stores (which key multiscales under 'datasets', not 'labels') — removed.


def zarr_data_to_dask(zarr_data):
    return [da.from_zarr(arr) for arr in zarr_data]


def zarr_data_to_list(zarr_store, multiscales=None, mode='r'):
    if type(zarr_store) == str:
        # only mutate on a WRITE open — a stray leaf .zarray shadowing a group dir breaks a write.
        # NEVER touch the store on a read (the napari bridge opens images strictly read-only).
        if mode != 'r' and os.path.exists(os.path.join(zarr_store, '.zarray')) \
                and os.path.exists(os.path.join(zarr_store, '.group')):
            os.unlink(os.path.join(zarr_store, '.zarray'))
        # step into the bioformats2raw series wrapper (path/0) when that's where `multiscales`
        # lives — detected by STRUCTURE (the attr), not the `.ome.zarr` suffix. Flat
        # create_multiscales stores keep multiscales at the root; series_base returns them as-is.
        zarr_store = series_base(zarr_store, mode=mode)

    zgroup = zarr.open(zarr_store, mode=mode)

    if 'multiscales' in zgroup.attrs and not isinstance(zgroup, zarr.Array):
        zarr_group_info = None

        datasets = zgroup.attrs['multiscales'][0]['datasets']
        if multiscales is None:
            multiscale_slices = slice(None)
        else:
            multiscales = min(multiscales, len(datasets))
            multiscale_slices = slice(0, multiscales, 1)

        zarr_data = [zgroup[dataset['path']] for dataset in datasets[multiscale_slices]]
    else:
        zarr_group_info = [dict(zgroup.info.obj.info_items())]
        zarr_data = [zgroup]

    return zarr_data, zarr_group_info


# ── OME-ZARR structure + NGFF geometry (read-only; shared with the napari bridge) ──────────────
# The single home for "where does this store keep its multiscales / axes / scale". The napari
# bridge used to carry its own copies of all of these (napari/napari_bridge.py) — they now live
# here so the bridge, the pipeline and any consumer (e.g. coastal) read OME-ZARR geometry ONE way.

def series_base(path, mode='r'):
    """The store path that holds the `multiscales` metadata: the bioformats2raw series wrapper
    (``path/0``) when that's where multiscales lives, else ``path`` (a flat create_multiscales
    store). STRUCTURE-based (checks the attr, not the ``.ome.zarr`` suffix) and read-only, so it
    tells a nested series group at ``0/`` apart from a flat store's level-0 array also at ``0/``
    (the latter has no ``multiscales`` attr). Works for zarr v2 (.zattrs) and v3 (zarr.json)."""
    series = os.path.join(path, "0")
    if not os.path.isdir(series):
        return path
    try:
        g = zarr.open_group(series, mode=mode)
        if g.attrs.get("multiscales"):
            return series
    except Exception:
        pass
    return path


def read_multiscales_meta(path):
    """First NGFF ``multiscales`` entry (dict) for a store, checking the series dir then the flat
    root; ``{}`` if there is none."""
    for candidate in (os.path.join(path, "0"), path):
        if not os.path.isdir(candidate):
            continue
        try:
            g = zarr.open_group(candidate, mode='r')
            ms = g.attrs.get("multiscales")
            if ms:
                return ms[0] if isinstance(ms, list) else {}
        except Exception:
            continue
    return {}


def read_axes(path):
    """NGFF axis names for a store (e.g. ``['t','c','z','y','x']``), or None."""
    ms = read_multiscales_meta(path)
    axes = ms.get("axes", [])
    return [ax["name"] for ax in axes] if axes else None


# NGFF axis-type by name; unit-abbreviation → NGFF (UDUNITS) name napari/read_ome_metadata expect.
_NGFF_AXIS_TYPE = {'t': 'time', 'c': 'channel', 'z': 'space', 'y': 'space', 'x': 'space'}
_NGFF_UNIT = {'µm': 'micrometer', 'um': 'micrometer', 'nm': 'nanometer', 'mm': 'millimeter',
              's': 'second', 'ms': 'millisecond', 'min': 'minute',
              'micrometer': 'micrometer', 'second': 'second'}


def set_ngff_axes(path, axis_names, scale=None, units=None, channels=None):
    """Create/overwrite NGFF ``multiscales`` ``axes`` (+ per-level ``coordinateTransformations`` scale,
    axis ``unit``s, and optional ``omero.channels``) on an EXISTING store. Used to upgrade a legacy
    bioformats2raw *v0.2 stub* (which carries no ``axes``) so the new stack — napari, ``dim_utils``,
    ``read_axes``/``read_scale`` — recognises the dimensions (esp. the channel axis).

    ``axis_names`` is the ARRAY order, lowercase (bioformats2raw is ``['t','c','z','y','x']``).
    ``scale``/``units`` are dicts keyed by axis name (units may be abbreviations, mapped to NGFF
    names). Writes to whichever group carries the ``multiscales`` attr (series ``/0`` or flat root),
    matching ``read_multiscales_meta``. Returns ``True`` on success. Sanctioned structural writer —
    the migration analogue of ``create_multiscales`` for an already-materialised store."""
    grp_path = next((c for c in (os.path.join(path, "0"), path)
                     if os.path.isdir(c) and _has_multiscales(c)), None)
    if grp_path is None:
        return False
    g = zarr.open_group(grp_path, mode='r+')
    ms = g.attrs.get("multiscales")
    ms0 = (ms[0] if isinstance(ms, list) else ms) or {}
    # NGFF uses `datasets`; legacy cecelia label stores used a non-standard `labels` key for the
    # pyramid levels — read either, always write `datasets` (and drop the legacy key).
    datasets = ms0.get("datasets") or ms0.get("labels") or [{"path": "0"}]
    ms0.pop("labels", None)

    # sanity: the level-0 array's ndim must match the axis list we're about to annotate
    try:
        arr = zarr.open_array(os.path.join(grp_path, str(datasets[0].get("path", "0"))), mode='r')
        if arr.ndim != len(axis_names):
            return False
    except Exception:
        return False

    axes = []
    for nm in axis_names:
        ax = {"name": nm, "type": _NGFF_AXIS_TYPE.get(nm, "space")}
        u = (units or {}).get(nm)
        u and ax.update(unit=_NGFF_UNIT.get(u, u))
        axes.append(ax)

    new_datasets = []
    for lvl, d in enumerate(datasets):
        entry = {"path": str(d.get("path", lvl))}
        if scale:
            # x/y halve each pyramid level (bioformats2raw power-of-two downsampling); t/c/z fixed
            entry["coordinateTransformations"] = [{"type": "scale", "scale": [
                float(scale.get(nm, 1.0)) * (2 ** lvl if nm in ("x", "y") else 1)
                for nm in axis_names]}]
        new_datasets.append(entry)

    ms0["axes"] = axes
    ms0["datasets"] = new_datasets
    g.attrs["multiscales"] = [ms0] if isinstance(ms, list) or ms is None else ms0
    if channels:
        g.attrs["omero"] = {"channels": [{"label": str(c), "active": True} for c in channels]}
    return True


def _has_multiscales(candidate):
    try:
        return bool(zarr.open_group(candidate, mode='r').attrs.get("multiscales"))
    except Exception:
        return False


def read_scale(path):
    """Per-axis physical scale (one value per axis) for a store: NGFF ``coordinateTransformations``
    first, falling back to OME-XML physical sizes when the NGFF metadata carries no scale (e.g.
    processed variants that omit it). None if neither is available."""
    ms = read_multiscales_meta(path)
    for dataset in ms.get("datasets", [])[:1]:
        for t in dataset.get("coordinateTransformations", []):
            if t.get("type") == "scale":
                return t["scale"]
    axes = read_axes(path)
    if axes:
        # lazy import: keeps `import zarr_utils` free of ome-types' pydantic build cost until a
        # store actually needs the OME-XML fallback (no cycle — ome_xml_utils has no cecelia deps).
        import cecelia.utils.ome_xml_utils as ome_xml_utils
        return ome_xml_utils.read_scale_from_ome_xml(path, axes)
    return None


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
        # Write into the group so the sub-array inherits zarr v2 format. Chunk PER PLANE — NOT with the
        # dask array's own chunksize, which for a correction built via `chunks='auto'` spans the whole
        # T/C axes (~128 MB chunks) and makes every napari plane access a full-timecourse read.
        pchunks = plane_chunks(im_array.shape, dim_utils)
        dest = multiscales_zarr.create_array(
            "0", shape=im_array.shape, chunks=pchunks, dtype=im_array.dtype
        )
        # Rechunk the SOURCE to the destination grid before storing. `da.store(lock=False)` is only safe
        # when each dest chunk has exactly one writer; im_array's own (auto) chunking does NOT align with
        # the per-plane dest grid, so without this two source blocks can race on a shared dest chunk
        # (zarr writes are read-modify-write per chunk file) → scrambled planes, worst on EXPANDED
        # outputs like drift (non-512-aligned canvas). Aligning source→dest keeps it 1-writer-per-chunk,
        # safe AND parallel (same pattern rechunk_zarr.py already uses). See docs/todo — regression from
        # the per-plane-chunking change, which kept lock=False after making dest chunks differ from source.
        da.store(im_array.rechunk(pchunks), dest, lock=False)
        im_chunks = list(pchunks)
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
