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
import contextlib
import os
import shutil
import time
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


def native_dtype(dtype):
    """Force NATIVE byte order for a STORED dtype. Big-endian data (e.g. `>u2` from bioformats2raw)
    is not rendered correctly by napari/OpenGL on little-endian systems, so every image store we
    write must be native-endian. Centralised here and applied by the writers (create_multiscales,
    write_multiscale_pyramid, open_multiscales_for_writing, create_zarr_from_ndarray) so callers
    don't each remember `.newbyteorder('=')`. No-op for 1-byte dtypes and already-native data;
    values are preserved (numpy/zarr convert byte order on write)."""
    return np.dtype(dtype).newbyteorder('=')


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


def read_time_increment(path):
    """Seconds per frame for a store: the NGFF time-axis scale first, falling back to the
    OME-XML ``TimeIncrement``. None when neither is available or there is no time axis.

    Mirrors `read_scale`, which prefers NGFF and falls back to OME-XML — one resolver, so a
    caller does not have to know which of the two sources a given store happens to carry.
    `ome_xml_utils.read_time_increment` remains the raw OME-XML reader underneath.

    Only a time axis EXPLICITLY in seconds is taken from NGFF. A unit-less t axis is a placeholder,
    not a reading — every writer defaults that scale to 1.0 when the interval is unknown — so it
    falls through to OME-XML rather than being reported as "1 second per frame"; so does any other
    unit, rather than being silently misinterpreted as seconds. Same gate as the Julia side
    (`omezarr.jl::read_ome_metadata`)."""
    axes = read_axes(path)
    scale = read_scale(path)
    if axes and scale is not None and len(axes) == len(scale):
        low = [a.lower() for a in axes]
        if 't' in low:
            unit = (read_axis_units(path) or {}).get('t')
            if unit in ('second', 's'):
                t_scale = scale[low.index('t')]
                if t_scale and float(t_scale) > 0:
                    return float(t_scale)
    from cecelia.utils import ome_xml_utils     # lazy: see read_scale
    return ome_xml_utils.read_time_increment(path)


def read_axis_units(path):
    """NGFF ``axes[].unit`` keyed by axis name (e.g. ``{'t': 'second', 'x': 'micrometer'}``), or
    None. Axes without a unit are omitted, so a caller can fall back per axis."""
    ms = read_multiscales_meta(path)
    axes = ms.get("axes", [])
    units = {ax["name"]: ax["unit"] for ax in axes if ax.get("unit")}
    return units or None


# NGFF axis-type by name; unit-abbreviation → NGFF (UDUNITS) name napari/read_ome_metadata expect.
_NGFF_AXIS_TYPE = {'t': 'time', 'c': 'channel', 'z': 'space', 'y': 'space', 'x': 'space'}
_NGFF_UNIT = {'µm': 'micrometer', 'um': 'micrometer', 'nm': 'nanometer', 'mm': 'millimeter',
              's': 'second', 'ms': 'millisecond', 'min': 'minute',
              'micrometer': 'micrometer', 'second': 'second'}


def ngff_axis_entry(name, unit=None):
    """One NGFF ``axes`` entry: ``name`` + ``type`` (+ ``unit``, mapped to its UDUNITS name).

    The single place that decides an axis entry's shape, shared by the writer that creates a store
    (`multiscales_metadata`) and the one that annotates an existing store (`set_ngff_axes`), so a
    migrated store and a freshly written one describe their axes identically."""
    nm = str(name).lower()
    entry = {"name": nm, "type": _NGFF_AXIS_TYPE.get(nm, "space")}
    if unit:
        entry["unit"] = _NGFF_UNIT.get(unit, unit)
    return entry


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

    axes = [ngff_axis_entry(nm, (units or {}).get(nm)) for nm in axis_names]

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


# ── Staged store writing ──────────────────────────────────────────────────────
#
# The store-level counterpart to `write_atomic`/`write_json_atomic` in app/src/utils.jl. Those exist
# because a truncating write-mode open on a STATE FILE leaves a half-written file if the process dies
# mid-write; the same is true of a STORE, only worse, because a store is filled over minutes:
# open-mode-'w'-then-stream destroys the previous contents up front, so a cancelled re-run of an
# already-registered value_name leaves ccid.json advertising a store that is now partial. On a
# multi-level image the next read raises `KeyError: '1'`; on a SINGLE-level one there is no error at
# all — unwritten frames read as zeros and downstream measurement/tracking silently produce numbers
# from a partial segmentation.
#
# So: never write a final store path directly. Write into a staging sibling and rename it into place
# once the store is complete. A cancelled run then leaves the previous store untouched and its own
# staging dir behind as recognisable garbage.

STAGING_SUFFIX = '.partial'      # in progress, safe to delete
SUPERSEDED_SUFFIX = '.superseded'  # the old store, mid-promote; safe to delete
# Julia's maintenance sweep needs these two names too — see `_STORE_TMP_SUFFIXES` in
# app/src/maintenance.jl. Keep them in step; there is no shared constant across the languages.


@contextlib.contextmanager
def staged_store(final_path):
    """Yield a staging path to write a multiscales store into, then rename it onto ``final_path``.

        with zarr_utils.staged_store(out_path) as staging:
            group, level0, chunks = zarr_utils.open_multiscales_for_writing(staging, ...)
            ...stream frames into level0...
            zarr_utils.write_multiscale_pyramid(group, level0, ...)
        # out_path now exists, complete, and was never partial

    Use this for EVERY write of a store the object model can point at (a label store, an image
    version). See the block comment above for why.

    Cancellation is a SIGTERM/SIGKILL from the scheduler (`_kill_tree`), which runs no `finally`
    block — deliberately fine here: the staging dir survives as garbage and the real store is
    untouched, which is the entire point. A Python-level exception (a genuine task failure) does
    unwind, and drops the staging dir on the way out."""
    staging = final_path + STAGING_SUFFIX

    # A staging dir here is debris from a previously killed run — the same self-healing the old
    # rmtree-the-target did, just aimed at garbage instead of at the user's data.
    if os.path.exists(staging):
        shutil.rmtree(staging)

    try:
        yield staging
    except BaseException:
        shutil.rmtree(staging, ignore_errors=True)
        raise

    promote_store(staging, final_path)


def promote_store(staging_path, final_path):
    """Rename a completed staging store onto its final path, replacing any previous store.

    Called for you by ``staged_store``. The old store is renamed aside FIRST and only deleted once
    the new one is in place, so the window in which ``final_path`` doesn't exist is two renames
    wide rather than however long an `rmtree` of a multi-GB store takes. If the process dies inside
    that window, the leftover is a `.superseded` sibling and the failure is a missing file — loud,
    not silent zeros."""
    if not os.path.isdir(staging_path):
        raise FileNotFoundError(
            f'nothing to promote — staging store missing: {staging_path}')

    superseded = final_path + SUPERSEDED_SUFFIX
    if os.path.exists(superseded):
        shutil.rmtree(superseded)

    had_previous = os.path.exists(final_path)
    if had_previous:
        _rename_store(final_path, superseded)

    try:
        _rename_store(staging_path, final_path)
    except BaseException:
        # Put the previous store back rather than leaving the object model pointing at nothing.
        if had_previous and not os.path.exists(final_path):
            _rename_store(superseded, final_path)
        raise

    if had_previous:
        shutil.rmtree(superseded, ignore_errors=True)


def _rename_store(src, dest):
    """``os.rename`` for a store directory, retrying briefly on a Windows sharing violation.

    On Windows renaming a directory fails with PermissionError while any handle inside it is open,
    and a store being promoted can legitimately be open elsewhere — the napari live preview reads
    the in-progress label store while the run finishes it. Handles there are per-chunk and
    short-lived, so a moment's wait clears it; without the retry a transient reader would fail a
    whole segmentation run at the last step. No-op on POSIX, which renames regardless."""
    for attempt in range(4):
        try:
            os.rename(src, dest)
            return
        except PermissionError:
            if attempt == 3:
                raise
            time.sleep(0.25)


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
        dtype=native_dtype(im_array.dtype),
        zarr_format=2,
    )

    if copy_values is True:
        new_zarr[:] = im_array

    return new_zarr, im_chunks


# The axes a pyramid level halves. Shared, because anything expressed in level-0 pixels has to be
# rescaled by the SAME rule to stay meaningful at level n — the NGFF scale (`multiscales_metadata`)
# and the valid box (`read_valid_box`) must not each decide this for themselves.
DOWNSAMPLED_AXES = ('X', 'Y')

# ── Valid box: which part of a store is data, and which is padding ────────────────────────────
# A task may write a canvas bigger than its data — drift correction expands to hold the whole
# trajectory and drops each frame into a ZEROED canvas at its own offset, which on real movies
# leaves 38–64% padding (one went from 8 z-planes to 22). Nothing in NGFF says where the data is,
# so a consumer either reads the padding as if it were background, or hunts down whichever task
# produced the store and re-derives its geometry.
#
# So it lives on the STORE, namespaced under `cecelia`, next to the pixels it describes: any
# consumer asks `read_valid_box(path)` and gets None — meaning "all of it" — for the stores that
# have no padding. One code path, no knowledge of the producer, and it survives a copy or export
# in a way a QC sidecar under `1/{uid}/qc/` does not.
#
# Coordinates are LEVEL-0 pixels in STORE axis order; `read_valid_box(path, level=n)` rescales.
CECELIA_ATTR = 'cecelia'


def write_valid_box(path, axes, boxes):
    """Record which region of ``path`` holds data. ``axes`` are the axis letters the box is given
    on (a subset — unlisted axes are wholly valid). ``boxes`` is either one ``{axis: (start, stop)}``
    for a static region, or ``{timepoint: {axis: (start, stop)}}`` when it moves per frame.

    Level-0 pixel coordinates. Writing this is the producer's job and it should pass the SAME
    numbers it placed the pixels with — for drift that is `correction_utils.drift_frame_slices`,
    the call the writer itself uses, so the region a consumer skips is the region the writer left
    empty rather than a second opinion about it."""
    axes = [str(a).upper() for a in axes]
    per_t = bool(boxes) and not isinstance(next(iter(boxes.values())), (list, tuple)) \
        and all(isinstance(k, (int, np.integer)) for k in boxes)

    def _one(b):
        return [[int(b[a][0]), int(b[a][1])] for a in axes]

    entry = {'axes': axes, 'perTimepoint': per_t}
    if per_t:
        entry['boxes'] = [_one(boxes[t]) for t in sorted(boxes)]
    else:
        entry['boxes'] = [_one(boxes)]

    g = zarr.open_group(series_base(path), mode='a')
    ns = dict(g.attrs.get(CECELIA_ATTR, {}))
    ns['validBox'] = entry
    g.attrs[CECELIA_ATTR] = ns
    return True


def read_valid_box(path, level=0, timepoint=None):
    """The data region of a store as ``{axis: (start, stop)}``, or **None when the whole store is
    valid** — which is the common case, so a consumer can treat None as "no special handling".

    ``level`` rescales from the stored level-0 coordinates using the same downsampling rule as the
    NGFF scale (`DOWNSAMPLED_AXES`): start floors, stop ceils, so the box never crops real data.
    ``timepoint`` picks one frame of a per-frame box; omitted, a per-frame box returns the UNION
    over all frames — the smallest region containing every frame's data.

    A per-frame box is not a crop. Each frame sits at its own offset *because* the correction
    aligned them in the shared canvas; cropping each to its own box would put them back out of
    register. Crop to a common region or not at all. Note that the intersection across frames can
    be empty when the drift exceeds the stack depth, which is real on this data."""
    try:
        g = zarr.open_group(series_base(path), mode='r')
        entry = (g.attrs.get(CECELIA_ATTR) or {}).get('validBox')
    except Exception:
        return None
    if not entry or not entry.get('boxes'):
        return None

    axes = [str(a).upper() for a in entry['axes']]
    boxes = entry['boxes']
    if entry.get('perTimepoint') and timepoint is not None:
        sel = boxes[int(timepoint)]
    elif entry.get('perTimepoint'):
        sel = [[min(b[i][0] for b in boxes), max(b[i][1] for b in boxes)] for i in range(len(axes))]
    else:
        sel = boxes[0]

    out = {}
    for ax, (lo, hi) in zip(axes, sel):
        if level and ax in DOWNSAMPLED_AXES:
            f = 2 ** int(level)
            lo, hi = lo // f, -(-hi // f)        # floor / ceil — never crop real data
        out[ax] = (int(lo), int(hi))
    return out


def multiscales_metadata(axes, nscales, scale_for_axis=None, keyword='datasets',
                         unit_for_axis=None):
    """Build the NGFF ``multiscales`` attr value (a 1-element list) shared by every multiscale
    writer — the image writer (`create_multiscales`) and the label writer
    (`segmentation_utils._write_labels_zarr`). One place that decides the datasets/axes/scale
    shape, so the layout can't drift between the two.

    ``axes``: ordered axis letters for the stored array (e.g. ``['T','C','Y','X']``, or the
    channel-dropped label axes). Empty → no ``axes`` key and no scale (legacy no-metadata store).
    ``scale_for_axis``: maps an axis letter → base physical scale (missing → 1.0). None → omit
    ``coordinateTransformations``. XY axes are downsampled by ``2**level``; all other axes keep
    the base scale.
    ``unit_for_axis``: maps an axis letter → unit string for the NGFF ``axes`` entry (e.g.
    ``{'T': 's', 'Z': 'um'}``). Missing/None → the axis gets ``type`` but no ``unit``.
    ``keyword``: the datasets key (``'datasets'``)."""
    axes = list(axes or [])
    datasets = []
    for lvl in range(nscales):
        entry = {'path': str(lvl)}
        if axes and scale_for_axis is not None:
            entry['coordinateTransformations'] = [{'type': 'scale', 'scale': [
                float(scale_for_axis.get(ax, 1.0)) * (2 ** lvl if ax in DOWNSAMPLED_AXES else 1.0)
                for ax in axes
            ]}]
        datasets.append(entry)
    ms_entry = {keyword: datasets}
    if axes:
        # `type` (and `unit` where known) per the NGFF axes spec. Without them a reader gets a
        # bare number and cannot tell seconds from micrometres — napari then labels every axis
        # with the spatial unit, so a correct time scale would still render as "10 um".
        ms_entry['axes'] = [ngff_axis_entry(ax, (unit_for_axis or {}).get(ax)) for ax in axes]
    return [ms_entry]



def calibration_for_axes(dim_utils, axes):
    """``(scale_for_axis, unit_for_axis)`` for a store's axes — the ONE derivation of "what physical
    calibration do these axes carry", shared by every NGFF writer (`create_multiscales`,
    `open_multiscales_for_writing`). ``(None, None)`` when there is nothing to say.

    Both were previously derived inline, twice, and the copies had already drifted: the streaming
    writer omitted units entirely, so every drift/AF/cellpose-corrected store shipped a unit-less
    t axis and depended on the OME-XML fallback to render its timestamp at all.

    Scale is mapped by axis NAME off ``dim_utils``' OWN order — never zipped positionally against
    ``axes``, which the caller may have overridden (a label store drops C; branching can drop Z or
    T), or a store that dropped an axis inherits its neighbours' scales.

    The t axis gets a ``unit`` only when the interval is actually KNOWN. Without a ``TimeIncrement``
    the t scale falls back to 1.0, and stamping a unit on that turns a placeholder into a claim of
    "1 second per frame" — `im_time_increment_unit()` returns 's' by default whether or not one was
    found. Readers on both sides (`read_time_increment`, Julia `read_ome_metadata`) gate on the unit
    being present precisely so an unknown interval stays visibly unknown.
    """
    if not axes or dim_utils is None:
        return None, None
    axes = [str(a).upper() for a in axes]
    src = {ax: (float(s) if s is not None else 1.0)
           for ax, s in zip(dim_utils.im_dim_order, dim_utils.im_scale())}
    scale_for_axis = {ax: src.get(ax, 1.0) for ax in axes}
    # Spatial units from the per-axis accessor (not one unit assumed for all); T from OME's
    # TimeIncrementUnit, and only when there is an increment to attach it to.
    unit_for_axis = {ax.upper(): u for ax, u in dim_utils.im_physical_units().items()}
    if dim_utils.is_timeseries() and dim_utils.im_time_increment() is not None:
        unit_for_axis['T'] = dim_utils.im_time_increment_unit()
    return scale_for_axis, unit_for_axis


# NGFF/dim_utils unit vocabulary → the abbreviation OME-XML uses. The Julia side keeps its own copy
# (`omezarr.jl::_OME_XML_UNIT`) because it cannot call in here; the cross-language golden test
# (app/test/runtests.jl → "calibration writers agree across languages") is what keeps them equal.
_OME_XML_UNIT = {'micrometer': 'µm', 'um': 'µm', 'µm': 'µm', 'nanometer': 'nm', 'nm': 'nm',
                 'millimeter': 'mm', 'mm': 'mm', 'second': 's', 's': 's',
                 'millisecond': 'ms', 'ms': 'ms', 'minute': 'min', 'min': 'min'}


def write_calibration(path, dim_utils, axes=None):
    """Stamp a store's physical calibration into BOTH of its on-disk copies — the NGFF ``.zattrs``
    (per-level ``scale`` + axis ``unit``s) and the OME-XML ``<Pixels>`` attributes — from ONE
    derivation (`calibration_for_axes`). Returns True if anything was written.

    THE point of this function is that the two copies cannot be written apart. They are read by
    different consumers — napari and coastal read NGFF, every Python task reads OME-XML through
    `DimUtils` — and each was previously written by a different call from a different source: the
    NGFF one from `dim_utils`, the OME-XML one copied verbatim from the source store by
    `save_meta_in_zarr`. Nothing checked that they agreed, and repeatedly they did not: a store
    shipped `TimeIncrement="10.0"` in XML against a `1.0` NGFF t scale, and napari picked the NGFF
    one. Call this after the pixels and the OME-XML sidecar are in place.

    Idempotent, and safe on a store created by any writer: the NGFF axes/levels are read back off
    disk rather than assumed, and everything else in the ``multiscales`` entry (``version``,
    ``name``, …) is preserved. A store with no OME-XML sidecar (a label store) just gets the NGFF
    half.

    ``dim_utils`` is the authority for what the calibration IS — so this writes what the task could
    see. A value only the importer can derive (the per-plane DeltaT interval, an ImageJ Z fix) is
    applied afterwards by the Julia side through `sync_zarr_calibration!`, which is the same stamp
    against the same two copies.
    """
    ms = read_multiscales_meta(path)
    store_axes = [str(a['name']).upper() for a in ms.get('axes', [])]
    if not store_axes and axes:
        store_axes = [str(a).upper() for a in axes]
    if not store_axes or dim_utils is None:
        return False

    scale_for_axis, unit_for_axis = calibration_for_axes(dim_utils, store_axes)

    # NGFF half. Rebuild via the shared builder (so the per-level X/Y downsampling rule lives in one
    # place, not a third copy of it) and merge over the entry already on disk, keeping its extra keys.
    keyword = 'datasets' if 'datasets' in ms else next(
        (k for k, v in ms.items() if isinstance(v, list) and v and isinstance(v[0], dict)
         and 'path' in v[0]), 'datasets')
    nscales = len(ms.get(keyword, [])) or 1
    built = multiscales_metadata(store_axes, nscales, scale_for_axis=scale_for_axis,
                                 keyword=keyword, unit_for_axis=unit_for_axis)[0]
    g = zarr.open_group(series_base(path), mode='a')
    g.attrs['multiscales'] = [{**ms, **built}]

    # OME-XML half — only the calibration attrs; the rest of the sidecar (channels, planes) is the
    # source's and stays untouched.
    from cecelia.utils import ome_xml_utils     # lazy: see read_scale
    omexml = ome_xml_utils.load_ome_xml(path)
    if omexml is None:
        return True
    px = omexml.images[0].pixels
    for ax, attr in (('X', 'physical_size_x'), ('Y', 'physical_size_y'), ('Z', 'physical_size_z')):
        if ax not in scale_for_axis:
            continue
        setattr(px, attr, float(scale_for_axis[ax]))
        unit = _OME_XML_UNIT.get(unit_for_axis.get(ax))
        unit and setattr(px, f'{attr}_unit', unit)
    # T only when the interval is known — same gate as the NGFF unit (see calibration_for_axes);
    # writing the 1.0 placeholder here would re-create the divergence from the other direction.
    if 'T' in scale_for_axis and unit_for_axis.get('T'):
        px.time_increment = float(scale_for_axis['T'])
        px.time_increment_unit = _OME_XML_UNIT.get(unit_for_axis['T'], 's')
    ome_xml_utils.write_ome_xml(path, omexml)
    return True


def create_multiscales(im_array, filepath, dim_utils=None, im_chunks=None,
                       x_idx=None, y_idx=None, nscales=1, keyword='datasets',
                       ignore_channel=False, reference_zarr=None, mode='w',
                       squeeze=False, idx_adjust=0, axes=None):
    """``axes``: explicit axis letters for the array being written, overriding the ones derived
    from ``dim_utils``. Pass this whenever the stored array's rank differs from the source image's
    — e.g. a LABEL store (no channel axis), or one where Z or T has been collapsed.

    Without it the axes and the per-axis ``scale`` are taken from ``dim_utils`` verbatim, which
    silently mislabels such a store: the branch-labels writer produced a 3-axis ``(T, Y, X)`` array
    tagged ``t,c,z,y,x`` with scale ``[1, 1, 3.0, 0.596, 0.596]``, so anything reading the scale
    positionally gave Y the Z step (3.0 µm) — a 5× stretch. See
    docs/todo/SPATIAL_ANISOTROPY_PLAN.md finding A8.
    """
    # Write zarr v2 format so napari and zarr_data_to_list can read .zattrs directly.
    multiscales_zarr = zarr.open_group(filepath, mode=mode, zarr_format=2)

    # Build the multiscales metadata (shared builder — see multiscales_metadata). Axes come from
    # dim_utils unless the caller declared the stored array's own; the calibration those axes carry
    # comes from the shared derivation (see calibration_for_axes).
    if axes is not None:
        axes = [str(a).upper() for a in axes]
    else:
        axes = list(dim_utils.im_dim_order) if (dim_utils is not None and dim_utils.im_dim_order) else []
    scale_for_axis, unit_for_axis = calibration_for_axes(dim_utils, axes)
    multiscales_zarr.attrs['multiscales'] = multiscales_metadata(
        axes, nscales, scale_for_axis=scale_for_axis, keyword=keyword,
        unit_for_axis=unit_for_axis)

    if isinstance(im_array, dask.array.core.Array):
        # Write into the group so the sub-array inherits zarr v2 format. Chunk PER PLANE — NOT with the
        # dask array's own chunksize, which for a correction built via `chunks='auto'` spans the whole
        # T/C axes (~128 MB chunks) and makes every napari plane access a full-timecourse read.
        pchunks = plane_chunks(im_array.shape, dim_utils)
        dest = multiscales_zarr.create_array(
            "0", shape=im_array.shape, chunks=pchunks, dtype=native_dtype(im_array.dtype)
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
        dest = multiscales_zarr.create_array("0", shape=im_array.shape, chunks=im_array.chunks, dtype=native_dtype(im_array.dtype))
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
        write_multiscale_pyramid(
            multiscales_zarr, im_array, dim_utils, nscales, im_chunks,
            x_idx=x_idx, y_idx=y_idx, ignore_channel=ignore_channel,
            squeeze=squeeze, idx_adjust=idx_adjust)


_DERIVE_T = object()   # sentinel: derive the time axis from dim_utils


def write_multiscale_pyramid(multiscales_zarr, level_source, dim_utils, nscales, im_chunks,
                             x_idx=None, y_idx=None, t_idx=_DERIVE_T, ignore_channel=False,
                             squeeze=False, idx_adjust=0):
    """Write downsampled pyramid levels 1..nscales-1 into an already-created multiscales group,
    slicing ``level_source`` (numpy / dask / an on-disk zarr level-0) with power-of-two XY strides.

    Shared by ``create_multiscales``, the streaming correction/cellpose writers, and the label
    writer. When the source is a timeseries, each level is filled ONE TIMEPOINT AT A TIME, so
    building the pyramid from an on-disk level 0 never pulls the whole stack back into RAM. Values
    are identical to slicing the full array in one go (levels downsample level 0, not each other).

    ``t_idx`` defaults to being derived from ``dim_utils`` (the image case). Callers whose axis
    layout differs from the image — e.g. the label store, which has no channel axis — pass an
    explicit ``t_idx`` (an int, or None for no time axis) together with explicit ``x_idx``/``y_idx``
    and ``dim_utils=None``."""
    if nscales <= 1:
        return

    shape0 = tuple(level_source.shape)
    slices = slice_utils.create_slices_multiscales(
        shape0, dim_utils=dim_utils,
        x_idx=x_idx, y_idx=y_idx,
        nscales=nscales - 1, ignore_channel=ignore_channel,
        squeeze=squeeze, idx_adjust=idx_adjust)

    if t_idx is _DERIVE_T:
        t_idx = dim_utils.dim_idx('T') if (dim_utils is not None and dim_utils.is_timeseries()) else None

    def _read(src_slice):
        return fortify(level_source[src_slice]) \
            if isinstance(level_source, dask.array.core.Array) else level_source[src_slice]

    for i, x in enumerate(slices):
        # destination shape = length of each (strided) source slice
        dest_shape = tuple(len(range(*x[d].indices(shape0[d]))) for d in range(len(shape0)))
        # clamp chunks to the (downsampled) level so a deep pyramid on a small array never asks for
        # a chunk larger than the axis — only the chunk layout changes, never the pixel values
        dest_chunks = tuple(max(1, min(c, s)) for c, s in zip(im_chunks, dest_shape))
        dest = multiscales_zarr.create_array(
            str(i + 1), shape=dest_shape, chunks=dest_chunks, dtype=native_dtype(level_source.dtype))
        if t_idx is None:
            dest[:] = _read(tuple(x))
        else:
            for t in range(shape0[t_idx]):
                rd = list(x);              rd[t_idx] = slice(t, t + 1, 1)
                wr = [slice(None)] * len(dest_shape); wr[t_idx] = slice(t, t + 1, 1)
                dest[tuple(wr)] = _read(tuple(rd))


def open_multiscales_for_writing(filepath, shape, dtype, dim_utils,
                                 nscales=1, keyword='datasets', mode='w'):
    """Create a multiscales group + an EMPTY, per-plane-chunked level-0 array on disk, write the
    NGFF ``multiscales`` metadata, and return ``(group, level0, pchunks)``.

    Stream data into ``level0`` (e.g. one timepoint or one channel at a time), then call
    ``write_multiscale_pyramid(group, level0, dim_utils, nscales, pchunks)`` to build the
    downsampled levels from the on-disk level 0. This keeps peak memory at ~one plane instead of
    the whole T×C×Z×Y×X image — the drift / AF / cellpose correction tasks used to allocate the
    entire corrected image in RAM (and OOM on large time-lapses). Metadata and chunking mirror the
    dask branch of ``create_multiscales`` exactly, so the resulting store is byte-for-byte the same
    layout — only the fill is streamed. Both derive their calibration from `calibration_for_axes`,
    which is what makes "exactly" true: this writer used to derive its own, without units, so the
    corrected stores it produces carried a unit-less t axis and leant on the OME-XML fallback."""
    multiscales_zarr = zarr.open_group(filepath, mode=mode, zarr_format=2)

    axes = list(dim_utils.im_dim_order) if (dim_utils is not None and dim_utils.im_dim_order) else []
    scale_for_axis, unit_for_axis = calibration_for_axes(dim_utils, axes)
    multiscales_zarr.attrs['multiscales'] = multiscales_metadata(
        axes, nscales, scale_for_axis=scale_for_axis, keyword=keyword,
        unit_for_axis=unit_for_axis)

    pchunks = plane_chunks(tuple(shape), dim_utils)
    level0 = multiscales_zarr.create_array("0", shape=tuple(shape), chunks=pchunks,
                                           dtype=native_dtype(dtype))
    return multiscales_zarr, level0, pchunks


def read_timepoint(level, dim_utils, t, drop_time=True, ignore_channel=False):
    """Read a single timepoint of an opened zarr/dask level fully into a numpy array.

    The generic "read one frame, then tile/slice/process it in RAM" primitive. Reading per
    (timepoint, tile) straight from the store re-fetches whole chunks for every tile — dask's auto
    chunks span the whole timecourse, so one tile pulls a ~128 MB block — which is why tasks used to
    load the ENTIRE level into RAM. Reading a single frame once gives the same fast in-RAM slicing
    with memory bounded to one timepoint. Reusable by any per-timepoint task (segmentation, cellpose,
    measure, denoise, …).

    ``drop_time`` squeezes the length-1 time axis so the frame carries the image's non-time axes in
    their original order (what per-tile slicing and model input expect); pass False to keep a
    length-1 T axis and the full image layout. For a non-timeseries level the whole level is returned
    (there is only one frame).

    ``ignore_channel=True`` resolves the time axis against a dim order with C removed — for a LABEL
    level, which has no channel axis while `dim_utils` describes the source image (with one). Every
    caller slicing labels must set it: with a C-before-T layout the default would otherwise pick the
    wrong axis, silently reading one channel's worth of Z instead of a timepoint. (It happens to be
    harmless for the T-first layouts cecelia imports today, which is exactly why it needs saying.)"""
    if dim_utils is None or not dim_utils.is_timeseries():
        return fortify(level)
    t_idx = dim_utils.dim_idx('T', ignore_channel=ignore_channel)
    sl = [slice(None)] * len(level.shape)
    sl[t_idx] = slice(t, t + 1, 1)
    frame = fortify(level[tuple(sl)])
    return np.squeeze(frame, axis=t_idx) if drop_time else frame


def copy_stream(dest, src, dim_utils=None):
    """Copy ``src`` (zarr / dask / numpy) into an on-disk ``dest`` of the same shape, one timepoint
    at a time so a full copy never materialises in RAM. Used when a task rewrites only some
    channels/planes and must carry the rest through unchanged (e.g. cellpose correct)."""
    is_ts = dim_utils is not None and dim_utils.is_timeseries()
    if not is_ts:
        dest[:] = fortify(src)
        return
    t_idx = dim_utils.dim_idx('T')
    for t in range(src.shape[t_idx]):
        sl = [slice(None)] * len(src.shape)
        sl[t_idx] = slice(t, t + 1, 1)
        sl = tuple(sl)
        dest[sl] = fortify(src[sl])


def apply_min(x, im_min):
    x[x == 0] = im_min
    return x


def get_minmax_from_low_res(im_dat):
    low_res = fortify(im_dat[len(im_dat) - 1])
    im_min = low_res[low_res > 0].min()
    im_max = low_res.max()
    return im_min, im_max
