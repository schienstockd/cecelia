"""
Export an image version as OME-TIFF.

The point of the task is that the CALIBRATION survives. People render figures in Imaris, which
cannot read our zarr stores; Imaris File Converter reads through Bio-Formats, so a correct OME-TIFF
is the input for the `.ims` route too. The route this replaces went OME-TIFF -> ImageJ -> plain TIFF
-> converter, and lost the pixel sizes on the ImageJ hop, because a plain TIFF has nowhere to record
Z spacing. So the `<Pixels>` block written here is the deliverable, not a detail.

Calibration is passed IN by the Julia handler, read from `ccid.json` — the authoritative copy (see
CLAUDE.md -> *Calibration — three copies, one stamp*). This runner deliberately does NOT re-derive it
from the store's own OME-XML, which is a derived copy and can have drifted.

Written plane-by-plane through `TiffWriter.write(<iterator>, shape=…, dtype=…)`: a full
201x21x4x544x548 uint16 movie is ~9.7 GB, so materialising it to hand tifffile one array would
defeat the streaming the rest of the pipeline is careful about. Memory stays at one YX plane.

Parameter contract (JSON written by Julia):
  imPath       - absolute path to the source .ome.zarr
  outPath      - absolute path of the .ome.tif to write
  channels     - channel indices to keep, in order; [] = all
  channelNames - names for the kept channels, same order
  zMip         - bool; max-project Z to a single plane
  timepoint    - int; single frame to export, -1 = all
  calibration  - dict of OME <Pixels> attrs (PhysicalSize*/units, TimeIncrement/unit), possibly empty
  qcOutPath    - where to write the shape/plane counts the Julia handler banks as QC
"""

import itertools
import os

import numpy as np
import tifffile

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic

# tifffile switches to the 64-bit offsets BigTIFF needs; classic TIFF tops out at 4 GB. Cut over
# well below the limit — the estimate is the raw pixel count and ignores tags/metadata overhead.
_BIGTIFF_BYTES = 3_500_000_000


def _remaining_axes(dim_order, fixed):
    """Axis names left, in source order, once `fixed` axes are indexed away with plain ints."""
    return [ax for ax in dim_order if ax not in fixed]


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path   = params['imPath']
    out_path  = params['outPath']
    z_mip     = bool(params.get('zMip', False))
    timepoint = int(params.get('timepoint', -1))
    cal       = dict(params.get('calibration') or {})
    ch_keep   = [int(c) for c in (params.get('channels') or [])]
    ch_names  = [str(n) for n in (params.get('channelNames') or [])]

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask, and one TIMEPOINT at a time via `read_timepoint` — the shared streaming
    # primitive. Slicing per (t, c, z) plane straight from the store re-fetches a whole chunk for
    # every plane, which is exactly what that helper exists to stop; reading the frame once and
    # slicing it in RAM costs one timepoint of memory (~50 MB on a 21x4x544x548 uint16 movie).
    # See docs/todo/ZARR_STREAMING_PLAN.md -> locked decisions 1 and 2.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    arr = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(arr.shape)
    dim_order = list(dim_utils.im_dim_order)
    log.log(f'>> image dims: {dim_order} {list(arr.shape)}')

    idx_of = {ax: dim_utils.dim_idx(ax) for ax in ('T', 'C', 'Z', 'Y', 'X')}
    has = {ax: idx_of.get(ax) is not None for ax in ('T', 'C', 'Z')}
    size = {ax: (arr.shape[idx_of[ax]] if has.get(ax) else 1) for ax in ('T', 'C', 'Z')}

    # ── what the output actually contains ────────────────────────────────────────────────────────
    # A single-timepoint export has no T to iterate and a Z-MIP has no Z, so those axes are dropped
    # from the file rather than written as a size-1 axis that implies a dimension we collapsed.
    ts = [timepoint] if (has['T'] and timepoint >= 0) else (list(range(size['T'])) if has['T'] else [None])
    if has['T'] and timepoint >= 0 and not (0 <= timepoint < size['T']):
        log.log(f'[ERROR] timepoint {timepoint} out of range (SizeT={size["T"]})')
        return
    cs = (ch_keep if ch_keep else list(range(size['C']))) if has['C'] else [None]
    bad = [c for c in cs if c is not None and not (0 <= c < size['C'])]
    if bad:
        log.log(f'[ERROR] channel(s) {bad} out of range (SizeC={size["C"]})')
        return
    zs = [None] if (z_mip or not has['Z']) else list(range(size['Z']))

    axes_out, shape_out = '', []
    if has['T'] and timepoint < 0:
        axes_out += 'T'; shape_out.append(len(ts))
    if has['C']:
        axes_out += 'C'; shape_out.append(len(cs))
    if has['Z'] and not z_mip:
        axes_out += 'Z'; shape_out.append(len(zs))
    axes_out += 'YX'
    shape_out += [arr.shape[idx_of['Y']], arr.shape[idx_of['X']]]

    total = len(ts) * len(cs) * len(zs)
    est   = int(np.prod(shape_out)) * np.dtype(arr.dtype).itemsize
    big   = est > _BIGTIFF_BYTES
    log.log(f'>> export {axes_out} {shape_out} {arr.dtype} '
            f'(~{est / 1024**3:.2f} GB, {"BigTIFF" if big else "TIFF"})')
    if z_mip:
        log.log(f'>> max-projecting {size["Z"]} z planes')

    # ── metadata ────────────────────────────────────────────────────────────────────────────────
    meta = {'axes': axes_out}
    meta.update(cal)
    names = ([ch_names[i] if i < len(ch_names) else f'Channel {c}' for i, c in enumerate(cs)]
             if has['C'] else [])
    if names:
        meta['Channel'] = {'Name': names}
        log.log(f'>> channels: {names}')
    if not any(k.startswith('PhysicalSize') for k in cal):
        log.log('[WARN] no physical pixel size to write — the export will have no scale')

    # ── stream planes ───────────────────────────────────────────────────────────────────────────
    def _plane(frame, c, z):
        """One YX plane out of an in-RAM timepoint, with C/Z indexed (or Z projected) away."""
        sl = [slice(None)] * frame.ndim
        fixed = set()
        if has['T']:
            sl[idx_of['T']] = 0; fixed.add('T')       # the frame carries a length-1 T axis
        if c is not None:
            sl[idx_of['C']] = c; fixed.add('C')
        if z is not None:
            sl[idx_of['Z']] = z; fixed.add('Z')
        block = frame[tuple(sl)]

        rest = _remaining_axes(dim_order, fixed)
        if z_mip and has['Z']:
            block = block.max(axis=rest.index('Z'))
            rest = [ax for ax in rest if ax != 'Z']
        # Everything but Y/X is indexed away by now; order the last two as YX.
        if rest != ['Y', 'X']:
            block = np.transpose(block, [rest.index('Y'), rest.index('X')])
        return np.ascontiguousarray(block)

    def planes():
        done = 0
        for t in ts:
            # ONE store read per timepoint; every (c, z) below is an in-RAM slice of that frame.
            # drop_time=False keeps the source layout, so `idx_of` stays valid against the frame.
            frame = zarr_utils.read_timepoint(arr, dim_utils, 0 if t is None else t,
                                              drop_time=False)
            for c, z in itertools.product(cs, zs):
                done += 1
                log.progress(done, total)
                yield _plane(frame, c, z)

    os.makedirs(os.path.dirname(out_path) or '.', exist_ok=True)
    # Staged like every other durable output: a cancelled export must not leave a half-written file
    # sitting in the user's folder looking like a finished one. task:cancel kills this process by
    # design, so the partial file would otherwise survive with no way to tell it apart.
    tmp_path = out_path + '.partial'
    log.progress(0, total)
    try:
        with tifffile.TiffWriter(tmp_path, bigtiff=big, ome=True) as tw:
            tw.write(planes(), shape=tuple(shape_out), dtype=arr.dtype, metadata=meta)
        os.replace(tmp_path, out_path)
    except BaseException:
        if os.path.exists(tmp_path):
            os.remove(tmp_path)
        raise

    write_json_atomic(params['qcOutPath'], {
        'shape':  [int(s) for s in shape_out],
        'axes':   axes_out,
        'sizeZ':  int(1 if z_mip else len(zs)),
        'sizeC':  int(len(cs) if has['C'] else 0),
        'planes': int(total),
    })
    log.log(f'>> done: {out_path}')
