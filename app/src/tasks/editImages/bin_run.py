"""
XY bin (downsample) task.

Reads an OME-ZARR image, combines each factorX × factorY block along the X and Y axes with a chosen
op (mean / sum / max / min), and writes the result as a NEW OME-ZARR multiscale store, streaming one
timepoint at a time. Z, T, C are unchanged. Any pixels that don't fit a full block along an axis
(SizeX % factorX != 0) are trimmed — matching what `_bin_inherited_meta` records with
`div(SizeX, factorX)`.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the binned .ome.zarr
  factorX  - integer XY bin factor along X (≥ 1)
  factorY  - integer XY bin factor along Y (≥ 1)
  op       - combining function: mean | sum | max | min
"""

import numpy as np
from skimage.measure import block_reduce

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_REDUCERS = {'mean': np.mean, 'sum': np.sum, 'max': np.max, 'min': np.min}


def _bin_frame(frame, y_pos, x_pos, fy, fx, reducer, out_dtype):
    """Trim ragged remainder along Y/X then block-reduce Y by fy, X by fx. `y_pos`/`x_pos` are the
    axis positions in `frame` (source dim_order with the time axis dropped)."""
    block = [1] * frame.ndim
    if fy > 1:
        block[y_pos] = fy
    if fx > 1:
        block[x_pos] = fx
    return block_reduce(frame, block_size=tuple(block), func=reducer).astype(out_dtype, copy=False)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    fx          = int(params['factorX'])
    fy          = int(params['factorY'])
    op          = params['op']
    if fx < 1 or fy < 1:
        raise ValueError(f'bin factors must be >= 1 (got X={fx}, Y={fy})')
    if op not in _REDUCERS:
        raise ValueError(f'unknown op: {op} (want one of {sorted(_REDUCERS)})')

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask — decision 2 of docs/todo/ZARR_STREAMING_PLAN.md. The transform is
    # per-plane (XY block-reduce, no cross-frame state), so streaming one timepoint at a time
    # bounds memory to one (C, Z, Y, X) frame regardless of T.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in  = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    x_idx = dim_utils.dim_idx('X')
    y_idx = dim_utils.dim_idx('Y')
    t_idx = dim_utils.dim_idx('T') if 'T' in dim_utils.im_dim_order else None
    if x_idx is None or y_idx is None:
        raise ValueError('image is missing X or Y axis')

    src_shape = tuple(level_in.shape)
    new_x = src_shape[x_idx] // fx if fx > 1 else src_shape[x_idx]
    new_y = src_shape[y_idx] // fy if fy > 1 else src_shape[y_idx]
    out_shape = list(src_shape)
    out_shape[x_idx] = new_x
    out_shape[y_idx] = new_y
    out_shape = tuple(out_shape)

    # Calibration + shape on dim_utils/omexml BEFORE the writers open the store — plane_chunks reads
    # `im_dim_order` (unchanged), and calibration_for_axes reads pixel sizes off the OMEXML directly.
    # `dim_utils.set_scale` doesn't exist; mutate the OMEXML fields so both write_calibration and
    # save_meta_in_zarr pick up the new pixel size.
    dim_utils.im_dim[x_idx] = new_x
    dim_utils.im_dim[y_idx] = new_y
    px = omexml.images[0].pixels
    try:
        if fx > 1 and px.physical_size_x is not None:
            px.physical_size_x = float(px.physical_size_x) * fx
        if fy > 1 and px.physical_size_y is not None:
            px.physical_size_y = float(px.physical_size_y) * fy
    except Exception:
        pass

    reducer = _REDUCERS[op]
    # Axis positions INSIDE a squeezed frame (time axis dropped by read_timepoint). Adjust y/x
    # positions when they follow the T axis in the source layout, else they stay put.
    def _pos_in_frame(i):
        return i - 1 if (t_idx is not None and i > t_idx) else i
    y_pos = _pos_in_frame(y_idx)
    x_pos = _pos_in_frame(x_idx)

    n_t = src_shape[t_idx] if t_idx is not None else 1
    total = n_t + 1
    done = 0
    log.progress(done, total)
    log.log(f'>> bin factor X={fx} Y={fy} op={op}: {src_shape} -> {out_shape}')
    log.log(f'>> write binned image: {im_out_path}')

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit source zarr format (ZARR_V3_PLAN D9)

        for t in range(n_t):
            frame = zarr_utils.read_timepoint(level_in, dim_utils, t)   # T squeezed
            binned = _bin_frame(frame, y_pos, x_pos, fy, fx, reducer, level_in.dtype)
            out_sl = [slice(None)] * len(out_shape)
            if t_idx is not None:
                out_sl[t_idx] = slice(t, t + 1)
                # read_timepoint squeezed T; re-insert a length-1 T at the same axis position
                level0[tuple(out_sl)] = np.expand_dims(binned, axis=t_idx)
            else:
                level0[tuple(out_sl)] = binned
            done += 1
            log.progress(done, total)

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,     # SizeX/Y shrink; PhysicalSizeX/Y already grown on omexml
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: XY sizes SHRINK by an integer factor, so a source box in level-0
        # coordinates does not describe this store. Rescaling by (fx, fy) is doable but unneeded —
        # no box means "all valid", so a consumer skips nothing and is merely slower, never wrong.
        # (`carry_valid_box` would refuse anyway: the shapes differ.)

    done += 1
    log.progress(done, total)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
