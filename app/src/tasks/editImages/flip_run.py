"""
Flip task.

Reads an OME-ZARR image, reverses order along one axis (X, Y or Z) and writes the result as a NEW
OME-ZARR multiscale store, streaming one timepoint at a time. Dims are preserved (only element
order changes), so existing per-image downstream artifacts still line up on the flipped version —
the Julia handler registers it as a NEW VERSION on the same image, not a new image.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the flipped .ome.zarr
  axis     - one of 'X', 'Y', 'Z'
"""

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    axis        = params['axis'].upper()
    if axis not in ('X', 'Y', 'Z'):
        raise ValueError(f'unknown axis: {axis} (want X, Y or Z)')

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask — flipping is a per-frame index reversal (np.flip on the frame's axis).
    # See docs/todo/ZARR_STREAMING_PLAN.md decision 2.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in  = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    axis_idx = dim_utils.dim_idx(axis)
    if axis_idx is None:
        raise ValueError(f'image has no {axis} axis to flip')
    t_idx = dim_utils.dim_idx('T') if 'T' in dim_utils.im_dim_order else None

    shape = tuple(level_in.shape)
    # Flipping is per-frame — the axis is X/Y/Z, never T — so read_timepoint (which squeezes T)
    # sees the flipped axis at position `flip_pos_in_frame`. Adjust the axis position if it comes
    # after T in the source layout, else it's the same.
    flip_pos = axis_idx - 1 if (t_idx is not None and axis_idx > t_idx) else axis_idx

    n_t = shape[t_idx] if t_idx is not None else 1
    total = n_t + 1
    done = 0
    log.progress(done, total)
    log.log(f'>> flip axis={axis}(idx={axis_idx}) — shape unchanged {shape}')
    log.log(f'>> write flipped image: {im_out_path}')

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit source zarr format (ZARR_V3_PLAN D9)

        for t in range(n_t):
            frame = zarr_utils.read_timepoint(level_in, dim_utils, t)   # T squeezed
            flipped = np.flip(frame, axis=flip_pos)
            out_sl = [slice(None)] * len(shape)
            if t_idx is not None:
                out_sl[t_idx] = slice(t, t + 1)
                level0[tuple(out_sl)] = np.expand_dims(flipped, axis=t_idx)
            else:
                level0[tuple(out_sl)] = flipped
            done += 1
            log.progress(done, total)

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        # shape unchanged, so no changed_shape override — the source's SizeX/Y/Z/T carry verbatim
        ome_xml_utils.save_meta_in_zarr(staging, im_path, dim_utils=dim_utils)
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: flip MIRRORS coordinates along the flipped axis, so a carried box would
        # be a precise-looking lie — a valid region at [x0, x1] on the source is at [W-x1, W-x0]
        # here. Mirroring the box in step is doable but unneeded — no box means "all valid", so a
        # consumer skips nothing and is merely slower, never wrong.

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
