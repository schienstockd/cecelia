"""
Z-projection task.

Reads an OME-ZARR image, collapses the Z axis with an ImageJ-style statistic (max/mean/median/sum/
min/std) — all channels kept, per-timepoint — and writes the result as a NEW OME-ZARR multiscale
store with SizeZ = 1, streaming one timepoint at a time. The Julia ZProject handler registers the
output as a new image in the set.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the projected .ome.zarr
  op       - reduction across Z: max | mean | median | sum | min | std
"""

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_REDUCERS = {
    'max':    np.max,
    'mean':   np.mean,
    'median': np.median,
    'min':    np.min,
    'sum':    np.sum,
    'std':    np.std,
}


def _project_frame(frame, axis, reducer, out_dtype):
    """Reduce `frame` along `axis`, keep the axis with size 1, cast back to source dtype. Same
    ImageJ Z-Project convention (source-typed output) the dask branch used."""
    return reducer(frame, axis=axis, keepdims=True).astype(out_dtype, copy=False)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    op          = params['op']
    if op not in _REDUCERS:
        raise ValueError(f'unknown op: {op} (want one of {sorted(_REDUCERS)})')

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask — the reduction is per-timepoint with the whole Z in RAM, so streaming
    # per t bounds memory to one (C, Z, Y, X) frame. Decision 2 of docs/todo/ZARR_STREAMING_PLAN.md.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in  = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    z_idx = dim_utils.dim_idx('Z')
    if z_idx is None:
        # No Z axis in this image — nothing to project. Refusing here beats writing a copy: the
        # user asked for a projection they cannot get from this shape, and the ImageTable would
        # otherwise show a "…(z-max)" duplicate identical to the source.
        raise ValueError('image has no Z axis to project')
    t_idx = dim_utils.dim_idx('T') if 'T' in dim_utils.im_dim_order else None

    src_shape = tuple(level_in.shape)
    out_shape = list(src_shape); out_shape[z_idx] = 1
    out_shape = tuple(out_shape)

    dim_utils.im_dim[z_idx] = 1   # shape only; calibration unchanged (per-pixel XY is intact)

    # Axis positions INSIDE a squeezed frame (read_timepoint drops the length-1 T).
    z_pos = z_idx - 1 if (t_idx is not None and z_idx > t_idx) else z_idx

    n_t = src_shape[t_idx] if t_idx is not None else 1
    total = n_t + 1
    done = 0
    log.progress(done, total)
    log.log(f'>> project op={op} axis=Z(idx={z_idx}) {src_shape} -> {out_shape}')
    log.log(f'>> write projected image: {im_out_path}')

    reducer = _REDUCERS[op]

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit source zarr format (ZARR_V3_PLAN D9)

        for t in range(n_t):
            frame = zarr_utils.read_timepoint(level_in, dim_utils, t)   # T squeezed
            reduced = _project_frame(frame, z_pos, reducer, level_in.dtype)
            out_sl = [slice(None)] * len(out_shape)
            if t_idx is not None:
                out_sl[t_idx] = slice(t, t + 1)
                level0[tuple(out_sl)] = np.expand_dims(reduced, axis=t_idx)
            else:
                level0[tuple(out_sl)] = reduced
            done += 1
            log.progress(done, total)

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,   # SizeZ → 1; SizeX/Y/T + PhysicalSize* carry over
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: Z is COLLAPSED to a single plane, so a Z span from the source has no
        # meaning on this store. XY validity from the source could be projected too, but no box
        # means "all valid" — a consumer skips nothing and is merely slower, never wrong.

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
