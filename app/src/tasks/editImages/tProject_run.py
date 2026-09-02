"""
T-projection task.

Reads an OME-ZARR image, collapses the T axis with an ImageJ-style statistic (max/mean/median/sum/
min/std) — all channels and Z-planes kept, one frame out — and writes the result as a NEW OME-ZARR
multiscale store with SizeT = 1. Mirrors `zProject_run.py` on the T axis; the Julia handler
registers the output as a new image in the set.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the projected .ome.zarr
  op       - reduction across T: max | mean | median | sum | min | std
"""

import numpy as np
import dask.array as da

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_OPS = {'max', 'mean', 'median', 'sum', 'min', 'std'}


def _project_along_axis(arr, axis, op):
    """Reduce `arr` along `axis` keeping the axis with size 1. Preserves source dtype for every op
    — same policy as ImageJ's Z-Project defaults, applied here on T. The rechunk collapses T into
    a single block per XY×Z×C tile so the median can see it whole (dask has no axis-aware median);
    max/mean/sum/min/std work either way, so one code path serves all six."""
    dtype = arr.dtype
    if len(arr.chunks[axis]) > 1:
        new_chunks = list(arr.chunksize)
        new_chunks[axis] = arr.shape[axis]
        arr = arr.rechunk(new_chunks)
    if op == 'median':
        out = arr.map_blocks(
            lambda x: np.median(x, axis=axis, keepdims=True).astype(dtype, copy=False),
            dtype=dtype,
            chunks=tuple(1 if i == axis else c for i, c in enumerate(arr.chunksize)),
        )
    else:
        fn = {'max': da.max, 'mean': da.mean, 'sum': da.sum, 'min': da.min, 'std': da.std}[op]
        out = fn(arr, axis=axis, keepdims=True).astype(dtype, copy=False)
    return out


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    op          = params['op']
    if op not in _OPS:
        raise ValueError(f'unknown op: {op} (want one of {sorted(_OPS)})')

    log.progress(0, 3)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    t_idx = dim_utils.dim_idx('T')
    if t_idx is None:
        # Task JSON `requires: {axes: ["T"]}` blocks this at the module-page gate, but a REPL or
        # chain caller may still reach the runner. Refuse rather than write a copy identical to
        # the source under a "t-<op>" name.
        raise ValueError('image has no T axis to project')

    projected = _project_along_axis(im_dat[0], t_idx, op)
    log.log(f'>> project op={op} axis=T(idx={t_idx}) {im_dat[0].shape} -> {projected.shape}')

    log.progress(1, 3)
    log.log(f'>> write projected image: {im_out_path}')
    with zarr_utils.staged_store(im_out_path) as staging:
        zarr_utils.create_multiscales(
            projected, staging,
            dim_utils=dim_utils,          # scale/axes are per-pixel — XYZ unchanged; SizeT shrinks
            reference_zarr=im_dat[0],
            nscales=len(im_dat),
        )

        log.progress(2, 3)
        log.log('>> save OME-XML metadata')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=projected.shape,  # SizeT shrinks to 1; SizeX/Y/Z + PhysicalSize* carry
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: T is COLLAPSED to a single frame. A per-frame box is keyed by frame
        # index and would have no frames to key on; the frame-wise union would over-report validity.
        # No box means "all valid" — a consumer skips nothing and is merely slower, never wrong.

    log.progress(3, 3)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
