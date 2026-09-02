"""
Z-projection task.

Reads an OME-ZARR image, collapses the Z axis with an ImageJ-style statistic (max/mean/median/sum/
min/std) — all channels kept, per-timepoint — and writes the result as a NEW OME-ZARR multiscale
store with SizeZ = 1. The Julia ZProject handler registers the output as a new image in the set;
this runner only does the zarr read → reduce → write. Mirrors `cropImage_run.py`'s pattern.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the projected .ome.zarr
  op       - reduction across Z: max | mean | median | sum | min | std
"""

import numpy as np
import dask.array as da

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_OPS = {'max', 'mean', 'median', 'sum', 'min', 'std'}


def _project_along_axis(arr, axis, op):
    """Reduce `arr` along `axis` keeping the axis with size 1. Preserves source dtype for max/min/
    median (safe casts); mean/std/sum are cast BACK to source dtype too — same policy as ImageJ's
    Z-Project, whose defaults are source-typed. A user needing higher precision runs the ImageJ
    convention "32-bit sum" separately; adding it is a spec change, not a runtime one."""
    dtype = arr.dtype
    # Rechunk Z into a single block so `map_blocks` sees the whole axis at once — required for
    # median, and harmless (a single-chunk reduction) for the others. Z is typically small
    # (≤~50 planes), so one chunk fits in memory per XY tile.
    chunks = list(arr.chunks)
    if len(chunks[axis]) > 1:
        new_chunks = list(arr.chunksize)
        new_chunks[axis] = arr.shape[axis]
        arr = arr.rechunk(new_chunks)
    if op == 'median':
        # dask has no axis-aware median; delegate to numpy per block.
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

    z_idx = dim_utils.dim_idx('Z')
    if z_idx is None:
        # No Z axis in this image — nothing to project. Refusing here beats writing a copy: the
        # user asked for a projection they cannot get from this shape, and the ImageTable would
        # otherwise show a "…(z-max)" duplicate identical to the source.
        raise ValueError('image has no Z axis to project')

    projected = _project_along_axis(im_dat[0], z_idx, op)
    log.log(f'>> project op={op} axis=Z(idx={z_idx}) {im_dat[0].shape} -> {projected.shape}')

    log.progress(1, 3)
    log.log(f'>> write projected image: {im_out_path}')
    with zarr_utils.staged_store(im_out_path) as staging:
        zarr_utils.create_multiscales(
            projected, staging,
            dim_utils=dim_utils,          # scale/axes are per-pixel — XY unchanged; SizeZ shrinks
            reference_zarr=im_dat[0],
            nscales=len(im_dat),
        )

        log.progress(2, 3)
        log.log('>> save OME-XML metadata')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=projected.shape,  # SizeZ shrinks to 1; SizeX/Y/T + PhysicalSize* carry
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

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
