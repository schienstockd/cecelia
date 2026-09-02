"""
XY bin (downsample) task.

Reads an OME-ZARR image, combines each factorX × factorY block along the X and Y axes with a chosen
op (mean / sum / max / min), and writes the result as a NEW OME-ZARR multiscale store. Z, T, C are
unchanged. Any pixels that don't fit a full block along an axis (SizeX % factorX != 0) are trimmed —
the same convention `dask.array.coarsen(..., trim_excess=True)` uses, matching what `_bin_inherited_meta`
records with `div(SizeX, factorX)`.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the binned .ome.zarr
  factorX  - integer XY bin factor along X (≥ 1)
  factorY  - integer XY bin factor along Y (≥ 1)
  op       - combining function: mean | sum | max | min
"""

import numpy as np
import dask.array as da

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_REDUCERS = {'mean': np.mean, 'sum': np.sum, 'max': np.max, 'min': np.min}


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

    log.progress(0, 3)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    x_idx = dim_utils.dim_idx('X')
    y_idx = dim_utils.dim_idx('Y')
    if x_idx is None or y_idx is None:
        raise ValueError('image is missing X or Y axis')

    # Update the calibration BEFORE writing so `write_calibration` records the binned pixel size —
    # SizeX/Y shrink (integer floor, matching `coarsen(..., trim_excess=True)`) and the per-pixel
    # physical extent grows by the same factor. dim_utils reads pixel size out of its own state, so
    # bumping it here is what propagates into the new store's NGFF scale.
    if fx > 1:
        dim_utils.im_dim[x_idx] = im_dat[0].shape[x_idx] // fx
        try:
            dim_utils.set_scale('X', dim_utils.scale('X') * fx)
        except Exception:
            pass                             # some builds don't expose scale(): NGFF scale is still
                                             # rewritten by write_calibration below via im_dim
    if fy > 1:
        dim_utils.im_dim[y_idx] = im_dat[0].shape[y_idx] // fy
        try:
            dim_utils.set_scale('Y', dim_utils.scale('Y') * fy)
        except Exception:
            pass

    src = im_dat[0]
    # da.coarsen collapses each block of size `factor` along the given axis. trim_excess=True drops
    # the ragged remainder so the output has integer-floor shape — matches the Julia meta helper.
    reducer = _REDUCERS[op]
    axes = {}
    if fx > 1:
        axes[x_idx] = fx
    if fy > 1:
        axes[y_idx] = fy
    binned = da.coarsen(reducer, src, axes, trim_excess=True).astype(src.dtype)
    log.log(f'>> bin factor X={fx} Y={fy} op={op}: {src.shape} -> {binned.shape}')

    log.progress(1, 3)
    log.log(f'>> write binned image: {im_out_path}')
    with zarr_utils.staged_store(im_out_path) as staging:
        zarr_utils.create_multiscales(
            binned, staging,
            dim_utils=dim_utils,
            reference_zarr=src,
            nscales=len(im_dat),
        )

        log.progress(2, 3)
        log.log('>> save OME-XML metadata')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=binned.shape,     # SizeX/Y shrink; PhysicalSizeX/Y are grown by dim_utils
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: XY sizes SHRINK by an integer factor, so a source box in level-0
        # coordinates does not describe this store. Rescaling by (fx, fy) is doable but unneeded —
        # no box means "all valid", so a consumer skips nothing and is merely slower, never wrong.
        # (`carry_valid_box` would refuse anyway: the shapes differ.)

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
