"""
Flip task.

Reads an OME-ZARR image, reverses order along one axis (X, Y or Z) and writes the result as a NEW
OME-ZARR multiscale store. Dims are preserved (only element order changes), so existing per-image
downstream artifacts still line up on the flipped version — the Julia handler registers it as a NEW
VERSION on the same image, not a new image.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the flipped .ome.zarr
  axis     - one of 'X', 'Y', 'Z'
"""

import dask.array as da

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

    log.progress(0, 3)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    axis_idx = dim_utils.dim_idx(axis)
    if axis_idx is None:
        raise ValueError(f'image has no {axis} axis to flip')

    flipped = da.flip(im_dat[0], axis=axis_idx)
    log.log(f'>> flip axis={axis}(idx={axis_idx}) — shape unchanged {im_dat[0].shape}')

    log.progress(1, 3)
    log.log(f'>> write flipped image: {im_out_path}')
    with zarr_utils.staged_store(im_out_path) as staging:
        zarr_utils.create_multiscales(
            flipped, staging,
            dim_utils=dim_utils,          # scale/axes carry over — same per-pixel calibration
            reference_zarr=im_dat[0],
            nscales=len(im_dat),
        )

        log.progress(2, 3)
        log.log('>> save OME-XML metadata')
        # shape unchanged, so no changed_shape override — the source's SizeX/Y/Z/T carry verbatim
        ome_xml_utils.save_meta_in_zarr(staging, im_path, dim_utils=dim_utils)
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
