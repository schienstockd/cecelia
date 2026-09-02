"""
Z isotropic resample task.

Reads an OME-ZARR image, resamples along the Z axis so the output has PhysicalSizeZ = PhysicalSizeX
(isotropic in-plane targeting X), and writes it as a NEW OME-ZARR multiscale store. XY, T, C are
unchanged. Rechunks so Z sits in one block per timepoint × channel × XY tile — scipy.ndimage.zoom
needs the whole axis at once.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the resampled .ome.zarr
  order    - interpolation: 'nearest' | 'linear' | 'cubic'
"""

import numpy as np
import dask.array as da
from scipy.ndimage import zoom as ndi_zoom

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_ORDERS = {'nearest': 0, 'linear': 1, 'cubic': 3}


def _resample_z_block(block, axis, ratio, order, dtype):
    """One dask block. `block` has `axis` sized 1 chunk (the whole Z at once). `ratio` is the zoom
    factor along `axis`; every other axis stays at ratio 1."""
    factors = [1.0] * block.ndim
    factors[axis] = ratio
    out = ndi_zoom(block, zoom=factors, order=order, mode='nearest', prefilter=order >= 2)
    return out.astype(dtype, copy=False)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    order_key   = params['order']
    if order_key not in _ORDERS:
        raise ValueError(f'unknown order: {order_key} (want one of {sorted(_ORDERS)})')
    order = _ORDERS[order_key]

    log.progress(0, 3)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    z_idx = dim_utils.dim_idx('Z')
    if z_idx is None or im_dat[0].shape[z_idx] <= 1:
        raise ValueError('image has no Z axis to resample (or only 1 plane)')

    px_x = ome_xml_utils.read_scale_from_ome_xml(omexml, 'X')
    px_z = ome_xml_utils.read_scale_from_ome_xml(omexml, 'Z')
    if not px_x or not px_z or px_x <= 0 or px_z <= 0:
        raise ValueError('source image has no recorded XY or Z pixel size — cannot compute isotropic target')
    ratio = px_z / px_x
    src = im_dat[0]
    src_z = src.shape[z_idx]
    new_z = max(1, int(round(src_z * ratio)))
    log.log(f'>> resample Z: {src_z} planes @ {px_z} -> {new_z} planes @ {px_x} (ratio {ratio:.3f}, order={order_key})')

    # rechunk so Z is a single block per XY×C×T tile — ndi_zoom needs the whole Z at once, and dask
    # keeps every other axis chunked, so memory stays bounded per block.
    new_chunks = list(src.chunksize)
    new_chunks[z_idx] = src_z
    rechunked = src.rechunk(new_chunks)
    out_chunks = tuple(new_z if i == z_idx else c for i, c in enumerate(rechunked.chunksize))

    dtype = src.dtype
    resampled = rechunked.map_blocks(
        _resample_z_block, axis=z_idx, ratio=ratio, order=order, dtype=dtype,
        chunks=out_chunks,
    )
    log.log(f'>> resample shape: {src.shape} -> {resampled.shape}')

    # Update dim_utils so `write_calibration` records the isotropic Z spacing on the output
    dim_utils.im_dim[z_idx] = new_z
    try:
        dim_utils.set_scale('Z', px_x)
    except Exception:
        pass

    log.progress(1, 3)
    log.log(f'>> write resampled image: {im_out_path}')
    with zarr_utils.staged_store(im_out_path) as staging:
        zarr_utils.create_multiscales(
            resampled, staging,
            dim_utils=dim_utils,
            reference_zarr=src,
            nscales=len(im_dat),
        )

        log.progress(2, 3)
        log.log('>> save OME-XML metadata')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=resampled.shape,   # SizeZ changes; PhysicalSizeZ = PhysicalSizeX via dim_utils
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: Z is RESAMPLED (SizeZ changes), so a source box in level-0 Z coords does
        # not describe this store. Rescaling Z bounds by `ratio` is doable but unneeded — no box
        # means "all valid", so a consumer skips nothing and is merely slower, never wrong.
        # (`carry_valid_box` would refuse anyway: Z shapes differ.)

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
