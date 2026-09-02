"""
Z isotropic resample task.

Reads an OME-ZARR image, resamples along the Z axis so the output has PhysicalSizeZ = PhysicalSizeX
(isotropic in-plane targeting X), and writes it as a NEW OME-ZARR multiscale store, streaming one
timepoint at a time. XY, T, C are unchanged; SizeZ changes to match XY spacing. scipy.ndimage.zoom
needs the whole Z at once, so the per-timepoint frame IS the streaming unit.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the resampled .ome.zarr
  order    - interpolation: 'nearest' | 'linear' | 'cubic'
"""

import numpy as np
from scipy.ndimage import zoom as ndi_zoom

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_ORDERS = {'nearest': 0, 'linear': 1, 'cubic': 3}


def _resample_z_frame(frame, z_pos, ratio, order, out_dtype):
    """Zoom one frame along its Z position by `ratio`, keep every other axis unchanged, cast back to
    source dtype. `mode='nearest'` at edges (no black border), `prefilter` only for spline orders
    (nearest/linear don't use it)."""
    factors = [1.0] * frame.ndim
    factors[z_pos] = ratio
    out = ndi_zoom(frame, zoom=factors, order=order, mode='nearest', prefilter=order >= 2)
    return out.astype(out_dtype, copy=False)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    order_key   = params['order']
    if order_key not in _ORDERS:
        raise ValueError(f'unknown order: {order_key} (want one of {sorted(_ORDERS)})')
    order = _ORDERS[order_key]

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask (docs/todo/ZARR_STREAMING_PLAN.md decision 2). The compute unit is one
    # (C, Z, Y, X) frame — ndi_zoom needs the whole Z at once but never more than one timepoint —
    # so streaming per t bounds peak memory to one frame regardless of T.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in  = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    z_idx = dim_utils.dim_idx('Z')
    if z_idx is None or level_in.shape[z_idx] <= 1:
        raise ValueError('image has no Z axis to resample (or only 1 plane)')
    t_idx = dim_utils.dim_idx('T') if 'T' in dim_utils.im_dim_order else None

    px_x = dim_utils.im_physical_size('x', default=None)
    px_z = dim_utils.im_physical_size('z', default=None)
    if not px_x or not px_z or px_x <= 0 or px_z <= 0:
        raise ValueError('source image has no recorded XY or Z pixel size — cannot compute isotropic target')
    px_x = float(px_x); px_z = float(px_z)
    ratio = px_z / px_x
    src_shape = tuple(level_in.shape)
    src_z = src_shape[z_idx]
    new_z = max(1, int(round(src_z * ratio)))
    out_shape = list(src_shape); out_shape[z_idx] = new_z
    out_shape = tuple(out_shape)
    log.log(f'>> resample Z: {src_z} planes @ {px_z} -> {new_z} planes @ {px_x} '
            f'(ratio {ratio:.3f}, order={order_key})')

    # Update dim_utils / omexml so plane_chunks sizes the new Z, write_calibration records the
    # isotropic spacing, and save_meta_in_zarr writes the new SizeZ + PhysicalSizeZ. `dim_utils`
    # has no scale setter — mutate the OMEXML fields directly (im_physical_size reads them).
    dim_utils.im_dim[z_idx] = new_z
    try:
        omexml.images[0].pixels.physical_size_z = px_x
    except Exception:
        pass

    # Z position INSIDE a squeezed frame (read_timepoint drops the length-1 T axis).
    z_pos = z_idx - 1 if (t_idx is not None and z_idx > t_idx) else z_idx

    n_t = src_shape[t_idx] if t_idx is not None else 1
    total = n_t + 1
    done = 0
    log.progress(done, total)
    log.log(f'>> write resampled image: {im_out_path}')

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit source zarr format (ZARR_V3_PLAN D9)

        for t in range(n_t):
            frame = zarr_utils.read_timepoint(level_in, dim_utils, t)   # T squeezed
            resampled = _resample_z_frame(frame, z_pos, ratio, order, level_in.dtype)
            out_sl = [slice(None)] * len(out_shape)
            if t_idx is not None:
                out_sl[t_idx] = slice(t, t + 1)
                level0[tuple(out_sl)] = np.expand_dims(resampled, axis=t_idx)
            else:
                level0[tuple(out_sl)] = resampled
            done += 1
            log.progress(done, total)

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,   # SizeZ changes; PhysicalSizeZ already updated on omexml
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: Z is RESAMPLED (SizeZ changes), so a source box in level-0 Z coords does
        # not describe this store. Rescaling Z bounds by `ratio` is doable but unneeded — no box
        # means "all valid", so a consumer skips nothing and is merely slower, never wrong.
        # (`carry_valid_box` would refuse anyway: Z shapes differ.)

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
