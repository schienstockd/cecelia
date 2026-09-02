"""
Bit-depth (dtype) conversion task.

Reads an OME-ZARR image and writes it back at a chosen target dtype (uint8 / uint16 / float32),
optionally with per-channel min–max rescaling into the target's range. Dims are preserved; the
Julia handler registers the output as a NEW VERSION on the same image.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the converted .ome.zarr
  dtype    - one of 'uint8' | 'uint16' | 'float32'
  rescale  - 'auto' (per-channel min-max stretched to the target's full range)
             or 'none' (direct cast; may saturate)
"""

import numpy as np
import dask.array as da

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_TARGETS = {
    'uint8':   (np.uint8,   0, 255),
    'uint16':  (np.uint16,  0, 65535),
    'float32': (np.float32, 0.0, 1.0),   # convention: auto-rescale to [0, 1] for float32
}


def _rescale_channels(arr, c_idx, np_dtype, out_max):
    """Per-channel linear stretch of `arr` from its own [min, max] to [0, out_max], then cast to
    `np_dtype`. The two extremes of the source are found with dask reductions; the whole array is
    then remapped through a broadcast multiply. `float32` targets map to [0, 1] (out_max=1)."""
    n_c = arr.shape[c_idx]
    # split along C so each channel gets its own scale, then concat back
    parts = []
    for c in range(n_c):
        sl = [slice(None)] * arr.ndim
        sl[c_idx] = slice(c, c + 1)
        chan = arr[tuple(sl)]
        lo = float(chan.min().compute())
        hi = float(chan.max().compute())
        span = hi - lo if hi > lo else 1.0     # a flat channel stays flat, no divide-by-zero
        scaled = ((chan.astype(np.float64) - lo) * (out_max / span)).astype(np_dtype)
        parts.append(scaled)
    return da.concatenate(parts, axis=c_idx)


def _clip_cast(arr, np_dtype, out_min, out_max):
    """`rescale='none'`: clip values above/below the target's range and then cast. For a float
    target the range is [0, 1] by convention (matching auto), so `none` on a float32 target just
    casts + clips to [0, 1] — not identical to auto, but a legible fallback."""
    if np.issubdtype(np_dtype, np.floating):
        clipped = da.clip(arr.astype(np.float32), out_min, out_max)
    else:
        clipped = da.clip(arr, out_min, out_max)
    return clipped.astype(np_dtype)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    dtype_key   = params['dtype']
    rescale     = params['rescale']
    if dtype_key not in _TARGETS:
        raise ValueError(f'unknown dtype: {dtype_key} (want one of {sorted(_TARGETS)})')
    if rescale not in ('auto', 'none'):
        raise ValueError(f'unknown rescale mode: {rescale} (want "auto" or "none")')
    np_dtype, out_min, out_max = _TARGETS[dtype_key]

    log.progress(0, 3)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    src = im_dat[0]

    c_idx = dim_utils.dim_idx('C')
    if rescale == 'auto' and c_idx is not None:
        converted = _rescale_channels(src, c_idx, np_dtype, out_max)
    elif rescale == 'auto':
        # no channel axis — one scalar min/max over the whole array
        lo = float(src.min().compute()); hi = float(src.max().compute())
        span = hi - lo if hi > lo else 1.0
        converted = ((src.astype(np.float64) - lo) * (out_max / span)).astype(np_dtype)
    else:
        converted = _clip_cast(src, np_dtype, out_min, out_max)

    log.log(f'>> convert dtype={dtype_key} rescale={rescale}: {src.dtype} -> {converted.dtype}')

    log.progress(1, 3)
    log.log(f'>> write converted image: {im_out_path}')
    with zarr_utils.staged_store(im_out_path) as staging:
        zarr_utils.create_multiscales(
            converted, staging,
            dim_utils=dim_utils,
            reference_zarr=src,
            nscales=len(im_dat),
        )

        log.progress(2, 3)
        log.log('>> save OME-XML metadata')
        # shape unchanged — no changed_shape override needed
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
