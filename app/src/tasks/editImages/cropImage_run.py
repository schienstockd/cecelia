"""
Crop image task.

Reads an OME-ZARR image, slices it to a pixel bounding box (X/Y always; Z/T when present — all channels
kept), and writes the cropped region as a NEW OME-ZARR multiscale store, streaming one timepoint at a
time. The Julia CropImage handler registers the output as a new image in the set.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the cropped .ome.zarr
  x0,x1    - X pixel bounds (half-open) at FULL resolution
  y0,y1    - Y pixel bounds (half-open)
  z0,z1    - Z pixel bounds (half-open); -1 = keep full Z (2D image / no z crop)
  t0,t1    - T pixel bounds (half-open); -1 = keep full T (no time trim)
"""

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.slice_utils import crop_slice_tuple  # pure logic lives in the IO library (testable)


def _t_range(t_idx, bounds_t, nt):
    """Half-open (t_lo, t_hi) inside the source; `-1` means keep the full extent."""
    if t_idx is None:
        return (0, 1)
    lo, hi = bounds_t
    if lo < 0 and hi < 0:
        return (0, nt)
    return (max(0, int(lo)), min(nt, int(hi)))


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    bounds = {
        'X': (int(params['x0']), int(params['x1'])),
        'Y': (int(params['y0']), int(params['y1'])),
        'Z': (int(params.get('z0', -1)), int(params.get('z1', -1))),
        'T': (int(params.get('t0', -1)), int(params.get('t1', -1))),
    }

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask: every read below is one `read_timepoint` per frame (chunk-aligned), so a
    # dask graph only adds overhead. Same reasoning as drift_correct_run.py / smooth_run.py.
    # See docs/todo/ZARR_STREAMING_PLAN.md → locked decision 2.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in  = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    axis_idx    = {ax: dim_utils.dim_idx(ax) for ax in ('X', 'Y', 'Z', 'T')}
    src_slices  = crop_slice_tuple(level_in.ndim, axis_idx, bounds)
    # Un-cropped axes (channels; also Z/T when bounds are -1) come back as `slice(None)`, so
    # resolve against the source shape rather than subtracting `.stop - .start` directly.
    out_shape   = tuple(len(range(*s.indices(n))) for s, n in zip(src_slices, level_in.shape))
    log.log(f'>> crop {level_in.shape} -> {out_shape}  bounds={bounds}')

    # Update dim_utils to reflect the OUTPUT shape so `open_multiscales_for_writing` chunks it, the
    # pyramid builds against the right dims, and `save_meta_in_zarr` records the new sizes. Calibration
    # (pixel size, time interval) is unchanged by cropping — no OMEXML mutation needed.
    for ax, i in axis_idx.items():
        if i is not None:
            dim_utils.im_dim[i] = out_shape[i]

    t_idx = axis_idx['T']
    t_lo, t_hi = _t_range(t_idx, bounds['T'], level_in.shape[t_idx] if t_idx is not None else 1)
    n_t_out = t_hi - t_lo if t_idx is not None else 1

    # `total` = frames + pyramid tick; matches drift/smooth's one-tick-per-frame pattern.
    total = n_t_out + 1
    done = 0
    log.progress(done, total)
    log.log(f'>> write cropped image: {im_out_path}')

    # Staged: the store lands on its final path only once complete (metadata included), so a cancelled
    # run can't leave a registered image version truncated. See docs/SEGMENTATION.md → *Stores are
    # written staged, never in place*.
    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit the source's zarr format (ZARR_V3_PLAN D9)

        # Per-frame slice INTO the source (image X/Y/Z bounds fixed; T bounds walked one at a time).
        # For a non-timeseries image the loop runs once and copies the whole spatial crop.
        for i, t in enumerate(range(t_lo, t_hi) if t_idx is not None else [0]):
            src_sl = list(src_slices)
            out_sl = [slice(None)] * len(out_shape)
            if t_idx is not None:
                src_sl[t_idx] = slice(t, t + 1)
                out_sl[t_idx] = slice(i, i + 1)
            level0[tuple(out_sl)] = np.asarray(level_in[tuple(src_sl)])
            done += 1
            log.progress(done, total)

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,   # SizeX/Y/Z/T shrink; PhysicalSize*/TimeIncrement carry over
            dim_utils=dim_utils,
        )
        # Both on-disk copies of the calibration, from one derivation — the NGFF scale/units and the
        # OME-XML <Pixels> attrs. `save_meta_in_zarr` copies the source sidecar verbatim, so without
        # this the two are written from different sources and can disagree. See write_calibration.
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: a crop MOVES the coordinates, so the parent's box would be a
        # precise-looking lie about this store. Translating it by the crop offset and clamping is
        # doable but unneeded — no box means "all valid", so a consumer skips nothing and is merely
        # slower, never wrong. (`carry_valid_box` would refuse anyway: the shapes differ.)

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
