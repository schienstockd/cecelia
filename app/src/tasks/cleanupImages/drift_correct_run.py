"""
Drift correction task.

Reads an OME-ZARR image, computes per-timepoint phase cross-correlation shifts
on a reference channel, applies the shifts to all channels, and writes the
drift-corrected image as a new OME-ZARR multiscale store.  The output array
may be spatially larger than the input to accommodate the cumulative drift.
Called by the Julia DriftCorrect task handler.

Parameter contract (JSON written by Julia):
  imPath             - absolute path to input .ome.zarr
  imCorrectionPath   - absolute path to write corrected .ome.zarr
  driftChannel       - int, 0-based channel index used as phase-correlation reference
  driftNormalisation - "phase" | "none"  (passed to skimage phase_cross_correlation)
"""

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.correction_utils as correction_utils
from cecelia.utils.atomic_io import write_json_atomic


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path            = params['imPath']
    im_correction_path = params['imCorrectionPath']
    drift_channel      = script_utils.channel_index(
        params.get('driftChannel'), 'driftChannel', 'drift_correct.jl')
    normalisation_raw  = params.get('driftNormalisation', 'none')
    normalisation      = normalisation_raw if normalisation_raw != 'none' else None

    log.progress(0, 4)
    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask: every read below goes through `fortify(arr[slice])` per frame, so the
    # dask handle only ever added graph overhead. Measured on a real store (zolIMa/ldYr8J, 0.78 GB):
    # a per-timepoint copy is 2.71 s from zarr vs 6.09 s from dask, at half the peak RSS. See
    # docs/todo/ZARR_STREAMING_PLAN.md -> locked decision 2.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    log.log(f'>> drift channel: {drift_channel}, normalisation: {normalisation_raw}')

    log.progress(1, 4)
    log.log('>> compute shifts')
    shifts = correction_utils.drift_correction_shifts(
        im_dat[0], drift_channel, dim_utils,
        normalisation=normalisation,
    )
    log.log(f'shifts: {shifts}')

    log.progress(2, 4)
    log.log('>> apply shifts (streaming to disk)')
    # Stream each corrected timepoint straight into the on-disk output store — the expanded
    # corrected image never lives in RAM (was the OOM on large time-lapses). Create level 0 up
    # front (shape known from the shifts), fill it per-timepoint, then build the pyramid from disk.
    out_shape, _ = correction_utils.drift_correct_shape(im_dat[0], dim_utils, shifts)
    out_dtype = im_dat[0].dtype   # writer forces native byte order (zarr_utils.native_dtype)
    # Staged: the store lands on its final path only once it is complete, metadata included, so
    # cancelling this task can't leave a registered image version truncated.
    # See docs/SEGMENTATION.md → *Stores are written staged, never in place*.
    with zarr_utils.staged_store(im_correction_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, out_dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit the source's zarr format (ZARR_V3_PLAN D9)
        correction_utils.drift_correct_im(
            im_dat[0], dim_utils, drift_channel, shifts=shifts, out=level0)

        log.progress(3, 4)
        log.log(f'>> build pyramid + save: {im_correction_path}')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))

        log.log('>> save OME-XML metadata')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,
            dim_utils=dim_utils,
        )
        # Both on-disk copies of the calibration, from one derivation — the NGFF scale/units
        # and the OME-XML <Pixels> attrs. `save_meta_in_zarr` copies the source sidecar
        # verbatim, so without this the two are written from different sources and can
        # disagree. See zarr_utils.write_calibration.
        zarr_utils.write_calibration(staging, dim_utils)

        # Which part of the expanded canvas is data. Drift drops each frame into a ZEROED canvas at
        # its own offset, so most of this store is padding (8 z-planes in a 22-plane canvas on the
        # worst movie here). Recorded on the STORE so any consumer asks one question —
        # `read_valid_box` — instead of knowing drift produced it and re-deriving the geometry. The
        # numbers come from the same call that placed the pixels.
        zarr_utils.write_valid_box(
            staging, dim_utils.spatial_axis(),
            correction_utils.drift_frame_origins(im_dat[0].shape, dim_utils, shifts))

    # Persist the APPLIED drift so it's inspectable and drives QC (the Julia task reads this, computes
    # findings, and writes the qc/ sidecar). shifts is [T, ndim] per-frame deltas; axes are Z,Y,X (3D)
    # or Y,X (2D). See docs/todo/QC_PLAN.md.
    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        n_axes = int(shifts.shape[1]) if shifts.ndim == 2 else len(shifts)
        axes = ['Z', 'Y', 'X'] if n_axes == 3 else ['Y', 'X']
        write_json_atomic(qc_out_path, {
            'dimOrder':    ''.join(dim_utils.im_dim_order),
            'sourceShape': [int(x) for x in im_dat[0].shape],
            'outputShape': [int(x) for x in out_shape],
            'shiftAxes':   axes,
            'shifts':      [[float(v) for v in row] for row in shifts],
        })
        log.log(f'>> saved drift/QC trajectory: {qc_out_path}')

    log.progress(4, 4)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
