"""
Drift correction task.

Reads an OME-ZARR image, estimates per-timepoint shifts by phase cross-correlation on a reference
channel, applies them to all channels, and writes the drift-corrected image as a new OME-ZARR
multiscale store.  The output array may be spatially larger than the input to accommodate the
cumulative drift.  Called by the Julia DriftCorrect task handler.

Parameter contract (JSON written by Julia):
  imPath             - absolute path to input .ome.zarr
  imCorrectionPath   - absolute path to write corrected .ome.zarr
  driftChannel       - int, 0-based channel index used as phase-correlation reference
  driftNormalisation - "none" | "phase"  (passed to skimage phase_cross_correlation; multiLag/chain
                       only — sitkRigid does not use PCC). Phase whitens the cross-power spectrum,
                       which helps on low-SNR frames or large per-frame drift where the raw peak is
                       too broad. Default `none` matches the pre-existing behaviour.
  driftEstimator     - "multiLag" | "chain" | "sitkRigid"  (see correction_utils.estimate_drift)
  driftMaxLag        - int, how far apart two frames may be and still be compared (multiLag only)
  driftMaxAngle      - float, degrees; per-frame |angle| cap (sitkRigid only)
  driftSmoothSigma   - float, gaussian sigma (frames) on the cumulative trajectory. Kills
                       integer-rounding jitter from sub-pixel-noise trajectories without
                       significantly changing real motion. 0 = off. Default: DRIFT_TASK_SMOOTH_SIGMA.
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
    estimator          = params.get('driftEstimator', 'multiLag')
    max_lag            = int(params.get('driftMaxLag', correction_utils.DRIFT_DEFAULT_MAX_LAG))
    max_angle_deg      = float(params.get('driftMaxAngle', correction_utils.DRIFT_DEFAULT_MAX_ANGLE))
    smooth_sigma       = float(params.get('driftSmoothSigma',
                                          correction_utils.DRIFT_TASK_SMOOTH_SIGMA))

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
    _est_extra = (f' (max lag {max_lag})' if estimator == 'multiLag'
                  else f' (max angle {max_angle_deg}°)' if estimator == 'sitkRigid'
                  else '')
    _smooth_extra = f', trajectory σ={smooth_sigma}' if smooth_sigma > 0 else ', no trajectory smoothing'
    _norm_extra = '' if estimator == 'sitkRigid' else f', normalisation: {normalisation_raw}'
    log.log(f'>> drift channel: {drift_channel}, estimator: {estimator}{_est_extra}{_norm_extra}{_smooth_extra}')

    # One progress scale across the whole run rather than a 4-step one, because both loops below
    # are minutes long on a real movie and the old scale stood still through each of them:
    # T frames estimated, T frames written, then the pyramid + metadata.
    n_t = dim_utils.dim_val('T')
    total = 2 * n_t + 1
    log.progress(0, total)

    log.log('>> estimate drift')
    est = correction_utils.estimate_drift(
        im_dat[0], drift_channel, dim_utils,
        normalisation=normalisation,
        estimator=estimator, max_lag=max_lag,
        max_angle_deg=max_angle_deg,
        trajectory_smooth_sigma=smooth_sigma,
        on_progress=lambda n, _t: log.progress(n, total),
    )
    shifts = est.shifts
    is_rigid = est.angles is not None
    log.log(f'>> {est.n_pairs} pair measurements, {est.n_rejected} outvoted; ' + (
        f'consistency {est.residual_rms:.2f} px RMS / {est.residual_p90:.2f} px p90'
        if est.residual_rms is not None
        else 'consistency not measurable (neighbour pairs only)'))
    if is_rigid:
        # A rigid run cannot report a translation-style residual, so the interesting number is the
        # angle range. `max()`/`min()` on an empty (T=1) run is fine — the guard is that T >= 2 to
        # even ENTER drift correction, which the Julia dispatcher already enforces.
        log.log(f'>> rotation range {est.angles.min():.2f}°..{est.angles.max():.2f}° '
                f'(cap {max_angle_deg}°)')
    if est.interpolated:
        why = 'exceeded angle cap' if is_rigid else 'could not be registered'
        log.log(f'[WARN] {len(est.interpolated)} frame(s) {why} — position '
                f'predicted from neighbours: {est.interpolated[:12]}'
                + ('…' if len(est.interpolated) > 12 else ''))
    log.log(f'shifts: {shifts}')

    log.log('>> apply shifts (streaming to disk)')
    # Stream each corrected timepoint straight into the on-disk output store — the expanded
    # corrected image never lives in RAM (was the OOM on large time-lapses). Create level 0 up
    # front (shape known from the trajectory), fill it per-timepoint, then build the pyramid from
    # disk. Two shape helpers depending on the trajectory kind: `drift_correct_shape` for pure
    # translation, `rigid_correct_geometry` for rotation-aware (canvas expands per rotated bbox).
    if is_rigid:
        out_shape, _, _ = correction_utils.rigid_correct_geometry(
            im_dat[0], dim_utils, est.positions, est.angles)
    else:
        out_shape, _ = correction_utils.drift_correct_shape(im_dat[0], dim_utils, shifts)
    out_dtype = im_dat[0].dtype   # writer forces native byte order (zarr_utils.native_dtype)
    # Staged: the store lands on its final path only once it is complete, metadata included, so
    # cancelling this task can't leave a registered image version truncated.
    # See docs/SEGMENTATION.md → *Stores are written staged, never in place*.
    with zarr_utils.staged_store(im_correction_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, out_dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit the source's zarr format (ZARR_V3_PLAN D9)
        if is_rigid:
            correction_utils.rigid_correct_im(
                im_dat[0], dim_utils, est.positions, est.angles, out=level0,
                on_progress=lambda n, _t: log.progress(n_t + n, total))
        else:
            correction_utils.drift_correct_im(
                im_dat[0], dim_utils, drift_channel, shifts=shifts, out=level0,
                on_progress=lambda n, _t: log.progress(n_t + n, total))

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
        # its own offset, so much of this store is padding (8 z-planes in an 18-plane canvas on the
        # worst movie here). Recorded on the STORE so any consumer asks one question —
        # `read_valid_box` — instead of knowing drift produced it and re-deriving the geometry. The
        # numbers come from the same call that placed the pixels, so this stays exact even when the
        # SHIFTS are poor: it describes where the pixels went, not where they should have gone.
        if is_rigid:
            zarr_utils.write_valid_box(
                staging, dim_utils.spatial_axis(),
                correction_utils.rigid_frame_origins(
                    im_dat[0], dim_utils, est.positions, est.angles))
        else:
            zarr_utils.write_valid_box(
                staging, dim_utils.spatial_axis(),
                correction_utils.drift_frame_origins(im_dat[0].shape, dim_utils, shifts))

    # Persist the APPLIED drift so it's inspectable and drives QC (the Julia task reads this, computes
    # findings, and writes the qc/ sidecar). shifts is [T, ndim] per-frame deltas; axes are Z,Y,X (3D)
    # or Y,X (2D). See docs/todo/QC_PLAN.md.
    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        doc = {
            'dimOrder':     ''.join(dim_utils.im_dim_order),
            'sourceShape':  [int(x) for x in im_dat[0].shape],
            'outputShape':  [int(x) for x in out_shape],
            'shiftAxes':    list(est.axes),
            'shifts':       [[float(v) for v in row] for row in shifts],
            'estimator':    est.estimator,
            'maxLag':       int(est.max_lag),
            'normalisation': normalisation_raw,
            'smoothSigma':  smooth_sigma,
            'nPairs':       int(est.n_pairs),
            'nRejected':    int(est.n_rejected),
            'interpolated': [int(t) for t in est.interpolated],
        }
        # How much the estimate can be trusted — see correction_utils.drift_residuals. OMITTED
        # rather than zeroed when the estimator had no redundancy to measure it from, so the
        # Julia side reads "not measured" instead of "measured as perfect".
        if est.residual_rms is not None:
            doc['residualRms'] = est.residual_rms
            doc['residualP90'] = est.residual_p90
        # Rigid trajectory adds one scalar per frame — the in-plane rotation. Persisted so the
        # Julia QC handler can bank a `drift.rotation.capped` finding when any frame's fit had to
        # be predicted from neighbours, and so `maxAngleDeg` reaches the cohort metrics.
        if est.angles is not None:
            doc['angles']       = [float(a) for a in est.angles]
            doc['maxAngleDeg']  = max((abs(float(a)) for a in est.angles), default=0.0)
            doc['maxAngleCap']  = float(max_angle_deg)
        write_json_atomic(qc_out_path, doc)
        log.log(f'>> saved drift/QC trajectory: {qc_out_path}')

    log.progress(total, total)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
