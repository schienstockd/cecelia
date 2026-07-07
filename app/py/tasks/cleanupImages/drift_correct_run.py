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

import json

# `py.*` resolves via PYTHONPATH=app/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import py.utils.zarr_utils as zarr_utils
import py.utils.ome_xml_utils as ome_xml_utils
from py.utils.dim_utils import DimUtils
import py.utils.script_utils as script_utils
import py.utils.correction_utils as correction_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path            = params['imPath']
    im_correction_path = params['imCorrectionPath']
    drift_channel      = int(params['driftChannel'])
    normalisation_raw  = params.get('driftNormalisation', 'none')
    normalisation      = normalisation_raw if normalisation_raw != 'none' else None

    log.progress(0, 4)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

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
    log.log('>> apply shifts')
    drift_image = correction_utils.drift_correct_im(
        im_dat[0], dim_utils, drift_channel, shifts=shifts,
    )

    log.progress(3, 4)
    log.log(f'>> save corrected image: {im_correction_path}')
    zarr_utils.create_multiscales(
        drift_image, im_correction_path,
        dim_utils=dim_utils,
        reference_zarr=im_dat[0],
        nscales=len(im_dat),
    )

    log.log('>> save OME-XML metadata')
    ome_xml_utils.save_meta_in_zarr(
        im_correction_path, im_path,
        changed_shape=drift_image.shape,
        dim_utils=dim_utils,
    )

    # Persist the APPLIED drift so it's inspectable and drives QC (the Julia task reads this, computes
    # findings, and writes the qc/ sidecar). shifts is [T, ndim] per-frame deltas; axes are Z,Y,X (3D)
    # or Y,X (2D). See docs/todo/QC_PLAN.md.
    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        n_axes = int(shifts.shape[1]) if shifts.ndim == 2 else len(shifts)
        axes = ['Z', 'Y', 'X'] if n_axes == 3 else ['Y', 'X']
        with open(qc_out_path, 'w') as f:
            json.dump({
                'dimOrder':    ''.join(dim_utils.im_dim_order),
                'sourceShape': [int(x) for x in im_dat[0].shape],
                'outputShape': [int(x) for x in drift_image.shape],
                'shiftAxes':   axes,
                'shifts':      [[float(v) for v in row] for row in shifts],
            }, f)
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
