"""
Within-stack XY alignment task.

For each timepoint's Z stack, estimate per-plane XY shifts to a chosen
reference plane and warp the whole stack so all planes share the same
lateral position. Writes a new OME-ZARR at `imAlignedPath`. Two gates
(confidence and max-shift) refuse to force structural Z differences into
shifts on movies where planes are just at different depths.

Design + measurements: docs/todo/STACK_ALIGN_PLAN.md.

Parameter contract (JSON written by Julia):
  imPath          - absolute path to input .ome.zarr
  imAlignedPath   - absolute path to write aligned .ome.zarr
  alignChannel    - int, 0-based channel index used to estimate shifts
  referenceMode   - "middle" | "sharpest"
  minConfidence   - float, gate on PC peak strength (default 0.35)
  maxShiftPx      - float, gate on estimated shift magnitude (default 8.0)
"""

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher
# (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.correction_utils as correction_utils
from cecelia.utils.atomic_io import write_json_atomic


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path         = params['imPath']
    im_aligned_path = params['imAlignedPath']
    align_channel   = script_utils.channel_index(
        params.get('alignChannel'), 'alignChannel', 'stack_align.jl')
    reference_mode  = params.get('referenceMode', 'middle')
    min_conf        = float(params.get('minConfidence',
                                       correction_utils.STACK_ALIGN_DEFAULT_MIN_CONF))
    max_shift_px    = float(params.get('maxShiftPx',
                                       correction_utils.STACK_ALIGN_DEFAULT_MAX_SHIFT_PX))

    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    log.log(f'>> align channel: {align_channel}, reference: {reference_mode}, '
            f'gates: min_conf={min_conf}, max_shift={max_shift_px} px')

    n_t = dim_utils.dim_val('T')
    # T estimate + T apply + 1 pyramid/metadata = one steady scale end-to-end.
    total = 2 * n_t + 1
    log.progress(0, total)

    log.log('>> estimate per-plane shifts')
    est = correction_utils.estimate_stack_alignment(
        im_dat[0], align_channel, dim_utils,
        reference=reference_mode, min_conf=min_conf, max_shift_px=max_shift_px,
        on_progress=lambda n, _t: log.progress(n, total),
    )
    n_planes_total    = int(est.applied.size)
    n_planes_applied  = int(est.applied.sum())
    n_planes_skipped  = n_planes_total - n_planes_applied
    log.log(f'>> {n_planes_applied}/{n_planes_total} planes aligned, '
            f'{n_planes_skipped} skipped by gate')

    log.log('>> apply shifts (streaming to disk)')
    # Same shape as input — per-plane shifts are small and stay inside the
    # frame. No canvas expansion (unlike drift correction, whose per-frame
    # shifts accumulate).
    out_shape = im_dat[0].shape
    out_dtype = im_dat[0].dtype
    with zarr_utils.staged_store(im_aligned_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, out_dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)
        correction_utils.apply_stack_alignment(
            im_dat[0], est, dim_utils, out=level0,
            on_progress=lambda n, _t: log.progress(n_t + n, total))

        log.log(f'>> build pyramid + save: {im_aligned_path}')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))

        log.log('>> save OME-XML metadata')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # Carry the SOURCE's valid box through. This aligner shifts pixels
        # laterally, so strictly the box shrinks by the max applied per-plane
        # shift on Y and X — but the gate caps that at `maxShiftPx` (default
        # 8 px) which is negligible against a typical 512+ px frame and much
        # smaller than the drift-canvas expansion this task is meant to feed
        # (the downstream `driftCorrect` recomputes the box anyway). If a
        # future dataset needs pixel-exact edge tracking, replace with a
        # per-(t, z) computed box from the applied shifts.
        if zarr_utils.carry_valid_box(im_path, staging):
            log.log(f'>> carried valid box from {im_path}')

    # QC sidecar: per-(t, z) shifts, confidence, applied flag, chosen ref.
    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        doc = {
            'dimOrder':      ''.join(dim_utils.im_dim_order),
            'sourceShape':   [int(x) for x in im_dat[0].shape],
            'referenceMode': reference_mode,
            'minConfidence': min_conf,
            'maxShiftPx':    max_shift_px,
            'refIdx':        [int(z) for z in est.ref_idx],
            'shifts':        [[[float(v) for v in dy_dx] for dy_dx in row]
                              for row in est.shifts],
            'confidence':    [[float(v) for v in row] for row in est.confidence],
            'applied':       [[bool(v) for v in row] for row in est.applied],
            'nPlanesTotal':   n_planes_total,
            'nPlanesApplied': n_planes_applied,
            'nPlanesSkipped': n_planes_skipped,
        }
        write_json_atomic(qc_out_path, doc)
        log.log(f'>> saved stack-align QC: {qc_out_path}')

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
