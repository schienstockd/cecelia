"""
Autofluorescence correction task.

Reads an OME-ZARR image, corrects each channel by the share of every voxel it dominates against its
competing channels, and writes the result as a new OME-ZARR multiscale store. Called by the Julia
AfCorrect task handler.

Parameter contract (JSON written by Julia):
  imPath           - absolute path to input .ome.zarr
  imCorrectionPath - absolute path to write corrected .ome.zarr
  afCombinations   - dict keyed by string channel index ("0", "1", …):
      competingChannels - list of 0-based integer channel indices sharing signal with it
  backgroundMethod - "triangle" | "otsu", global to every combination
  qcOutPath        - where to write per-channel output stats for QC

A combination is **just channels**. Everything that used to be a number here — two background
percentiles, a rescale window, a median filter, a gaussian, a rolling ball, a top hat, a denoiser, an
inverse channel — is either derived from the data (`correction_utils.af_weight_stats`) or gone. Those
parameters accreted while fitting individual datasets and were never revisited; a correction task
should correct, not carry a filter toolbox.
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
    af_combinations    = params['afCombinations']
    background_method  = str(params.get('backgroundMethod', 'triangle'))
    qc_out_path        = params.get('qcOutPath')

    log.progress(0, 3)
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
    log.log(f'>> afCombinations: {af_combinations}')

    log.progress(1, 3)
    log.log('>> correct image (streaming to disk)')
    # Stream channel-by-channel into the on-disk output store — the whole corrected image never lives
    # in RAM (was the OOM on large time-lapses). Size level 0 up front, fill per-channel, then build
    # the pyramid. The output has the same channels as the input now that inverse channels are gone.
    out_shape = correction_utils.af_correction_output_shape(im_dat[0], dim_utils)
    out_dtype = im_dat[0].dtype   # writer forces native byte order (zarr_utils.native_dtype)
    output_stats = {}
    # Staged: the store lands on its final path only once it is complete, metadata included, so
    # cancelling this task can't leave a registered image version truncated.
    # See docs/SEGMENTATION.md → *Stores are written staged, never in place*.
    with zarr_utils.staged_store(im_correction_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, out_dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit the source's zarr format (ZARR_V3_PLAN D9)
        correction_utils.af_correct_image(
            im_dat[0], af_combinations,
            dim_utils=dim_utils,
            logfile_utils=log,
            background_method=background_method,
            out=level0,
            output_stats=output_stats,
        )

        log.progress(2, 3)
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

        # Same for the valid box: this correction rewrites intensities without moving a pixel, and it
        # normally runs on a DRIFT-CORRECTED store whose canvas is mostly padding. Dropping the box
        # here made segmentation downstream process the padding it describes.
        if zarr_utils.carry_valid_box(im_path, staging):
            log.log('   carried the source valid box forward (per timepoint)')

    # Per-channel output stats for QC. The correction has no free parameter left to land badly, so the
    # objective signals are the INPUT's saturation (clipped at the sensor, unrecoverable here) and how
    # coarsely the output ends up quantised. See `af_qc_findings` in af_correct.jl.
    #
    # `.get` with a default, not `[...]`: this log line referenced `clippedFrac` by subscript and threw
    # `KeyError` on a real run AFTER the corrected store had already been written — the work was done and
    # the task still failed. A progress log must never be able to fail a completed run.
    if qc_out_path:
        write_json_atomic(qc_out_path, output_stats)
        for ch, s in sorted(output_stats.items()):
            log.log(f">> ch{ch}: {s.get('saturatedFrac', 0.0) * 100:.3f}% of input saturated, "
                    f"{s.get('levelsUsed', 0)}/{s.get('levelsAvailable', 0)} levels used")

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
