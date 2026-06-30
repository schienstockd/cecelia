"""
Autofluorescence correction task.

Reads an OME-ZARR image, applies per-channel AF correction (ratio against
reference channels), and writes the result as a new OME-ZARR multiscale store.
Called by the Julia AfCorrect task handler.

Parameter contract (JSON written by Julia):
  imPath              - absolute path to input .ome.zarr
  imCorrectionPath    - absolute path to write corrected .ome.zarr
  afCombinations      - dict keyed by string channel index ("0", "1", …):
      divisionChannels  - list of 0-based integer channel indices (AF reference)
      correctionMode    - "divide" | "none"
      channelPercentile - float, background percentile to subtract from channel
      correctionPercentile - float, background percentile for AF reference
      summaryMode       - "maximum" | "percentile"
      summaryPercentile - float, used when summaryMode == "percentile"
      correctionMin     - float, lower rescale percentile
      correctionMax     - float, upper rescale percentile
      generateInverse   - bool
      medianFilter      - int, median kernel half-width (0 = off)
      topHatRadius      - int, top-hat radius (0 = off)
      rollingBallRadius - int, rolling ball radius (0 = off)
      rollingBallPadding - int
      denoiseFun        - "NONE" | "wavelet" | "tv"
      waveletMethod     - "BayesShrink" | "VisuShrink"
      waveletMode       - "soft" | "hard"
      tvWeight          - int, TV weight (used as-is)
  applyGaussian         - bool, apply Gaussian to corrected channels
  applyGaussianToOthers - bool, apply Gaussian to uncorrected channels
"""

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
    af_combinations    = params['afCombinations']
    apply_gaussian     = bool(params.get('applyGaussian', True))
    apply_gaussian_to_others = bool(params.get('applyGaussianToOthers', True))

    log.progress(0, 3)
    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    log.log(f'>> afCombinations: {af_combinations}')

    log.progress(1, 3)
    log.log('>> correct image')
    corrected_image = correction_utils.af_correct_image(
        im_dat[0], af_combinations,
        dim_utils=dim_utils,
        logfile_utils=log,
        apply_gaussian=apply_gaussian,
        apply_gaussian_to_others=apply_gaussian_to_others,
        use_dask=False,
    )

    log.progress(2, 3)
    log.log(f'>> save corrected image: {im_correction_path}')
    zarr_utils.create_multiscales(
        corrected_image, im_correction_path,
        dim_utils=dim_utils,
        nscales=len(im_dat),
    )

    log.log('>> save OME-XML metadata')
    ome_xml_utils.save_meta_in_zarr(
        im_correction_path, im_path,
        changed_shape=corrected_image.shape,
        dim_utils=dim_utils,
    )

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
