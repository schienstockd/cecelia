"""
16→8-bit rescale on import.

Reads the (transient) 16-bit OME-ZARR that bioformats2raw produced, computes a per-channel intensity
window over the whole stack, linearly rescales + clips to uint8, and writes the final 8-bit
multiscale store. Called by the Julia ImportOmezarr task handler when `convertTo8bit` is set; the
16-bit input is a scratch copy the handler deletes afterwards.

Parameter contract (JSON written by Julia):
  imPath        - absolute path to the transient 16-bit .ome.zarr (bioformats2raw, nested layout)
  outPath       - absolute path to write the 8-bit .ome.zarr (flat create_multiscales layout)
  nscales       - number of pyramid levels for the output
  lowPercentile  - bottom of the per-channel window (0 = true min)
  highPercentile - top of the per-channel window (100 = true max)
  resultPath    - absolute path to write the per-channel window/QC result JSON

Writes `resultPath` with `{"channels": [{"index","vmin","vmax","clipLowFrac","clipHighFrac",
"p999","trueMax","rangeSpan"}, …]}` — the Julia handler turns this into ccid meta + QC findings.
"""

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.intensity_utils as intensity_utils
from cecelia.utils.atomic_io import write_json_atomic


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    out_path    = params['outPath']
    nscales     = int(params.get('nscales', 1))
    low_pct     = float(params.get('lowPercentile', 0.0))
    high_pct    = float(params.get('highPercentile', 100.0))
    # An absolute window, in RAW units, shared by every channel and every image. Overrides the
    # percentiles when set (fixedMax > fixedMin). See intensity_utils.channel_ranges.
    fixed_min   = float(params.get('fixedMin', 0.0))
    fixed_max   = float(params.get('fixedMax', 0.0))
    fixed       = (fixed_min, fixed_max) if fixed_max > fixed_min else None
    # This image is its set's REFERENCE: derive the window from its own histograms instead of being
    # handed one, and report it back so the set can apply it to every other image. > 0 turns it on.
    derive      = float(params.get('deriveLeeway', 0.0))
    result_path = params['resultPath']

    log.progress(0, 4)
    log.log(f'>> open 16-bit image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)
    level0 = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level0.shape)
    c_idx = dim_utils.dim_idx('C')
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim} (channel axis={c_idx})')

    log.progress(1, 4)
    log.log('>> compute intensity window over the stack ' + (
        f'(FIXED [{fixed_min:.0f}, {fixed_max:.0f}], shared by all channels)' if fixed
        else f'(DERIVED from this reference image, leeway {derive}x)' if derive > 0
        else f'(per channel, low={low_pct}%, high={high_pct}%)'))
    # Histograms are computed either way — with a fixed window they no longer choose it, but
    # clip_stats still reports how much of each channel it actually clips, which is the number
    # that tells you whether the window was set sensibly.
    hists = intensity_utils.channel_histograms(level0, c_idx)
    if fixed is None and derive > 0:
        fixed = intensity_utils.reference_window(hists, leeway=derive)
        if fixed is None:
            log.log('   [WARN] no signal in any channel — falling back to the percentile window')
    ranges = intensity_utils.channel_ranges(hists, low_pct, high_pct, fixed=fixed)

    channels = []
    for i, (h, (vmin, vmax)) in enumerate(zip(hists, ranges)):
        stats = intensity_utils.clip_stats(h, vmin, vmax)
        # robustMax: the ceiling THIS image would have asked for. Recorded per channel so the set can
        # be compared afterwards — the image with the largest one is, by construction, the reference
        # that would clip the others least. The QC uses it to name a better nomination.
        stats['robustMax'] = intensity_utils.robust_hist_max(h)
        # Saturated at acquisition: the ceiling is unknowable and no window can recover it. Recorded
        # so the QC reports it as its own problem instead of blaming the rescale.
        stats['saturated'] = bool(intensity_utils.is_saturated(h))
        channels.append({'index': i, 'vmin': vmin, 'vmax': vmax, **stats})
        log.log(f'   ch{i}: window [{vmin:.0f}, {vmax:.0f}] '
                f'(robustMax={stats["robustMax"]}, trueMax={stats["trueMax"]}, '
                f'clipHigh={stats["clipHighFrac"]*100:.3f}%)')

    log.progress(2, 4)
    log.log(f'>> rescale to 8-bit and write: {out_path}')
    rescaled = intensity_utils.rescale_stack_to_uint8(level0, c_idx, ranges)
    # Staged: the store lands on its final path only once it is complete, metadata included, so
    # cancelling this task can't leave a registered image version truncated.
    # See docs/SEGMENTATION.md → *Stores are written staged, never in place*.
    with zarr_utils.staged_store(out_path) as staging:
        zarr_utils.create_multiscales(
            rescaled, staging,
            dim_utils=dim_utils,
            nscales=nscales,
        )

        log.progress(3, 4)
        log.log('>> save OME-XML metadata (pixel type → uint8)')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=level0.shape,
            dim_utils=dim_utils,
        )
        # save_meta_in_zarr copies the source OME-XML verbatim, so Type still reads uint16 — correct
        # it to match the data we just wrote (downstream reads dtype from the zarr array, but keep
        # the sidecar honest for any OME-XML consumer).
        ome_xml_utils.change_pixel_type(staging, 'uint8')
        # Both on-disk copies of the calibration, from one derivation — the NGFF scale/units
        # and the OME-XML <Pixels> attrs. `save_meta_in_zarr` copies the source sidecar
        # verbatim, so without this the two are written from different sources and can
        # disagree. See zarr_utils.write_calibration.
        zarr_utils.write_calibration(staging, dim_utils)

    # `window` is what was APPLIED, whatever chose it — so the caller stores one thing on the set and
    # every later image is handed the same numbers rather than re-deriving them per image.
    write_json_atomic(result_path, {
        'channels': channels,
        'window': None if fixed is None else {'min': float(fixed[0]), 'max': float(fixed[1])},
    })

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
