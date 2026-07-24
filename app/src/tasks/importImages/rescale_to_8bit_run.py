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
import json

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.intensity_utils as intensity_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    out_path    = params['outPath']
    nscales     = int(params.get('nscales', 1))
    low_pct     = float(params.get('lowPercentile', 0.0))
    high_pct    = float(params.get('highPercentile', 100.0))
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
    log.log(f'>> compute per-channel intensity window over the stack '
            f'(low={low_pct}%, high={high_pct}%)')
    hists  = intensity_utils.channel_histograms(level0, c_idx)
    ranges = [intensity_utils.range_from_hist(h, low_pct, high_pct) for h in hists]

    channels = []
    for i, (h, (vmin, vmax)) in enumerate(zip(hists, ranges)):
        stats = intensity_utils.clip_stats(h, vmin, vmax)
        channels.append({'index': i, 'vmin': vmin, 'vmax': vmax, **stats})
        log.log(f'   ch{i}: window [{vmin:.0f}, {vmax:.0f}] '
                f'(trueMax={stats["trueMax"]}, p99.9={stats["p999"]}, '
                f'clipHigh={stats["clipHighFrac"]*100:.3f}%)')

    log.progress(2, 4)
    log.log(f'>> rescale to 8-bit and write: {out_path}')
    rescaled = intensity_utils.rescale_stack_to_uint8(level0, c_idx, ranges)
    zarr_utils.create_multiscales(
        rescaled, out_path,
        dim_utils=dim_utils,
        nscales=nscales,
    )

    log.progress(3, 4)
    log.log('>> save OME-XML metadata (pixel type → uint8)')
    ome_xml_utils.save_meta_in_zarr(
        out_path, im_path,
        changed_shape=level0.shape,
        dim_utils=dim_utils,
    )
    # save_meta_in_zarr copies the source OME-XML verbatim, so Type still reads uint16 — correct it
    # to match the data we just wrote (downstream reads dtype from the zarr array, but keep the
    # sidecar honest for any OME-XML consumer).
    ome_xml_utils.change_pixel_type(out_path, 'uint8')

    with open(result_path, 'w') as f:
        json.dump({'channels': channels}, f)

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
