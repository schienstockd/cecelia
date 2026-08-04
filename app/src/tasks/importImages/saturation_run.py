"""
Detect channels that CLIPPED AT ACQUISITION, from the store the import just wrote.

Runs on every import. A clipped channel has lost information the pipeline cannot recover — no
correction, threshold or rescale puts it back — so the only useful moment to say so is at import,
while the answer is still "re-acquire with less gain" rather than "re-do the analysis".

Detection is `intensity_utils.is_saturated`: structural (a pile-up in the brightest occupied bin),
so it does not need to know the detector's bit depth. That matters here — measured on the nine
`kSUFux` movies, the sensor clips at 4095 inside 16-bit words, so the obvious test (fraction of
voxels at the dtype maximum) reports zero on data that is visibly clipped. See that docstring.

Cost: one streamed pass over the store, ~3 s/GB measured (5.3 s for a 1.74 GB store). Deliberately a
FULL pass rather than a strided one — the threshold is a voxel count, so subsampling scales the
pile-up down with it and flips marginal channels.

Parameter contract (JSON written by Julia):
  imPath     - absolute path to the .ome.zarr the import just wrote
  resultPath - absolute path to write the result JSON to

Result JSON: {"channels": [{"index", "saturated", "topValue", "topCount", "topFrac"}, …]} — the Julia
handler turns this into ccid meta + QC findings. `{}` when the store cannot be read as integer data
(nothing to say, and a QC pass must never fail an otherwise-good import).
"""

import cecelia.utils.script_utils as script_utils
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.intensity_utils as intensity_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.atomic_io import write_json_atomic
import cecelia.utils.ome_xml_utils as ome_xml_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    result_path = params['resultPath']
    result = {}

    log.progress(0, 2)
    try:
        dim_utils = DimUtils(ome_xml_utils.load_ome_xml(im_path))
        levels, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)
        level0 = levels[0]
        dim_utils.calc_image_dimensions(level0.shape)
        c_idx = dim_utils.dim_idx('C') if 'C' in dim_utils.im_dim_order else None

        log.log(f'>> checking {level0.dtype} {level0.shape} for clipping at acquisition')
        log.progress(1, 2)
        hists = intensity_utils.channel_histograms(level0, c_idx)

        channels = []
        for i, h in enumerate(hists):
            stats = intensity_utils.saturation_stats(h)
            stats['index'] = i
            channels.append(stats)
            log.log(f'   ch{i}: {"SATURATED" if stats["saturated"] else "ok"} '
                    f'(top occupied value {stats["topValue"]}, {stats["topCount"]} voxels '
                    f'= {stats["clippedSignalFrac"] * 100:.4f}% of signal)')
        result = {'channels': channels}
    except Exception as e:
        # Advisory QC on a store that already converted successfully: report and move on. A
        # non-integer dtype (channel_histograms raises) or an unreadable OME is not an import failure.
        log.log(f'>> [WARN] could not check for saturation: {e}')

    write_json_atomic(result_path, result)
    log.progress(2, 2)


def main():
    params = script_utils.script_params()
    if params is None:
        return
    run(params)


if __name__ == '__main__':
    main()
