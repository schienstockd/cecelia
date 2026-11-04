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

Result JSON: `cecelia.utils.data_path.measure_store` — {"channels": [{"index", "saturated",
"topValue", …, "containerBits", "range", "noise", "profile"}, …], "sampling"} — the Julia
handler turns this into ccid meta + QC findings. `{}` when the store cannot be read as integer data
(nothing to say, and a QC pass must never fail an otherwise-good import).
"""

import cecelia.utils.script_utils as script_utils
import cecelia.utils.data_path as data_path
from cecelia.utils.atomic_io import write_json_atomic


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    result_path = params['resultPath']
    result = {}

    log.progress(0, 2)
    try:
        log.progress(1, 2)
        result = data_path.measure_store(im_path, log=log.log)
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
