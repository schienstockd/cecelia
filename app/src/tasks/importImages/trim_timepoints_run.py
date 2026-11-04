"""
Drop the timepoints past `sizeT` from a store the import has just written.

Called by the `importImages.omezarr` task when the saturation pass reports a last timepoint the
acquisition never finished (`partialTimepoint`). Thin by design: the trim is
`zarr_utils.trim_timepoints`, which resizes every pyramid level in place. The OME-XML `SizeT` is updated
by the Julia caller afterwards.

Parameter contract (JSON written by Julia):
  imPath     - absolute path to the .ome.zarr the import just wrote (nothing references it yet)
  sizeT      - int, how many timepoints to keep
  resultPath - absolute path to write the result JSON to: {"levels": n} on success, {"error": msg}
"""

import cecelia.utils.script_utils as script_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.atomic_io import write_json_atomic


def run(params):
    log = script_utils.get_logfile_utils(params)
    im_path     = params['imPath']
    size_t      = int(params['sizeT'])
    result_path = params['resultPath']
    log.progress(0, 1)
    try:
        n = zarr_utils.trim_timepoints(im_path, size_t)
        log.log(f'>> kept the first {size_t} timepoint(s) on {n} level(s)')
        result = {'levels': n}
    except Exception as e:
        log.log(f'>> [WARN] could not trim the store: {e}')
        result = {'error': str(e)}
    write_json_atomic(result_path, result)
    log.progress(1, 1)


def main():
    params = script_utils.script_params()
    if params is None:
        return
    run(params)


if __name__ == '__main__':
    main()
