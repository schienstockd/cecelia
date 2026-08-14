"""Recover the frame interval from an Imaris `.ims` source.

Bio-Formats' Imaris reader exposes no `TimeIncrement` and no per-plane `DeltaT` (it puts the timing in
the unstructured original-metadata table instead), so a converted timelapse arrives with no interval
at all. This reads it from the source file. Called by the `importImages.omezarr` task after the
conversion, and by `resync_ome_meta!` for images imported before this existed.

Thin by design — the logic and its tests live in `cecelia.utils.ims_meta`.

Params: `imPath` (the .ims), `resultPath` (where to write the JSON result).
"""
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils.ims_meta import time_increment


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = script_utils.get_param(params, 'imPath', default=None)
    result_path = script_utils.get_param(params, 'resultPath', default=None)
    if not im_path or not result_path:
        log.log('[ERROR] imPath and resultPath are required')
        return

    result = time_increment(im_path)
    if 'TimeIncrement' in result:
        log.log(f'>> frame interval {result["TimeIncrement"]} s from {result["source"]}')
    else:
        log.log(f'>> no frame interval: {result.get("reason", "unknown")}')
    write_json_atomic(result_path, result)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
