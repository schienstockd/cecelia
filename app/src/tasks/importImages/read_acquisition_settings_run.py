"""Read the acquisition settings an `.oir` or `.czi` source records but Bio-Formats drops.

Called by the `importImages.omezarr` task after the conversion, and by `resync_ome_meta!` for images
imported before this existed. Thin by design: the logic and its tests live in `cecelia.utils.oir_meta`
and `cecelia.utils.czi_meta`.

Params: `imPath` (the source file), `resultPath` (where to write the JSON result).
Result: the settings dict, or `{"reason": …}` when there are none.
"""
import os

import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils import czi_meta, oir_meta

#: Source formats with a settings reader. Others get a reason, not an error. Keep in step with
#: `ACQUISITION_SETTINGS_FORMATS` in `omezarr/acquisition.jl`, which gates the call.
_READERS = {'.oir': oir_meta.acquisition_settings, '.czi': czi_meta.acquisition_settings}


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = script_utils.get_param(params, 'imPath', default=None)
    result_path = script_utils.get_param(params, 'resultPath', default=None)
    if not im_path or not result_path:
        log.log('[ERROR] imPath and resultPath are required')
        return

    reader = _READERS.get(os.path.splitext(im_path)[1].lower())
    result = reader(im_path) if reader else {'reason': 'no settings reader for this format'}
    if 'reason' in result:
        log.log(f'>> no acquisition settings: {result["reason"]}')
    else:
        log.log(f'>> acquisition settings read: {result.get("system")} {result.get("softwareVersion")}')
    write_json_atomic(result_path, result)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
