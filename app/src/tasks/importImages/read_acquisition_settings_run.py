"""Read the acquisition settings a source file records but Bio-Formats drops.

Called by the `importImages.omezarr` task after the conversion, and by `resync_ome_meta!` for images
imported before this existed. Thin by design: one reader module per format, listed in
`acquisition_readers.json` beside this file; the logic and its tests live in the modules.

Params: `imPath` (the source file), `series` (which image of a multi-image file: the series picker's index, `meta.ori_series`;
0 when absent), `resultPath` (where to write the JSON result).
Result: the settings dict, or `{"reason": …}` when there are none.
"""
import importlib
import json
import os

import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic

_REGISTRY = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'acquisition_readers.json')


def _reader(ext):
    """The `acquisition_settings` of the module registered for `ext`, or None."""
    with open(_REGISTRY, encoding='utf-8') as f:
        rows = json.load(f)['readers']
    row = next((r for r in rows if r['ext'] == ext), None)
    return importlib.import_module(f'cecelia.utils.{row["module"]}').acquisition_settings if row else None


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = script_utils.get_param(params, 'imPath', default=None)
    result_path = script_utils.get_param(params, 'resultPath', default=None)
    series      = int(script_utils.get_param(params, 'series', default=0) or 0)
    if not im_path or not result_path:
        log.log('[ERROR] imPath and resultPath are required')
        return

    reader = _reader(os.path.splitext(im_path)[1].lower())
    result = reader(im_path, series) if reader else {'reason': 'no settings reader for this format'}
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
