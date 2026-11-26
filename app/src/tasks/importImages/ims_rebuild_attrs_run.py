"""Rebuild the dense-attribute groups of an Imaris `.ims` WORKING COPY so Bio-Formats can read it.

Called by the `importImages.omezarr` task when bioformats2raw dies in netcdf-java's fractal-heap
reader, on the local copy it converts from — never on the user's source. Thin by design: the logic
and its tests live in `cecelia.utils.ims_relink` (`rebuild_dense_attrs`).

Params: `imPath` (the working copy).
"""
import cecelia.utils.script_utils as script_utils
from cecelia.utils.ims_relink import rebuild_dense_attrs


def run(params):
    log = script_utils.get_logfile_utils(params)
    im_path = script_utils.get_param(params, 'imPath', default=None)
    if not im_path:
        log.log('[ERROR] imPath is required')
        raise SystemExit(1)
    names = rebuild_dense_attrs(im_path, log=log.log)
    log.log(f'>> rebuilt {len(names)} group(s) with dense attribute storage')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
