"""Migrate ONE legacy image (data + segmentation + tracking) into the new Pineapple layout. Called by
the Julia ``importImages.migrateLegacy`` task handler, once per image. Read-only on the source; writes
only under the new object dirs. Writes the image's ccid field dict to ``resultPath`` for Julia to
apply. See ``legacy_migrate.migrate_image`` and ``docs/todo/LEGACY_MIGRATION_PLAN.md``.

Parameter contract (JSON written by Julia):
  sourceProjectDir - the legacy project dir (…/projects/<PROJ>)
  sourceUid        - the legacy image UID to migrate
  zeroDir          - new image-data dir   ({proj}/0/{uid})
  metaDir          - new metadata dir     ({proj}/1/{uid})
  resultPath       - where to write the ccid field dict JSON
  mode             - "copy" (default) or "symlink" for the zarr
  rscript          - Rscript to use (optional; default "Rscript")
"""
import json

import cecelia.utils.script_utils as script_utils
from cecelia.utils.legacy_migrate import migrate_image


def run(params):
    # print → stdout so run_py routes [PROGRESS] to on_progress and the rest to on_log
    fields = migrate_image(
        params['sourceProjectDir'], params['sourceUid'],
        params['zeroDir'], params['metaDir'],
        mode=params.get('mode', 'copy'),
        rscript=params.get('rscript', 'Rscript'),
        log=lambda m: print(m, flush=True),
    )
    with open(params['resultPath'], 'w') as f:
        json.dump(fields, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
