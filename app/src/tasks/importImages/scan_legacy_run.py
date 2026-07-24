"""Read-only scan of a legacy R/Shiny cecelia project → a preview manifest (what will / won't
transfer, per image). Called by the Julia ``/api/import/scan-legacy`` handler; writes JSON to
``resultPath``. See ``legacy_migrate.scan_project`` and ``docs/todo/LEGACY_MIGRATION_PLAN.md``.

Parameter contract (JSON written by Julia):
  sourceProjectDir - the legacy project dir (…/projects/<PROJ>)
  resultPath       - where to write the manifest JSON
  rscript          - Rscript to use (optional; default "Rscript")
  imageUids        - optional list to restrict the scan
"""
import json

import cecelia.utils.script_utils as script_utils
from cecelia.utils.legacy_migrate import scan_project


def run(params):
    log = script_utils.get_logfile_utils(params)
    manifest = scan_project(
        params['sourceProjectDir'],
        rscript=params.get('rscript', 'Rscript'),
        uids=params.get('imageUids'),
    )
    with open(params['resultPath'], 'w') as f:
        json.dump(manifest, f)
    log.log(f"[INFO] Scanned {manifest.get('n_images', 0)} image(s) in {params['sourceProjectDir']}")


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
