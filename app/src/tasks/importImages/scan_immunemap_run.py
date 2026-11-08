"""Read-only preview of an Immunemap experiment: its conditions and videos, with sizes, calibration,
channel labels and which videos are public. Called by the Julia ``/api/import/scan-immunemap``
handler; writes the manifest JSON to ``resultPath``. Logic and tests: ``cecelia.utils.immunemap``.

Params: ``experiment`` (id or experiment-public-view URL), ``resultPath``.
"""
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic
from cecelia.utils.immunemap import experiment_manifest


def run(params):
    log = script_utils.get_logfile_utils(params)
    manifest = experiment_manifest(params['experiment'])
    write_json_atomic(params['resultPath'], manifest)
    t = manifest['totals']
    log.log(f"[INFO] Experiment {manifest['experiment']['id']}: {t['importable']}/{t['videos']} "
            f"video(s) importable, {t['bytes'] / 1e9:.1f} GB")


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
