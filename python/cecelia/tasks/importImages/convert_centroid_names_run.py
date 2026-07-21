"""run_py entry for the centroid-axis data patch (the Settings → Data patches runner; registered as
`centroid-axes` in app/src/maintenance.jl). Reads {root, apply} from the params JSON, converts every
labelProps h5ad under the project `root`, and streams `[PROGRESS] n/total` + per-file log lines so the
run shows live in the UI. The conversion itself is `convert_centroid_names.convert_file`."""
import cecelia.utils.script_utils as script_utils
from cecelia.tasks.importImages.convert_centroid_names import _labelprops_files, convert_file


def run(params):
    root = script_utils.get_param(params, "root", default=None)
    apply = bool(script_utils.get_param(params, "apply", default=False))
    if not root:
        print("[ERROR] no root project directory in params", flush=True)
        return

    files = _labelprops_files(root)
    total = len(files)
    print(f"Scanning {total} label-props file(s) in this project"
          + ("" if apply else "  [DRY-RUN — no writes; use Apply to write]"), flush=True)
    print(f"[PROGRESS] 0/{max(total, 1)}", flush=True)

    n_changed = 0
    for i, f in enumerate(files):
        try:
            changes = convert_file(f, apply=apply)
        except Exception as e:                          # report and continue — one bad file shouldn't abort
            print(f"  !! {f}: ERROR {e}", flush=True)
            print(f"[PROGRESS] {i + 1}/{max(total, 1)}", flush=True)
            continue
        if changes:
            n_changed += 1
            print(("  converted: " if apply else "  would convert: ") + f, flush=True)
            for c in changes:
                print(f"      - {c}", flush=True)
        print(f"[PROGRESS] {i + 1}/{max(total, 1)}", flush=True)

    print(f"Done. {n_changed}/{total} file(s) "
          + ("converted." if apply else "need conversion."), flush=True)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
