"""
Convert existing label-props h5ad files to the explicit centroid-axis convention.

Walks `<root>/**/labelProps/*.h5ad` and normalises each cell-level file to
`centroid_x/_y/_z` + `centroid_t` (see `cecelia.utils.centroid_migrate.normalise_centroids` for the
handled prior formats). Idempotent. **Dry-run by default** — pass `--apply` to write. This is the
remedy for the strict read-time guard that rejects pre-migration `centroid-N` / `t` files
(docs/todo/CENTROID_AXES_PLAN.md).

    pixi run python -m cecelia.utils.convert_centroid_names <projects_dir>           # dry-run
    pixi run python -m cecelia.utils.convert_centroid_names <projects_dir> --apply   # write

Per-track feature tables (`*__tracks.h5ad`) carry no spatial/temporal obsm and are skipped.
"""
import argparse
import glob
import os
import sys

import anndata as ad

from cecelia.utils.centroid_migrate import normalise_centroids


def _labelprops_files(root):
    # {proj}/1/{img}/labelProps/{value_name}.h5ad — exclude the per-track tables (no centroids there)
    hits = glob.glob(os.path.join(root, "**", "labelProps", "*.h5ad"), recursive=True)
    return sorted(f for f in hits if not f.endswith("__tracks.h5ad"))


def convert_file(path, apply=False):
    """Normalise one file. Returns the list of changes (empty ⇒ nothing to do). Writes only if `apply`."""
    a = ad.read_h5ad(path)
    a, changes = normalise_centroids(a)
    if changes and apply:
        a.write_h5ad(path)
    return changes


def main(argv=None):
    ap = argparse.ArgumentParser(description="Convert label-props h5ad centroids to explicit axis names.")
    ap.add_argument("root", help="projects directory to scan (e.g. ~/cecelia-pineapple/projects)")
    ap.add_argument("--apply", action="store_true", help="write changes (default: dry-run, no writes)")
    args = ap.parse_args(argv)

    root = os.path.expanduser(args.root)
    files = _labelprops_files(root)
    print(f"Scanning {len(files)} label-props file(s) under {root}"
          f"{'' if args.apply else '  [DRY-RUN — no writes; pass --apply to write]'}")

    n_changed = 0
    for f in files:
        try:
            changes = convert_file(f, apply=args.apply)
        except Exception as e:                                   # noqa: BLE001 — report and continue
            print(f"  !! {f}: ERROR {e}")
            continue
        if changes:
            n_changed += 1
            verb = "converted" if args.apply else "would convert"
            print(f"  {verb}: {f}")
            for c in changes:
                print(f"      - {c}")

    print(f"Done. {n_changed}/{len(files)} file(s) {'converted' if args.apply else 'need conversion'}.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
