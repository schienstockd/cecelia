#!/usr/bin/env python3
"""Fetch cellpose custom models from schienstockd/ceceliaModels into <repo>/models/.

Mirrors what install.sh / install.ps1 do at install time — but usable outside the installer,
so a developer can `pixi run models-fetch` after cloning (or re-fetch when the upstream set
changes). Cross-platform (stdlib only: urllib + zipfile).

Only `cellposeModels/` is installed; `btrackModels/` is deliberately skipped because the
pineapple btrack config is vendored beside its runner (`app/src/tasks/tracking/cell_config.json`).

The resolver on the app side is `cellpose_model_path(name)` in `app/src/config.jl` — it checks
`<repo>/models/cellposeModels/` first (this bundle) and `<config_dir>/models/cellposeModels/`
second (user override slot).
"""
import argparse
import os
import shutil
import sys
import tempfile
import urllib.request
import zipfile


DEFAULT_REF = "master"
UPSTREAM = "schienstockd/ceceliaModels"


def _repo_root() -> str:
    return os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def fetch(dest_root: str, ref: str) -> str:
    """Download and unpack `cellposeModels/` into `<dest_root>/cellposeModels/`.
    Returns the destination path. Idempotent — an existing directory is replaced."""
    url = f"https://github.com/{UPSTREAM}/archive/refs/heads/{ref}.zip"
    print(f"Fetching {url}…", flush=True)
    with tempfile.TemporaryDirectory() as tmp:
        zip_path = os.path.join(tmp, "models.zip")
        with urllib.request.urlopen(url) as resp, open(zip_path, "wb") as f:
            shutil.copyfileobj(resp, f)
        with zipfile.ZipFile(zip_path) as z:
            z.extractall(tmp)
        # Extracted root is `ceceliaModels-<ref>/`; the ref becomes the folder suffix.
        roots = [d for d in os.listdir(tmp)
                 if d.startswith("ceceliaModels-") and os.path.isdir(os.path.join(tmp, d))]
        if not roots:
            print(f"[ERROR] no ceceliaModels-* directory in the zip payload", file=sys.stderr)
            sys.exit(1)
        src = os.path.join(tmp, roots[0], "cellposeModels")
        if not os.path.isdir(src):
            print(f"[ERROR] {src} missing in the zip payload — upstream layout changed?",
                  file=sys.stderr)
            sys.exit(1)
        os.makedirs(dest_root, exist_ok=True)
        dst = os.path.join(dest_root, "cellposeModels")
        if os.path.isdir(dst):
            shutil.rmtree(dst)
        shutil.move(src, dst)
        return dst


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--dest", default=None,
                    help="Target models/ directory (default: <repo>/models). Set to your install "
                         "root's `models/` to match `cellpose_model_path`'s bundled slot.")
    ap.add_argument("--ref", default=DEFAULT_REF,
                    help=f"Branch/tag of {UPSTREAM} to fetch (default: {DEFAULT_REF})")
    args = ap.parse_args()

    dest = args.dest or os.path.join(_repo_root(), "models")
    installed = fetch(dest, args.ref)
    print(f"Installed cellpose models → {installed}")


if __name__ == "__main__":
    main()
