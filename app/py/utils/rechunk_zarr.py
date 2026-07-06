"""Rechunk existing OME-ZARR corrections to per-plane chunks (one-off maintenance).

Corrections written before the plane-chunk fix (``zarr_utils.plane_chunks``) can be chunked across the
whole T/C axes (dask ``chunks='auto'`` → ~128 MB chunks). napari slices per (t,c,z), so a single plane
access then costs a full-timecourse read — slow on first open, fast once OS-cached. This rewrites each
multiscale level with per-plane chunks (1 along T/C/Z, ``xy_tile``-capped along Y/X). **Pixel data is
unchanged** — only the on-disk chunk layout differs.

Usage (run in the analysis env, e.g. ``pixi run python``):
    python app/py/utils/rechunk_zarr.py PATH [--replace] [--force] [--xy-tile 512]

    PATH       a single ``*.ome.zarr`` store, or a directory scanned recursively for corrected stores.
    --replace  swap the rechunked copy in place; the original is backed up to ``<name>.bak.ome.zarr``.
               Without it, a ``<name>.rechunked.ome.zarr`` is written and the original left untouched.
    --force    rechunk even if the store already looks per-plane-chunked.

Only FLAT corrected stores (numeric level arrays at the group root, i.e. our correction output) are
touched; bioformats2raw originals (a nested series group) are skipped — they're already pyramided.
"""
import argparse
import os
import shutil
import sys

import dask.array as da
import zarr

# app/ on the path so `py.utils.zarr_utils` imports whether run as a script or a module.
_APP_DIR = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
if _APP_DIR not in sys.path:
    sys.path.insert(0, _APP_DIR)
from py.utils.zarr_utils import plane_chunks   # noqa: E402


def _levels(group):
    """Numeric level arrays at the group root (our flat correction layout), sorted 0,1,2,…"""
    return sorted((k for k in group.array_keys() if str(k).isdigit()), key=int)


def needs_rechunk(arr, xy_tile=512):
    """True if any non-spatial (all but the last two) axis has a chunk > 1 — i.e. the chunk spans
    time/channel/z, the pattern that makes napari plane access read far more than one plane."""
    n = len(arr.shape)
    return any(c > 1 for i, c in enumerate(arr.chunks) if i < n - 2)


def rechunk_store(path, xy_tile=512, replace=False, force=False):
    """Rechunk one ``*.ome.zarr``. Returns (status, detail)."""
    try:
        src = zarr.open_group(path, mode="r")
    except Exception as e:
        return ("skip", f"not a zarr group ({e})")
    levels = _levels(src)
    if not levels:
        return ("skip", "no root-level arrays (bioformats2raw original or unknown layout)")
    if not force and not needs_rechunk(src[levels[0]], xy_tile):
        return ("ok", "already per-plane chunked")

    tmp = path.rstrip("/") + ".rechunk_tmp"
    if os.path.exists(tmp):
        shutil.rmtree(tmp)
    dst = zarr.open_group(tmp, mode="w", zarr_format=2)
    dst.attrs.update(dict(src.attrs))                         # multiscales metadata, verbatim
    for k in levels:
        s = src[k]
        ch = plane_chunks(s.shape, xy_tile=xy_tile)
        d = dst.create_array(k, shape=s.shape, chunks=ch, dtype=s.dtype)
        da.store(da.from_array(s, chunks=ch), d, lock=False)  # streams level→level, plane-chunked
    # copy any non-array members verbatim (e.g. an `OME/` metadata subgroup); levels + dotfiles handled
    for entry in os.listdir(path):
        if entry in levels or entry.startswith("."):
            continue
        dstp = os.path.join(tmp, entry)
        if os.path.exists(dstp):
            continue
        srcp = os.path.join(path, entry)
        (shutil.copytree if os.path.isdir(srcp) else shutil.copy2)(srcp, dstp)

    base = path.rstrip("/")
    base = base[: -len(".ome.zarr")] if base.endswith(".ome.zarr") else base
    if replace:
        bak = base + ".bak.ome.zarr"
        if os.path.exists(bak):
            shutil.rmtree(bak)
        os.rename(path, bak)          # keep the original as a backup
        os.rename(tmp, path)
        return ("rechunked", f"replaced in place (backup: {os.path.basename(bak)})")
    out = base + ".rechunked.ome.zarr"
    if os.path.exists(out):
        shutil.rmtree(out)
    os.rename(tmp, out)
    return ("rechunked", f"wrote {os.path.basename(out)} (original untouched)")


def _find_stores(root):
    """Yield every ``*.ome.zarr`` dir under `root` without descending into one."""
    if root.endswith(".ome.zarr"):
        yield root
        return
    for dirpath, dirnames, _ in os.walk(root):
        keep = []
        for d in dirnames:
            if d.endswith(".ome.zarr"):
                yield os.path.join(dirpath, d)
            elif not d.endswith((".bak.ome.zarr", ".rechunk_tmp")):
                keep.append(d)
        dirnames[:] = keep


def main(argv=None):
    ap = argparse.ArgumentParser(description="Rechunk OME-ZARR corrections to per-plane chunks.")
    ap.add_argument("path", help="a .ome.zarr store or a directory to scan")
    ap.add_argument("--replace", action="store_true", help="swap in place (backs up to *.bak.ome.zarr)")
    ap.add_argument("--force", action="store_true", help="rechunk even if already per-plane")
    ap.add_argument("--xy-tile", type=int, default=512, help="max chunk size along Y/X (default 512)")
    a = ap.parse_args(argv)

    stores = list(_find_stores(os.path.abspath(a.path)))
    if not stores:
        print(f"no .ome.zarr stores under {a.path}")
        return
    for s in stores:
        status, detail = rechunk_store(s, xy_tile=a.xy_tile, replace=a.replace, force=a.force)
        print(f"[{status:>9}] {s} — {detail}")


if __name__ == "__main__":
    main()
