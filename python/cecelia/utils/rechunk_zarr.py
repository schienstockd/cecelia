"""Rechunk + recompress existing OME-ZARR corrections (one-off maintenance).

Two on-disk properties, both fixed by the same rewrite and neither touching a pixel value:

**Chunking.** Corrections written before the plane-chunk fix (``zarr_utils.plane_chunks``) can be
chunked across the whole T/C axes (dask ``chunks='auto'`` → ~128 MB chunks). napari slices per
(t,c,z), so a single plane access then costs a full-timecourse read — slow on first open, fast once
OS-cached. Each multiscale level is rewritten with per-plane chunks (1 along T/C/Z, ``xy_tile``-capped
along Y/X).

**Compressor.** Stores written before `zarr_utils.store_compressor` existed carry whichever codec the
library defaulted to at the time (`blosc/lz4-5` under zarr-python 2 and from bioformats2raw; plain
`zstd` since the zarr 3 migration). This is the migration path onto the canonical one — measured 33%
smaller than `blosc/lz4-5` on 16-bit acquisition data. **Pixel data is unchanged** either way; only
the layout and the encoding differ.

Usage (run in the analysis env, e.g. ``pixi run python``):
    python python/cecelia/utils/rechunk_zarr.py PATH [--replace] [--force] [--xy-tile 512]

    PATH       a single ``*.ome.zarr`` store, or a directory scanned recursively for corrected stores.
    --replace  swap the rechunked copy in place; the original is backed up to ``<name>.bak.ome.zarr``.
               Without it, a ``<name>.rechunked.ome.zarr`` is written and the original left untouched.
    --force    rewrite even if the store already looks per-plane-chunked on the canonical codec.

Only FLAT corrected stores (numeric level arrays at the group root, i.e. our correction output) are
touched; bioformats2raw originals (a nested series group) are skipped — they're already pyramided.
The sweep matches ``*.ome.zarr``, so LABEL stores (``labels/<name>.zarr``) are never picked up — hence
the ``kind='image'`` default; pass ``kind='labels'`` to `rechunk_store` for one by hand.

**Writes here do NOT use `zarr_utils.staged_store`, deliberately** — it is the canonical write-to-a-
staging-path-then-rename helper every TASK writer uses (see docs/SEGMENTATION.md →
*Stores are written staged, never in place*), and this script's two
temp-then-rename paths are similar but not the same: without `--replace` the rename target is a NEW
name (`*.rechunked.ome.zarr`), and with it the original is RETAINED as `*.bak.ome.zarr` where
`staged_store` deletes the superseded copy. Neither temp can be named by a `ccid.json`, so the
truncated-registered-store failure staging exists to prevent cannot happen here. Exempted by name in
`python/cecelia/tests/test_store_staging_convention.py`.
"""
import argparse
import os
import shutil
import sys

import dask.array as da
import zarr

# `cecelia.*` resolves via the editable install in the pixi env — no sys.path needed.
from cecelia.utils.zarr_utils import (plane_chunks, store_compressor, _codec_kwargs,
                                      _group_format, _group_separator)


def _levels(group):
    """Numeric level arrays at the group root (our flat correction layout), sorted 0,1,2,…"""
    return sorted((k for k in group.array_keys() if str(k).isdigit()), key=int)


def needs_rechunk(arr, xy_tile=512):
    """True if any non-spatial (all but the last two) axis has a chunk > 1 — i.e. the chunk spans
    time/channel/z, the pattern that makes napari plane access read far more than one plane."""
    n = len(arr.shape)
    return any(c > 1 for i, c in enumerate(arr.chunks) if i < n - 2)


def _codec_identity(cfg):
    """The part of a numcodecs config that decides the bytes on disk, normalised for comparison.

    ``zstd`` level 0 is the library's sentinel for its default level (3) — measured byte-identical
    output — so a store written at level 0 must NOT read as needing a rewrite to level 3. Without
    this, every store written since the zarr 3 migration would be swept for no gain."""
    cid = cfg.get('id')
    level = cfg.get('clevel', cfg.get('level', 0))
    if cid == 'zstd' and level == 0:
        level = 3
    return (cid, cfg.get('cname'), int(level), int(cfg.get('shuffle', 0)))


def needs_recompress(arr, kind='image'):
    """True if the array's compressor isn't the canonical one for its kind.

    Checked alongside `needs_rechunk` so an already-plane-chunked store still gets swept onto the
    canonical codec — otherwise the rewrite this module performs would skip exactly the stores that
    have the old compressor and correct chunking, which is most of them."""
    want = _codec_identity(store_compressor(kind).get_config())
    have = [_codec_identity(c.get_config()) for c in (arr.compressors or ())]
    return want not in have


def rechunk_store(path, xy_tile=512, replace=False, force=False, kind='image'):
    """Rechunk one ``*.ome.zarr``. Returns (status, detail).

    Also re-lands the store on the canonical compressor (`zarr_utils.store_compressor`), because it
    is rewriting every chunk anyway — which makes this the migration path for stores written before
    that choice existed (a `blosc/lz4-5` bioformats2raw original is ~33% larger than it needs to be).
    ``kind='labels'`` for a label store; the default suits the flat correction stores this targets."""
    try:
        src = zarr.open_group(path, mode="r")
    except Exception as e:
        return ("skip", f"not a zarr group ({e})")
    levels = _levels(src)
    if not levels:
        return ("skip", "no root-level arrays (bioformats2raw original or unknown layout)")
    rechunk    = needs_rechunk(src[levels[0]], xy_tile)
    recompress = needs_recompress(src[levels[0]], kind)
    if not force and not rechunk and not recompress:
        return ("ok", "already per-plane chunked, canonical codec")
    reasons = ", ".join(r for r, on in (("rechunk", rechunk), ("recompress", recompress)) if on) or "forced"

    tmp = path.rstrip("/") + ".rechunk_tmp"
    if os.path.exists(tmp):
        shutil.rmtree(tmp)
    # PRESERVE the source's zarr format. This rewrites an existing store, so hardcoding v2 here would
    # silently DOWNGRADE a v3 store to v2 while claiming only to have rechunked it — and `dst.attrs`
    # below copies the source attrs verbatim, so a v3 store's `ome`-nested metadata would land in a v2
    # container and read as having no multiscales at all. See docs/todo/ZARR_V3_PLAN.md D9.
    src_fmt = _group_format(src)
    src_sep = _group_separator(src)      # preserve the layout too, not just the format
    dst = zarr.open_group(tmp, mode="w", zarr_format=src_fmt)
    dst.attrs.update(dict(src.attrs))                         # multiscales metadata, verbatim
    for k in levels:
        s = src[k]
        ch = plane_chunks(s.shape, xy_tile=xy_tile)
        d = dst.create_array(k, shape=s.shape, chunks=ch, dtype=s.dtype,
                             **_codec_kwargs(kind, src_fmt, separator=src_sep))
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
        return ("rechunked", f"{reasons}; replaced in place (backup: {os.path.basename(bak)})")
    out = base + ".rechunked.ome.zarr"
    if os.path.exists(out):
        shutil.rmtree(out)
    os.rename(tmp, out)
    return ("rechunked", f"wrote {os.path.basename(out)} (original untouched)")


def _find_stores(root):
    """Yield every ``*.ome.zarr`` dir under `root` without descending into one. Backups
    (``*.bak.ome.zarr``) and temps (``*.rechunk_tmp``) are skipped — `--replace` leaves a backup
    behind, and a rescan must NOT then re-rechunk that backup (its own name also ends in .ome.zarr)."""
    root_norm = root.rstrip("/")
    if root_norm.endswith(".ome.zarr"):
        if not root_norm.endswith(".bak.ome.zarr"):           # explicit backup target → skip
            yield root
        return
    for dirpath, dirnames, _ in os.walk(root):
        keep = []
        for d in dirnames:
            if d.endswith((".bak.ome.zarr", ".rechunk_tmp")):
                continue                              # never yield OR descend into backups/temps
            if d.endswith(".ome.zarr"):
                yield os.path.join(dirpath, d)
            else:
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
