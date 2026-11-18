#!/usr/bin/env python3
"""Cut a committed test fixture from a real image, to meet a request in `test-data/NEEDS.toml`.

    pixi run fixture-cut -- --image <projects>/<project>/<n>/<uid> --need <id> [--max-bytes N]

Writes `test-data/projects/<FIXPRJ>/1/<uid>/`: a `ccid.json` keeping only what a fixture needs (meta,
channel names, the cell table entry; the image's name replaced by `fixture-<uid>`), the cell table
cut to a spatial window — the cells nearest the image's centre, so neighbourhoods stay whole — and
the gating sidecars of that segmentation. The window shrinks until the table is under the 1 MB
per-file cap.

When the image has an image store (`<project>/0/<uid>/`) AND a label store for that segmentation, both
are cropped to one box instead — at most `XY` × `XY` px around the cells' median, the first `FRAMES`
frames, the first `CHANNELS` channels, `Z` planes around the median, one resolution level, the same
NGFF axes, scale and units — and the cell table keeps the cells whose centroid lies in the box, moved
to the box's origin. `--no-stores` cuts the table alone.

Nothing is committed: read what it wrote, register it in `test-data/fixtures.toml` (the printed
entry), mark the request `met: <uid>`, and commit it yourself — it is lab data.
"""
from __future__ import annotations

import argparse
import json
import pathlib
import shutil
import sys

import numpy as np

_REPO = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_REPO / "python"))
sys.path.insert(0, str(_REPO / "scripts" / "judge"))
from cecelia.utils import zarr_utils  # noqa: E402
from cecelia.utils.atomic_io import write_h5ad_atomic, write_json_atomic  # noqa: E402
from cecelia.utils.label_props_utils import LabelPropsView  # noqa: E402
from cecelia.utils.vn_versioning import versioned_active, versioned_get_field_at  # noqa: E402

CAP = 1024 * 1024
FIXTURE_PROJECT = "FIXPRJ"
#: ccid.json fields a fixture keeps; the rest (file paths, attributes, history) stays behind.
KEEP = ("class", "uid", "kind", "status", "imChannelNames", "meta")
#: the store crop's box (NEEDS.toml → cells-with-image-store budget)
XY, FRAMES, CHANNELS, Z = 64, 5, 4, 16
IMAGE_STORE = "ccidImage.ome.zarr"


def window(view: LabelPropsView, n: int) -> np.ndarray:
    """Labels of the `n` cells nearest the median x/y, pooled over frames: a disc of the field, so a
    cell's neighbours are kept with it."""
    df = view.only_centroid_cols().as_df()
    xy = df[["centroid_x", "centroid_y"]].to_numpy(dtype=float)
    d = np.hypot(*(xy - np.median(xy, axis=0)).T)
    keep = np.argsort(d, kind="stable")[:n]
    return df["label"].to_numpy()[np.sort(keep)] if "label" in df else view.labels()[np.sort(keep)]


def cut_table(src: pathlib.Path, dst: pathlib.Path, *, max_bytes: int = CAP) -> int:
    """Write the windowed table to `dst`, halving the window until it fits; returns the cell count."""
    view = LabelPropsView(str(src))
    try:
        n = view.adata.n_obs
        while True:
            labels = window(view, n)
            # creating a new .h5ad: a subset of the source AnnData, written by the canonical writer
            sub = view.adata[view.adata.obs.index.astype(np.int64).isin(labels)].copy()
            write_h5ad_atomic(sub, str(dst))
            if dst.stat().st_size <= max_bytes or n <= 1:
                return int(sub.n_obs)
            n //= 2
    finally:
        view.close()


def _stores(image: pathlib.Path, ccid: dict, vn: str) -> tuple[pathlib.Path, pathlib.Path] | None:
    """(image store, label store) of `vn`, or None when either is missing."""
    rel = versioned_get_field_at(ccid, "filepath") if "filepath" in ccid else None
    lab = versioned_get_field_at(ccid, "labels", vn) if "labels" in ccid else None
    if isinstance(lab, list):
        lab = lab[0] if lab else None
    if not rel or not lab:
        return None
    im = image.parent.parent / "0" / image.name / str(rel)
    lb = image / "labels" / str(lab)
    return (im, lb) if im.is_dir() and lb.is_dir() else None


def crop_box(view: LabelPropsView, axes: list[str], shape: tuple[int, ...], *, xy: int = XY,
             frames: int = FRAMES, channels: int = CHANNELS, z: int = Z) -> dict[str, tuple[int, int]]:
    """`axis → (start, stop)` in pixels/frames for every axis of the image store: `xy` square around
    the median centroid of the cells in the first `frames` frames, `z` planes around their median z."""
    df = view.only_centroid_cols().as_df()
    if "centroid_t" in df:
        df = df[df["centroid_t"] < frames]
    size = dict(zip(axes, shape))
    box = {}
    for ax in axes:
        n = size[ax]
        if ax == "t":
            box[ax] = (0, min(n, frames))
        elif ax == "c":
            box[ax] = (0, min(n, channels))
        else:
            w = min(n, z if ax == "z" else xy)
            col = f"centroid_{ax}"
            mid = float(np.median(df[col])) if col in df and len(df) else n / 2
            lo = int(np.clip(round(mid - w / 2), 0, n - w))
            box[ax] = (lo, lo + w)
    return box


def _write_store(src: pathlib.Path, dst: pathlib.Path, box: dict[str, tuple[int, int]], kind: str) -> list[str]:
    """Crop level 0 of `src` to `box` into a new store at `dst` with `src`'s axes, scale and units."""
    levels, _ = zarr_utils.open_as_zarr(str(src))
    lvl0 = levels[0]
    axes = zarr_utils.read_axes(str(src)) or []
    if len(axes) != lvl0.ndim:
        raise SystemExit(f"{src}: {lvl0.ndim}-d array but NGFF axes {axes} — crop it by hand")
    data = np.asarray(lvl0[tuple(slice(*box[a]) if a in box else slice(None) for a in axes)])
    src_scale = zarr_utils.read_scale(str(src))
    scale = dict(zip(axes, src_scale)) if src_scale else None       # none stated stays none stated
    units = zarr_utils.read_axis_units(str(src)) or None
    with zarr_utils.staged_store(str(dst)) as staging:
        zarr_utils.create_multiscales(data, staging, axes=[a.upper() for a in axes], nscales=1, kind=kind,
                                      reference_zarr=lvl0, im_chunks=tuple(data.shape))
        zarr_utils.set_ngff_axes(staging, axes, scale=scale, units=units)
    return axes


def cut_box_table(src: pathlib.Path, dst: pathlib.Path, box: dict[str, tuple[int, int]], *,
                  max_bytes: int = CAP) -> int:
    """The cells whose centroid lies in `box`, centroids moved to the box's origin; returns the count."""
    view = LabelPropsView(str(src))
    try:
        df = view.only_centroid_cols().as_df()
        inside = np.ones(len(df), dtype=bool)
        for ax, (lo, hi) in box.items():
            col = f"centroid_{ax}"
            if col in df:
                v = df[col].to_numpy(dtype=float)
                inside &= (v >= lo) & (v < hi)
        labels = df["label"].to_numpy()[inside] if "label" in df else view.labels()[inside]
        # creating a new .h5ad: a subset of the source AnnData, centroids moved, by the canonical writer
        sub = view.adata[view.adata.obs.index.astype(np.int64).isin(labels)].copy()
        for key, names in (("spatial", "spatial_cols"), ("temporal", "temporal_cols")):
            if key not in sub.obsm:
                continue
            m = np.array(sub.obsm[key], dtype=float)
            for k, name in enumerate(sub.uns[names]):
                ax = str(name).removeprefix("centroid_")
                if ax in box:
                    m[:, k] -= box[ax][0]
            sub.obsm[key] = m
        write_h5ad_atomic(sub, str(dst))
        if dst.stat().st_size > max_bytes:
            raise SystemExit(f"{dst}: {dst.stat().st_size} bytes over the {max_bytes} cap — pass a smaller box")
        return int(sub.n_obs)
    finally:
        view.close()


def cut(image: pathlib.Path, need: str, *, test_data: pathlib.Path = _REPO / "test-data",
        max_bytes: int = CAP, stores: bool = True, xy: int = XY) -> dict:
    import fixture_requests as fr   # the judge step's property reader, so the entry says what it saw

    ccid = json.loads((image / "ccid.json").read_text(encoding="utf-8"))
    uid = ccid["uid"]
    vn = versioned_active(ccid["label_props"])
    src = image / "labelProps" / versioned_get_field_at(ccid, "label_props")
    out = test_data / "projects" / FIXTURE_PROJECT / "1" / uid
    if out.exists():
        raise SystemExit(f"{out} exists — a fixture with this uid is already cut")
    (out / "labelProps").mkdir(parents=True)
    found = _stores(image, ccid, vn) if stores else None
    box = None
    if found is None:
        n = cut_table(src, out / "labelProps" / f"{vn}.h5ad", max_bytes=max_bytes)
    else:
        im, lb = found
        levels, _ = zarr_utils.open_as_zarr(str(im))
        view = LabelPropsView(str(src))
        try:
            box = crop_box(view, zarr_utils.read_axes(str(im)) or [], levels[0].shape, xy=xy)
        finally:
            view.close()
        zero = test_data / "projects" / FIXTURE_PROJECT / "0" / uid
        zero.mkdir(parents=True)
        _write_store(im, zero / IMAGE_STORE, box, "image")
        (out / "labels").mkdir()
        _write_store(lb, out / "labels" / f"{vn}.zarr", box, "labels")
        n = cut_box_table(src, out / "labelProps" / f"{vn}.h5ad", box, max_bytes=max_bytes)
    for g in sorted((image / "gating").glob(f"{vn}*.json")):
        (out / "gating").mkdir(exist_ok=True)
        shutil.copy2(g, out / "gating" / g.name)
    kept = {k: ccid[k] for k in KEEP if k in ccid}
    kept.update(name=f"fixture-{uid}", label_props={vn: f"{vn}.h5ad", "_active": vn})
    if box is not None:
        kept.update(filepath={"default": IMAGE_STORE, "_active": "default"}, labels={vn: f"{vn}.zarr"})
        meta = dict(kept.get("meta") or {})
        for ax, (lo, hi) in box.items():
            if f"Size{ax.upper()}" in meta:
                meta[f"Size{ax.upper()}"] = str(hi - lo)
        kept["meta"] = meta
        if "c" in box and "imChannelNames" in kept:      # the channels kept, in order
            kept["imChannelNames"] = {k: (v[:box["c"][1]] if isinstance(v, list) else v)
                                      for k, v in kept["imChannelNames"].items()}
    write_json_atomic(str(out / "ccid.json"), kept, indent=2)
    proj = test_data / "projects" / FIXTURE_PROJECT / "project.json"
    if not proj.exists():
        write_json_atomic(str(proj), {"class": "CciaProject", "uid": FIXTURE_PROJECT,
                                      "name": "Fixtures cut from real projects", "set_uids": []}, indent=4)
    props = fr.image_properties(out) or {"properties": [], "technology": None}
    return {"uid": uid, "dir": str(out), "value_name": vn, "cells": n, "properties": props["properties"],
            "technology": props["technology"], "need": need, "box": box}


def toml_entry(c: dict) -> str:
    image_dir = f'image_dir = "{FIXTURE_PROJECT}/0/{c["uid"]}"\n' if c.get("box") else ""
    where = ("in a " + " × ".join(f"{hi - lo} {ax}" for ax, (lo, hi) in c["box"].items()) + " crop"
             if c.get("box") else "nearest the centre")
    return (f'[fixtures.{c["uid"]}]\ndir = "{FIXTURE_PROJECT}/1/{c["uid"]}"\n{image_dir}value_name = "{c["value_name"]}"\n'
            f'technology = "{c["technology"] or "unknown"}"\nproperties = {json.dumps(c["properties"])}\n'
            f'note = "Cut for NEEDS.toml → {c["need"]}: {c["cells"]} cells {where}. Add what it is."\n')


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--image", type=pathlib.Path, required=True, help="the image dir holding ccid.json")
    ap.add_argument("--need", required=True, help="the NEEDS.toml request this meets")
    ap.add_argument("--max-bytes", type=int, default=CAP)
    ap.add_argument("--no-stores", action="store_true", help="cut the cell table only, even with stores")
    ap.add_argument("--xy", type=int, default=XY, help="the store crop's x/y size in pixels")
    args = ap.parse_args(argv)
    import fixture_requests as fr
    if args.need not in fr.open_requests():
        print(f"{args.need} is not an open request in test-data/NEEDS.toml", file=sys.stderr)
        return 2
    c = cut(args.image.expanduser(), args.need, max_bytes=args.max_bytes, stores=not args.no_stores, xy=args.xy)
    print(f"wrote {c['dir']} ({c['cells']} cells). Read it, then add to test-data/fixtures.toml:\n")
    print(toml_entry(c))
    print(f'and in test-data/NEEDS.toml → {args.need}: status = "met: {c["uid"]}". Commit it yourself.')
    return 0


if __name__ == "__main__":
    sys.exit(main())
