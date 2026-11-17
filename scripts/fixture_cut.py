#!/usr/bin/env python3
"""Cut a committed test fixture from a real image, to meet a request in `test-data/NEEDS.toml`.

    pixi run fixture-cut -- --image <projects>/<project>/<n>/<uid> --need <id> [--max-bytes N]

Writes `test-data/projects/<FIXPRJ>/1/<uid>/`: a `ccid.json` keeping only what a fixture needs (meta,
channel names, the cell table entry; the image's name replaced by `fixture-<uid>`), the cell table
cut to a spatial window — the cells nearest the image's centre, so neighbourhoods stay whole — and
the gating sidecars of that segmentation. The window shrinks until the table is under the 1 MB
per-file cap. Nothing is committed: read what it wrote, register it in `test-data/fixtures.toml`
(the printed entry), mark the request `met: <uid>`, and commit it yourself — it is lab data.
Image and label stores are not cut here: crop those by hand (`test-data/README.md` → ZARRFMT).
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
from cecelia.utils.atomic_io import write_h5ad_atomic, write_json_atomic  # noqa: E402
from cecelia.utils.label_props_utils import LabelPropsView  # noqa: E402
from cecelia.utils.vn_versioning import versioned_active, versioned_get_field_at  # noqa: E402

CAP = 1024 * 1024
FIXTURE_PROJECT = "FIXPRJ"
#: ccid.json fields a fixture keeps; the rest (file paths, attributes, history) stays behind.
KEEP = ("class", "uid", "kind", "status", "imChannelNames", "meta")


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


def cut(image: pathlib.Path, need: str, *, test_data: pathlib.Path = _REPO / "test-data",
        max_bytes: int = CAP) -> dict:
    import fixture_requests as fr   # the judge step's property reader, so the entry says what it saw

    ccid = json.loads((image / "ccid.json").read_text(encoding="utf-8"))
    uid = ccid["uid"]
    vn = versioned_active(ccid["label_props"])
    src = image / "labelProps" / versioned_get_field_at(ccid, "label_props")
    out = test_data / "projects" / FIXTURE_PROJECT / "1" / uid
    if out.exists():
        raise SystemExit(f"{out} exists — a fixture with this uid is already cut")
    (out / "labelProps").mkdir(parents=True)
    n = cut_table(src, out / "labelProps" / f"{vn}.h5ad", max_bytes=max_bytes)
    for g in sorted((image / "gating").glob(f"{vn}*.json")):
        (out / "gating").mkdir(exist_ok=True)
        shutil.copy2(g, out / "gating" / g.name)
    kept = {k: ccid[k] for k in KEEP if k in ccid}
    kept.update(name=f"fixture-{uid}", label_props={vn: f"{vn}.h5ad", "_active": vn})
    write_json_atomic(str(out / "ccid.json"), kept, indent=2)
    proj = test_data / "projects" / FIXTURE_PROJECT / "project.json"
    if not proj.exists():
        write_json_atomic(str(proj), {"class": "CciaProject", "uid": FIXTURE_PROJECT,
                                      "name": "Fixtures cut from real projects", "set_uids": []}, indent=4)
    props = fr.image_properties(out) or {"properties": [], "technology": None}
    return {"uid": uid, "dir": str(out), "value_name": vn, "cells": n, "properties": props["properties"],
            "technology": props["technology"], "need": need}


def toml_entry(c: dict) -> str:
    return (f'[fixtures.{c["uid"]}]\ndir = "{FIXTURE_PROJECT}/1/{c["uid"]}"\nvalue_name = "{c["value_name"]}"\n'
            f'technology = "{c["technology"] or "unknown"}"\nproperties = {json.dumps(c["properties"])}\n'
            f'note = "Cut for NEEDS.toml → {c["need"]}: {c["cells"]} cells nearest the centre. Add what it is."\n')


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--image", type=pathlib.Path, required=True, help="the image dir holding ccid.json")
    ap.add_argument("--need", required=True, help="the NEEDS.toml request this meets")
    ap.add_argument("--max-bytes", type=int, default=CAP)
    args = ap.parse_args(argv)
    import fixture_requests as fr
    if args.need not in fr.open_requests():
        print(f"{args.need} is not an open request in test-data/NEEDS.toml", file=sys.stderr)
        return 2
    c = cut(args.image.expanduser(), args.need, max_bytes=args.max_bytes)
    print(f"wrote {c['dir']} ({c['cells']} cells). Read it, then add to test-data/fixtures.toml:\n")
    print(toml_entry(c))
    print(f'and in test-data/NEEDS.toml → {args.need}: status = "met: {c["uid"]}". Commit it yourself.')
    return 0


if __name__ == "__main__":
    sys.exit(main())
