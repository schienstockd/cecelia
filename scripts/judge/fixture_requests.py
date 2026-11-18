#!/usr/bin/env python3
"""Weekly judge — which of your images could meet an open fixture request (`test-data/NEEDS.toml`).

A test that needs data no committed fixture has files a request instead of using a stand-in
(`docs/DEV.md` → *Ask for properties, not a path*). This step reads the open requests, looks at the
images in a projects dir, and lists the ones whose properties match — the same vocabulary as
`test-data/fixtures.toml`, derived from each image's `ccid.json` and cell table. Properties nothing on
disk can show (`structure`) are left for you to confirm by eye; every image here is `real`.

**Names stay local.** The pass's record is published (the status issue, PRs), so it carries only each
request's id and candidate count. The candidates themselves — project, image, cell count — are written
to `<state dir>/fixture-candidates.json` and printed by `pixi run judge-fixtures`. Cutting one is
`pixi run fixture-cut`, by you: it puts lab data into git.

The projects dir: `--projects-dir`, else `CECELIA_FIXTURE_SOURCE_PROJECTS`. Neither set → the step
reports the open requests and that no source is configured; it never guesses a dir.

Usage:
    pixi run judge-fixtures [--projects-dir D]     # print candidates; writes the local list
"""
from __future__ import annotations

import argparse
import json
import os
import pathlib
import re
import sys
import tomllib
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.state import state_dir  # noqa: E402
from cecelia.utils.atomic_io import write_json_atomic  # noqa: E402
from cecelia.utils.vn_versioning import versioned_active, versioned_get_field_at  # noqa: E402

ENV = "CECELIA_FIXTURE_SOURCE_PROJECTS"
#: Properties no file shows: a person confirms them on the candidate.
UNCHECKED = frozenset({"structure"})
#: Candidates listed per request, smallest cell table first (the cheapest to cut under the cap).
MAX_CANDIDATES = 5
#: 10+ channels is a multiplex panel (`MULTIPLEX_MIN` in frontend/src/utils/setRecipe.ts).
MULTIPLEX_MIN = 10
_PLACEHOLDER = re.compile(r"^(ch|c|channel)[ _-]?\d+$", re.I)


def source_dir(arg: pathlib.Path | None = None) -> pathlib.Path | None:
    p = arg or (pathlib.Path(os.environ[ENV]) if os.environ.get(ENV) else None)
    return p.expanduser() if p else None


def open_requests(test_data: pathlib.Path = _REPO / "test-data") -> dict[str, dict]:
    with open(test_data / "NEEDS.toml", "rb") as f:   # the tests' reader (cecelia.tests.fixture_registry) is
        needs = tomllib.load(f).get("needs", {})      # pinned to the repo's test-data; this one takes a root
    return {k: v for k, v in needs.items() if v.get("status") == "open"}


def _float(v) -> float | None:
    try:
        return float(v)
    except (TypeError, ValueError):
        return None


def image_properties(img_dir: pathlib.Path) -> dict | None:
    """The fixture properties of one image dir (`<project>/<n>/<uid>/ccid.json`), or None when it has no
    cell table. Reads the table through `LabelPropsView`, never raw HDF5."""
    from cecelia.utils.label_props_utils import LabelPropsView

    try:
        ccid = json.loads((img_dir / "ccid.json").read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    vn = versioned_active(ccid["label_props"]) if ccid.get("label_props") else None
    rel = versioned_get_field_at(ccid, "label_props")
    if not vn or not isinstance(rel, str) or not (table := img_dir / "labelProps" / rel).is_file():
        return None
    meta = ccid.get("meta") or {}
    chans = versioned_get_field_at(ccid, "imChannelNames") or []
    props = {"cells", "real"}
    try:
        view = LabelPropsView(str(table))
    except Exception:     # an unreadable table is not a candidate, and must not stop the pass
        return None
    try:
        obs = set(view.adata.obs.columns)
        n = int(view.adata.n_obs)
        if "centroid_z" in view.centroid_columns():
            props.add("3d")
        else:
            props.add("2d")
        tcols = view.temporal_columns()
        nt = view.only_centroid_cols().as_df()[tcols[0]].nunique() if tcols else 0
    finally:
        view.close()
    props.add("movie" if nt > 1 else "still")
    if "track_id" in obs:
        props.add("tracked")
    if (img_dir / "labelProps" / f"{vn}__tracks.h5ad").is_file():
        props.add("track-table")
    if any(c.startswith("regions.") for c in obs):
        props.add("regions")
    sx, sy = _float(meta.get("PhysicalSizeX")), _float(meta.get("PhysicalSizeY"))
    if sx and sy and sx != 1.0 and sy != 1.0:
        props.add("calibrated")
    if chans and not any(_PLACEHOLDER.match(str(c)) for c in chans):
        props.add("marker-names")
    if len(chans) >= MULTIPLEX_MIN:
        props.add("multiplex")
    if any((img_dir / "labels").glob("*.zarr")):
        props.add("label-store")
    if any(img_dir.parent.parent.glob(f"*/{img_dir.name}/ccidImage.ome.zarr")):   # as fixture_registry.jl
        props.add("image-store")
    if sum(1 for p in (img_dir / "labelProps").glob("*.h5ad") if "__" not in p.name) > 1:
        props.add("multi-segmentation")
    tech = (meta.get("acquisition") or {}).get("technology") if isinstance(meta.get("acquisition"), dict) else None
    return {"project": img_dir.parent.parent.name, "image": img_dir.name, "dir": str(img_dir),
            "value_name": vn, "cells": n, "technology": tech, "properties": sorted(props)}


def scan(projects: pathlib.Path | None) -> list[dict]:
    """Every image with a cell table under `projects` (`<project>/<n>/<uid>/ccid.json`)."""
    if projects is None or not projects.is_dir():
        return []
    out = []
    for ccid in sorted(projects.glob("*/*/*/ccid.json")):
        if (p := image_properties(ccid.parent)) is not None:
            out.append(p)
    return out


def match(requests: dict[str, dict], images: _t.Sequence[dict]) -> dict[str, list[dict]]:
    """Per open request, the images with every checkable property (and its technology, when it names
    one), smallest first."""
    out = {}
    for rid, r in requests.items():
        want = set(r["properties"]) - UNCHECKED
        tech = r.get("technology")
        hits = [im for im in images if want <= set(im["properties"]) and (tech is None or im["technology"] == tech)]
        out[rid] = sorted(hits, key=lambda im: im["cells"])[:MAX_CANDIDATES]
    return out


def summary(requests: dict[str, dict], matches: dict[str, list[dict]], *, configured: bool) -> dict:
    """What the published record may carry: ids, counts and the properties to confirm. No names."""
    return {"configured": configured,
            "requests": [{"id": rid, "candidates": len(matches.get(rid, [])),
                          "confirm": sorted(set(r["properties"]) & UNCHECKED), "synthetic": bool(r.get("synthetic"))}
                         for rid, r in sorted(requests.items())]}


def candidates_path() -> pathlib.Path:
    return state_dir() / "fixture-candidates.json"


def run(projects: pathlib.Path | None, *, write: bool = True, test_data: pathlib.Path = _REPO / "test-data") -> dict:
    """The pass's step: match, write the local candidate list, return the record-safe summary."""
    requests = open_requests(test_data)
    src = source_dir(projects)
    matches = match(requests, scan(src)) if src else {}
    if write and src:
        write_json_atomic(str(candidates_path()), {"source": str(src), "matches": matches}, indent=2)
    return summary(requests, matches, configured=src is not None)


def render(requests: dict[str, dict], matches: dict[str, list[dict]]) -> str:
    lines = []
    for rid, r in sorted(requests.items()):
        lines.append(f"{rid}: {r['why']}")
        hits = matches.get(rid, [])
        if not hits:
            lines.append("  no matching image")
        for im in hits:
            lines.append(f"  {im['project']}/{im['image']} — {im['cells']} cells, {im['technology'] or 'technology unknown'}")
            lines.append(f"    pixi run fixture-cut -- --image {im['dir']} --need {rid}")
        confirm = sorted(set(r["properties"]) & UNCHECKED)
        if hits and confirm:
            lines.append(f"  confirm by eye before cutting: {', '.join(confirm)}")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--projects-dir", type=pathlib.Path, help=f"default: ${ENV}")
    args = ap.parse_args(argv)
    src = source_dir(args.projects_dir)
    requests = open_requests()
    if src is None:
        print(f"No source projects: pass --projects-dir or set {ENV}.\nOpen requests: {', '.join(sorted(requests)) or 'none'}")
        return 0
    matches = match(requests, scan(src))
    write_json_atomic(str(candidates_path()), {"source": str(src), "matches": matches}, indent=2)
    print(render(requests, matches))
    return 0


if __name__ == "__main__":
    sys.exit(main())
