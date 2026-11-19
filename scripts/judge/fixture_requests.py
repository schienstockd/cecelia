#!/usr/bin/env python3
"""Weekly judge — which of your images could meet an open fixture request (`test-data/NEEDS.toml`).

A test that needs data no committed fixture has files a request instead of using a stand-in
(`docs/DEV.md` → *Ask for properties, not a path*). This step reads the open requests, looks at the
images in a projects dir, and lists the ones whose properties match — the same vocabulary as
`test-data/fixtures.toml`, derived from each image's `ccid.json` and cell table. Properties nothing on
disk can show (`structure`) are left for you to confirm by eye; every image here is `real`.

**Names stay local.** The pass's record is published (the status issue, PRs), so it carries only each
request's id, candidate count and whether a cut is ready. The candidates themselves — project, image,
cell count — are written to `<state dir>/fixture-candidates.json` and printed by
`pixi run judge-fixtures`.

**Cuts are staged, never committed.** For each open real-data request with a candidate, the pass cuts
the first candidate that cuts cleanly (`scripts/fixture_cut.py`) into `<state dir>/fixture-cuts/<id>/`
— outside the repo — and leaves it there until you look at it. `pixi run fixture-accept -- <id>` moves
it into `test-data/`, registers it and marks the request met; committing it is yours, because the repo
is public and the cut is lab data. A request that is already staged is not cut again; one that needs a
property no file shows (`structure`) is still cut, and accepting it asks you to confirm that property.

The projects dir: `--projects-dir`, else `CECELIA_FIXTURE_SOURCE_PROJECTS`, else `[dirs] projects` in
this checkout's Cecelia config (`custom.toml` in the dir `.env` → `CECELIA_DEV_DIR` names — the judge's
worktree has the same `.env`). None of those → the step reports the open requests and that no source
is configured; it never guesses a dir.

Usage:
    pixi run judge-fixtures [--projects-dir D] [--cut]   # print candidates and staged cuts; --cut stages
"""

from __future__ import annotations

import argparse
import json
import os
import pathlib
import re
import shutil
import sys
import tomllib
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.state import state_dir  # noqa: E402
from cecelia.utils.atomic_io import write_json_atomic  # noqa: E402
from cecelia.utils.loopback import config_dir  # noqa: E402
from cecelia.utils.vn_versioning import versioned_active, versioned_get_field_at  # noqa: E402

ENV = "CECELIA_FIXTURE_SOURCE_PROJECTS"
#: Properties no file shows: a person confirms them on the candidate.
UNCHECKED = frozenset({"structure"})
#: Candidates listed per request, smallest cell table first (the cheapest to cut under the cap).
MAX_CANDIDATES = 5
#: 10+ channels is a multiplex panel (`MULTIPLEX_MIN` in frontend/src/utils/setRecipe.ts).
MULTIPLEX_MIN = 10
#: A request naming one of these gets the image and label stores cropped; any other, the cell table alone.
STORE_PROPERTIES = frozenset({"image-store", "label-store"})
_PLACEHOLDER = re.compile(r"^(ch|c|channel)[ _-]?\d+$", re.I)


def configured_projects(root: pathlib.Path = _REPO) -> pathlib.Path | None:
    """`[dirs] projects` in the Cecelia config `root`'s `.env` points at (`custom.toml`), or None."""
    try:
        with open(pathlib.Path(config_dir(root)) / "custom.toml", "rb") as f:
            p = tomllib.load(f).get("dirs", {}).get("projects")
    except (OSError, tomllib.TOMLDecodeError):
        return None
    return pathlib.Path(p) if isinstance(p, str) and p.strip() else None


def source_dir(arg: pathlib.Path | None = None) -> pathlib.Path | None:
    p = arg or (pathlib.Path(os.environ[ENV]) if os.environ.get(ENV) else None) or configured_projects()
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


def match(requests: dict[str, dict], images: _t.Sequence[dict],
          limit: int | None = MAX_CANDIDATES) -> dict[str, list[dict]]:
    """Per open request, the images with every checkable property (and its technology, when it names
    one), smallest first; the first `limit` of them (None: all)."""
    out = {}
    for rid, r in requests.items():
        want = set(r["properties"]) - UNCHECKED
        tech = r.get("technology")
        hits = [im for im in images if want <= set(im["properties"]) and (tech is None or im["technology"] == tech)]
        out[rid] = sorted(hits, key=lambda im: im["cells"])[:limit]
    return out


def summary(requests: dict[str, dict], matches: dict[str, list[dict]], *, configured: bool,
            cuts: dict[str, str] | None = None) -> dict:
    """What the published record may carry: ids, counts, the properties to confirm and whether a cut is
    staged (`ready` / `failed`). No names, no paths."""
    cuts = cuts or {}
    return {"configured": configured, "cuts_ready": sum(1 for v in cuts.values() if v == "ready"),
            "requests": [{"id": rid, "candidates": len(matches.get(rid, [])),   # the listed few, at most
                          "confirm": sorted(set(r["properties"]) & UNCHECKED), "synthetic": bool(r.get("synthetic")),
                          **({"cut": cuts[rid]} if rid in cuts else {})}
                         for rid, r in sorted(requests.items())]}


def candidates_path() -> pathlib.Path:
    return state_dir() / "fixture-candidates.json"


def cuts_dir() -> pathlib.Path:
    """Where staged cuts wait for review: `<id>/projects/FX<uid>/…` plus `<id>/cut.json`. Not the repo."""
    return state_dir() / "fixture-cuts"


def staged(rid: str, root: pathlib.Path | None = None) -> dict | None:
    """The staged cut for request `rid` (its `cut.json`), or None."""
    try:
        return json.loads(((root or cuts_dir()) / rid / "cut.json").read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None


def _fixture_cut():
    import importlib.util
    if str(_REPO / "scripts" / "judge") not in sys.path:    # fixture_cut imports this module by name
        sys.path.insert(0, str(_REPO / "scripts" / "judge"))
    spec = importlib.util.spec_from_file_location("fixture_cut", _REPO / "scripts" / "fixture_cut.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def stage_cuts(requests: dict[str, dict], matches: dict[str, list[dict]],
               root: pathlib.Path | None = None) -> dict[str, str]:
    """Cut a candidate for every open real-data request into `root/<id>/` (default `cuts_dir()`), the
    candidates in `match` order until one cuts cleanly (under the cap, every checkable property kept;
    the stores cropped only when the request names one). A request already staged keeps its cut; a
    leftover from a broken cut is cleared first. Returns `id → "ready" | "failed"` for every request
    with a candidate or a staged cut; a failure's reasons go to `<id>/failed.json`, never the record."""
    root = root or cuts_dir()
    fc = None
    out: dict[str, str] = {}
    for rid, r in sorted(requests.items()):
        if r.get("synthetic"):
            continue                          # an agent builds those; real data never stands in
        if staged(rid, root) is not None:
            out[rid] = "ready"
            continue
        hits = matches.get(rid) or []
        if not hits:
            continue
        fc = fc or _fixture_cut()
        d = root / rid
        want = set(r["properties"]) - UNCHECKED
        # a store crop keeps a few channels and a small box: only for a request that asks for a store
        stores = bool(want & STORE_PROPERTIES)
        errors = []
        for im in hits:
            shutil.rmtree(d, ignore_errors=True)
            try:
                c = fc.cut(pathlib.Path(im["dir"]), rid, test_data=d, stores=stores)
            except (Exception, SystemExit) as e:      # one bad image must not stop the pass
                errors.append({"image": im["dir"], "error": f"{type(e).__name__}: {e}"})
                continue
            if lost := sorted(want - set(c["properties"])):     # the crop cut away what was asked for
                errors.append({"image": im["dir"], "error": f"the cut has no {', '.join(lost)}"})
                continue
            confirm = sorted(set(r["properties"]) & UNCHECKED)
            write_json_atomic(str(d / "cut.json"), {**c, "source": im["dir"], "source_project": im["project"],
                                                    "confirm": confirm, "entry": fc.toml_entry(c)}, indent=2)
            out[rid] = "ready"
            break
        else:
            shutil.rmtree(d, ignore_errors=True)
            d.mkdir(parents=True, exist_ok=True)
            write_json_atomic(str(d / "failed.json"), {"errors": errors}, indent=2)
            out[rid] = "failed"
    return out


def run(projects: pathlib.Path | None, *, write: bool = True, test_data: pathlib.Path = _REPO / "test-data",
        cut: bool | None = None) -> dict:
    """The pass's step: match, write the local candidate list, stage cuts (`cut`, default = `write`),
    return the record-safe summary."""
    requests = open_requests(test_data)
    src = source_dir(projects)
    every = match(requests, scan(src), limit=None) if src else {}
    matches = {rid: hits[:MAX_CANDIDATES] for rid, hits in every.items()}
    cuts = {}
    if write and src:
        write_json_atomic(str(candidates_path()), {"source": str(src), "matches": matches}, indent=2)
    if (write if cut is None else cut) and src:
        cuts = stage_cuts(requests, every)     # past the listed few: the smallest may not cut cleanly
    return summary(requests, matches, configured=src is not None, cuts=cuts)


def render(requests: dict[str, dict], matches: dict[str, list[dict]], root: pathlib.Path | None = None) -> str:
    lines = []
    for rid, r in sorted(requests.items()):
        lines.append(f"{rid}: {r['why']}")
        confirm = sorted(set(r["properties"]) & UNCHECKED)
        c = staged(rid, root)
        if c is not None:
            flag = "".join(f" --confirm {p}" for p in c.get("confirm", []))
            lines.append(f"  CUT READY: {c['cells']} cells from {c['source_project']}/{c['uid']} — {c['dir']}")
            if c.get("confirm"):
                lines.append(f"    check by eye that it has: {', '.join(c['confirm'])}")
            lines.append(f"    review it, then: pixi run fixture-accept -- {rid}{flag}")
            lines.append(f"    (or discard it: pixi run fixture-accept -- {rid} --discard)")
        hits = matches.get(rid, [])
        if not hits:
            lines.append("  no matching image")
        for im in hits:
            lines.append(f"  {im['project']}/{im['image']} — {im['cells']} cells, {im['technology'] or 'technology unknown'}")
            if c is None:
                lines.append(f"    pixi run fixture-cut -- --image {im['dir']} --need {rid}")
        if hits and confirm and c is None:
            lines.append(f"  confirm by eye before cutting: {', '.join(confirm)}")
    return "\n".join(lines)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--projects-dir", type=pathlib.Path,
                    help=f"default: ${ENV}, else [dirs] projects in the Cecelia config")
    ap.add_argument("--cut", action="store_true", help="stage a cut for each request with a candidate, as the pass does")
    args = ap.parse_args(argv)
    src = source_dir(args.projects_dir)
    requests = open_requests()
    if src is None:
        print(f"No source projects: pass --projects-dir, set {ENV}, or set [dirs] projects in custom.toml."
              f"\nOpen requests: {', '.join(sorted(requests)) or 'none'}")
        return 0
    every = match(requests, scan(src), limit=None)
    matches = {rid: hits[:MAX_CANDIDATES] for rid, hits in every.items()}
    write_json_atomic(str(candidates_path()), {"source": str(src), "matches": matches}, indent=2)
    if args.cut:
        stage_cuts(requests, every)
    print(render(requests, matches))
    return 0


if __name__ == "__main__":
    sys.exit(main())
