"""A doc fixture's rebuilt gates take over their source gates' tracks.

Each tracked cell names the population that tracked it (`track_source`, a population uid), and the
viewer draws a population's cell-track ribbons only from cells it authored
(`_pop_has_authored_tracks`, app/src/gating/popmanager/allow_list.jl). A fixture copy keeps the
source's cell table but recreates its gates under new uids, so no population on the copy owns a track
and no ribbon is drawn. This rewrites each source uid to the uid of the copy's population at the same
path. Cells tracked by the whole segmentation (or by a population the copy has no gate for) keep their
marker. Idempotent: a second run finds no source uid left to rewrite.

    python scripts/docshots/adopt_tracks.py --source <source image dir> --copy <copy image dir> --value-name Tcells
"""
from __future__ import annotations

import argparse
import json
import pathlib

from cecelia.utils.label_props_utils import LabelPropsView


def pop_uids(gating_file: pathlib.Path) -> dict[str, str]:
    """Population path (`/qc/OTI`) → uid, over the whole tree of one gating sidecar."""
    out: dict[str, str] = {}

    def walk(pops, parent):
        for p in pops or []:
            path = f"{parent}/{p['name']}"
            if p.get("uid"):
                out[path] = p["uid"]
            walk(p.get("children"), path)

    walk(json.loads(gating_file.read_text(encoding="utf-8")).get("populations"), "")
    return out


def uid_map(source: dict[str, str], copy: dict[str, str]) -> dict[str, str]:
    """Source uid → copy uid, for every path both trees have."""
    return {uid: copy[path] for path, uid in source.items() if path in copy and copy[path] != uid}


def adopt(source_dir: pathlib.Path, copy_dir: pathlib.Path, value_name: str) -> int:
    """Rewrite the copy's `track_source`; returns how many cells changed hands."""
    sidecar = pathlib.Path("gating") / f"{value_name}.json"
    if not (source_dir / sidecar).exists() or not (copy_dir / sidecar).exists():
        return 0
    remap = uid_map(pop_uids(source_dir / sidecar), pop_uids(copy_dir / sidecar))
    h5ad = copy_dir / "labelProps" / f"{value_name}.h5ad"
    if not remap or not h5ad.exists():
        return 0
    view = LabelPropsView(str(h5ad))
    df = view.as_df()
    if "track_source" not in df.columns:
        return 0
    old = [s if isinstance(s, str) else None for s in df["track_source"].astype(object)]
    new = [remap.get(s, s) if s is not None else None for s in old]
    changed = sum(a != b for a, b in zip(old, new))
    if changed:
        view.add_categorical_obs("track_source", df["label"].to_numpy(), new).save()
    return changed


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--source", required=True, help="the source image's directory (read only)")
    ap.add_argument("--copy", required=True, help="the copy's image directory")
    ap.add_argument("--value-name", required=True, help="the segmentation")
    a = ap.parse_args(argv)
    print(adopt(pathlib.Path(a.source), pathlib.Path(a.copy), a.value_name))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
