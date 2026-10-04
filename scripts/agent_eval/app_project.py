"""A disposable project for an agent run against the RUNNING app (docs/todo/AGENT_OVERNIGHT_PLAN.md,
app tier): the run images of a source project copied as a fresh project, in one set, each holding
only its raw `default` version — no segmentation, gates, tracks or run log — so the agent starts
where a user would, and the source project is never written.

With `knowledge`, the source project's lab-knowledge entries (Blackboard entries a person marked as
knowledge, docs/todo/AGENT_RUN_REVIEW_PLAN.md P4) are copied in as well, and nothing else from its
Blackboard: no run records, no notes, no profile.

The copy lands in the app's projects dir (the running app lists projects by scanning it), under new
uids and a name that says what it is. `--source-project`/`--image` are read, never written.

    pixi run python scripts/agent_eval/app_project.py --projects-dir ~/cecelia-feijoa/projects \\
        --source-project tSJpBI --image yDfwP7 --image UJS0Hz --name "Agent run 2026-10-05" --out run.json [--knowledge]
"""
from __future__ import annotations

import argparse
import json
import pathlib
import secrets
import shutil
import string

from cecelia.utils import vn_versioning
from cecelia.utils.atomic_io import write_json_atomic

_ALPHABET = string.ascii_letters + string.digits
# the run's own provenance, not the source's: re-derived by whatever the agent runs
_DROP_META = ("funParams", "funParamsByName")


def new_uid(taken) -> str:
    while True:
        uid = "".join(secrets.choice(_ALPHABET) for _ in range(6))
        if uid not in taken:
            return uid


def raw_ccid(src: dict, uid: str, filename: str) -> dict:
    """The source image's ccid.json reduced to what an import leaves: identity, channels, physical
    metadata and the one raw `default` store."""
    names = vn_versioning.versioned_get_field_at(src, "imChannelNames", "default") or \
        vn_versioning.versioned_get_field_at(src, "imChannelNames")
    return {"uid": uid, "class": "CciaImage", "name": src.get("name", uid), "status": "done",
            "included": True, "starred": False, "note": "", "attr": {}, "branch_labels": {},
            "imChannelNames": {"default": list(names or []), "_active": "default"},
            "meta": {k: v for k, v in (src.get("meta") or {}).items() if k not in _DROP_META},
            "filepath": {"default": filename, "_active": "default"}, "labels": {}, "label_props": {}}


def knowledge_entries(src_root: pathlib.Path) -> list[tuple[pathlib.Path, dict]]:
    """The source project's Blackboard entries marked as lab knowledge: `(entry dir, meta)`."""
    out = []
    for d in sorted((src_root / "blackboard").glob("*/meta.json")):
        try:
            with open(d, encoding="utf-8") as f:
                meta = json.load(f)
        except (OSError, ValueError):
            continue
        if isinstance(meta.get("knowledge"), dict) and "agentRun" not in meta and (d.parent / "entry.md").exists():
            out.append((d.parent, meta))
    return out


def copy_knowledge(src_root: pathlib.Path, root: pathlib.Path) -> list[dict]:
    """Copy the knowledge entries' live text into `root`'s Blackboard as fresh entries: no history,
    no attachments (captures stay in the source), no link back to the run section a lesson came from."""
    copied, registry = [], {}
    for d, meta in knowledge_entries(src_root):
        eid = d.name
        (root / "blackboard" / eid).mkdir(parents=True)
        shutil.copyfile(d / "entry.md", root / "blackboard" / eid / "entry.md")
        kn = {k: v for k, v in meta["knowledge"].items() if k != "from"}
        new = {"entryId": eid, "title": meta.get("title", eid), "createdAt": meta.get("createdAt", ""),
               "updatedAt": meta.get("updatedAt", ""), "current": 0, "attachments": [], "snapshots": [],
               "status": "open", "knowledge": kn,
               **{k: meta[k] for k in ("createdBy", "updatedBy") if k in meta}}
        write_json_atomic(root / "blackboard" / eid / "meta.json", new, indent=2)
        registry[eid] = {"title": new["title"], "current": 0, "updatedAt": new["updatedAt"], "status": "open"}
        copied.append({"entryId": eid, "title": new["title"]})
    if registry:
        (root / "settings").mkdir(parents=True, exist_ok=True)
        write_json_atomic(root / "settings" / "blackboard.json", registry, indent=2)
    return copied


def build(projects_dir: pathlib.Path, source_project: str, image_uids: list[str], name: str,
          knowledge: bool = False) -> dict:
    """Copy `image_uids` (in order) into one set of a new project. `images` maps each copy to its
    source image, which is how a run's record names images after the copy is deleted. `knowledge`
    lists the lab-knowledge entries carried over (always present; empty without `knowledge`)."""
    src_root = projects_dir / source_project
    srcs = []
    for image_uid in image_uids:
        with open(src_root / "1" / image_uid / "ccid.json", encoding="utf-8") as f:
            src = json.load(f)
        filename = vn_versioning.versioned_get_field_at(src, "filepath", "default")
        if not filename:
            raise SystemExit(f"{source_project}/{image_uid} has no `default` image version")
        srcs.append((image_uid, src, filename))
    taken = {p.name for p in projects_dir.iterdir()}
    proj_uid = new_uid(taken)
    taken.add(proj_uid)
    set_uid = new_uid(taken)
    taken.add(set_uid)
    root = projects_dir / proj_uid
    images = []
    for image_uid, src, filename in srcs:
        img_uid = new_uid(taken)
        taken.add(img_uid)
        shutil.copytree(src_root / "0" / image_uid / filename, root / "0" / img_uid / filename)
        (root / "1" / img_uid).mkdir(parents=True)
        write_json_atomic(root / "1" / img_uid / "ccid.json", raw_ccid(src, img_uid, filename), indent=4)
        images.append({"imageUid": img_uid, "imageName": src.get("name", img_uid),
                       "sourceImageUid": image_uid})
    (root / "1" / set_uid).mkdir(parents=True)
    write_json_atomic(root / "1" / set_uid / "ccid.json",
                      {"uid": set_uid, "class": "CciaSet", "name": "images",
                       "image_uids": [im["imageUid"] for im in images], "meta": {}}, indent=4)
    source = {"projectUid": source_project, "imageUids": list(image_uids)}
    write_json_atomic(root / "project.json",
                      {"uid": proj_uid, "name": name, "set_uids": [set_uid], "owners": ["default"],
                       "meta": {"agentEvalSource": source}}, indent=4)
    carried = copy_knowledge(src_root, root) if knowledge else []
    return {"projectUid": proj_uid, "projectDir": str(root), "setUid": set_uid, "images": images,
            "source": source, "knowledge": carried}


def copy_images(info: dict) -> list[dict]:
    """`images` of a run.json, including the one-image shape written before 2026-10-05
    (`imageUid` / `imageName` / `source.imageUid`)."""
    if "images" in info:
        return info["images"]
    return [{"imageUid": info["imageUid"], "imageName": info.get("imageName", info["imageUid"]),
             "sourceImageUid": info["source"]["imageUid"]}]


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--projects-dir", required=True)
    ap.add_argument("--source-project", required=True)
    ap.add_argument("--image", required=True, action="append", help="repeat for each run image")
    ap.add_argument("--name", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--knowledge", action="store_true", help="carry the source's lab-knowledge entries")
    a = ap.parse_args(argv)
    info = build(pathlib.Path(a.projects_dir).expanduser(), a.source_project, a.image, a.name, a.knowledge)
    write_json_atomic(a.out, info, indent=2)
    print(json.dumps(info))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
