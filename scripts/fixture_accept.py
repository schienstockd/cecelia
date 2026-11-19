#!/usr/bin/env python3
"""Accept a cut the weekly judge staged for a fixture request: move it into `test-data/`.

    pixi run fixture-accept -- <request id> [--confirm structure ...]
    pixi run fixture-accept -- <request id> --discard

The judge cuts a candidate for each open request into `<state dir>/fixture-cuts/<id>/` (outside the
repo; `pixi run judge-fixtures` lists them). Look at it first — it is lab data and the repo is public.
Accepting it:

- copies `projects/FX<uid>/` into `test-data/projects/`,
- appends its entry to `test-data/fixtures.toml` (with the properties you `--confirm`ed, which no file
  shows: a request that names `structure` is refused until you confirm it),
- marks the request `status = "met: <uid>"` in `test-data/NEEDS.toml`,
- and clears the staged cut.

It commits nothing: read `git status` / `git diff`, fill in the entry's note, and commit it yourself.
`--discard` drops the staged cut instead; the next pass cuts the next candidate that matches.
"""
from __future__ import annotations

import argparse
import json
import pathlib
import re
import shutil
import sys

_REPO = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_REPO / "python"))
sys.path.insert(0, str(_REPO / "scripts" / "judge"))
sys.path.insert(0, str(_REPO / "scripts"))
import fixture_cut as fc  # noqa: E402
import fixture_requests as fr  # noqa: E402
from cecelia.utils.atomic_io import write_atomic  # noqa: E402


def mark_met(needs_text: str, rid: str, uid: str) -> str:
    """`needs_text` with `[needs.<rid>]`'s `status = "open"` set to `met: <uid>` — a line edit, so the
    file's comments and layout stay."""
    head = re.search(rf"^\[needs\.{re.escape(rid)}\]\s*$", needs_text, re.M)
    if head is None:
        raise SystemExit(f"NEEDS.toml has no [needs.{rid}]")
    nxt = re.search(r"^\[", needs_text[head.end():], re.M)
    end = head.end() + (nxt.start() if nxt else len(needs_text) - head.end())
    body, n = re.subn(r'^status\s*=\s*"open"\s*$', f'status = "met: {uid}"', needs_text[head.end():end],
                      count=1, flags=re.M)
    if n != 1:
        raise SystemExit(f"[needs.{rid}] has no `status = \"open\"` line to mark")
    return needs_text[:head.end()] + body + needs_text[end:]


def accept(rid: str, *, confirm: tuple[str, ...] = (), test_data: pathlib.Path = _REPO / "test-data",
           root: pathlib.Path | None = None) -> dict:
    """Move the staged cut for `rid` into `test_data`; returns the cut with its final properties."""
    root = root or fr.cuts_dir()
    requests = fr.open_requests(test_data)
    if rid not in requests:
        raise SystemExit(f"{rid} is not an open request in {test_data / 'NEEDS.toml'}")
    c = fr.staged(rid, root)
    if c is None:
        raise SystemExit(f"no staged cut for {rid} in {root / rid} — `pixi run judge-fixtures -- --cut` stages one")
    need = set(c.get("confirm", []))
    unknown = set(confirm) - set(requests[rid]["properties"])
    if unknown:
        raise SystemExit(f"--confirm {', '.join(sorted(unknown))}: {rid} doesn't ask for that")
    if need - set(confirm):
        raise SystemExit(f"{rid} needs {', '.join(sorted(need - set(confirm)))}, which no file shows: look at the "
                         f"cut ({c['dir']}) and pass --confirm for each one it has")
    src = root / rid / "projects" / c["project"]
    dst = test_data / "projects" / c["project"]
    if dst.exists():
        raise SystemExit(f"{dst} exists — a fixture from this image is already in test-data")
    fixtures = test_data / "fixtures.toml"
    if re.search(rf"^\[fixtures\.{re.escape(c['uid'])}\]", fixtures.read_text(encoding="utf-8"), re.M):
        raise SystemExit(f"fixtures.toml already has [fixtures.{c['uid']}]")
    needs = test_data / "NEEDS.toml"
    needs_text = mark_met(needs.read_text(encoding="utf-8"), rid, c["uid"])   # fails before anything is copied
    props = sorted(set(c["properties"]) | set(confirm))
    entry = fc.toml_entry({**c, "properties": props})
    shutil.copytree(src, dst)
    text = fixtures.read_text(encoding="utf-8").rstrip("\n")
    with write_atomic(fixtures) as f:
        f.write(f"{text}\n\n{entry}")
    with write_atomic(needs) as f:
        f.write(needs_text)
    shutil.rmtree(root / rid)
    return {**c, "properties": props, "dir": str(dst / "1" / c["uid"])}


def discard(rid: str, root: pathlib.Path | None = None) -> bool:
    d = (root or fr.cuts_dir()) / rid
    if not d.exists():
        return False
    shutil.rmtree(d)
    return True


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("need", help="the NEEDS.toml request id")
    ap.add_argument("--confirm", action="append", default=[], metavar="PROPERTY",
                    help="a property no file shows that you checked the cut has (repeat)")
    ap.add_argument("--discard", action="store_true", help="drop the staged cut instead")
    args = ap.parse_args(argv)
    if args.discard:
        print(f"discarded the staged cut for {args.need}" if discard(args.need) else f"nothing staged for {args.need}")
        return 0
    c = accept(args.need, confirm=tuple(args.confirm))
    print(f"wrote {c['dir']} ({c['cells']} cells), its entry in test-data/fixtures.toml and "
          f'NEEDS.toml → {args.need}: "met: {c["uid"]}".\n'
          "Fill in the entry's note, run the fixture registry tests, and commit it yourself — it is lab data.")
    print(json.dumps({"properties": c["properties"]}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
