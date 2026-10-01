"""Make a ready-to-run agent fixture (docs/todo/AGENT_OVERNIGHT_PLAN.md, P1): synthetic sources →
isolated project (imported, optional prior segmentation) → canary snapshot. Everything lands under
`--root`, which must not be inside the user's dev or projects dir.

    pixi run python scripts/agent_eval/setup.py --root /tmp/agent-run-1 [--images 2] [--seed 0] [--prior none]
    pixi run python scripts/agent_eval/setup.py --root /tmp/agent-run-2 --fixture /tmp/agent-crop   # crop.py output

Writes `<root>/run.json`: the project uid/dir, image uids, ground-truth paths and the pre-run snapshot.
"""
from __future__ import annotations

import argparse
import importlib.util
import json
import pathlib
import shutil
import subprocess
import sys
from cecelia.utils.atomic_io import write_json_atomic

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[1]


def _load(name):
    key = f"agent_eval_{name}"
    spec = importlib.util.spec_from_file_location(key, HERE / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    sys.modules[key] = mod
    spec.loader.exec_module(mod)
    return mod


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--root", required=True)
    ap.add_argument("--images", type=int, default=2)
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--prior", choices=["cellpose", "none"], default="cellpose")
    ap.add_argument("--bioformats2raw", default=None, help="default: the one in your dev config")
    ap.add_argument("--fixture", default=None, help="use an existing fixture dir (e.g. crop.py output)")
    a = ap.parse_args(argv)

    root = pathlib.Path(a.root).resolve()
    if root.exists() and any(root.iterdir()):
        raise SystemExit(f"{root} is not empty — pick a fresh root")
    fixture, score = _load("fixture"), _load("score")
    if a.fixture:
        fixture_dir = pathlib.Path(a.fixture).resolve()
        with open(fixture_dir / "fixture.json", encoding="utf-8") as f:
            kind = json.load(f)["kind"]
        prior = a.prior if kind == "synthetic" else "none"   # the prior is cellpose on the `cells` channel
    else:
        fixture_dir, prior = root / "fixture", a.prior
        fixture.write_fixture(fixture_dir, a.images, a.seed)

    julia = shutil.which("julia") or sys.exit("julia not on PATH")
    out = root / "setup.json"
    subprocess.run([julia, "--project=app", str(HERE / "setup_project.jl"),
                    "--fixture", str(fixture_dir), "--dev-dir", str(root / "dev"),
                    "--out", str(out), "--prior", prior, "--python", sys.executable,
                    *(["--bioformats2raw", a.bioformats2raw] if a.bioformats2raw else [])],
                   cwd=REPO, check=True)
    with open(out, encoding="utf-8") as f:
        run = json.load(f)
    run["fixtureDir"] = str(fixture_dir)
    run["snapshot"] = score.snapshot(run["projectDir"])
    write_json_atomic(root / "run.json", run, indent=2)
    print(json.dumps({"root": str(root), "projectUid": run["projectUid"],
                      "images": [i["uid"] for i in run["images"]]}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
