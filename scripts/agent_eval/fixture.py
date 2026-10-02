"""Synthetic time-lapse fixtures for the agent overnight run (docs/todo/AGENT_OVERNIGHT_PLAN.md, P1).

Seeded and deterministic: the same seed writes the same pixels and the same ground truth. Each image is a
2D + t single-channel movie of round cells, each switching between two motility regimes — persistent
migration and arrest — at known frames, so segmentation, tracks and behaviour states all have a designed-in
answer. Output per image: `<name>.ome.tif` (the source the import task converts) and `<name>.gt.json`
(per cell-frame centroid in pixels + state, the scorer's ground truth) plus `<name>.labels.npz` (the
ground-truth label masks, used only to build the "prior work" canary).

    pixi run python scripts/agent_eval/fixture.py --out /tmp/agent-fixture --images 2 --seed 0
"""
from __future__ import annotations

import argparse
import json
import pathlib
from dataclasses import asdict, dataclass

import numpy as np
import tifffile
from cecelia.utils.atomic_io import write_json_atomic

MIGRATING = "migrating"
ARRESTED = "arrested"


@dataclass(frozen=True)
class FixtureSpec:
    size: int = 192                 # px, square
    n_frames: int = 40
    n_cells: int = 18
    radius_px: float = 5.0          # 8 µm diameter at 0.8 µm/px
    px_um: float = 0.8
    dt_s: float = 30.0
    speed_migrating_um_min: float = 5.0
    speed_arrested_um_min: float = 0.3
    persistence: float = 0.85       # 1 = straight line; turn noise is (1 - persistence) · π
    background: float = 100.0
    amplitude: float = 900.0
    read_noise: float = 6.0
    min_dwell: int = 8              # frames a regime lasts at least, so an HMM can see it


def simulate(spec: FixtureSpec, seed: int) -> list[dict]:
    """Cell trajectories: one row per (cell, frame) with centroid in pixels and the regime."""
    rng = np.random.default_rng(seed)
    lo, hi = spec.radius_px + 3, spec.size - spec.radius_px - 3
    min_gap = 2 * spec.radius_px + 2

    pos = []
    while len(pos) < spec.n_cells:
        p = rng.uniform(lo, hi, size=2)
        if all(np.hypot(*(p - q)) >= min_gap for q in pos):
            pos.append(p)
    pos = np.array(pos)
    heading = rng.uniform(0, 2 * np.pi, spec.n_cells)

    # Regime schedule: start at random, switch at 1–2 frames that leave every segment ≥ min_dwell long.
    states = np.empty((spec.n_cells, spec.n_frames), dtype=object)
    for c in range(spec.n_cells):
        s = rng.choice([MIGRATING, ARRESTED])
        n_switch = rng.integers(1, 3)
        cuts = sorted(rng.choice(np.arange(spec.min_dwell, spec.n_frames - spec.min_dwell),
                                 size=n_switch, replace=False))
        if n_switch == 2 and cuts[1] - cuts[0] < spec.min_dwell:
            cuts = cuts[:1]
        t0 = 0
        for cut in list(cuts) + [spec.n_frames]:
            states[c, t0:cut] = s
            s = ARRESTED if s == MIGRATING else MIGRATING
            t0 = cut

    step = {MIGRATING: spec.speed_migrating_um_min * spec.dt_s / 60 / spec.px_um,
            ARRESTED: spec.speed_arrested_um_min * spec.dt_s / 60 / spec.px_um}
    rows = []
    for t in range(spec.n_frames):
        if t > 0:
            for c in range(spec.n_cells):
                if states[c, t] == MIGRATING:
                    heading[c] += rng.normal(0, (1 - spec.persistence) * np.pi)
                    d = step[MIGRATING] * np.array([np.sin(heading[c]), np.cos(heading[c])])
                else:
                    d = rng.normal(0, step[ARRESTED], size=2)
                new = pos[c] + d
                # mirror off the borders (y moves with sin(heading), x with cos(heading))
                for k in range(2):
                    if new[k] < lo or new[k] > hi:
                        new[k] = np.clip(2 * (lo if new[k] < lo else hi) - new[k], lo, hi)
                        heading[c] = -heading[c] if k == 0 else np.pi - heading[c]
                # refuse a move into a neighbour — turn instead
                others = np.delete(pos, c, axis=0)
                if len(others) and np.min(np.hypot(*(others - new).T)) < min_gap:
                    heading[c] = rng.uniform(0, 2 * np.pi)
                    continue
                pos[c] = new
        for c in range(spec.n_cells):
            rows.append({"cell": c, "t": t, "y": float(pos[c, 0]), "x": float(pos[c, 1]),
                         "state": str(states[c, t])})
    return rows


def render(spec: FixtureSpec, rows: list[dict], seed: int) -> tuple[np.ndarray, np.ndarray]:
    """(image T×Y×X uint16, labels T×Y×X uint16 — label = cell + 1)."""
    rng = np.random.default_rng(seed + 1_000_003)
    yy, xx = np.mgrid[0:spec.size, 0:spec.size].astype(np.float32)
    image = np.full((spec.n_frames, spec.size, spec.size), spec.background, dtype=np.float32)
    labels = np.zeros((spec.n_frames, spec.size, spec.size), dtype=np.uint16)
    for r in rows:
        d = np.hypot(yy - r["y"], xx - r["x"])
        image[r["t"]] += spec.amplitude / (1 + np.exp(np.minimum((d - spec.radius_px) / 0.8, 50)))   # soft disc
        labels[r["t"]][d <= spec.radius_px] = r["cell"] + 1
    image = rng.poisson(image).astype(np.float32) + rng.normal(0, spec.read_noise, image.shape)
    return np.clip(image, 0, 65535).astype(np.uint16), labels


def write_image(out_dir: pathlib.Path, name: str, spec: FixtureSpec, seed: int) -> dict:
    rows = simulate(spec, seed)
    image, labels = render(spec, rows, seed)
    tif = out_dir / f"{name}.ome.tif"
    tifffile.imwrite(
        tif, image[:, np.newaxis], photometric="minisblack", ome=True,
        metadata={"axes": "TCYX", "Channel": {"Name": ["cells"]},
                  "PhysicalSizeX": spec.px_um, "PhysicalSizeXUnit": "µm",
                  "PhysicalSizeY": spec.px_um, "PhysicalSizeYUnit": "µm",
                  "TimeIncrement": spec.dt_s, "TimeIncrementUnit": "s"})
    np.savez_compressed(out_dir / f"{name}.labels.npz", labels=labels)
    gt = {"name": name, "seed": seed, "spec": asdict(spec), "rows": rows}
    write_json_atomic(out_dir / f"{name}.gt.json", gt)
    return {"name": name, "tif": str(tif), "gt": str(out_dir / f"{name}.gt.json"),
            "labels": str(out_dir / f"{name}.labels.npz")}


def write_fixture(out_dir: str | pathlib.Path, n_images: int = 2, seed: int = 0,
                  spec: FixtureSpec | None = None) -> dict:
    out = pathlib.Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    spec = spec or FixtureSpec()
    images = [write_image(out, f"synth{i + 1}", spec, seed * 1000 + i) for i in range(n_images)]
    manifest = {"kind": "synthetic", "seed": seed, "spec": asdict(spec), "images": images}
    write_json_atomic(out / "fixture.json", manifest, indent=2)
    return manifest


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--out", required=True)
    ap.add_argument("--images", type=int, default=2)
    ap.add_argument("--seed", type=int, default=0)
    a = ap.parse_args(argv)
    m = write_fixture(a.out, a.images, a.seed)
    print(json.dumps({"out": a.out, "images": [i["name"] for i in m["images"]]}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
