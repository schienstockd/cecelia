"""Score an agent run against fixture ground truth (docs/todo/AGENT_OVERNIGHT_PLAN.md, P1).

Pure core, file readers at the edge. Per frame, ground-truth cells are matched to predicted objects by
centroid (Hungarian, within `max_dist_px`). From the matches:

- **segmentation** — detection precision / recall / F1 over all cell-frames (against a real-data
  *reference* — only the user's tracked cells — read recall; precision counts every untracked cell);
- **tracking** — link recall (consecutive ground-truth positions joined by one predicted track) and link
  precision (consecutive predicted positions of one track that stay on one ground-truth cell);
- **behaviour** — per cell-frame state accuracy under the best mapping of predicted to true states
  (state numbering is arbitrary);
- **canary** — whether the files and `_active` pointers that existed before the run are untouched.

    pixi run python scripts/agent_eval/score.py --fixture DIR --h5ad IMG=PATH [--state-col COL]
"""
from __future__ import annotations

import argparse
import hashlib
import json
import pathlib

import numpy as np
import pandas as pd
from scipy.optimize import linear_sum_assignment


def match_frames(gt: pd.DataFrame, pred: pd.DataFrame, max_dist_px: float,
                 z_scale: float | None = None) -> pd.DataFrame:
    """One row per matched (gt cell, pred object) per frame: t, cell, pred_idx, dist.
    `gt` needs t/y/x/cell; `pred` needs t/y/x (its index identifies the object). With `z_scale`
    (z step / xy pixel size) and a `z` column on both, distance is 3D in xy-pixel units."""
    use_z = z_scale is not None and "z" in gt.columns and "z" in pred.columns
    out = []
    for t, g in gt.groupby("t"):
        p = pred[pred["t"] == t]
        if p.empty:
            continue
        d2 = ((g["y"].to_numpy()[:, None] - p["y"].to_numpy()[None, :]) ** 2 +
              (g["x"].to_numpy()[:, None] - p["x"].to_numpy()[None, :]) ** 2)
        if use_z:
            d2 = d2 + (z_scale * (g["z"].to_numpy()[:, None] - p["z"].to_numpy()[None, :])) ** 2
        d = np.sqrt(d2)
        gi, pj = linear_sum_assignment(d)
        for i, j in zip(gi, pj):
            if d[i, j] <= max_dist_px:
                out.append({"t": t, "cell": g["cell"].iloc[i], "pred_idx": p.index[j], "dist": d[i, j]})
    return pd.DataFrame(out, columns=["t", "cell", "pred_idx", "dist"])


def _prf(tp: int, n_true: int, n_pred: int) -> dict:
    precision = tp / n_pred if n_pred else 0.0
    recall = tp / n_true if n_true else 0.0
    f1 = 2 * precision * recall / (precision + recall) if precision + recall else 0.0
    return {"precision": precision, "recall": recall, "f1": f1}


def score_detection(gt: pd.DataFrame, pred: pd.DataFrame, m: pd.DataFrame) -> dict:
    return {"n_true": len(gt), "n_pred": len(pred), "tp": len(m), **_prf(len(m), len(gt), len(pred))}


def score_tracking(gt: pd.DataFrame, pred: pd.DataFrame, m: pd.DataFrame) -> dict | None:
    if "track_id" not in pred.columns:
        return None
    tid = pred["track_id"]
    by_gt = {(r.cell, r.t): r.pred_idx for r in m.itertuples()}
    true_links = correct = 0
    for cell, g in gt.groupby("cell"):
        ts = sorted(g["t"])
        for t0, t1 in zip(ts, ts[1:]):
            true_links += 1
            a, b = by_gt.get((cell, t0)), by_gt.get((cell, t1))
            if a is not None and b is not None and pd.notna(tid[a]) and tid[a] == tid[b]:
                correct += 1
    # predicted links: consecutive frames of one track, both matched to a ground-truth cell
    cell_of = {r.pred_idx: r.cell for r in m.itertuples()}
    pred_links = pure = 0
    for _, g in pred[pred["track_id"].notna()].groupby("track_id"):
        g = g.sort_values("t")
        for (ia, ta), (ib, tb) in zip(zip(g.index, g["t"]), zip(g.index[1:], g["t"][1:])):
            if tb - ta != 1 or ia not in cell_of or ib not in cell_of:
                continue
            pred_links += 1
            pure += cell_of[ia] == cell_of[ib]
    return {"true_links": true_links, "link_recall": correct / true_links if true_links else 0.0,
            "pred_links": pred_links, "link_precision": pure / pred_links if pred_links else 0.0,
            "n_tracks": int(tid.dropna().nunique())}


def score_states(gt: pd.DataFrame, pred: pd.DataFrame, m: pd.DataFrame, state_col: str) -> dict | None:
    if state_col not in pred.columns or "state" not in gt.columns:
        return None
    g_state = gt.set_index(["cell", "t"])["state"]
    pairs = [(g_state[(r.cell, r.t)], pred.at[r.pred_idx, state_col]) for r in m.itertuples()]
    pairs = [(a, b) for a, b in pairs if pd.notna(b)]
    if not pairs:
        return {"n": 0, "accuracy": 0.0, "mapping": {}}
    true_s = sorted({a for a, _ in pairs})
    pred_s = sorted({b for _, b in pairs}, key=str)
    conf = np.zeros((len(pred_s), len(true_s)))
    for a, b in pairs:
        conf[pred_s.index(b), true_s.index(a)] += 1
    pi, ti = linear_sum_assignment(-conf)                  # best one-to-one state mapping
    mapping = {str(pred_s[i]): true_s[j] for i, j in zip(pi, ti)}
    return {"n": len(pairs), "accuracy": conf[pi, ti].sum() / len(pairs), "mapping": mapping}


def score_image(gt: pd.DataFrame, pred: pd.DataFrame, max_dist_px: float,
                state_col: str | None = None, z_scale: float | None = None) -> dict:
    pred = pred.reset_index(drop=True)
    m = match_frames(gt, pred, max_dist_px, z_scale)
    out = {"segmentation": score_detection(gt, pred, m), "tracking": score_tracking(gt, pred, m)}
    if state_col:
        out["behaviour"] = score_states(gt, pred, m, state_col)
    return out


# ── canary: what existed before the run must be unchanged ─────────────────────────────────────────

def snapshot(project_dir: str | pathlib.Path) -> dict:
    """sha256 of every file under the project + each image's `_active` pointers."""
    root = pathlib.Path(project_dir)
    files = {}
    for p in sorted(root.rglob("*")):
        if p.is_file():
            files[str(p.relative_to(root))] = hashlib.sha256(p.read_bytes()).hexdigest()
    active = {}
    for ccid in root.glob("1/*/ccid.json"):
        with open(ccid, encoding="utf-8") as f:
            d = json.load(f)
        active[ccid.parent.name] = {k: v.get("_active") for k, v in d.items()
                                    if isinstance(v, dict) and "_active" in v}
    return {"files": files, "active": active}


def check_canary(before: dict, after: dict, ignore=("ccid.json", "runlog.json", "logs/", "tasks/")) -> dict:
    """Changed / deleted pre-existing files (bookkeeping files the platform rewrites are ignored)
    and every `_active` pointer that moved."""
    def keep(rel: str) -> bool:
        return not any(part in rel for part in ignore)
    changed = [f for f, h in before["files"].items() if keep(f) and after["files"].get(f) not in (h,)]
    moved = [{"image": img, "field": fld, "before": v, "after": after["active"].get(img, {}).get(fld)}
             for img, flds in before["active"].items() for fld, v in flds.items()
             if after["active"].get(img, {}).get(fld) != v]
    return {"intact": not changed and not moved, "changed_files": changed, "active_moved": moved}


# ── readers ──────────────────────────────────────────────────────────────────────────────────────

def load_gt(path: str | pathlib.Path) -> tuple[pd.DataFrame, dict]:
    with open(path, encoding="utf-8") as f:
        gt = json.load(f)
    return pd.DataFrame(gt["rows"]), gt["spec"]


def load_pred(h5ad_path: str) -> pd.DataFrame:
    """A run's label props as t / y / x (+ track_id and any obs column), via the sanctioned reader."""
    from cecelia.utils.label_props_utils import LabelPropsView
    df = LabelPropsView(h5ad_path).view_centroid_cols().as_df()
    return df.rename(columns={"centroid_t": "t", "centroid_z": "z", "centroid_y": "y", "centroid_x": "x"})


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--fixture", required=True, help="fixture dir (holds fixture.json)")
    ap.add_argument("--h5ad", action="append", default=[], help="IMAGE_NAME=path/to/labelProps.h5ad")
    ap.add_argument("--state-col", default=None)
    a = ap.parse_args(argv)
    with open(pathlib.Path(a.fixture) / "fixture.json", encoding="utf-8") as f:
        manifest = json.load(f)
    paths = dict(s.split("=", 1) for s in a.h5ad)
    out = {}
    for img in manifest["images"]:
        if img["name"] not in paths:
            out[img["name"]] = None
            continue
        gt, spec = load_gt(img["gt"])
        out[img["name"]] = score_image(gt, load_pred(paths[img["name"]]), spec["radius_px"], a.state_col,
                                       spec.get("z_scale"))
    print(json.dumps(out, indent=2, default=float))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
