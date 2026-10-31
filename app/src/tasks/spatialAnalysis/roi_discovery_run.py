"""
spatialAnalysis.roiDiscovery (image-scope) — Python runner.

Where do the objects of one population gather (docs/todo/ROI_TRIAGE_PLAN.md)? The question is in
the population — segmentation + gating, filter populations on any measure — so this only finds the
places: groups in each time window's pooled positions (HDBSCAN), ranked by how many more objects
(or how much more rank-measure weight) they hold than the window's average density would put there
(`roi_utils.find_rois`). Optional rank measures weight each object
by their mean percentile; without them every object counts 1.

Reads the object table only — no image or label pixels; the label store gives the geometry (shape,
voxel size). Output: `rois/{suffix}.json` (atomic). Reviews live in a separate file the API owns —
this runner never touches them (Decision 6).

Membership comes from Julia (roiDiscovery.jl): the population's label list.
"""

import datetime

import numpy as np
import pandas as pd

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.roi_utils as roi_utils
from cecelia.utils.label_props_utils import LabelPropsView
from cecelia.utils.atomic_io import write_json_atomic


def _spatial_scale(path, axes):
    scale = zarr_utils.read_scale(path) or [1.0] * len(axes)
    s = dict(zip(axes, scale))
    return float(s.get("z", 1.0)), float(s.get("y", 1.0)), float(s.get("x", 1.0))


def _f(v):
    v = float(v)
    return round(v, 4) if np.isfinite(v) else None


def run(params):
    log = script_utils.get_logfile_utils(params)
    labels_path = params["labelsPath"]
    rois_path = params["roisPath"]
    qc_path = params.get("qcOutPath")
    members = set(int(x) for x in script_utils.get_param(params, "labels", default=[]) or [])
    rank_by = [str(c) for c in script_utils.get_param(params, "rankBy", default=[]) or []]
    min_objects = int(script_utils.get_param(params, "minObjects", default=20))
    max_rois = int(script_utils.get_param(params, "maxRois", default=50))

    if not members:
        log.log("[ERROR] roiDiscovery: no objects")
        _dump(qc_path, {"nObjects": 0, "nRois": 0})
        return

    # geometry from the STORE, never ccid meta: the two can disagree on SizeZ (ROI_TRIAGE_PLAN §1a)
    axes = [a.lower() for a in (zarr_utils.read_axes(labels_path) or ["t", "z", "y", "x"])]
    shape = dict(zip(axes, zarr_utils.open_as_zarr(labels_path)[0][0].shape))
    shape.setdefault("t", 1)
    shape.setdefault("z", 1)
    sz, sy, sx = _spatial_scale(labels_path, axes)

    view = LabelPropsView(params["propsPath"]).view_centroid_cols().filter_by_label(members)
    if rank_by:
        view = view.view_cols(rank_by)
    df = view.as_df()
    for col in ("centroid_t", "centroid_z"):
        if col not in df.columns:
            df[col] = 0.0
    missing = [c for c in rank_by if c not in df.columns]
    if missing:
        log.log(f"[ERROR] roiDiscovery: no such measure(s): {', '.join(missing)}")
        _dump(qc_path, {"nObjects": len(df), "nRois": 0})
        return
    t = np.rint(df["centroid_t"].to_numpy()).astype(int)
    px = df[["centroid_x", "centroid_y", "centroid_z"]].to_numpy(dtype=float)
    um = px * np.array([sx, sy, sz])
    # a categorical measure has no order to rank by: its values read as NaN (weight 0), not a crash
    measures = [pd.to_numeric(df[c], errors="coerce").to_numpy(dtype=float) for c in rank_by]
    weight = roi_utils.object_weights(measures, len(df))
    log.log(f">> {len(df)} objects over {len(np.unique(t))} frames"
            + (f"; ranked by {', '.join(rank_by)}" if rank_by else "; ranked by count"))

    labels = df["label"].to_numpy()

    def _obj(i):
        o = {"label": int(labels[i]), "t": int(t[i]),
             "x": _f(px[i, 0]), "y": _f(px[i, 1]), "z": _f(px[i, 2]), "weight": _f(weight[i])}
        for c, m in zip(rank_by, measures):
            o[c] = _f(m[i])
        return o

    rois, groups = roi_utils.find_rois(um, px, t, shape, min_objects, voxel_um=(sx, sy, sz), weight=weight,
                                       objects=[_obj(i) for i in range(len(df))], max_rois=max_rois)
    print("[PROGRESS] 1/1", flush=True)
    write_json_atomic(rois_path, {
        "version": 2, "suffix": params.get("suffix", "default"), "valueName": params.get("valueName"),
        "generatedAt": datetime.datetime.now().isoformat(timespec="seconds"),
        "params": params.get("taskParams", {}),
        "labels": list(params.get("reviewLabels", [])),
        "rankBy": rank_by,
        "geometry": {"shape": {k: int(v) for k, v in shape.items()},
                     "umPerPx": {"x": sx, "y": sy, "z": sz},
                     "frameIntervalS": zarr_utils.read_time_increment(params["imPath"]) if params.get("imPath") else None},
        "rois": rois}, indent=1)
    n_grouped = int((groups >= 0).sum())
    log.log(f">> {n_grouped} of {len(df)} objects in groups → {len(rois)} ROI(s) → {rois_path}")
    _dump(qc_path, {"nObjects": int(len(df)), "nGrouped": n_grouped, "nRois": len(rois),
                    "nSites": len({r["site"] for r in rois})})


def _dump(path, obj):
    if path is not None:
        write_json_atomic(path, obj)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
