"""
spatialAnalysis.roiDiscovery (image-scope) — Python runner.

Finds regions where something happens to fragments, without tracking them
(docs/todo/ROI_TRIAGE_PLAN.md). Per candidate object, per frame:

- contact   — fraction of its voxels inside a thresholded mask channel (CD169), or proximity to it
- density   — other candidates within `densityRadiusUm` in the same frame (TBMs collect fragments,
              and the mask misses dim TBMs — plan §1b)
- turnover  — how much that neighbourhood changed against t±1 (identity-free)
- naked     — in the "nuclear" population and not in the "membrane" one
- partner   — proximity to the nearest partner object (e.g. a big fragment) in the same frame

Cues are rank-normalised, weighted into one score (`roi_utils.score_objects`), the top-scoring
objects are clustered in (x, y, z, t) with DBSCAN, and each cluster becomes an ROI: a box, a time
window, the cue summary and its object table. Output: `rois/{suffix}.json` (atomic). Reviews of
these ROIs live in a separate file the API owns — this runner never touches them (Decision 6).

Membership comes from Julia (roiDiscovery.jl): candidate / partner / nuc / mem label lists.
"""

import datetime

import numpy as np

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.roi_utils as roi_utils
from cecelia.utils.label_props_utils import LabelPropsView
from cecelia.utils.atomic_io import write_json_atomic

# object-table columns copied into each ROI (when present) — the numbers Claude cross-checks
_MORPH_COLS = ("volume_mesh", "solidity", "ellipticity_interm_oblate")


def _labels_set(params, key):
    return set(int(x) for x in script_utils.get_param(params, key, default=[]) or [])


def _frame_zyx(level, dim_utils, t, channel=None):
    """One timepoint as (z, y, x) — the image's `channel`, or a label level (`channel=None`).

    Through `zarr_utils.read_timepoint`, which resolves the T axis against the image's dim order
    (`ignore_channel=True` for a label level, which has no C axis)."""
    is_label = channel is None
    frame = zarr_utils.read_timepoint(level, dim_utils, t, ignore_channel=is_label)
    order = [a for a in dim_utils.im_dim_order if a != "T" and not (is_label and a == "C")]
    if not is_label:
        frame = np.take(frame, int(channel), axis=order.index("C"))
        order.remove("C")
    if "Z" not in order:
        frame = frame[None]
        order.insert(0, "Z")
    return np.transpose(frame, [order.index(a) for a in ("Z", "Y", "X")])


def _spatial_scale(path, axes):
    scale = zarr_utils.read_scale(path) or [1.0] * len(axes)
    s = dict(zip(axes, scale))
    return float(s.get("z", 1.0)), float(s.get("y", 1.0)), float(s.get("x", 1.0))


def run(params):
    log = script_utils.get_logfile_utils(params)
    props_path  = params["propsPath"]
    labels_path = params["labelsPath"]
    im_path     = params["imPath"]
    rois_path   = params["roisPath"]
    qc_path     = params.get("qcOutPath")
    cand        = _labels_set(params, "candidateLabels")
    partners    = _labels_set(params, "partnerLabels")
    nuc         = _labels_set(params, "nucLabels")
    mem         = _labels_set(params, "memLabels")
    mask_ch     = int(script_utils.get_param(params, "maskChannel", default=-1))
    weights     = dict(script_utils.get_param(params, "weights", default={}) or {})
    g = lambda k, d: float(script_utils.get_param(params, k, default=d))

    if not cand:
        log.log("[ERROR] roiDiscovery: no candidate objects")
        _dump(qc_path, {"nCandidates": 0, "nRois": 0})
        return

    # geometry from the STORES, never ccid meta: the two can disagree on SizeZ (ROI_TRIAGE_PLAN §1a)
    lab_axes = [a.lower() for a in (zarr_utils.read_axes(labels_path) or ["t", "z", "y", "x"])]
    lab_arr = zarr_utils.open_as_zarr(labels_path)[0][0]
    shape = dict(zip(lab_axes, lab_arr.shape))
    shape.setdefault("t", 1)
    shape.setdefault("z", 1)
    sz, sy, sx = _spatial_scale(labels_path, lab_axes)

    # ── object table: candidates + partners, centroids in px → µm ────────────────────────────────
    df = LabelPropsView(props_path).view_centroid_cols().filter_by_label(cand | partners).as_df()
    if "centroid_t" not in df.columns:
        df["centroid_t"] = 0.0
    if "centroid_z" not in df.columns:
        df["centroid_z"] = 0.0
    df["_t"] = np.rint(df["centroid_t"].to_numpy()).astype(int)
    px_all = df[["centroid_x", "centroid_y", "centroid_z"]].to_numpy(dtype=float)
    um_all = px_all * np.array([sx, sy, sz])
    is_cand = df["label"].isin(cand).to_numpy()
    is_part = df["label"].isin(partners).to_numpy()
    c = df[is_cand].reset_index(drop=True)
    c_px, c_um, c_t = px_all[is_cand], um_all[is_cand], df["_t"].to_numpy()[is_cand]
    n = len(c)
    log.log(f">> {n} candidate objects over {len(np.unique(c_t))} frames; {int(is_part.sum())} partner objects")

    # ── mask cues, frame by frame (only frames that hold candidates) ─────────────────────────────
    frac = np.zeros(n)
    dist = np.full(n, np.inf)
    mask_frac, thr_used = [], []
    frames = np.unique(c_t)
    if mask_ch >= 0:
        im_arr = zarr_utils.open_as_zarr(im_path)[0][0]
        # dims from the image's OME metadata, as every per-frame runner (cell_contacts_mesh_run.py)
        dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
        dim_utils.calc_image_dimensions(im_arr.shape)
        sigma_um = g("maskSmoothUm", 0.5)
        sigma = (sigma_um / sz, sigma_um / sy, sigma_um / sx)
        min_vox = int(round(g("maskMinVolumeUm3", 50.0) / (sx * sy * sz)))
        for i, f in enumerate(frames):
            idx = np.flatnonzero(c_t == f)
            stack = _frame_zyx(im_arr, dim_utils, f, mask_ch)
            mask, thr = roi_utils.channel_mask(stack, g("maskThreshold", 0.0), sigma, min_vox)
            labs = _frame_zyx(lab_arr, dim_utils, f)
            zyx = c_px[idx][:, [2, 1, 0]]
            frac[idx], dist[idx] = roi_utils.mask_cues(labs, mask, c["label"].to_numpy()[idx], zyx, (sz, sy, sx))
            mask_frac.append(float(mask.mean()))
            thr_used.append(float(thr))
            print(f"[PROGRESS] {i + 1}/{len(frames) + 1}", flush=True)
        log.log(f">> mask channel {mask_ch}: threshold {np.median(thr_used):.3g} (median), "
                f"mean coverage {100 * np.mean(mask_frac):.2f}% of voxels")
    contact_scale = g("contactScaleUm", 2.0)
    contact = np.maximum(frac, roi_utils.proximity(dist, contact_scale))

    # ── point cues ───────────────────────────────────────────────────────────────────────────────
    radius = g("densityRadiusUm", 10.0)
    n_now, n_prev, n_next = roi_utils.local_density(c_um, c_t, radius)
    turn = roi_utils.turnover(n_now, n_prev, n_next)
    labels = c["label"].to_numpy()
    naked = np.array([1.0 if (l in nuc and l not in mem) else 0.0 for l in labels])
    if is_part.any():
        pdist = roi_utils.nearest_distance(c_um, c_t, um_all[is_part], df["_t"].to_numpy()[is_part],
                                           exclude_ids=labels, target_ids=df["label"].to_numpy()[is_part])
        partner = roi_utils.proximity(pdist, g("partnerScaleUm", 3.0))
    else:
        pdist = np.full(n, np.inf)
        partner = np.zeros(n)
    cues = {"contact": contact, "density": n_now, "turnover": turn, "naked": naked, "partner": partner}
    score, normed = roi_utils.score_objects(cues, weights)

    # ── clusters → ROIs ──────────────────────────────────────────────────────────────────────────
    keep = roi_utils.candidate_mask(score, g("candidateQuantile", 0.9))
    kidx = np.flatnonzero(keep)
    cl = roi_utils.cluster_4d(c_um[kidx], c_t[kidx], g("clusterRadiusUm", 10.0),
                              g("timeScaleUm", 2.0), int(g("minObjects", 5)))
    cl = roi_utils.split_windows(cl, c_t[kidx], int(g("windowFrames", 12)), int(g("minObjects", 5)))

    def _f(v):
        v = float(v)
        return round(v, 4) if np.isfinite(v) else None

    def _obj(i):
        row = c.iloc[i]
        o = {"label": int(row["label"]), "t": int(c_t[i]),
             "x": _f(c_px[i, 0]), "y": _f(c_px[i, 1]), "z": _f(c_px[i, 2]),
             "score": _f(score[i]), "fracInside": _f(frac[i]), "distMaskUm": _f(dist[i]),
             "density": int(n_now[i]), "partnerDistUm": _f(pdist[i]),
             "nuc": bool(labels[i] in nuc), "mem": bool(labels[i] in mem)}
        for col in _MORPH_COLS:
            if col in c.columns:
                o[col] = _f(row[col])
        for k in range(8):
            col = f"mean_intensity_{k}"
            if col in c.columns:
                o[col] = _f(row[col])
        return o

    pad = g("paddingUm", 5.0)
    rois = roi_utils.build_rois(
        cl, c_px[kidx], c_t[kidx], score[kidx], {k: v[kidx] for k, v in normed.items()}, weights,
        shape, (pad / sx, pad / sy, pad / sz), objects=[_obj(i) for i in kidx],
        max_rois=int(g("maxRois", 50)))

    write_json_atomic(rois_path, {
        "version": 1, "suffix": params.get("suffix", "default"), "valueName": params.get("valueName"),
        "generatedAt": datetime.datetime.now().isoformat(timespec="seconds"),
        "params": params.get("taskParams", {}),
        "geometry": {"shape": {k: int(v) for k, v in shape.items()},
                     "umPerPx": {"x": sx, "y": sy, "z": sz},
                     "frameIntervalS": zarr_utils.read_time_increment(im_path)},
        "rois": rois}, indent=1)
    print(f"[PROGRESS] {len(frames) + 1}/{len(frames) + 1}", flush=True)
    log.log(f">> {len(kidx)} high-scoring objects → {len(rois)} ROI(s) → {rois_path}")
    _dump(qc_path, {"nCandidates": int(n), "nHigh": int(len(kidx)), "nRois": len(rois),
                    "nNoise": int((cl < 0).sum()) if len(cl) else 0,
                    "maskCoverage": float(np.mean(mask_frac)) if mask_frac else None,
                    "maskThreshold": float(np.median(thr_used)) if thr_used else None,
                    "fracInMask": float((frac > 0.5).mean()) if n else 0.0})


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
