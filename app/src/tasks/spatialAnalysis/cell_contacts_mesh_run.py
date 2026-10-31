"""
spatialAnalysis.contactsMeshes (image-scope) — Python runner (MESH route).

Surface-to-surface contact: for each population-A cell, the minimum SURFACE distance to any
population-B cell, from meshes built on the fly (marching cubes on the label submasks) — the accurate
counterpart to the fast centroid (points) route, mostly for live images (docs/todo/SPATIAL_REGIONS_PLAN.md
Decision 11). Per timepoint for live. Candidate pairs are pre-filtered by centroid distance
(mesh_utils.nearest_surface), so it's O(edges), not O(N²), and needs no fcl.

A and B are each a single segmentation (label ids collide across segmentations, so B is one seg). Params:
imPath (for dims), aLabelPath / bLabelPath (label zarrs), aLabels (member label ids), targets
(`[{target: obs-column suffix, bLabels: member label ids}]` — one column set per target, #1734; B is
meshed once per timepoint and split per target), physicalSizes [sz,sy,sx], maxContactDist (µm),
checkContained (bool — also write `contained_by#` / `contains_n#`, `mesh_utils.containment_per_target`),
popType, propsPath (A's labelProps to write), qcOutPath.
"""

import numpy as np
import pandas as pd

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.mesh_utils as mesh_utils
from cecelia.utils.label_props_utils import LabelPropsView
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.atomic_io import write_json_atomic


def _extract_t(arr, t_axis, t):
    if t_axis is None:
        return np.asarray(arr)
    idx = [slice(None)] * arr.ndim
    idx[t_axis] = t
    return np.asarray(arr[tuple(idx)])


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path   = params["imPath"]
    a_labels  = set(int(x) for x in script_utils.get_param(params, "aLabels", default=[]))
    targets   = [(str(t["target"]), set(int(x) for x in t.get("bLabels", [])))
                 for t in script_utils.get_param(params, "targets", default=[])]
    targets   = [(name, labs) for name, labs in targets if labs]
    b_labels  = set().union(*(labs for _, labs in targets)) if targets else set()
    phys      = np.asarray(script_utils.get_param(params, "physicalSizes", default=[1.0, 1.0, 1.0]), dtype=float)
    max_dist  = float(script_utils.get_param(params, "maxContactDist", default=10.0))
    contained = bool(script_utils.get_param(params, "checkContained", default=False))
    pop_type  = script_utils.get_param(params, "popType", default="flow")
    props     = params["propsPath"]
    qc_path   = params.get("qcOutPath")

    if not a_labels or not b_labels:
        log.log("[ERROR] contactsMeshes: empty A or B population")
        return

    # dims from the image OME metadata; label axis order = image order minus channel
    dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
    dim_utils.calc_image_dimensions(zarr_utils.open_as_zarr(im_path)[0][0].shape)
    label_order = [ax for ax in dim_utils.im_dim_order if ax != "C"]
    t_axis = label_order.index("T") if "T" in label_order else None
    n_t = int(dict(zip(dim_utils.im_dim_order, dim_utils.im_dim)).get("T", 1))
    n_spatial = len(label_order) - (1 if t_axis is not None else 0)
    scale = phys[-n_spatial:]                              # spatial dims only (drops T)

    a_dat, _ = zarr_utils.open_as_zarr(params["aLabelPath"])
    b_dat, _ = zarr_utils.open_as_zarr(params["bLabelPath"])
    a_arr, b_arr = a_dat[0], b_dat[0]
    # per target, per-A-cell nearest B surface distance + id, accumulated across timepoints
    min_dist = {name: {l: float("inf") for l in a_labels} for name, _ in targets}
    contact_id = {name: {l: 0 for l in a_labels} for name, _ in targets}
    # containment, max over timepoints (a label is one object; a tracked id held over frames keeps
    # its highest count rather than a sum that counts the same fragment once per frame)
    contained_by = {name: {l: 0 for l in a_labels} for name, _ in targets}
    contains_n = {name: {l: 0 for l in a_labels} for name, _ in targets}
    # targets with at least one B mesh at some timepoint; the rest (every cell below min_voxels, or
    # absent from the B zarr) get no columns and are reported back for the QC warning
    meshed = set()

    for t in range(n_t):
        a_vol = _extract_t(a_arr, t_axis, t)
        b_vol = _extract_t(b_arr, t_axis, t)
        if a_vol.max() == 0 or b_vol.max() == 0:
            print(f"[PROGRESS] {t + 1}/{n_t}", flush=True)
            continue
        a_meshes = mesh_utils.build_label_meshes(a_vol, a_labels, scale)
        b_meshes = mesh_utils.build_label_meshes(b_vol, b_labels, scale)
        meshed |= {name for name, labs in targets if any(bl in labs for bl in b_meshes)}
        for name, nearest in mesh_utils.nearest_surface_per_target(a_meshes, b_meshes, targets).items():
            for al, (d, bl) in nearest.items():
                if d < min_dist[name][al]:
                    min_dist[name][al] = d
                    contact_id[name][al] = bl or 0
        if contained:
            per = mesh_utils.containment_per_target(a_vol, a_labels, b_vol, targets)
            for name, (inside, held) in per.items():
                for al, v in inside.items():
                    contained_by[name][al] = max(contained_by[name][al], v)
                for al, v in held.items():
                    contains_n[name][al] = max(contains_n[name][al], v)
        log.log(f">> t{t}: {len(a_meshes)} A / {len(b_meshes)} B meshes")
        print(f"[PROGRESS] {t + 1}/{n_t}", flush=True)

    labels = sorted(a_labels)
    cols = {"label": labels}
    in_any = np.zeros(len(labels), dtype=bool)
    in_host = np.zeros(len(labels), dtype=bool)
    for name, _ in targets:
        if name not in meshed:
            log.log(f"[WARN] contactsMeshes: no B meshes for {name} — skipped")
            continue
        dist_arr = np.array([min_dist[name][l] if np.isfinite(min_dist[name][l]) else np.nan for l in labels])
        contact = (dist_arr <= max_dist).astype(float)
        cols[f"{pop_type}.cell.contact#{name}"] = contact
        cols[f"{pop_type}.cell.min_distance#{name}"] = dist_arr
        cols[f"{pop_type}.cell.contact_id#{name}"] = [float(contact_id[name][l]) for l in labels]
        in_any |= contact > 0
        log.log(f">> contactsMeshes: {int(np.nansum(contact))}/{len(labels)} A cells contact {name} (≤{max_dist}µm)")
        if contained:
            inside = np.array([float(contained_by[name][l]) for l in labels])
            cols[f"{pop_type}.cell.contained_by#{name}"] = inside
            cols[f"{pop_type}.cell.contains_n#{name}"] = [float(contains_n[name][l]) for l in labels]
            in_host |= inside > 0
            log.log(f">> contactsMeshes: {int(inside.sum())}/{len(labels)} A cells inside {name}")
    if len(cols) > 1:
        view = LabelPropsView(props)
        if not contained:
            # a re-run without containment must not leave an earlier run's containment beside the
            # fresh distances, reading as if it were measured on them
            view.drop_obs([f"{pop_type}.cell.{k}#{name}" for name, _ in targets if name in meshed
                           for k in ("contained_by", "contains_n")])
        view.add_obs(pd.DataFrame(cols)).save()

    # "in contact" with ANY target — what the single pooled column used to report
    n_contact = int(in_any.sum())
    if qc_path is not None:
        write_json_atomic(qc_path, {"nCellsA": len(labels), "nContacts": n_contact,
                                   "fracInContact": (n_contact / len(labels)) if labels else 0.0,
                                   "emptyTargets": [name for name, _ in targets if name not in meshed],
                                   **({"nContained": int(in_host.sum())} if contained else {})})


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
