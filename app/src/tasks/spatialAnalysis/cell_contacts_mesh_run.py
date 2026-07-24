"""
spatialAnalysis.contactsMeshes (image-scope) — Python runner (MESH route).

Surface-to-surface contact: for each population-A cell, the minimum SURFACE distance to any
population-B cell, from meshes built on the fly (marching cubes on the label submasks) — the accurate
counterpart to the fast centroid (points) route, mostly for live images (docs/todo/SPATIAL_REGIONS_PLAN.md
Decision 11). Per timepoint for live. Candidate pairs are pre-filtered by centroid distance
(mesh_utils.nearest_surface), so it's O(edges), not O(N²), and needs no fcl.

A and B are each a single segmentation (label ids collide across segmentations, so B is one seg). Params:
imPath (for dims), aLabelPath / bLabelPath (label zarrs), aLabels / bLabels (member label ids),
physicalSizes [sz,sy,sx], maxContactDist (µm), target (obs-column suffix), popType, propsPath (A's
labelProps to write), qcOutPath.
"""
import json

import numpy as np
import pandas as pd

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.mesh_utils as mesh_utils
from cecelia.utils.label_props_utils import LabelPropsView
from cecelia.utils.dim_utils import DimUtils


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
    b_labels  = set(int(x) for x in script_utils.get_param(params, "bLabels", default=[]))
    phys      = np.asarray(script_utils.get_param(params, "physicalSizes", default=[1.0, 1.0, 1.0]), dtype=float)
    max_dist  = float(script_utils.get_param(params, "maxContactDist", default=10.0))
    target    = script_utils.get_param(params, "target", default="B")
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
    # per-A-cell nearest B surface distance + id, accumulated across timepoints
    min_dist = {l: float("inf") for l in a_labels}
    contact_id = {l: 0 for l in a_labels}

    for t in range(n_t):
        a_vol = _extract_t(a_arr, t_axis, t)
        b_vol = _extract_t(b_arr, t_axis, t)
        if a_vol.max() == 0 or b_vol.max() == 0:
            print(f"[PROGRESS] {t + 1}/{n_t}", flush=True)
            continue
        a_meshes = mesh_utils.build_label_meshes(a_vol, a_labels, scale)
        b_meshes = mesh_utils.build_label_meshes(b_vol, b_labels, scale)
        for al, (d, bl) in mesh_utils.nearest_surface(a_meshes, b_meshes).items():
            if d < min_dist[al]:
                min_dist[al] = d
                contact_id[al] = bl or 0
        log.log(f">> t{t}: {len(a_meshes)} A / {len(b_meshes)} B meshes")
        print(f"[PROGRESS] {t + 1}/{n_t}", flush=True)

    labels = sorted(a_labels)
    dist_arr = np.array([min_dist[l] if np.isfinite(min_dist[l]) else np.nan for l in labels])
    contact = (dist_arr <= max_dist).astype(float)
    df = pd.DataFrame({
        "label": labels,
        f"{pop_type}.cell.contact#{target}": contact,
        f"{pop_type}.cell.min_distance#{target}": dist_arr,
        f"{pop_type}.cell.contact_id#{target}": [float(contact_id[l]) for l in labels],
    })
    LabelPropsView(props).add_obs(df).save()

    n_contact = int(np.nansum(contact))
    log.log(f">> contactsMeshes: {n_contact}/{len(labels)} A cells contact {target} (≤{max_dist}µm)")
    if qc_path is not None:
        with open(qc_path, "w") as f:
            json.dump({"nCellsA": len(labels), "nContacts": n_contact,
                       "fracInContact": (n_contact / len(labels)) if labels else 0.0}, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
