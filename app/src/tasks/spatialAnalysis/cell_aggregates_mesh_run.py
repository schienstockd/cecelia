"""
spatialAnalysis.aggregatesMeshes (image-scope) — Python runner (MESH route).

Mesh-based aggregate detection: a population's cells whose SURFACE-to-surface distance is within a
threshold are linked into a proximity graph; connected components of ≥ minCells are aggregates (legacy
cellClustersMeshes). The surface counterpart to the fast points DBSCAN route (detectAggregates), mostly
for live images (docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 11). Meshes built on the fly per timepoint.

Membership + label-zarr path come from Julia (aggregatesMeshes.jl). Params: imPath, labelPath, labels
(member ids), physicalSizes [sz,sy,sx], maxClusterDist (µm), minCells, popType, propsPath, qcOutPath.
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
    labels    = set(int(x) for x in script_utils.get_param(params, "labels", default=[]))
    phys      = np.asarray(script_utils.get_param(params, "physicalSizes", default=[1.0, 1.0, 1.0]), dtype=float)
    max_dist  = float(script_utils.get_param(params, "maxClusterDist", default=5.0))
    min_cells = int(script_utils.get_param(params, "minCells", default=5))
    pop_type  = script_utils.get_param(params, "popType", default="flow")
    props     = params["propsPath"]
    qc_path   = params.get("qcOutPath")

    if not labels:
        log.log("[ERROR] aggregatesMeshes: empty population")
        return

    dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
    dim_utils.calc_image_dimensions(zarr_utils.open_as_zarr(im_path)[0][0].shape)
    label_order = [ax for ax in dim_utils.im_dim_order if ax != "C"]
    t_axis = label_order.index("T") if "T" in label_order else None
    n_t = int(dict(zip(dim_utils.im_dim_order, dim_utils.im_dim)).get("T", 1))
    n_spatial = len(label_order) - (1 if t_axis is not None else 0)
    scale = phys[-n_spatial:]

    arr = zarr_utils.open_as_zarr(params["labelPath"])[0][0]
    agg_of = {l: 0 for l in labels}
    offset = 0
    for t in range(n_t):
        vol = _extract_t(arr, t_axis, t)
        if vol.max() == 0:
            print(f"[PROGRESS] {t + 1}/{n_t}", flush=True)
            continue
        meshes = mesh_utils.build_label_meshes(vol, labels, scale)
        ids = mesh_utils.mesh_aggregates(meshes, max_dist, min_cells)
        n_here = max(ids.values(), default=0)
        for lb, aid in ids.items():
            if aid > 0:
                agg_of[lb] = aid + offset
        offset += n_here
        log.log(f">> t{t}: {len(meshes)} meshes, {n_here} aggregate(s)")
        print(f"[PROGRESS] {t + 1}/{n_t}", flush=True)

    lbls = sorted(labels)
    ids = np.array([agg_of[l] for l in lbls], dtype=float)
    is_agg = (ids > 0).astype(float)
    df = pd.DataFrame({
        "label": lbls,
        f"{pop_type}.cell.is.aggregate": is_agg,
        f"{pop_type}.cell.aggregate.id": ids,
    })
    LabelPropsView(props).add_obs(df).save()

    n_agg = int(len({i for i in agg_of.values() if i > 0}))
    frac = float(is_agg.sum() / len(lbls)) if lbls else 0.0
    log.log(f">> aggregatesMeshes: {n_agg} aggregate(s), {int(is_agg.sum())}/{len(lbls)} cells aggregated")
    if qc_path is not None:
        with open(qc_path, "w") as f:
            json.dump({"nCells": len(lbls), "nAggregates": n_agg, "fracAggregated": frac}, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
