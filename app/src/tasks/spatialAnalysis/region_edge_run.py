"""
spatialAnalysis.regionEdge (image-scope) — Python runner.

Every cell's distance to a region's boundary, and whether it is inside, with the region's shape drawn
from its own cells' centroids (`cecelia.utils.region_edge_utils`). Per frame on a timecourse; in 3D on
a stack (the boundary LINES are drawn per plane, from the 3D shape at that plane's depth).

Params (built in `regionEdge.jl`): imPath, propsPath, regionLabels (null = every cell — the tissue),
physicalSizes [sz,sy,sx] µm, closeRadius µm, fillHoles, distCol, insideCol (null = not written),
edgePath (the boundary file), qcOutPath.

The boundary file (`{image}/regionEdges/<name>.json`): `{closeRadius, step, fillHoles, lines}`, each
line `{t, plane, x, y}` in NATIVE PIXELS — the same space the centroids are stored in, scaled to µm by
the Julia reader on the way out (`region_edge_lines.jl`). `t` -1 on a still image, `plane` 0 in 2D.
"""

import numpy as np
import pandas as pd

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.region_edge_utils as reu
from cecelia.utils.label_props_utils import LabelPropsView, scale_centroids
from cecelia.utils.atomic_io import write_json_atomic


def _frame_extent(im_path, axes_zyx, sizes, coords_px, close_um, log):
    """``(extent_um, n_px)`` per centroid axis (``axes_zyx`` e.g. ``['y','x']``) from the image's level-0
    shape. Without a readable image store the frame is unknown: the cells' own extent plus twice the
    radius stands in, so no boundary touches it — every edge then counts, the frame rule cannot apply."""
    size_of = {"z": sizes[0], "y": sizes[1], "x": sizes[2]}
    try:
        shape = zarr_utils.open_as_zarr(im_path)[0][0].shape
        names = [a.lower() for a in (zarr_utils.read_axes(im_path) or [])]
        n_px = {a: int(shape[names.index(a)]) for a in axes_zyx}
    except Exception as e:          # noqa: BLE001 — any unreadable store means "frame unknown"
        log.log(f"[WARN] regionEdge: image store unreadable ({e}); the image border is not known, so every edge counts")
        n_px = {a: int(np.ceil(np.nanmax(coords_px[:, k]) + 2 * close_um / size_of[a])) + 1 for k, a in enumerate(axes_zyx)}
    return [n_px[a] * size_of[a] for a in axes_zyx], n_px


def _lines_for_frame(mask, step, axes, n_px, sizes, t):
    """Boundary lines of one frame's shape, native pixels. A 3D shape is cut at every image plane."""
    sy, sx = sizes[1], sizes[2]
    out = []
    if "z" in axes:
        sz = sizes[0]
        for k in range(n_px["z"]):
            g = min(int(np.floor(k * sz / step)), mask.shape[0] - 1)
            for c in reu.edge_contours(mask[g], step):
                out.append({"t": t, "plane": k, "y": (c[:, 0] / sy).round(2).tolist(), "x": (c[:, 1] / sx).round(2).tolist()})
    else:
        for c in reu.edge_contours(mask, step):
            out.append({"t": t, "plane": 0, "y": (c[:, 0] / sy).round(2).tolist(), "x": (c[:, 1] / sx).round(2).tolist()})
    return out


def run(params):
    log = script_utils.get_logfile_utils(params)
    sizes = [float(v) for v in script_utils.get_param(params, "physicalSizes", default=[1.0, 1.0, 1.0])]
    close_um = float(script_utils.get_param(params, "closeRadius", default=75.0))
    fill_holes = bool(script_utils.get_param(params, "fillHoles", default=True))   # resolved Julia-side
    region_labels = script_utils.get_param(params, "regionLabels", default=None)
    dist_col = params["distCol"]
    inside_col = script_utils.get_param(params, "insideCol", default=None)

    view = LabelPropsView(params["propsPath"]).only_centroid_cols()
    d = view.as_df(); ccols = view.centroid_columns(order=["z", "y", "x"]); tcols = view.temporal_columns(); view.close()
    if d.shape[0] == 0:
        log.log("[ERROR] regionEdge: the segmentation has no cells")
        return
    axes = [c.split("_")[1] for c in ccols]
    extent, n_px = _frame_extent(params["imPath"], axes, sizes, d[ccols].to_numpy(dtype=np.float64), close_um, log)
    if "z" in axes and n_px["z"] <= 1:          # a single plane stored with a z centroid: draw it in 2D
        ccols, axes = ccols[1:], axes[1:]
        extent = extent[1:]
        n_px.pop("z")
    coords = scale_centroids(d, sizes)[ccols].to_numpy(dtype=np.float64)
    labels = d["label"].to_numpy(dtype=np.int64)
    in_region = np.ones(len(labels), bool) if region_labels is None else np.isin(labels, np.asarray(region_labels, dtype=np.int64))
    frames = d[tcols[0]].to_numpy() if tcols and tcols[0] in d.columns else np.full(len(labels), -1.0)
    frame_ids = sorted(set(np.where(np.isfinite(frames), frames, -1).astype(int).tolist()))

    dist = np.full(len(labels), np.nan)
    inside = np.zeros(len(labels), bool)
    lines, pieces, small, step = [], [], [], reu.grid_step(close_um)
    for i, t in enumerate(frame_ids):
        here = np.where(np.isfinite(frames), frames, -1).astype(int) == t
        mask, step = reu.region_mask(coords[here & in_region], extent, close_um, fill_holes)
        ins, dd = reu.edge_distances(mask, step, coords[here])
        inside[here] = ins; dist[here] = dd
        pieces.append(reu.n_pieces(mask))
        small.append(reu.frac_in_small_pieces(mask, step, coords[here & in_region]))
        lines.extend(_lines_for_frame(mask, step, axes, n_px, sizes, t if tcols else -1))
        print(f"[PROGRESS] {i + 1}/{len(frame_ids)}", flush=True)

    cols = {"label": labels, dist_col: dist}
    if inside_col:
        cols[inside_col] = inside.astype(float)
    LabelPropsView(params["propsPath"]).add_obs(pd.DataFrame(cols)).save()
    write_json_atomic(params["edgePath"], {
        "closeRadius": close_um, "step": step, "fillHoles": fill_holes, "axes": axes, "lines": lines})

    n_region = int(in_region.sum())
    qc = {"nCells": int(len(labels)), "nRegionCells": n_region,
          "fracInside": float(inside.mean()) if len(labels) else 0.0,
          "fracNoBoundary": float(np.isnan(dist).mean()) if len(labels) else 0.0,
          "maxPieces": int(max(pieces, default=0)), "fracInSmallPieces": float(max(small, default=0.0)), "medianDist": float(np.nanmedian(dist)) if np.isfinite(dist).any() else None,
          "nLines": len(lines)}
    log.log(f">> regionEdge: {n_region} region cells, {qc['nCells']} cells measured, "
            f"{round(qc['fracInside'] * 100)}% inside, {qc['maxPieces']} piece(s), {len(lines)} boundary line(s)")
    if params.get("qcOutPath"):
        write_json_atomic(params["qcOutPath"], qc)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
