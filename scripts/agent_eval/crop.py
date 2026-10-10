"""Real-data fixture tier (docs/todo/AGENT_OVERNIGHT_PLAN.md): a small window of an existing analysed
image, with the user's own tracks inside it as the REFERENCE — a comparison point, not ground truth.

Reads the source project (store via `zarr_utils`, tracks via `LabelPropsView`) and writes only under
`--out`: `<name>.ome.tif` (all channels, the window), `<name>.gt.json` (reference rows: track_id as
`cell`, frame, z/y/x in window pixels) and `fixture.json`, the same layout `fixture.py` writes, so
`setup.py --fixture DIR` imports it unchanged. The window is placed where the reference has the most
tracked cell-frames.

    pixi run python scripts/agent_eval/crop.py --project-dir ~/…/projects/zolIMa --image fXgbTl \\
        --reference-vn flowTom --out /tmp/agent-crop
"""
from __future__ import annotations

import argparse
import json
import pathlib

import numpy as np
import tifffile

from cecelia.utils import vn_versioning, zarr_utils
from cecelia.utils.label_props_utils import LabelPropsView
from cecelia.utils.atomic_io import write_json_atomic


def best_window(ref, shape_zyx, size, nz, nt, step=16):
    """(t0, z0, y0, x0) maximising the reference rows that fall inside the window."""
    Z, Y, X = shape_zyx
    best, arg = -1, (0, 0, 0, 0)
    t_max = int(ref["t"].max()) if len(ref) else 0
    for t0 in range(0, max(1, t_max - nt + 2), 2):
        rt = ref[(ref["t"] >= t0) & (ref["t"] < t0 + nt)]
        for z0 in range(0, max(1, Z - nz + 1), 2):
            rz = rt[(rt["z"] >= z0) & (rt["z"] < z0 + nz)]
            for y0 in range(0, max(1, Y - size + 1), step):
                ry = rz[(rz["y"] >= y0) & (rz["y"] < y0 + size)]
                for x0 in range(0, max(1, X - size + 1), step):
                    n = int(((ry["x"] >= x0) & (ry["x"] < x0 + size)).sum())
                    if n > best:
                        best, arg = n, (t0, z0, y0, x0)
    return arg


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--project-dir", required=True)
    ap.add_argument("--image", required=True)
    ap.add_argument("--reference-vn", required=True)
    ap.add_argument("--value-name", default="default", help="image version to crop")
    ap.add_argument("--out", required=True)
    ap.add_argument("--size", type=int, default=128)
    ap.add_argument("--z", type=int, default=12)
    ap.add_argument("--frames", type=int, default=20)
    a = ap.parse_args(argv)

    proj = pathlib.Path(a.project_dir).expanduser()
    meta_dir = proj / "1" / a.image
    with open(meta_dir / "ccid.json", encoding="utf-8") as f:
        ccid = json.load(f)
    store = str(proj / "0" / a.image / vn_versioning.versioned_get_field_at(ccid, "filepath", a.value_name))
    axes = [x.lower() for x in zarr_utils.read_axes(store)]
    if axes != ["t", "c", "z", "y", "x"]:
        raise SystemExit(f"expected a t,c,z,y,x store, got {axes}")
    scale = dict(zip(axes, zarr_utils.read_scale(store)))
    levels, _ = zarr_utils.open_as_zarr(store)
    level0 = levels[0]

    ref_fn = vn_versioning.versioned_get_field_at(ccid, "label_props", a.reference_vn)
    if not ref_fn:
        raise SystemExit(f"no label set {a.reference_vn!r} on {a.image}")
    ref = (LabelPropsView(str(meta_dir / "labelProps" / ref_fn))
           .view_centroid_cols().as_df()
           .rename(columns={"centroid_t": "t", "centroid_z": "z", "centroid_y": "y", "centroid_x": "x"}))
    ref = ref[ref["track_id"].notna()]
    t0, z0, y0, x0 = best_window(ref, level0.shape[2:], a.size, a.z, a.frames)
    win = level0[t0:t0 + a.frames, :, z0:z0 + a.z, y0:y0 + a.size, x0:x0 + a.size]
    image = np.asarray(win)

    out = pathlib.Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    name = f"{a.image}-crop"
    channels = ccid.get("imChannelNames", {}).get("default") or [f"ch{i}" for i in range(image.shape[1])]
    tifffile.imwrite(out / f"{name}.ome.tif", image, photometric="minisblack", ome=True,
                     metadata={"axes": "TCZYX", "Channel": {"Name": list(channels)},
                               "PhysicalSizeX": scale["x"], "PhysicalSizeXUnit": "µm",
                               "PhysicalSizeY": scale["y"], "PhysicalSizeYUnit": "µm",
                               "PhysicalSizeZ": scale["z"], "PhysicalSizeZUnit": "µm",
                               "TimeIncrement": scale["t"], "TimeIncrementUnit": "s"})
    inside = ref[(ref["t"] >= t0) & (ref["t"] < t0 + a.frames) & (ref["z"] >= z0) & (ref["z"] < z0 + a.z)
                 & (ref["y"] >= y0) & (ref["y"] < y0 + a.size) & (ref["x"] >= x0) & (ref["x"] < x0 + a.size)]
    rows = [{"cell": int(r.track_id), "t": int(r.t - t0), "z": float(r.z - z0),
             "y": float(r.y - y0), "x": float(r.x - x0)} for r in inside.itertuples()]
    spec = {"radius_px": 3.0 / scale["x"],          # match within 3 µm of a reference centroid
            "z_scale": scale["z"] / scale["x"], "px_um": scale["x"], "dt_s": scale["t"]}
    source = {"image": a.image, "valueName": a.value_name, "referenceVn": a.reference_vn,
              "window": {"t0": t0, "z0": z0, "y0": y0, "x0": x0, "size": a.size, "z": a.z, "frames": a.frames}}
    write_json_atomic(out / f"{name}.gt.json",
                      {"name": name, "kind": "reference", "source": source, "spec": spec, "rows": rows})
    manifest = {"kind": "reference-crop", "source": source, "spec": spec,
                "images": [{"name": name, "tif": str(out / f"{name}.ome.tif"), "gt": str(out / f"{name}.gt.json")}]}
    write_json_atomic(out / "fixture.json", manifest, indent=2)
    print(json.dumps({"out": str(out), "window": source["window"], "shape": list(image.shape),
                      "mb": round(image.nbytes / 1e6, 1), "reference_rows": len(rows),
                      "reference_tracks": len({r["cell"] for r in rows})}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
