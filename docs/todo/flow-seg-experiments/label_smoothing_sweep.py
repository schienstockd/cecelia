"""How much `labelSmoothing` is "a tiny tiny bit"?

The outlines came out wrinkled — high-frequency boundary noise on an otherwise good mask. Smoothing
is cheap to add and easy to overdo, and it is NOT free: it changes every measured shape descriptor
(area, perimeter, solidity, circularity), so the sigma has to be chosen against what it costs, not
by eye alone.

So this sweeps sigma over the REAL labels the task wrote and reports both sides: how much boundary
roughness goes away (perimeter at fixed area — the scale-free roughness measure) against how much
area moves. A smoother that shifts area is quietly resizing cells.
"""
import json
import sys

sys.path.insert(0, "/home/dominik/cc-workspace/coastal")

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from skimage import measure, segmentation, exposure

from cecelia.utils import zarr_utils
from cecelia.utils.segmentation_utils import SegmentationUtils

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM, VALUE_NAME, T = "fXgbTl", 2, "coastalTest", 15
SIGMAS = [0.0, 0.5, 1.0, 1.5, 2.0, 3.0]


class _Stub(SegmentationUtils):
    def predict_slice(self, *a, **k):
        raise NotImplementedError


px = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
img_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidTemporalSmoothed.ome.zarr", as_dask=True)
lab_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/1/{IMG}/labels/{VALUE_NAME}.zarr", as_dask=True)
zc = img_levels[0].shape[2] // 2
frame = np.asarray(img_levels[0][T, CH_TOM, zc]).astype(np.float32)
plane = np.asarray(lab_levels[0][T, zc]).astype(np.uint32)
disp = exposure.rescale_intensity(frame, in_range=tuple(np.percentile(frame, (1, 99.7))))
print(f"{IMG} t={T} z={zc}: {len(np.unique(plane)) - 1} labels on this plane", flush=True)

seg = _Stub({'taskDir': '/tmp'}, None)


def stats(lab):
    """Roughness = perimeter / perimeter of a circle with the same area. 1.0 = perfectly round."""
    props = [p for p in measure.regionprops(lab) if p.area > 4]
    rough = [p.perimeter / (2 * np.sqrt(np.pi * p.area)) for p in props if p.perimeter > 0]
    return np.array([p.area for p in props]), np.array(rough)


base_area, base_rough = stats(plane)
rows = []
smoothed = {}
for s in SIGMAS:
    out = plane if s == 0 else seg._smooth_labels(plane, s, False)
    smoothed[s] = out
    a, r = stats(out)
    n = len(np.unique(out)) - 1
    # area change is measured on the SET of labels, since a label must never disappear
    d_area = 100 * (a.sum() - base_area.sum()) / base_area.sum()
    rows.append((s, n, np.median(r), d_area))
    print(f"  sigma {s:>3}: {n:>3} labels · roughness {np.median(r):.3f} · area {d_area:+.1f}%",
          flush=True)

fig = plt.figure(figsize=(17.5, 6.6))
gs = fig.add_gridspec(2, len(SIGMAS) // 2 + 1)
for i, s in enumerate(SIGMAS):
    ax = fig.add_subplot(gs[i % 2, i // 2])
    ax.imshow(disp, cmap="gray")
    ax.contour(segmentation.find_boundaries(smoothed[s], mode="thick"),
               levels=[0.5], colors="#ffe000", linewidths=0.7)
    _, n, rough, da = rows[i]
    ax.set_title(f"σ = {s}" + ("  (current)" if s == 0 else "") +
                 f"\nroughness {rough:.3f} · area {da:+.1f}%", fontsize=8)
    ax.set_xticks([]); ax.set_yticks([])

ax = fig.add_subplot(gs[:, -1])
ss = [r[0] for r in rows]
ax.plot(ss, [r[2] for r in rows], "o-", color="tab:blue", label="boundary roughness")
ax.set_xlabel("labelSmoothing σ (px)", fontsize=9)
ax.set_ylabel("perimeter / circle of same area", color="tab:blue", fontsize=9)
ax.tick_params(axis="y", labelcolor="tab:blue")
ax.grid(alpha=.3)
ax2 = ax.twinx()
ax2.plot(ss, [r[3] for r in rows], "s--", color="tab:red")
ax2.set_ylabel("area change (%)", color="tab:red", fontsize=9)
ax2.tick_params(axis="y", labelcolor="tab:red")
ax.set_title("what each σ buys, and what it costs", fontsize=9)

fig.suptitle(f"labelSmoothing on the real {VALUE_NAME} labels — {IMG} mem-TOM, t={T}, mid-z\n"
             f"roughness 1.0 = a perfect circle; area change is the cost of the smoothing",
             fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.90))
fig.savefig(f"{OUT}/flow_16_label_smoothing_{IMG}.png", dpi=110)
print(f"\nfigure: {OUT}/flow_16_label_smoothing_{IMG}.png")
