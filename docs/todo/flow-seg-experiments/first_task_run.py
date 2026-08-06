"""What the first real `segment.coastal` run actually produced.

6220 objects over 31 timepoints is ~200 per frame, which a count alone cannot tell apart from
"200 cells" vs "30 cells shattered into fragments". Only the picture and the size distribution can,
so this renders both — plus the intensity baseline the tuning work has been scored against all along.
"""
import json
import sys

sys.path.insert(0, "/home/dominik/cc-workspace/coastal")

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy import ndimage
from skimage import filters, morphology, measure, segmentation, exposure

from cecelia.utils import zarr_utils

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM = "fXgbTl", 2
SHOW_T = [2, 15, 28]

px = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
img_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidTemporalSmoothed.ome.zarr", as_dask=True)
lab_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/1/{IMG}/labels/coastalTest.zarr", as_dask=True)
im, lab = img_levels[0], lab_levels[0]
print("image", im.shape, "labels", lab.shape, f"px={px:.4f} um")
zc = im.shape[2] // 2


def baseline(frame):
    """The intensity watershed every tuning run has been scored against."""
    b = filters.gaussian(frame, sigma=(2.0 / px), preserve_range=True)
    fg = ndimage.binary_fill_holes(morphology.remove_small_objects(
        b > filters.threshold_triangle(b[b > 0]), int(np.pi * (3.0 / px) ** 2 / 4)))
    d = ndimage.distance_transform_edt(fg)
    return segmentation.watershed(-d, measure.label(morphology.h_maxima(d, h=(2.0 / px))), mask=fg)


# ── 3D object sizes over the whole movie ──────────────────────────────────────────────────
vox_um3 = px * px * 2.0            # z step 2.0 um (anisotropic, see the plan)
diams, zext, per_t = [], [], []
for t in range(lab.shape[0]):
    vol = np.asarray(lab[t])
    props = measure.regionprops(vol)
    per_t.append(len(props))
    for p in props:
        diams.append(2 * (3 * p.area * vox_um3 / (4 * np.pi)) ** (1 / 3))
        zext.append(p.bbox[3] - p.bbox[0])
diams, zext = np.array(diams), np.array(zext)
print(f"{len(diams)} objects, median {np.median(diams):.1f} um, median z-extent {np.median(zext):.1f}")

fig = plt.figure(figsize=(16.5, 9.4))
gs = fig.add_gridspec(3, len(SHOW_T) + 1, width_ratios=[1] * len(SHOW_T) + [0.95])

for r, (title, source) in enumerate([("coastal (3D, mid-z)", "coastal"),
                                     ("intensity baseline (2D)", "baseline"),
                                     ("raw mem-TOM", "raw")]):
    for c, t in enumerate(SHOW_T):
        frame = np.asarray(im[t, CH_TOM, zc]).astype(np.float32)
        disp = exposure.rescale_intensity(frame, in_range=tuple(np.percentile(frame, (1, 99.7))))
        ax = fig.add_subplot(gs[r, c])
        ax.imshow(disp, cmap="gray")
        if source == "coastal":
            plane = np.asarray(lab[t, zc])
            n3d = per_t[t]
            ax.contour(segmentation.find_boundaries(plane, mode="thick"), levels=[0.5],
                       colors="yellow", linewidths=0.5)
            ax.set_title(f"t={t}: {len(np.unique(plane)) - 1} on this plane · {n3d} in 3D",
                         fontsize=8)
        elif source == "baseline":
            b = baseline(frame)
            ax.contour(segmentation.find_boundaries(b, mode="thick"), levels=[0.5],
                       colors="red", linewidths=0.5)
            ax.set_title(f"t={t}: {len(measure.regionprops(b))} objects", fontsize=8)
        else:
            ax.set_title(f"t={t}", fontsize=8)
        if c == 0:
            ax.set_ylabel(title, fontsize=9)
        ax.set_xticks([]); ax.set_yticks([])

ax = fig.add_subplot(gs[0, -1])
ax.hist(diams, bins=60, range=(0, 25), color="tab:blue")
ax.axvline(11.0, color="red", ls="--", lw=1)
ax.text(11.3, ax.get_ylim()[1] * 0.85, "11 µm\n(expected cell)", color="red", fontsize=7)
ax.set_xlabel("equivalent sphere diameter (µm)", fontsize=8)
ax.set_ylabel("objects", fontsize=8)
ax.set_title(f"all {len(diams)} objects · median {np.median(diams):.1f} µm", fontsize=8)

ax = fig.add_subplot(gs[1, -1])
ax.hist(zext, bins=np.arange(0.5, 20.5), color="tab:green")
ax.set_xlabel("z extent (planes)", fontsize=8)
ax.set_ylabel("objects", fontsize=8)
ax.set_title(f"median {np.median(zext):.1f} planes ({np.median(zext) * 2.0:.0f} µm)", fontsize=8)

ax = fig.add_subplot(gs[2, -1])
ax.plot(per_t, "o-", ms=3, color="tab:blue", label="coastal 3D")
ax.set_xlabel("timepoint", fontsize=8)
ax.set_ylabel("objects", fontsize=8)
ax.set_title(f"per timepoint · total {sum(per_t)}", fontsize=8)
ax.grid(alpha=.3); ax.legend(fontsize=7)

fig.suptitle(
    f"First end-to-end run of segment.coastal — {IMG} mem-TOM, model trained by opticalFlow.train\n"
    f"31 T x 32 Z in 689 s · seed_blur 8, seed_size 12, prob 0.3, affinity 0.5, z-stitch 0.2",
    fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(f"{OUT}/flow_10_first_task_run_{IMG}.png", dpi=105)
print(f"figure: {OUT}/flow_10_first_task_run_{IMG}.png")
