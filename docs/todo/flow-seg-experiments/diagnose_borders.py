"""The label BORDERS are serrated — 1-2 px teeth all the way round, not gentle waviness.

The first smoothing sweep scored the MEDIAN object and concluded sigma 1.0 was enough. That
understated the problem: the complaint is about the objects with the worst borders, and a median
hides them by construction. So this looks at the distribution and at the worst offenders directly.

Roughness = perimeter / perimeter of a circle with the same area (scale-free; 1.0 = a perfect disc).
On a pixel grid even a perfect disc scores ~1.10 because the staircase itself has length, so that is
the floor, not 1.0 — worth knowing before reading any of these numbers as "still rough".
"""
import json
import sys

sys.path.insert(0, "/home/dominik/cc-workspace/coastal")

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from skimage import measure, segmentation, exposure, draw

from cecelia.utils import zarr_utils
from cecelia.utils.segmentation_utils import SegmentationUtils

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM, VALUE_NAME = "fXgbTl", 2, "coastalTest"
SIGMAS = [0.0, 1.0, 1.5, 2.0]


class _Stub(SegmentationUtils):
    def predict_slice(self, *a, **k):
        raise NotImplementedError


def roughness(p):
    return p.perimeter / (2 * np.sqrt(np.pi * p.area)) if p.area > 0 else np.nan


# the pixel-grid floor: a perfect disc of a typical cell radius
disc = np.zeros((80, 80), np.uint32)
rr, cc = draw.disk((40, 40), 16)
disc[rr, cc] = 1
FLOOR = roughness(measure.regionprops(disc)[0])
print(f"pixel-grid floor (a perfect 32 px disc): roughness {FLOOR:.3f}")

px = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
img_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidTemporalSmoothed.ome.zarr", as_dask=True)
lab_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/1/{IMG}/labels/{VALUE_NAME}.zarr", as_dask=True)
im, lab = img_levels[0], lab_levels[0]
T, nz = im.shape[0], im.shape[2]
seg = _Stub({'taskDir': '/tmp'}, None)

rough, where = [], []
for t in range(0, T, 3):                      # every 3rd frame: this is a distribution, not a census
    vol = np.asarray(lab[t])
    for z in range(0, nz, 2):
        pl = vol[z]
        if not pl.any():
            continue
        for p in measure.regionprops(pl):
            if p.area < 100:                  # below ~3.5 um across, shape is grid noise
                continue
            rough.append(roughness(p)); where.append((t, z, p.label))
rough = np.array(rough)
print(f"\n{len(rough)} cross-sections >=100 px")
for q in (50, 75, 90, 99):
    print(f"  p{q:<2} roughness {np.percentile(rough, q):.3f}")
print(f"  worst      {rough.max():.3f}")

# what each sigma does to the WORST decile, which is what the eye lands on
worst_idx = np.argsort(rough)[-max(1, len(rough) // 10):]
print("\nsigma sweep on the worst decile:")
for s in SIGMAS:
    vals = []
    for i in worst_idx:
        t, z, lb = where[i]
        pl = np.asarray(lab[t, z])
        m = (pl == lb).astype(np.uint32)
        out = m if s == 0 else seg._smooth_labels(m, s, False)
        props = measure.regionprops(out)
        if props:
            vals.append(roughness(props[0]))
    print(f"  sigma {s:>4}: median roughness {np.median(vals):.3f}")

# ── picture: the four worst borders, each at every sigma ─────────────────────────────────
show = np.argsort(rough)[-4:][::-1]
fig, axes = plt.subplots(len(show), len(SIGMAS) + 1, figsize=(3.0 * (len(SIGMAS) + 1), 3.1 * len(show)))
for r, i in enumerate(show):
    t, z, lb = where[i]
    frame = np.asarray(im[t, CH_TOM, z]).astype(np.float32)
    pl = np.asarray(lab[t, z])
    m = (pl == lb)
    ys, xs = np.where(m)
    pad = 12
    sl = (slice(max(0, ys.min() - pad), min(pl.shape[0], ys.max() + pad)),
          slice(max(0, xs.min() - pad), min(pl.shape[1], xs.max() + pad)))
    sub = frame[sl]
    disp = exposure.rescale_intensity(sub, in_range=tuple(np.percentile(sub, (1, 99.7))))
    axes[r, 0].imshow(disp, cmap="gray"); axes[r, 0].set_xticks([]); axes[r, 0].set_yticks([])
    axes[r, 0].set_ylabel(f"t={t} z={z}", fontsize=8)
    if r == 0:
        axes[r, 0].set_title("raw mem-TOM", fontsize=9)
    for c, s in enumerate(SIGMAS):
        mm = m[sl].astype(np.uint32)
        out = mm if s == 0 else seg._smooth_labels(mm, s, False)
        props = measure.regionprops(out)
        ax = axes[r, c + 1]
        ax.imshow(disp, cmap="gray")
        ax.contour(segmentation.find_boundaries(out, mode="thick"),
                   levels=[0.5], colors="#00e5ff", linewidths=1.0)
        ax.set_xticks([]); ax.set_yticks([])
        rr_ = roughness(props[0]) if props else np.nan
        ax.set_title((f"σ = {s}" + ("  (current)" if s == 0 else "")) +
                     f"\nroughness {rr_:.2f}", fontsize=8)

fig.suptitle(f"The border, not the shape — {IMG} {VALUE_NAME}, the 4 worst cross-sections\n"
             f"pixel-grid floor is {FLOOR:.2f}; p90 of all objects is "
             f"{np.percentile(rough, 90):.2f}, worst {rough.max():.2f}", fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(f"{OUT}/flow_17_borders_{IMG}.png", dpi=110)
print(f"\nfigure: {OUT}/flow_17_borders_{IMG}.png")
