"""Everything the model is fed, and what it returns.

16 input channels (1 smoothed frame + 15 flow metric planes), then the prob head, the instances
after region growing, and the intensity baseline for scale. One figure, so the inputs can be judged
by eye rather than by name -- several of the 15 metrics turn out to be visibly featureless, which no
summary statistic says.

Held-out frame by default (the model never saw it).
"""
import json
import sys
import time

sys.path.insert(0, "/home/dominik/cc-workspace/coastal")

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy import ndimage
from skimage import filters, morphology, measure, segmentation, exposure

from cecelia.utils import zarr_utils
from coastal.flow import prepare_data_for_unet, normalize_and_project
from coastal.train import train_with_metrics
from coastal.segment import LearnedAffinityInference

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM = "fXgbTl", 2
SIGMA_PX, SCALES, CUMWIN, EPOCHS = 3.0, [1, 2, 4, 8], 5, 30
SHOW_T = 24        # held out

t0 = time.time()
levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidDriftCorrected.ome.zarr", as_dask=True)
arr = levels[0]
PX = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
zc = arr.shape[2] // 2
raw = np.asarray(arr[:, CH_TOM, zc]).astype(np.float32)
sm = np.stack([filters.gaussian(f, sigma=SIGMA_PX, preserve_range=True) for f in raw])
_, frames_proj = normalize_and_project(sm[:, None])
frames_prep, _, _, temporal_metrics = prepare_data_for_unet(
    frames_proj, temporal_scales=SCALES, cumulative_window=CUMWIN, verbose=False)
tm = list(temporal_metrics)

print(f"== {IMG} mem-TOM z={zc} sigma={SIGMA_PX} px, showing t={SHOW_T} (held out)", flush=True)
model = train_with_metrics(frames_prep, tm, variance_metrics_norm=None, num_epochs=EPOCHS,
                          intensity_weight=1.0, foreground_weight=1.0, temporal_weight=2.0,
                          confetti_weight=0.0, variance_as_input=False, seed=42, device="cuda")
if isinstance(model, tuple):
    model = model[0]
inf = LearnedAffinityInference(
    model=model, device="cuda", affinity_threshold=0.5, merge_affinity_threshold=0.65,
    merge_max_distance=1.5, prob_weight=0.3, seed_size=12, prob_threshold=0.3,
    embedding_blur_sigma=1.5, prob_blur_sigma=1.5, max_iter=200, min_component_size=20)
prob, instances, _ = inf.predict_frame(frames_prep[SHOW_T], tm[SHOW_T])
prob, instances = np.asarray(prob), np.asarray(instances)

b = filters.gaussian(sm[SHOW_T], sigma=(2.0 / PX), preserve_range=True)
fg = ndimage.binary_fill_holes(morphology.remove_small_objects(
    b > filters.threshold_triangle(b[b > 0]), int(np.pi * (3.0 / PX) ** 2 / 4)))
d = ndimage.distance_transform_edt(fg)
base = segmentation.watershed(-d, measure.label(morphology.h_maxima(d, h=(2.0 / PX))), mask=fg)


def nobj(l):
    return len([p for p in measure.regionprops(np.asarray(l)) if p.area > 0])


# how much structure does each input plane actually carry? cell-vs-background contrast, using a
# mask from the frame itself (so this ranks INFORMATIVENESS, not correctness)
cm = morphology.remove_small_objects(b > filters.threshold_triangle(b[b > 0]),
                                     int(np.pi * (3.0 / PX) ** 2 / 2))
bgm = ~ndimage.binary_dilation(cm, iterations=int(3.0 / PX))


def contrast(f):
    f = np.abs(np.asarray(f, dtype=np.float64))
    lo, hi = float(np.median(f[bgm])), float(np.median(f[cm]))
    return hi / max(lo, 1e-9)


names = sorted(tm[SHOW_T])
planes = [("INPUT 0: frame (smoothed)", frames_prep[SHOW_T])] + \
         [(f"INPUT {i+1}: {n}", tm[SHOW_T][n]) for i, n in enumerate(names)]

print(f"\n{'input plane':>28} | {'cell/bg contrast':>16}")
print("-" * 50)
for nm, pl in planes:
    print(f"{nm.split(': ')[1]:>28} | {contrast(pl):>16.2f}", flush=True)

cols, rows = 5, 4
fig, ax = plt.subplots(rows, cols, figsize=(4.0 * cols, 4.15 * rows))
axf = ax.ravel()
for k, (nm, pl) in enumerate(planes):
    a = axf[k]
    pl = np.asarray(pl, dtype=np.float32)
    a.imshow(exposure.rescale_intensity(pl, in_range=tuple(np.percentile(pl, (1, 99)))),
             cmap="gray" if k == 0 else "viridis")
    a.set_title(f"{nm}\ncell/bg {contrast(pl):.2f}", fontsize=8)
    a.axis("off")

disp = exposure.rescale_intensity(sm[SHOW_T], in_range=tuple(np.percentile(sm[SHOW_T], (1, 99.7))))
axf[16].imshow(prob, cmap="magma", vmin=0, vmax=1)
axf[16].set_title(f"OUTPUT: prob head\n{measure.label(morphology.remove_small_objects(prob>0.3,50)).max()} comps ≥50px",
                  fontsize=8, color="darkred")
axf[17].imshow(disp, cmap="gray")
axf[17].contour(segmentation.find_boundaries(instances, mode="thick"), levels=[0.5],
                colors="yellow", linewidths=0.5)
axf[17].set_title(f"OUTPUT: after region growing\n{nobj(instances)} instances",
                  fontsize=8, color="darkred")
axf[18].imshow(disp, cmap="gray")
axf[18].contour(segmentation.find_boundaries(base, mode="thick"), levels=[0.5],
                colors="red", linewidths=0.5)
axf[18].set_title(f"reference: intensity baseline\n{nobj(base)} objects", fontsize=8)
axf[19].axis("off")
axf[19].text(0.02, 0.5,
             "cell/bg = median |value| inside cells\n÷ median outside, from an\nintensity-derived mask.\n\n"
             "It ranks how much each plane\nvaries with the cells — NOT\nwhether it is correct, and a\n"
             "plane near 1.0 is telling the\nmodel nothing.\n\n"
             f"σ={SIGMA_PX} px spatial, no temporal\nsmoothing. Lags 15/30/60/120 s.\n"
             f"t={SHOW_T}, held out.",
             fontsize=8.5, va="center", family="monospace")
for a in axf[16:19]:
    a.axis("off")
fig.suptitle(f"Everything the model is fed, and what it returns — {IMG} mem-TOM, t={SHOW_T} (held out)\n"
             f"16 input channels: 1 smoothed frame + 15 optical-flow metric planes", fontsize=12)
fig.tight_layout(rect=(0, 0, 1, 0.955))
fig.savefig(f"{OUT}/flow_7_model_io_{IMG}.png", dpi=100)
print(f"\nfigure: {OUT}/flow_7_model_io_{IMG}.png   total {time.time()-t0:.0f}s")
