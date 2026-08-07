"""Does optical flow do anything for the prob head, or is intensity doing all the work?

The prob head is supervised by IntensityLoss + ForegroundLoss, both brightness-derived. Flow enters
only as input CHANNELS (15 metric planes) and via the temporal embedding loss. So "the prob head
looks right" is not evidence that flow contributed. Ablate it and see.

  full        frame + 15 flow metrics, temporal_weight=2.0   <- the current best (88/83 objects)
  no-temporal frame + 15 flow metrics, temporal_weight=0.0   <- is the temporal LOSS doing anything?
  no-flow     frame only, temporal_weight=0.0                <- is flow doing anything AT ALL?

If `no-flow` matches `full`, optical flow is decoration on this data and the segmenter is an
intensity model with extra steps. Same seed, same input, same epochs, same inference params --
only the flow contribution changes.
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
TRAIN_T, TEST_T = 2, 24

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
# flow removed by handing over metric dicts with no planes -- the model's input channel count is
# 1 + len(metrics), so this is a genuine ablation of the flow inputs, not a zeroing of them
tm_none = [{} for _ in tm]
print(f"== {IMG} mem-TOM z={zc} sigma={SIGMA_PX} px | metrics {len(tm[0])} -> ablated to 0",
      flush=True)


def baseline(img):
    b = filters.gaussian(img, sigma=(2.0 / PX), preserve_range=True)
    fg = ndimage.binary_fill_holes(morphology.remove_small_objects(
        b > filters.threshold_triangle(b[b > 0]), int(np.pi * (3.0 / PX) ** 2 / 4)))
    d = ndimage.distance_transform_edt(fg)
    mk = measure.label(morphology.h_maxima(d, h=(2.0 / PX)))
    return segmentation.watershed(-d, mk, mask=fg)


def nobj(lab):
    return len([p for p in measure.regionprops(np.asarray(lab)) if p.area > 0])


ARMS = [("full (flow + temporal loss)", tm, 2.0),
        ("flow inputs, no temporal loss", tm, 0.0),
        ("NO flow at all", tm_none, 0.0)]

results = {}
for name, metrics, tw in ARMS:
    print(f"\n--- {name}", flush=True)
    model = train_with_metrics(frames_prep, metrics, variance_metrics_norm=None,
                              num_epochs=EPOCHS, intensity_weight=1.0, foreground_weight=1.0,
                              temporal_weight=tw, confetti_weight=0.0,
                              variance_as_input=False, seed=42, device="cuda")
    if isinstance(model, tuple):
        model = model[0]
    inf = LearnedAffinityInference(
        model=model, device="cuda", affinity_threshold=0.5, merge_affinity_threshold=0.65,
        merge_max_distance=1.5, prob_weight=0.3, seed_size=12, prob_threshold=0.3,
        embedding_blur_sigma=1.5, prob_blur_sigma=1.5, max_iter=200, min_component_size=20)
    per_t = {}
    for t in (TRAIN_T, TEST_T):
        prob, instances, _ = inf.predict_frame(frames_prep[t], metrics[t])
        prob, instances = np.asarray(prob), np.asarray(instances)
        n_probbig = measure.label(morphology.remove_small_objects(prob > 0.3, 50)).max()
        per_t[t] = (prob, instances, int(n_probbig), nobj(instances))
    results[name] = per_t

print(f"\n{'arm':>31} | {'prob ≥50px t2':>13} {'t24':>5} | {'instances t2':>12} {'t24':>5}")
print("-" * 76)
for name, _, _ in ARMS:
    r = results[name]
    print(f"{name:>31} | {r[TRAIN_T][2]:>13} {r[TEST_T][2]:>5} | "
          f"{r[TRAIN_T][3]:>12} {r[TEST_T][3]:>5}", flush=True)
b2, b24 = nobj(baseline(sm[TRAIN_T])), nobj(baseline(sm[TEST_T]))
print(f"{'intensity baseline':>31} | {'—':>13} {'—':>5} | {b2:>12} {b24:>5}")

fig, ax = plt.subplots(len(ARMS), 4, figsize=(19, 4.6 * len(ARMS)))
for i, (name, _, _) in enumerate(ARMS):
    for j, t in enumerate((TRAIN_T, TEST_T)):
        prob, instances, npb, ni = results[name][t]
        disp = exposure.rescale_intensity(sm[t], in_range=tuple(np.percentile(sm[t], (1, 99.7))))
        ax[i, 2 * j].imshow(prob, cmap="magma", vmin=0, vmax=1)
        ax[i, 2 * j].set_title(f"{name}\nt={t} prob head · {npb} comps ≥50px", fontsize=8)
        ax[i, 2 * j + 1].imshow(disp, cmap="gray")
        ax[i, 2 * j + 1].contour(segmentation.find_boundaries(instances, mode="thick"),
                                 levels=[0.5], colors="yellow", linewidths=0.5)
        ax[i, 2 * j + 1].set_title(f"t={t} instances · {ni} "
                                   f"(baseline {b2 if t == TRAIN_T else b24})", fontsize=8)
    for a in ax[i]:
        a.axis("off")
fig.suptitle("Does optical flow do anything for the prob head? — fXgbTl mem-TOM, σ=3 px, same seed\n"
             "if the bottom row matches the top, flow is decoration and this is an intensity model",
             fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.94))
fig.savefig(f"{OUT}/flow_6_ablation_{IMG}.png", dpi=110)
print(f"\nfigure: {OUT}/flow_6_ablation_{IMG}.png   total {time.time()-t0:.0f}s")
