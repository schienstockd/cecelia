"""Where do the 162 fragments come from — the prob head, or the region growing?

The retrain (spatial sigma=1 px, float32 flow, intensity_weight=1.0) still returns ~5x too many
objects, and every INPUT-side correction left it unchanged. Two very different causes:

  A. the prob head itself is fragmented   -> the learned representation is the problem
  B. prob is fine, region growing shreds it -> inference is the problem, flow is exonerated

coastal's docs claim B ("the prob head resolves cells, but sits on a 1-3 px noise floor ... this,
not the inference parameters, is why ~86% of detections are fragments"). Worth seeing rather than
believing, because the fix is completely different in each case.

Also sweeps the spatial sigma the training never explored. At sigma=1 px background |v| is 2.95 px
against 2.64 px inside cells -- flow magnitude does not separate cell from background AT ALL there
(AUC 0.57). At sigma=3 px background |v| collapses to 0.17 but the estimator starts underestimating
a known 1.0 px shift as 0.69. The useful window, if there is one, is in between and was skipped.

mem-TOM here is germinal-centre B cells: motile, ~2.95 um/min measured at 15 s. So flow SHOULD have
something to say, unlike the Kat-channel macrophages the earlier sessile measurements were about.
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
from coastal.flow import (prepare_data_for_unet, normalize_and_project,
                          calc_flow_farneback_between_frames)
from coastal.train import train_with_metrics
from coastal.segment import LearnedAffinityInference

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM = "fXgbTl", 2
SCALES, CUMWIN, EPOCHS = [1, 2, 4, 8], 5, 30
TRAIN_T, TEST_T = 2, 24
SIGMAS = [1.0, 1.5, 2.0, 3.0]        # px; the 1-3 window the training never explored

t0 = time.time()
levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidDriftCorrected.ome.zarr", as_dask=True)
arr = levels[0]
T, C, Z, Y, X = arr.shape
PX = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
zc = Z // 2
raw = np.asarray(arr[:, CH_TOM, zc]).astype(np.float32)
print(f"== {IMG} mem-TOM z={zc} {raw.shape}  px {PX:.4f}", flush=True)


def cellmask(img):
    b = filters.gaussian(img, sigma=(2.0 / PX), preserve_range=True)
    return morphology.remove_small_objects(b > filters.threshold_triangle(b[b > 0]),
                                           int(np.pi * (3.0 / PX) ** 2 / 2))


def baseline(img):
    b = filters.gaussian(img, sigma=(2.0 / PX), preserve_range=True)
    fg = ndimage.binary_fill_holes(morphology.remove_small_objects(
        b > filters.threshold_triangle(b[b > 0]), int(np.pi * (3.0 / PX) ** 2 / 4)))
    d = ndimage.distance_transform_edt(fg)
    mk = measure.label(morphology.h_maxima(d, h=(2.0 / PX)))
    return segmentation.watershed(-d, mk, mask=fg)


def nobj(lab):
    return len([p for p in measure.regionprops(np.asarray(lab)) if p.area > 0])


# ── does flow separate cell from background at each sigma? ────────────────────────────────
cm = cellmask(np.stack([filters.gaussian(f, sigma=1.0, preserve_range=True)
                        for f in raw[max(0, TRAIN_T - 4):TRAIN_T + 5]]).mean(axis=0))
bg = ~ndimage.binary_dilation(cm, iterations=int(3.0 / PX))
print(f"\n{'sigma px':>9} | {'|v| cell':>9} {'|v| bg':>8} {'ratio':>6} | what it means")
print("-" * 72)
sep = {}
for s in SIGMAS:
    a = filters.gaussian(raw[TRAIN_T], sigma=s, preserve_range=True)
    b = filters.gaussian(raw[TRAIN_T + 1], sigma=s, preserve_range=True)
    u, v = calc_flow_farneback_between_frames(a, b)
    mag = np.hypot(u, v)
    mc, mb = float(np.median(mag[cm])), float(np.median(mag[bg]))
    sep[s] = mc / max(mb, 1e-9)
    print(f"{s:>9.1f} | {mc:>9.3f} {mb:>8.3f} {sep[s]:>6.2f} | "
          f"{'flow is cell-specific' if sep[s] > 2 else 'background flows as much as cells'}",
          flush=True)

BEST = max(SIGMAS, key=lambda s: sep[s])
print(f"\nbest cell/background flow contrast at sigma = {BEST} px "
      f"(training used 1.0)", flush=True)

# ── retrain at that sigma, then open the black box ────────────────────────────────────────
sm = np.stack([filters.gaussian(f, sigma=BEST, preserve_range=True) for f in raw])
_, frames_proj = normalize_and_project(sm[:, None])
frames_prep, _, _, temporal_metrics = prepare_data_for_unet(
    frames_proj, temporal_scales=SCALES, cumulative_window=CUMWIN, verbose=False)
tm = list(temporal_metrics)

print(f"\nretraining at sigma={BEST} px, {EPOCHS} epochs...", flush=True)
model = train_with_metrics(frames_prep, tm, variance_metrics_norm=None, num_epochs=EPOCHS,
                           intensity_weight=1.0, foreground_weight=1.0, temporal_weight=2.0,
                           confetti_weight=0.0, variance_as_input=False, device="cuda")
if isinstance(model, tuple):
    model = model[0]

inf = LearnedAffinityInference(
    model=model, device="cuda", affinity_threshold=0.5, merge_affinity_threshold=0.65,
    merge_max_distance=1.5, prob_weight=0.3, seed_size=12, prob_threshold=0.3,
    embedding_blur_sigma=1.5, prob_blur_sigma=1.5, max_iter=200, min_component_size=20)

fig, ax = plt.subplots(2, 4, figsize=(20, 10))
for i, t in enumerate((TRAIN_T, TEST_T)):
    prob, instances, _ = inf.predict_frame(frames_prep[t], tm[t])
    prob, instances = np.asarray(prob), np.asarray(instances)
    base = baseline(sm[t])
    # A vs B: how many pieces does prob ALONE break into, before region growing?
    prob_cc = measure.label(prob > 0.3)
    prob_big = morphology.remove_small_objects(prob > 0.3, 50)
    n_prob, n_probbig, n_inst, n_base = (prob_cc.max(), measure.label(prob_big).max(),
                                         nobj(instances), nobj(base))
    print(f"t={t:>3} | prob>0.3 components {n_prob:>4} (>=50 px: {n_probbig:>3}) | "
          f"instances {n_inst:>4} | baseline {n_base:>3}", flush=True)

    disp = exposure.rescale_intensity(sm[t], in_range=tuple(np.percentile(sm[t], (1, 99.7))))
    ax[i, 0].imshow(disp, cmap="gray")
    ax[i, 0].set_title(f"t={t} {'(trained)' if t == TRAIN_T else '(HELD OUT)'}  σ={BEST} px", fontsize=9)
    ax[i, 1].imshow(prob, cmap="magma", vmin=0, vmax=1)
    ax[i, 1].set_title(f"prob head\n{n_prob} components >0.3 ({n_probbig} of ≥50 px)", fontsize=9)
    ax[i, 2].imshow(disp, cmap="gray")
    ax[i, 2].contour(segmentation.find_boundaries(instances, mode="thick"), levels=[0.5],
                     colors="yellow", linewidths=0.5)
    ax[i, 2].set_title(f"after region growing\n{n_inst} instances", fontsize=9)
    ax[i, 3].imshow(disp, cmap="gray")
    ax[i, 3].contour(segmentation.find_boundaries(base, mode="thick"), levels=[0.5],
                     colors="red", linewidths=0.5)
    ax[i, 3].set_title(f"intensity baseline\n{n_base} objects", fontsize=9)
    for a in ax[i]:
        a.axis("off")

fig.suptitle(f"Where the fragments are made — {IMG} mem-TOM (motile GC B cells), σ={BEST} px\n"
             f"if prob is already shredded the representation is at fault; if prob is clean and "
             f"instances are not, region growing is", fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.92))
fig.savefig(f"{OUT}/flow_5_where_fragments_{IMG}.png", dpi=110)
print(f"\nfigure: {OUT}/flow_5_where_fragments_{IMG}.png   total {time.time()-t0:.0f}s")
