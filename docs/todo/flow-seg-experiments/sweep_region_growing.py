"""Is `seed_size` the reason the instance count is double the prob-component count?

Seeds are local maxima of the prob map in a seed_size x seed_size window
(`segment.py` -> `predict_frame`), and region growing can only EXPAND seeds -- two seeds inside one
cell can never merge back (`_merge_split_instances` is the only merge path and coastal's docs note
it is largely inert at the tuned params). coastal defaults to seed_size=12 px and documents [10-15].

These cells are ~11 um / 0.3315 um-per-px = ~33 px across, so the default window is about a THIRD of
a cell. Any internal texture in the prob map then puts several maxima in one cell.

Inference-only sweep: train once, vary the parameters. Scored on object count AND median equivalent
diameter, because a count alone cannot tell "correctly merged" from "over-merged into blobs" -- the
baseline gives ~30 objects at 10-12 um, which is the target to land on, not just the count.
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
SIGMA_PX, SCALES, CUMWIN, EPOCHS, SEED = 3.0, [1, 2, 4, 8], 5, 30, 42
TRAIN_T, TEST_T = 2, 24
DEAD = ("divergence", "vorticity", "flow_structure_alignment")
SEED_SIZES = [12, 20, 28, 34, 44]        # 12 = coastal default; 34 ~ one cell diameter
AFFINITIES = [0.3, 0.5, 0.7]

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
tm = [{k: v for k, v in d.items() if k not in DEAD} for d in list(temporal_metrics)]
print(f"== {IMG} mem-TOM  cell ~{11.0/PX:.0f} px across, coastal default seed_size=12", flush=True)

model = train_with_metrics(frames_prep, tm, variance_metrics_norm=None, num_epochs=EPOCHS,
                          intensity_weight=1.0, foreground_weight=1.0, temporal_weight=2.0,
                          confetti_weight=0.0, variance_as_input=False, seed=SEED, device="cuda")
if isinstance(model, tuple):
    model = model[0]
print(f"trained in {time.time()-t0:.0f}s — sweeping inference only\n", flush=True)


def stats(lab):
    props = [p for p in measure.regionprops(np.asarray(lab)) if p.area > 0]
    if not props:
        return 0, 0.0, 0.0
    eqd = 2 * np.sqrt(np.array([p.area for p in props]) * PX ** 2 / np.pi)
    return len(props), float(np.median(eqd)), float(np.median([p.solidity for p in props]))


def baseline(img):
    b = filters.gaussian(img, sigma=(2.0 / PX), preserve_range=True)
    fg = ndimage.binary_fill_holes(morphology.remove_small_objects(
        b > filters.threshold_triangle(b[b > 0]), int(np.pi * (3.0 / PX) ** 2 / 4)))
    d = ndimage.distance_transform_edt(fg)
    return segmentation.watershed(-d, measure.label(morphology.h_maxima(d, h=(2.0 / PX))), mask=fg)


bs = {t: stats(baseline(sm[t])) for t in (TRAIN_T, TEST_T)}
print(f"baseline: t=2 {bs[TRAIN_T][0]} obj @ {bs[TRAIN_T][1]:.1f} um | "
      f"t=24 {bs[TEST_T][0]} obj @ {bs[TEST_T][1]:.1f} um  <- target\n")

print(f"{'seed_size':>9} {'affinity':>9} | {'t=2 n':>6} {'µm':>5} {'sol':>5} | "
      f"{'t=24 n':>7} {'µm':>5} {'sol':>5}")
print("-" * 62)
grid, best = {}, None
for ss in SEED_SIZES:
    for af in AFFINITIES:
        inf = LearnedAffinityInference(
            model=model, device="cuda", affinity_threshold=af, merge_affinity_threshold=0.65,
            merge_max_distance=1.5, prob_weight=0.3, seed_size=ss, prob_threshold=0.3,
            embedding_blur_sigma=1.5, prob_blur_sigma=1.5, max_iter=200, min_component_size=20)
        out = {}
        for t in (TRAIN_T, TEST_T):
            _, inst, _ = inf.predict_frame(frames_prep[t], tm[t])
            out[t] = (np.asarray(inst),) + stats(inst)
        grid[(ss, af)] = out
        n2, d2, s2 = out[TRAIN_T][1:]
        n24, d24, s24 = out[TEST_T][1:]
        print(f"{ss:>9} {af:>9.1f} | {n2:>6} {d2:>5.1f} {s2:>5.2f} | {n24:>7} {d24:>5.1f} {s24:>5.2f}",
              flush=True)
        # closeness to the baseline on BOTH count and size, held-out frame weighted equally
        err = (abs(n2 - bs[TRAIN_T][0]) / bs[TRAIN_T][0] + abs(n24 - bs[TEST_T][0]) / bs[TEST_T][0]
               + abs(d2 - bs[TRAIN_T][1]) / bs[TRAIN_T][1] + abs(d24 - bs[TEST_T][1]) / bs[TEST_T][1])
        if best is None or err < best[0]:
            best = (err, ss, af)

print(f"\nclosest to baseline on count+size: seed_size={best[1]}, affinity_threshold={best[2]}")

SHOW = [(12, 0.5), (best[1], best[2]), (SEED_SIZES[-1], 0.5)]
fig, ax = plt.subplots(2, len(SHOW) + 1, figsize=(4.6 * (len(SHOW) + 1), 9.4))
for r, t in enumerate((TRAIN_T, TEST_T)):
    disp = exposure.rescale_intensity(sm[t], in_range=tuple(np.percentile(sm[t], (1, 99.7))))
    for c, key in enumerate(SHOW):
        inst, n, d, s = grid[key][t]
        ax[r, c].imshow(disp, cmap="gray")
        ax[r, c].contour(segmentation.find_boundaries(inst, mode="thick"), levels=[0.5],
                         colors="yellow", linewidths=0.5)
        tag = " (coastal default)" if key == (12, 0.5) else (" ← best" if key == (best[1], best[2]) else "")
        ax[r, c].set_title(f"seed_size={key[0]}, affinity={key[1]}{tag}\n"
                           f"t={t}: {n} obj · {d:.1f} µm · sol {s:.2f}", fontsize=8)
    b = baseline(sm[t])
    nb, db, sb = stats(b)
    ax[r, -1].imshow(disp, cmap="gray")
    ax[r, -1].contour(segmentation.find_boundaries(b, mode="thick"), levels=[0.5],
                      colors="red", linewidths=0.5)
    ax[r, -1].set_title(f"intensity baseline\nt={t}: {nb} obj · {db:.1f} µm · sol {sb:.2f}", fontsize=8)
    for a in ax[r]:
        a.axis("off")
fig.suptitle(f"Region growing: seeds are prob-map local maxima in a seed_size window, and cells are "
             f"~{11.0/PX:.0f} px across\n{IMG} mem-TOM, σ={SIGMA_PX} px, 12 metrics — inference-only "
             f"sweep on one trained model (seed {SEED})", fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(f"{OUT}/flow_9_seed_sweep_{IMG}.png", dpi=110)
print(f"figure: {OUT}/flow_9_seed_sweep_{IMG}.png   total {time.time()-t0:.0f}s")
