"""Do the three dead flow planes cost anything, and does dropping them help?

`flow_7_model_io` showed three of the 15 metric planes carry no cell/background structure at all:

    divergence               cell/bg 0.99
    vorticity                cell/bg 1.00
    flow_structure_alignment cell/bg 1.65   (salt-and-pepper across the whole field)

They are also exactly the three the original AUC table scored at 0.51-0.53. Dropping them takes the
model from 16 to 13 input channels for free -- IF it does not hurt.

Three seeds per arm, because the same config on the same seed already produced 84 and 79 instances
across two runs (cuDNN non-determinism, ~6% variance). A single run per arm cannot resolve a
3-channel change, so anything smaller than the seed spread is not a result.
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
SEEDS = [42, 7, 123]
DEAD = ("divergence", "vorticity", "flow_structure_alignment")

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
tm_full = list(temporal_metrics)
tm_pruned = [{k: v for k, v in d.items() if k not in DEAD} for d in tm_full]
tm_none = [{} for _ in tm_full]
print(f"== {IMG} mem-TOM z={zc} sigma={SIGMA_PX} px | metrics {len(tm_full[0])} full, "
      f"{len(tm_pruned[0])} pruned (dropped {', '.join(DEAD)})", flush=True)


def baseline(img):
    b = filters.gaussian(img, sigma=(2.0 / PX), preserve_range=True)
    fg = ndimage.binary_fill_holes(morphology.remove_small_objects(
        b > filters.threshold_triangle(b[b > 0]), int(np.pi * (3.0 / PX) ** 2 / 4)))
    d = ndimage.distance_transform_edt(fg)
    return segmentation.watershed(-d, measure.label(morphology.h_maxima(d, h=(2.0 / PX))), mask=fg)


def nobj(l):
    return len([p for p in measure.regionprops(np.asarray(l)) if p.area > 0])


ARMS = [(f"full ({len(tm_full[0])} metrics, 16 ch)", tm_full, 2.0),
        (f"pruned ({len(tm_pruned[0])} metrics, 13 ch)", tm_pruned, 2.0),
        ("no flow (1 ch)", tm_none, 0.0)]

res = {}      # arm -> {seed -> {t -> (prob, instances, n_probbig, n_inst)}}
for name, metrics, tw in ARMS:
    res[name] = {}
    for seed in SEEDS:
        model = train_with_metrics(frames_prep, metrics, variance_metrics_norm=None,
                                  num_epochs=EPOCHS, intensity_weight=1.0, foreground_weight=1.0,
                                  temporal_weight=tw, confetti_weight=0.0,
                                  variance_as_input=False, seed=seed, device="cuda")
        if isinstance(model, tuple):
            model = model[0]
        inf = LearnedAffinityInference(
            model=model, device="cuda", affinity_threshold=0.5, merge_affinity_threshold=0.65,
            merge_max_distance=1.5, prob_weight=0.3, seed_size=12, prob_threshold=0.3,
            embedding_blur_sigma=1.5, prob_blur_sigma=1.5, max_iter=200, min_component_size=20)
        per_t = {}
        for t in (TRAIN_T, TEST_T):
            prob, inst, _ = inf.predict_frame(frames_prep[t], metrics[t])
            prob, inst = np.asarray(prob), np.asarray(inst)
            npb = measure.label(morphology.remove_small_objects(prob > 0.3, 50)).max()
            per_t[t] = (prob, inst, int(npb), nobj(inst))
        res[name][seed] = per_t
        print(f"  {name:>30} seed {seed:>3} | prob {per_t[TRAIN_T][2]:>3}/{per_t[TEST_T][2]:>3} | "
              f"inst {per_t[TRAIN_T][3]:>4}/{per_t[TEST_T][3]:>4}", flush=True)

b2, b24 = nobj(baseline(sm[TRAIN_T])), nobj(baseline(sm[TEST_T]))
print(f"\n{'arm':>30} | {'instances t=2':>22} | {'instances t=24 (held out)':>26}")
print("-" * 86)
summary = {}
for name, _, _ in ARMS:
    i2 = [res[name][s][TRAIN_T][3] for s in SEEDS]
    i24 = [res[name][s][TEST_T][3] for s in SEEDS]
    summary[name] = (i2, i24)
    print(f"{name:>30} | {np.mean(i2):>7.1f}  (range {min(i2)}–{max(i2)}) | "
          f"{np.mean(i24):>7.1f}  (range {min(i24)}–{max(i24)})", flush=True)
print(f"{'intensity baseline':>30} | {b2:>7}  {'':>16} | {b24:>7}")

# ── figure: same layout as the previous ablation, plus per-seed spread ────────────────────
fig, ax = plt.subplots(len(ARMS), 5, figsize=(20.5, 4.5 * len(ARMS)))
SHOW = SEEDS[0]
for i, (name, _, _) in enumerate(ARMS):
    for j, t in enumerate((TRAIN_T, TEST_T)):
        prob, inst, npb, ni = res[name][SHOW][t]
        disp = exposure.rescale_intensity(sm[t], in_range=tuple(np.percentile(sm[t], (1, 99.7))))
        ax[i, 2 * j].imshow(prob, cmap="magma", vmin=0, vmax=1)
        ax[i, 2 * j].set_title(f"{name}\nt={t} prob head · {npb} comps ≥50px", fontsize=8)
        ax[i, 2 * j + 1].imshow(disp, cmap="gray")
        ax[i, 2 * j + 1].contour(segmentation.find_boundaries(inst, mode="thick"),
                                 levels=[0.5], colors="yellow", linewidths=0.5)
        ax[i, 2 * j + 1].set_title(f"t={t} instances · {ni} "
                                   f"(baseline {b2 if t == TRAIN_T else b24})", fontsize=8)
        ax[i, 2 * j].axis("off"); ax[i, 2 * j + 1].axis("off")
    a = ax[i, 4]
    i2, i24 = summary[name]
    a.scatter([0] * len(i2), i2, s=45, label="t=2 (trained)")
    a.scatter([1] * len(i24), i24, s=45, marker="s", label="t=24 (held out)")
    a.axhline(b2, color="red", ls="--", lw=1)
    a.text(1.05, b2, "baseline", color="red", fontsize=7, va="center")
    a.set_xlim(-0.5, 1.9); a.set_xticks([0, 1]); a.set_xticklabels(["t=2", "t=24"], fontsize=8)
    a.set_ylim(0, max(130, max(i2 + i24) * 1.15)); a.set_ylabel("instances", fontsize=8)
    a.set_title(f"{len(SEEDS)} seeds\nmean {np.mean(i2):.0f} / {np.mean(i24):.0f}", fontsize=8)
    a.legend(fontsize=6); a.grid(alpha=.3)
fig.suptitle(f"Dropping the 3 dead flow planes (divergence, vorticity, flow_structure_alignment) — "
             f"{IMG} mem-TOM, σ={SIGMA_PX} px, {len(SEEDS)} seeds per arm\n"
             f"images are seed {SHOW}; the right column shows the seed spread, which is what any "
             f"difference has to beat", fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(f"{OUT}/flow_8_pruned_ablation_{IMG}.png", dpi=105)
print(f"\nfigure: {OUT}/flow_8_pruned_ablation_{IMG}.png   total {time.time()-t0:.0f}s")
