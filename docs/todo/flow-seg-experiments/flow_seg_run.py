"""coastal flow segmentation on fXgbTl mem-TOM, with everything today's measurements changed.

What is different from the run recorded in SEGMENTATION_OPEN_PROBLEM.md (167/182 objects,
~7x over-segmented against a 22-object intensity baseline):

  input      raw / temporally-smoothed  ->  SPATIAL smoothing only, sigma = 1 px
             Temporal smoothing destroys the flow signal it is supposed to help: measured today,
             a 3-frame median drops the 15 s photometric gain from 29% to 3.3% because consecutive
             windows share 2 of 3 frames. Spatial-only is what makes flow visible at all.

  8-bit      np.array(frames, dtype=np.uint8), which WRAPPED  ->  float32 throughout
             Farneback accepts float32 directly. Removed in coastal PR #19.

  intensity_weight   0.0  ->  1.0
             Setting it to zero switched off the only input carrying signal. Independently, flow
             does NOT beat plain intensity at finding these cells (AUC 0.958 vs 0.980), so the
             prob head needs an intensity supervisor.

  prob_blur_sigma    off  ->  on
             coastal's own docs reach the same conclusion this session reached from the data: the
             foreground is speckle and it is the real bottleneck. Cells (~15-20 px) and speckle
             (1-3 px) differ by SCALE, so a blur separates them where a threshold cannot.

Scored against the same deliberately dumb intensity baseline, because that is the thing to beat:
blur at cell scale -> triangle threshold -> distance-transform watershed.
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
SIGMA_PX = 1.0                      # today's measured optimum for flow
SCALES, CUMWIN = [1, 2, 4, 8], 5    # coastal's recommended; today's data says these lags are fine
EPOCHS = 30
TRAIN_T, TEST_T = 2, 24             # same frames the earlier run used, so it is comparable

t0 = time.time()
levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidDriftCorrected.ome.zarr", as_dask=True)
arr = levels[0]
T, C, Z, Y, X = arr.shape
PX = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
zc = Z // 2
print(f"== {IMG} {arr.shape}  px {PX:.4f} um  z={zc}  spatial sigma {SIGMA_PX} px", flush=True)

# [T, C=1, H, W] for one z plane, spatially smoothed. No temporal filtering of any kind.
raw = np.asarray(arr[:, CH_TOM, zc]).astype(np.float32)          # [T, H, W]
sm = np.stack([filters.gaussian(f, sigma=SIGMA_PX, preserve_range=True) for f in raw])
seq = sm[:, None]                                                # [T, 1, H, W]
print(f"loaded+smoothed {seq.shape} in {time.time()-t0:.1f}s", flush=True)

frames_multi, frames_proj = normalize_and_project(seq)
print(f"normalize_and_project -> {frames_proj.dtype} range {frames_proj.min():.1f}-{frames_proj.max():.1f} "
      f"(float32, not quantised)", flush=True)

frames_prep, _, _, temporal_metrics = prepare_data_for_unet(
    frames_proj, temporal_scales=SCALES, cumulative_window=CUMWIN, verbose=False)
tm = list(temporal_metrics)
print(f"metrics per frame: {len(tm[0])} -> {sorted(tm[0])}", flush=True)

print(f"\ntraining {EPOCHS} epochs...", flush=True)
model = train_with_metrics(
    frames_prep, tm, variance_metrics_norm=None,
    num_epochs=EPOCHS,
    intensity_weight=1.0,        # was 0.0 -- the change that matters most
    foreground_weight=1.0,
    temporal_weight=2.0,
    confetti_weight=0.0,
    variance_as_input=False,
    device="cuda")
if isinstance(model, tuple):
    model = model[0]
print(f"trained in {time.time()-t0:.0f}s", flush=True)

# Same inference class the earlier (167/182-object) run used, so the comparison is like-for-like.
# Two arms: prob_blur_sigma off and on. coastal's docs say the prob head sits on a 1-3 px speckle
# floor that also crosses prob_threshold, and that raising the threshold does NOT fix it (0.4->0.9
# drops fragments 88%->58% while labels collapse 719->40). Cells and speckle differ by SCALE, so
# this is the arm that should matter on photon-limited data.
def make_inf(blur):
    return LearnedAffinityInference(
        model=model, device="cuda",
        affinity_threshold=0.5, merge_affinity_threshold=0.65, merge_max_distance=1.5,
        prob_weight=0.3, seed_size=12, prob_threshold=0.3,
        embedding_blur_sigma=1.5, prob_blur_sigma=blur,
        max_iter=200, min_component_size=20)


ARMS = [("coastal flow, blur off", make_inf(0.0)), ("coastal flow, prob_blur 1.5", make_inf(1.5))]


def baseline(img):
    """The six lines that must be beaten."""
    b = filters.gaussian(img, sigma=(2.0 / PX), preserve_range=True)
    fg = ndimage.binary_fill_holes(morphology.remove_small_objects(
        b > filters.threshold_triangle(b[b > 0]), int(np.pi * (3.0 / PX) ** 2 / 4)))
    d = ndimage.distance_transform_edt(fg)
    mk = measure.label(morphology.h_maxima(d, h=(2.0 / PX)))
    return segmentation.watershed(-d, mk, mask=fg)


def stats(lab):
    props = [p for p in measure.regionprops(lab) if p.area > 0]
    if not props:
        return dict(n=0, eqd=0.0, sol=0.0, frac=0.0)
    eqd = 2 * np.sqrt(np.array([p.area for p in props]) * PX ** 2 / np.pi)
    return dict(n=len(props), eqd=float(np.median(eqd)),
                sol=float(np.median([p.solidity for p in props])),
                frac=100 * float((lab > 0).mean()))


rows, panels = [], {}
for t in (TRAIN_T, TEST_T):
    got = []
    for nm, inf in ARMS:
        # predict_frame returns (prob_map, instances, props) -- taking [0] silently hands back the
        # probability map as if it were labels (trap 4 in SEGMENTATION_OPEN_PROBLEM.md)
        _, instances, _ = inf.predict_frame(frames_prep[t], tm[t])
        got.append((nm, np.asarray(instances)))
    got.append(("intensity baseline", baseline(sm[t])))
    for nm, lab in got:
        st = stats(lab)
        rows.append((t, nm, st))
        print(f"t={t:>3} {nm:>28} | {st['n']:>4} obj | eq.diam {st['eqd']:>5.1f} um | "
              f"solidity {st['sol']:.2f} | area {st['frac']:>4.1f}%", flush=True)
    panels[t] = (sm[t], got)

fig, ax = plt.subplots(2, 4, figsize=(20, 10))
COLS = ["lime", "yellow", "red"]
for i, t in enumerate((TRAIN_T, TEST_T)):
    img, got = panels[t]
    disp = exposure.rescale_intensity(img, in_range=tuple(np.percentile(img, (1, 99.7))))
    ax[i, 0].imshow(disp, cmap="gray")
    ax[i, 0].set_title(f"t={t} {'(trained)' if t == TRAIN_T else '(HELD OUT)'}\n"
                       f"mem-TOM, spatial sigma={SIGMA_PX} px", fontsize=9)
    for j, (nm, lab) in enumerate(got, start=1):
        st = stats(lab)
        ax[i, j].imshow(disp, cmap="gray")
        ax[i, j].contour(segmentation.find_boundaries(lab, mode="thick"), levels=[0.5],
                         colors=COLS[j - 1], linewidths=0.5)
        ax[i, j].set_title(f"{nm}\n{st['n']} obj · {st['eqd']:.1f} um · solidity {st['sol']:.2f}",
                           fontsize=9)
    for a in ax[i]:
        a.axis("off")
fig.suptitle("coastal flow segmentation, fXgbTl mem-TOM — spatial-only smoothing, float32 flow, "
             "intensity_weight=1.0\n"
             "the run this replaces: 167 (t=2) / 182 (t=24) objects against a 22-object intensity "
             "baseline, i.e. ~7x over-segmented", fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.92))
fig.savefig(f"{OUT}/flow_4_segmentation_{IMG}.png", dpi=110)
print(f"\nfigure: {OUT}/flow_4_segmentation_{IMG}.png   total {time.time()-t0:.0f}s")
