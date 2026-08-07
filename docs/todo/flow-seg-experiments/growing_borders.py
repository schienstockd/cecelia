"""Can the coastline be fixed at SOURCE instead of smoothed away afterwards?

`labelSmoothing` is post-hoc: it shortens a border but cannot un-merge two cells joined through a
faint bridge, and on the worst objects it only got roughness 3.31 -> 2.48 against a pixel-grid floor
of 1.01. That says the border is not noise on a good mask — the MASK is coastline-shaped.

Region growing is confined to `prob_map > prob_threshold`, so that thresholded map IS the outline.
`prob_blur_sigma` smooths it before thresholding, which should turn a ragged level set into a smooth
one. It is the lever `seed_blur_sigma` was deliberately built to avoid (seeding wanted merging
WITHOUT moving the outline); here moving the outline is the point.

Runs the real `CoastalUtils.predict_slice`, so what is measured is what the task would produce.
"""
import json
import time

# NO sys.path insert: coastal is a pinned dependency now, and pointing at the working checkout
# shadows it with whatever branch happens to be out (which is how this first failed).
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from skimage import measure, segmentation, exposure

from cecelia.utils import zarr_utils, ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.coastal_utils import CoastalUtils

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
VAULT = ("/tmp/claude-1000/-home-dominik-cc-workspace-cecelia/"
         "c3df1c28-d87d-4b68-8b7f-6ba081bf2bf8/scratchpad/devdir/models/coastalModels/flowTest.pt")
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM = "fXgbTl", 2
CASES = [(6, 28), (3, 10), (15, 16)]
# (embedding_blur, prob_threshold) — the two things that decide where GROWING stops
ARMS = [(1.5, 0.3), (3.0, 0.3), (5.0, 0.3), (1.5, 0.5), (3.0, 0.5)]

im_path = f"{PROJ}/0/{IMG}/ccidTemporalSmoothed.ome.zarr"
levels, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)
dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
dim_utils.calc_image_dimensions(levels[0].shape)
px = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
T = int(dim_utils.dim_val('T'))


def base_params(arm):
    emb, pth = arm
    return {'model': VAULT, 'matchAs': 'base', 'cellChannels': [CH_TOM],
            'normalise': 99.99, 'seedSize': 12, 'seedBlurSigma': 8.0,
            'probThreshold': pth, 'affinityThreshold': 0.5, 'minComponentSize': 20,
            'probBlurSigma': 0.0, 'embeddingBlurSigma': emb,
            'mergeAffinityThreshold': 0.65, 'mergeMaxDistance': 1.5,
            'probWeight': 0.3, 'maxIter': 200}


def roughness_of(lab):
    props = [p for p in measure.regionprops(lab) if p.area >= 100 and p.perimeter > 0]
    if not props:
        return np.nan, 0, 0
    r = [p.perimeter / (2 * np.sqrt(np.pi * p.area)) for p in props]
    return float(np.median(r)), len(props), int(sum(p.area for p in props))


cu = CoastalUtils({'taskDir': '/tmp', 'models': {'0': base_params(ARMS[0])},
                   'normaliseToWhole': False}, dim_utils)
norm = None                                   # per-window percentile; same for every arm

results = {}
for (t, z) in CASES:
    lo, hi = max(0, t - cu.TEMPORAL_RADIUS), min(T - 1, t + cu.TEMPORAL_RADIUS)
    window = np.stack([np.asarray(levels[0][tt, :, z]) for tt in range(lo, hi + 1)])  # [W,C,Y,X]
    tile = window[t - lo]
    for arm in ARMS:
        t0 = time.time()
        m = cu.predict_slice(tile, base_params(arm), norm, context=window, context_index=t - lo)
        rough, n, area = roughness_of(np.asarray(m))
        results[(t, z, arm)] = np.asarray(m)
        print(f"t={t:2d} z={z:2d} emb_blur {arm[0]:>3} prob_thr {arm[1]}: {n:>3} objects · "
              f"roughness {rough:.2f} · area {area * px * px:.0f} um2 · "
              f"{time.time() - t0:.1f}s", flush=True)

fig, axes = plt.subplots(len(CASES), len(ARMS) + 1,
                         figsize=(3.1 * (len(ARMS) + 1), 3.2 * len(CASES)))
for r, (t, z) in enumerate(CASES):
    frame = np.asarray(levels[0][t, CH_TOM, z]).astype(np.float32)
    disp = exposure.rescale_intensity(frame, in_range=tuple(np.percentile(frame, (1, 99.7))))
    axes[r, 0].imshow(disp, cmap="gray"); axes[r, 0].set_xticks([]); axes[r, 0].set_yticks([])
    axes[r, 0].set_ylabel(f"t={t} z={z}", fontsize=8)
    if r == 0:
        axes[r, 0].set_title("raw mem-TOM", fontsize=9)
    for c, arm in enumerate(ARMS):
        m = results[(t, z, arm)]
        rough, n, _ = roughness_of(m)
        ax = axes[r, c + 1]
        ax.imshow(disp, cmap="gray")
        ax.contour(segmentation.find_boundaries(m, mode="thick"),
                   levels=[0.5], colors="#00e5ff", linewidths=0.7)
        ax.set_xticks([]); ax.set_yticks([])
        ax.set_title(f"emb {arm[0]} · thr {arm[1]}" + ("  (current)" if arm == (1.5, 0.3) else "") +
                     f"\n{n} obj · roughness {rough:.2f}", fontsize=8)

fig.suptitle("The coastline comes from the GROWING frontier, not the prob mask\n"
             f"{IMG} mem-TOM — prob_blur was measured to do nothing; these are the two levers that stop growth",
             fontsize=11)
fig.tight_layout(rect=(0, 0, 1, 0.93))
fig.savefig(f"{OUT}/flow_18_growing_{IMG}.png", dpi=110)
print(f"\nfigure: {OUT}/flow_18_prob_blur_{IMG}.png")
