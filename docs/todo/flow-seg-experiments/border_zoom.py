"""The embedding-blur choice, at a zoom you can actually judge.

The trade is not visible at whole-frame scale: `embedding_blur_sigma` 5.0 halves the measured area
while making the border clean, and whether that is a better cell or a shrunken one is a call about
where the cell ENDS on a cytoplasmic reporter. That has to be looked at over the raw signal, close up.

Mid-stack plane on purpose (z=16 of 32): the worst borders were at z=26-30 where everything is dim
and out of focus, and choosing a parameter on those would be choosing on the worst data.
"""
import json
import time

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from skimage import segmentation, exposure, measure

from cecelia.utils import zarr_utils, ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.coastal_utils import CoastalUtils

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
VAULT = ("/tmp/claude-1000/-home-dominik-cc-workspace-cecelia/"
         "c3df1c28-d87d-4b68-8b7f-6ba081bf2bf8/scratchpad/devdir/models/coastalModels/flowTest.pt")
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM, T, Z = "fXgbTl", 2, 15, 16
ARMS = [1.5, 3.0, 5.0]                      # embedding_blur_sigma
CROPS = [(60, 40), (250, 120), (120, 260)]  # (y, x) top-left of 140 px windows
SIZE = 140

im_path = f"{PROJ}/0/{IMG}/ccidTemporalSmoothed.ome.zarr"
levels, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)
dim_utils = DimUtils(ome_xml_utils.parse_meta(im_path), use_channel_axis=True)
dim_utils.calc_image_dimensions(levels[0].shape)
px = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
nT = int(dim_utils.dim_val('T'))


def params(emb):
    return {'model': VAULT, 'matchAs': 'base', 'cellChannels': [CH_TOM], 'normalise': 99.99,
            'seedSize': 12, 'seedBlurSigma': 8.0, 'probThreshold': 0.3,
            'affinityThreshold': 0.5, 'minComponentSize': 20, 'probBlurSigma': 0.0,
            'embeddingBlurSigma': emb, 'mergeAffinityThreshold': 0.65,
            'mergeMaxDistance': 1.5, 'probWeight': 0.3, 'maxIter': 200}


cu = CoastalUtils({'taskDir': '/tmp', 'models': {'0': params(1.5)},
                   'normaliseToWhole': False}, dim_utils)
lo, hi = max(0, T - cu.TEMPORAL_RADIUS), min(nT - 1, T + cu.TEMPORAL_RADIUS)
window = np.stack([np.asarray(levels[0][tt, :, Z]) for tt in range(lo, hi + 1)])
tile = window[T - lo]

masks = {}
for emb in ARMS:
    t0 = time.time()
    masks[emb] = np.asarray(cu.predict_slice(tile, params(emb), None,
                                             context=window, context_index=T - lo))
    props = [p for p in measure.regionprops(masks[emb]) if p.area >= 100]
    diam = [2 * np.sqrt(p.area / np.pi) * px for p in props]
    print(f"emb {emb}: {len(props)} objects >=100px · median diameter {np.median(diam):.1f} um "
          f"· {time.time() - t0:.1f}s", flush=True)

frame = np.asarray(levels[0][T, CH_TOM, Z]).astype(np.float32)
lo_p, hi_p = np.percentile(frame, (1, 99.7))

fig, axes = plt.subplots(len(CROPS), len(ARMS) + 1,
                         figsize=(5.0 * (len(ARMS) + 1), 5.0 * len(CROPS)))
for r, (y, x) in enumerate(CROPS):
    sl = (slice(y, y + SIZE), slice(x, x + SIZE))
    disp = exposure.rescale_intensity(frame[sl], in_range=(lo_p, hi_p))
    axes[r, 0].imshow(disp, cmap="gray", interpolation="nearest")
    axes[r, 0].set_xticks([]); axes[r, 0].set_yticks([])
    if r == 0:
        axes[r, 0].set_title("raw mem-TOM", fontsize=13)
    # a 10 um bar so the eye has a cell-sized reference
    axes[r, 0].plot([6, 6 + 10 / px], [SIZE - 8, SIZE - 8], "-", color="w", lw=3)
    axes[r, 0].text(6, SIZE - 13, "10 µm", color="w", fontsize=9)
    for c, emb in enumerate(ARMS):
        ax = axes[r, c + 1]
        ax.imshow(disp, cmap="gray", interpolation="nearest")
        ax.contour(segmentation.find_boundaries(masks[emb][sl], mode="thick"),
                   levels=[0.5], colors="#00e5ff", linewidths=1.6)
        ax.set_xticks([]); ax.set_yticks([])
        if r == 0:
            ax.set_title(f"embedding blur {emb}" + ("   (current)" if emb == 1.5 else ""),
                         fontsize=13)

fig.suptitle(f"Where does the cell end? — {IMG} mem-TOM, t={T}, mid-stack z={Z}\n"
             f"blur 5.0 gives a clean border by dropping the dim periphery; "
             f"the question is whether that periphery is cell", fontsize=15)
fig.tight_layout(rect=(0, 0, 1, 0.94))
fig.savefig(f"{OUT}/flow_19_border_zoom_{IMG}.png", dpi=115)
print(f"\nfigure: {OUT}/flow_19_border_zoom_{IMG}.png")
