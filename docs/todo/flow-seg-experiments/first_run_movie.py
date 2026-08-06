"""Movie of what `segment.coastal` actually wrote — the task's own output, read back off disk.

Not a re-run: this opens `labels/coastalTest.zarr` produced by the real task, so what you watch is
what the pipeline stores, not what a script can be made to produce.

Contours are coloured by 3D object SIZE, because that is the split the size histogram says exists
(mode 4-5 um against an 11 um cell) and the one decision left to make about this output. Size is
measured on the whole 3D object, not on its mid-plane cross-section — a cell caught at its edge has a
small cross-section and is still a cell. It is a VIEWING aid: the real split is a gating decision,
not a segmentation parameter.

Left/right rather than one panel: the judgement is a comparison, and an outline drawn over an image
hides the thing you are checking it against.
"""
import json
import sys

sys.path.insert(0, "/home/dominik/cc-workspace/coastal")

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg
from skimage import measure, segmentation, exposure

from cecelia.utils import zarr_utils
from cecelia.utils.movie_io import movie_writer

PROJ = "/home/dominik/cecelia-feijoa/projects/zolIMa"
OUT = "/home/dominik/Downloads/TMP"
IMG, CH_TOM, VALUE_NAME = "fXgbTl", 2, "coastalTest"
FPS = 6                      # 31 frames — at the 20 fps used for Dml3RG this would be 1.5 s
CELL_UM = 8.0                # equivalent-sphere diameter above which we draw it as a cell
Z_STEP_UM = 2.0

px = json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]
img_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidTemporalSmoothed.ome.zarr", as_dask=True)
lab_levels, _ = zarr_utils.open_as_zarr(f"{PROJ}/1/{IMG}/labels/{VALUE_NAME}.zarr", as_dask=True)
im, lab = img_levels[0], lab_levels[0]
T, zc = im.shape[0], im.shape[2] // 2
vox_um3 = px * px * Z_STEP_UM
print(f"{IMG}: image {im.shape}, labels {lab.shape}, mid-z {zc}, {px:.4f} um/px", flush=True)


def frame_split(t):
    """(cells, particles) mid-z label planes, classified on 3D volume."""
    vol = np.asarray(lab[t])
    plane = vol[zc]
    big = set()
    for p in measure.regionprops(vol):
        if 2 * (3 * p.area * vox_um3 / (4 * np.pi)) ** (1 / 3) >= CELL_UM:
            big.add(p.label)
    cells = np.where(np.isin(plane, list(big)), plane, 0)
    parts = np.where((plane > 0) & (cells == 0), plane, 0)
    return cells, parts, len(np.unique(vol)) - 1


fig = plt.figure(figsize=(11.4, 5.9), dpi=100)
ax_raw = fig.add_axes([0.005, 0.02, 0.49, 0.90])
ax_seg = fig.add_axes([0.505, 0.02, 0.49, 0.90])
canvas = FigureCanvasAgg(fig)

path = f"{OUT}/flow_14_first_task_run_{IMG}.mp4"
n_cells, n_parts = [], []
with movie_writer(path, FPS) as w:
    for t in range(T):
        frame = np.asarray(im[t, CH_TOM, zc]).astype(np.float32)
        disp = exposure.rescale_intensity(frame, in_range=tuple(np.percentile(frame, (1, 99.7))))
        cells, parts, n3d = frame_split(t)
        nc = len(np.unique(cells)) - 1
        npar = len(np.unique(parts)) - 1
        n_cells.append(nc); n_parts.append(npar)

        for ax, title in ((ax_raw, "mem-TOM (mid-z)"), (ax_seg, "segment.coastal")):
            ax.clear(); ax.axis("off")
            ax.imshow(disp, cmap="gray")
            ax.set_title(title, fontsize=9)
        ax_seg.contour(segmentation.find_boundaries(cells, mode="thick"),
                       levels=[0.5], colors="#ffe000", linewidths=0.8)
        ax_seg.contour(segmentation.find_boundaries(parts, mode="thick"),
                       levels=[0.5], colors="#00e5ff", linewidths=0.6)

        fig.suptitle(
            f"{IMG} mem-TOM   t={t:02d}/{T - 1}  ({t * 15}s)   "
            f"mid-z: {nc} ≥{CELL_UM:.0f}µm (yellow) · {npar} smaller (cyan)   "
            f"— {n3d} objects in the full 3D stack",
            fontsize=9)
        canvas.draw()
        w.append_data(np.asarray(canvas.buffer_rgba())[..., :3])
        if t % 10 == 0:
            print(f"  t={t}/{T}", flush=True)

print(f"\nmid-z cells     : mean {np.mean(n_cells):.1f} (range {min(n_cells)}-{max(n_cells)})")
print(f"mid-z particles : mean {np.mean(n_parts):.1f} (range {min(n_parts)}-{max(n_parts)})")
print(f"movie: {path}  ({T} frames @ {FPS} fps = {T / FPS:.1f}s)")
