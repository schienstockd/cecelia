"""Two-pass segmentation (cells + apoptotic bodies) and a movie of the result.

The single-pass config is tuned for ~11 um cells: seed_blur 8 px stops one cell fragmenting, and
min_component_size 20 suppresses speckle. Both of those also delete apoptotic bodies, which are
legitimately small. So the two passes want OPPOSITE settings, which is what the two-pass class is
for -- now with per-pass seed_blur_sigma and its own size floor.

  pass 1 (cells)   seed_blur 8, seed_size 34, min_component_size 60
  pass 2 (bodies)  seed_blur 0, seed_size 7,  min_component_size_small 6

Pass 2 only ever runs on what pass 1 left behind (`mask_remaining`), so it cannot re-cut cells.

Movie: every frame, contours coloured by pass, written with cecelia's own movie_writer.
"""
import json, sys
sys.path.insert(0, "/home/dominik/cc-workspace/coastal")
import numpy as np
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.backends.backend_agg import FigureCanvasAgg
from skimage import filters, measure, segmentation, exposure
from cecelia.utils import zarr_utils
from cecelia.utils.movie_io import movie_writer
from coastal.flow import prepare_data_for_unet, normalize_and_project
from coastal.train import train_with_metrics
from coastal.segment import TwoPassSegmentationInference

PROJ="/home/dominik/cecelia-feijoa/projects/zolIMa"; FPS=20
IMG = sys.argv[1] if len(sys.argv) > 1 else "fXgbTl"
# Train on a strided subset, infer on every frame. Materialising 12 metric planes for all 181
# frames of a full 1036x1055 plane is 9.5 GB on top of 6.3 GB of flows -- more than is free with
# the app running. prepare_data_for_unet hands back a LAZY TemporalMetrics, so only the training
# subset is realised; inference indexes it one frame at a time.
TRAIN_STRIDE = int(sys.argv[2]) if len(sys.argv) > 2 else 1
OUT="/home/dominik/Downloads/TMP"
DEAD=("divergence","vorticity","flow_structure_alignment")
lv,_=zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidDriftCorrected.ome.zarr",as_dask=True)
a=lv[0]; PX=json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]; zc=a.shape[2]//2
T=a.shape[0]
raw=np.asarray(a[:,2,zc]).astype(np.float32)
sm=np.stack([filters.gaussian(f,sigma=3.0,preserve_range=True) for f in raw])
_,fp=normalize_and_project(sm[:,None])
prep,_,_,tmx=prepare_data_for_unet(fp,temporal_scales=[1,2,4,8],cumulative_window=5,verbose=False)
def metrics_at(t):
    return {k:v for k,v in tmx[t].items() if k not in DEAD}
idx=list(range(0,T,TRAIN_STRIDE))
print(f"training on {len(idx)}/{T} frames (stride {TRAIN_STRIDE}), inferring on all {T}",flush=True)
tm_train=[metrics_at(t) for t in idx]
m=train_with_metrics(prep[idx],tm_train,variance_metrics_norm=None,num_epochs=30,intensity_weight=1.0,
  foreground_weight=1.0,temporal_weight=2.0,confetti_weight=0.0,variance_as_input=False,seed=42,device="cuda")
del tm_train
if isinstance(m,tuple): m=m[0]

BIG_UM2 = np.pi*(5.0/2)**2          # >5 um equivalent diameter counts as a cell, not a body
inf = TwoPassSegmentationInference(
    model=m, device="cuda", prob_threshold=0.3,
    seed_size_large=14, affinity_threshold_large=0.3, embedding_blur_sigma_large=1.5,
    # tuned on t=92: seed_blur 8.0 under-segmented by a third (131 cells vs ~195 expected by
    # area from fXgbTl). seed_blur dominates merging; seed_size is the weaker lever.
    seed_blur_sigma_large=5.0,
    merge_max_distance_large=1.5, merge_affinity_threshold_large=0.65,
    seed_size_small=7, affinity_threshold_small=0.4, embedding_blur_sigma_small=1.0,
    seed_blur_sigma_small=0.0, merge_max_distance_small=1.5, merge_affinity_threshold_small=0.60,
    max_iter=200, min_component_size=60, min_component_size_small=6)

print(f"segmenting {T} frames…", flush=True)
labs=[]; provs=[]
for t in range(T):
    _,inst,_,prov = inf.predict_frame(prep[t], metrics_at(t), return_provenance=True)
    labs.append(np.asarray(inst)); provs.append(np.asarray(prov))
    if t % 20 == 0: print(f"  t={t}/{T}", flush=True)

def split(lab, prov):
    """Colour by WHICH PASS found the object, not by its size.

    Size is not provenance: pass 1 can return a small object and pass 2 a larger one, so a
    size split mislabels exactly the cases worth inspecting. Each label is assigned to the
    pass that owns most of its pixels.
    """
    p1=np.zeros_like(lab); p2=np.zeros_like(lab)
    for p in measure.regionprops(lab):
        m = lab==p.label
        (p1 if (prov[m]==1).sum() >= (prov[m]==2).sum() else p2)[m] = p.label
    return p1, p2

nb=[]; ns=[]
for l,pv in zip(labs,provs):
    b,s_=split(l,pv); nb.append(len(np.unique(b))-1); ns.append(len(np.unique(s_))-1)
print(f"\npass 1 (cells)    : mean {np.mean(nb):.1f} (range {min(nb)}–{max(nb)})")
print(f"pass 2 (particles): mean {np.mean(ns):.1f} (range {min(ns)}–{max(ns)})")

fig=plt.figure(figsize=(6.4,6.6),dpi=100); ax=fig.add_axes([0,0,1,0.94]); ax.axis("off")
canvas=FigureCanvasAgg(fig)
path=f"{OUT}/flow_13_twopass_{IMG}.mp4"
with movie_writer(path, FPS) as w:
    for t in range(T):
        ax.clear(); ax.axis("off")
        disp=exposure.rescale_intensity(sm[t],in_range=tuple(np.percentile(sm[t],(1,99.7))))
        ax.imshow(disp,cmap="gray")
        b,s_=split(labs[t],provs[t])
        ax.contour(segmentation.find_boundaries(b,mode="thick"),levels=[0.5],colors="#00e5ff",linewidths=0.8)
        ax.contour(segmentation.find_boundaries(s_,mode="thick"),levels=[0.5],colors="#ff3ba7",linewidths=0.8)
        fig.suptitle(f"{IMG} mem-TOM  t={t:02d}/{T-1}  ({t*15}s)   "
                     f"pass 1 cells {len(np.unique(b))-1} (cyan) · pass 2 particles {len(np.unique(s_))-1} (pink)",
                     fontsize=9)
        canvas.draw()
        buf=np.asarray(canvas.buffer_rgba())[..., :3]
        w.append_data(buf)
print(f"movie: {path}  ({T} frames @ {FPS} fps = {T/FPS:.1f}s)")
