"""seed_blur_sigma in coastal proper: does it keep the count AND the shape?

Four arms on the same trained model:
  coastal default   prob_blur 1.5, seed_size 12   -- fragmented
  blurred           prob_blur 8.0, seed_size 34   -- right count, round (shape destroyed)
  seed_blur (NEW)   prob_blur 0.0, seed_blur 8.0  -- seeds blurred, outline sharp
  intensity baseline                              -- the six-line reference

Scored on count, size, solidity AND circularity, because count alone cannot see the rounding.
"""
import json, sys, time
sys.path.insert(0, "/home/dominik/cc-workspace/coastal")
import numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy import ndimage
from skimage import filters, morphology, measure, segmentation, exposure
from cecelia.utils import zarr_utils
from coastal.flow import prepare_data_for_unet, normalize_and_project
from coastal.train import train_with_metrics
from coastal.segment import LearnedAffinityInference

PROJ="/home/dominik/cecelia-feijoa/projects/zolIMa"; IMG="fXgbTl"
DEAD=("divergence","vorticity","flow_structure_alignment")
lv,_=zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidDriftCorrected.ome.zarr",as_dask=True)
a=lv[0]; PX=json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]; zc=a.shape[2]//2
raw=np.asarray(a[:,2,zc]).astype(np.float32)
sm=np.stack([filters.gaussian(f,sigma=3.0,preserve_range=True) for f in raw])
_,fp=normalize_and_project(sm[:,None])
prep,_,_,tmx=prepare_data_for_unet(fp,temporal_scales=[1,2,4,8],cumulative_window=5,verbose=False)
tm=[{k:v for k,v in d.items() if k not in DEAD} for d in list(tmx)]
m=train_with_metrics(prep,tm,variance_metrics_norm=None,num_epochs=30,intensity_weight=1.0,
  foreground_weight=1.0,temporal_weight=2.0,confetti_weight=0.0,variance_as_input=False,seed=42,device="cuda")
if isinstance(m,tuple): m=m[0]

def shp(l):
    p=[q for q in measure.regionprops(np.asarray(l)) if q.area>20]
    if not p: return 0,0.,0.,0.
    e=2*np.sqrt(np.array([q.area for q in p])*PX**2/np.pi)
    return (len(p), float(np.median(e)), float(np.median([q.solidity for q in p])),
            float(np.median([min(1.,4*np.pi*q.area/max(q.perimeter**2,1e-9)) for q in p])))

def mk(**kw):
    base=dict(model=m,device="cuda",affinity_threshold=0.3,merge_affinity_threshold=0.65,
              merge_max_distance=1.5,prob_weight=0.3,prob_threshold=0.3,
              embedding_blur_sigma=1.5,max_iter=200,min_component_size=20)
    base.update(kw); return LearnedAffinityInference(**base)

ARMS=[("coastal default\nprob_blur 1.5 · seed 12", mk(prob_blur_sigma=1.5,seed_size=12), "orange"),
      ("blurred\nprob_blur 8 · seed 34",           mk(prob_blur_sigma=8.0,seed_size=34), "yellow"),
      ("seed_blur (NEW)\nseed_blur 8 · outline sharp", mk(prob_blur_sigma=0.0,seed_blur_sigma=8.0,seed_size=34), "cyan")]

def base_seg(img):
    b=filters.gaussian(img,sigma=2.0/PX,preserve_range=True)
    fg=ndimage.binary_fill_holes(morphology.remove_small_objects(b>filters.threshold_triangle(b[b>0]),int(np.pi*(3.0/PX)**2/4)))
    d=ndimage.distance_transform_edt(fg)
    return segmentation.watershed(-d,measure.label(morphology.h_maxima(d,h=2.0/PX)),mask=fg)

print(f"{'t':>3} {'arm':>34} | {'n':>4} {'µm':>5} {'sol':>5} {'circ':>5}")
print("-"*64)
fig,ax=plt.subplots(2,4,figsize=(19,9.7))
for r,t in enumerate((2,24)):
    disp=exposure.rescale_intensity(sm[t],in_range=tuple(np.percentile(sm[t],(1,99.7))))
    for c,(nm,inf,col) in enumerate(ARMS):
        _,inst,_=inf.predict_frame(prep[t],tm[t]); inst=np.asarray(inst)
        n,d,so,ci=shp(inst)
        print(f"{t:>3} {nm.replace(chr(10),' '):>34} | {n:>4} {d:>5.1f} {so:>5.2f} {ci:>5.2f}",flush=True)
        ax[r,c].imshow(disp,cmap="gray")
        ax[r,c].contour(segmentation.find_boundaries(inst,mode="thick"),levels=[0.5],colors=col,linewidths=0.6)
        ax[r,c].set_title(f"{nm}\nt={t}: {n} obj · {d:.1f} µm · sol {so:.2f} · circ {ci:.2f}",fontsize=8)
    b=base_seg(sm[t]); n,d,so,ci=shp(b)
    print(f"{t:>3} {'intensity baseline':>34} | {n:>4} {d:>5.1f} {so:>5.2f} {ci:>5.2f}",flush=True)
    ax[r,3].imshow(disp,cmap="gray")
    ax[r,3].contour(segmentation.find_boundaries(b,mode="thick"),levels=[0.5],colors="red",linewidths=0.6)
    ax[r,3].set_title(f"intensity baseline\nt={t}: {n} obj · {d:.1f} µm · sol {so:.2f} · circ {ci:.2f}",fontsize=8)
    for x in ax[r]: x.axis("off")
fig.suptitle("seed_blur_sigma — seeding and the outline no longer share one blur\n"
             "fXgbTl mem-TOM (motile GC B cells), σ=3 px input, 12 flow metrics, seed 42. "
             "lower circularity = shape preserved",fontsize=11)
fig.tight_layout(rect=(0,0,1,0.92))
fig.savefig("/home/dominik/Downloads/TMP/flow_12_seed_blur_fXgbTl.png",dpi=110)
print("figure: /home/dominik/Downloads/TMP/flow_12_seed_blur_fXgbTl.png")
