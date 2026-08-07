"""Is the extra boundary detail from seed_blur_sigma real morphology, or noise-following?

Circularity fell 0.82 -> 0.65, which is what we want IF the detail is the cell. A boundary that
merely chases shot noise would also look non-round, so the number alone proves nothing.

Test: a real cell's SHAPE persists between consecutive frames (15 s apart, cells move ~2 px);
noise re-randomises. So match each object at t to the nearest object at t+1, translate its mask so
the centroids coincide (removing motion), and take the IoU of the outlines.

The chance floor matters: round blobs match each other trivially, so a rounded method scores high
for a stupid reason. Every arm is therefore also scored against a SHUFFLED pairing — each object
matched to a random OTHER cell. The real quantity is the excess over that floor: how much of the
agreement is this cell's own shape rather than "cells are blobs".
"""
import json, sys
sys.path.insert(0, "/home/dominik/cc-workspace/coastal")
import numpy as np
from scipy import ndimage
from skimage import filters, morphology, measure, segmentation
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

def mk(**kw):
    b=dict(model=m,device="cuda",affinity_threshold=0.3,merge_affinity_threshold=0.65,
           merge_max_distance=1.5,prob_weight=0.3,prob_threshold=0.3,embedding_blur_sigma=1.5,
           max_iter=200,min_component_size=20)
    b.update(kw); return LearnedAffinityInference(**b)

def base_seg(img):
    b=filters.gaussian(img,sigma=2.0/PX,preserve_range=True)
    fg=ndimage.binary_fill_holes(morphology.remove_small_objects(b>filters.threshold_triangle(b[b>0]),int(np.pi*(3.0/PX)**2/4)))
    d=ndimage.distance_transform_edt(fg)
    return segmentation.watershed(-d,measure.label(morphology.h_maxima(d,h=2.0/PX)),mask=fg)

def patches(lab, size=64):
    """centroid-centred binary crop per object, so IoU compares SHAPE not position"""
    out=[]
    for p in measure.regionprops(np.asarray(lab)):
        if p.area < 60: continue
        cy,cx=[int(round(v)) for v in p.centroid]
        m_=np.zeros((size,size),bool); h=size//2
        y0,y1=cy-h,cy+h; x0,x1=cx-h,cx+h
        src=(np.asarray(lab)==p.label)
        ys0,ys1=max(0,y0),min(src.shape[0],y1); xs0,xs1=max(0,x0),min(src.shape[1],x1)
        m_[ys0-y0:ys1-y0, xs0-x0:xs1-x0]=src[ys0:ys1, xs0:xs1]
        if m_.sum()>0: out.append(m_)
    return out

def iou(a,b): 
    u=(a|b).sum(); return (a&b).sum()/u if u else 0.0

rng=np.random.default_rng(0)
ARMS=[("blurred (round)", lambda t: np.asarray(mk(prob_blur_sigma=8.0,seed_size=34).predict_frame(prep[t],tm[t])[1])),
      ("seed_blur (sharp)", lambda t: np.asarray(mk(prob_blur_sigma=0.0,seed_blur_sigma=8.0,seed_size=34).predict_frame(prep[t],tm[t])[1])),
      ("intensity baseline", lambda t: base_seg(sm[t]))]

print(f"{'arm':>20} | {'matched IoU':>11} {'shuffled':>9} {'excess':>7} | n")
print("-"*58)
for nm,fn in ARMS:
    matched, shuffled = [], []
    for t in (2, 10, 18, 24):
        A, B = patches(fn(t)), patches(fn(t+1))
        if not A or not B: continue
        for pa in A:
            matched.append(max(iou(pa,pb) for pb in B))       # best shape match at t+1
            shuffled.append(iou(pa, B[rng.integers(len(B))]))  # a random other cell
    print(f"{nm:>20} | {np.mean(matched):>11.3f} {np.mean(shuffled):>9.3f} "
          f"{np.mean(matched)-np.mean(shuffled):>7.3f} | {len(matched)}", flush=True)
print("\nexcess over shuffled = shape information that is THIS cell's, not 'cells are blobs'.")
