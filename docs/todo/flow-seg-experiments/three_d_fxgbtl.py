"""3D two-pass segmentation on the small crop, to see if 3D is worth wiring in.

Method (coastal's own): segment each z-plane in 2D through time, then match labels across Z by IoU
(`utils.match_masks_3d`). NOT 3D optical flow — voxels are 6x anisotropic here (2.0 um z vs 0.331 xy)
and SEGMENTATION_OPEN_PROBLEM.md records that OpticalFlow3D's axial speeds are not credible at that
sampling. Per-plane 2D + IoU stitching respects the anisotropy instead of pretending it away.

The model is trained ONCE on the mid plane and reused for every z. Training per plane would be 31x
the cost for a model that sees the same cells.

The question this answers: does a cell come out as ONE 3D object spanning several planes, or one
object per plane? A cell is ~11 um across and z spacing is 2.0 um, so a real cell should span ~5-6
planes. Median z-extent near 1 would mean the stitching is not working.
"""
import json, sys, time
sys.path.insert(0, "/home/dominik/cc-workspace/coastal")
import numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from skimage import filters, measure, segmentation, exposure
from cecelia.utils import zarr_utils
from coastal.flow import prepare_data_for_unet, normalize_and_project
from coastal.train import train_with_metrics
from coastal.segment import TwoPassSegmentationInference
from coastal.utils import match_masks_3d

PROJ="/home/dominik/cecelia-feijoa/projects/zolIMa"; IMG="fXgbTl"
OUT="/home/dominik/Downloads/TMP"; DEAD=("divergence","vorticity","flow_structure_alignment")
STITCH=0.2
t0=time.time()
lv,_=zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidDriftCorrected.ome.zarr",as_dask=True)
a=lv[0]; T,C,Z,H,W=a.shape
meta=json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]
PX=meta["PhysicalSizeX"]; PZ=meta["PhysicalSizeZ"]
print(f"== {IMG} {a.shape}  xy {PX:.3f} um  z {PZ:.1f} um  ({PZ/PX:.1f}x anisotropic)",flush=True)

def plane_metrics(z):
    raw=np.asarray(a[:,2,z]).astype(np.float32)
    sm=np.stack([filters.gaussian(f,sigma=3.0,preserve_range=True) for f in raw])
    _,fp=normalize_and_project(sm[:,None])
    prep,_,_,tmx=prepare_data_for_unet(fp,temporal_scales=[1,2,4,8],cumulative_window=5,verbose=False)
    return sm, prep, [{k:v for k,v in d.items() if k not in DEAD} for d in list(tmx)]

zc=Z//2
sm_mid, prep_mid, tm_mid = plane_metrics(zc)
print(f"training once on z={zc}…",flush=True)
m=train_with_metrics(prep_mid,tm_mid,variance_metrics_norm=None,num_epochs=30,intensity_weight=1.0,
  foreground_weight=1.0,temporal_weight=2.0,confetti_weight=0.0,variance_as_input=False,seed=42,device="cuda")
if isinstance(m,tuple): m=m[0]
inf=TwoPassSegmentationInference(model=m,device="cuda",prob_threshold=0.3,
    seed_size_large=14,affinity_threshold_large=0.3,embedding_blur_sigma_large=1.5,
    seed_blur_sigma_large=5.0,merge_max_distance_large=1.5,merge_affinity_threshold_large=0.65,
    seed_size_small=7,affinity_threshold_small=0.4,embedding_blur_sigma_small=1.0,
    seed_blur_sigma_small=0.0,merge_max_distance_small=1.5,merge_affinity_threshold_small=0.60,
    max_iter=200,min_component_size=60,min_component_size_small=6)

lab2d=np.zeros((Z,T,H,W),np.uint32)
mid_disp=None
for z in range(Z):
    smz, prepz, tmz = (sm_mid, prep_mid, tm_mid) if z==zc else plane_metrics(z)
    if z==zc: mid_disp=smz
    for t in range(T):
        _,inst,_=inf.predict_frame(prepz[t],tmz[t])
        lab2d[z,t]=np.asarray(inst)
    print(f"  z={z}/{Z-1}  ({time.time()-t0:.0f}s)",flush=True)

print("\nstitching across Z per timepoint…",flush=True)
VOX=PX*PX*PZ
lab3d_all=[]
rows=[]
for t in range(T):
    stack=lab2d[:,t]
    m3=np.asarray(match_masks_3d(stack, stitch_threshold=STITCH))
    lab3d_all.append(m3)
    n2d=int(sum(len(np.unique(stack[z]))-1 for z in range(Z)))
    props=[p for p in measure.regionprops(m3) if p.area>0]
    d=np.array([2*(3*p.area*VOX/(4*np.pi))**(1/3) for p in props])
    ze=np.array([p.bbox[3]-p.bbox[0] for p in props])
    rows.append((n2d, len(props), d, ze))
np.savez_compressed(f"{OUT}/_fxgbtl_lab3d.npz", lab3d=np.stack(lab3d_all).astype(np.uint16))

# Stratified, because a single median describes the wrong population here: pass-2 particles
# outnumber cells ~10:1 and are single-plane by nature, so a pooled median z-extent reports on
# particles and says nothing about whether CELLS stitched across Z. 6 um splits them.
print(f"\n{'':<22}{'count':>7} {'med z-extent':>13} {'med diam':>10}")
print("-"*56)
for nm, sel in (("cells (>=6 um)", lambda d: d>=6.0), ("particles (<6 um)", lambda d: d<6.0)):
    cs=[];zs=[];ds=[]
    for n2d,n3,d,ze in rows:
        k=sel(d)
        cs.append(int(k.sum()))
        if k.any(): zs.append(float(np.median(ze[k]))); ds.append(float(np.median(d[k])))
    print(f"{nm:<22}{np.mean(cs):>7.1f} {np.median(zs):>10.1f} pl {np.median(ds):>9.1f} um")
print(f"\n2D labels summed over Z : {np.mean([r[0] for r in rows]):.0f}")
print(f"3D objects after stitch : {np.mean([r[1] for r in rows]):.0f}")
print(f"a real cell is ~11 um across; z spacing {PZ} um, so it should span ~{11.0/PZ:.0f} planes")
zext=[np.median(r[3]) for r in rows]; n3d=[r[1] for r in rows]

TS=15; m3=lab3d_all[TS]
zs=[zc-4,zc-2,zc,zc+2,zc+4]
fig,ax=plt.subplots(2,len(zs),figsize=(4.3*len(zs),9))
for c,z in enumerate(zs):
    raw=np.asarray(a[TS,2,z]).astype(np.float32)
    d=exposure.rescale_intensity(filters.gaussian(raw,sigma=3.0,preserve_range=True),
                                 in_range=tuple(np.percentile(raw,(1,99.7))))
    ax[0,c].imshow(d,cmap="gray"); ax[0,c].set_title(f"z={z} raw",fontsize=8)
    ax[1,c].imshow(d,cmap="gray")
    ax[1,c].contour(segmentation.find_boundaries(m3[z],mode="thick"),levels=[0.5],colors="#00e5ff",linewidths=0.6)
    ax[1,c].set_title(f"z={z}: {len(np.unique(m3[z]))-1} labels\n(3D-stitched IDs)",fontsize=8)
    for r in (0,1): ax[r,c].axis("off")
fig.suptitle(f"{IMG} 3D two-pass, t={TS} — per-plane 2D then IoU stitching across Z (thr {STITCH})\n"
             f"{np.mean(n3d):.0f} 3D objects vs {np.mean(n2d):.0f} 2D labels summed over Z; "
             f"median z-extent {np.median(zext):.1f} planes = {np.median(zext)*PZ:.1f} um",fontsize=11)
fig.tight_layout(rect=(0,0,1,0.9))
fig.savefig(f"{OUT}/flow_15_3d_{IMG}.png",dpi=105)
print(f"\nfigure: {OUT}/flow_15_3d_{IMG}.png   total {time.time()-t0:.0f}s")
