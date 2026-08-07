"""Pass 1 is merging touching cells at Dml3RG t=92. Tune on that frame before re-running 181.

Hypothesis: seed_size_large=34 is the culprit, not seed_blur. Seeds are local maxima in a
seed_size window, so two cells whose centres are closer than seed_size can only produce ONE seed
and are necessarily merged. Cells here are ~33 px across, so 34 forbids any touching pair. That
value was chosen to fight fragmentation BEFORE seed_blur_sigma existed; seed blur now does that
job, so seed_size should be able to come back down.

Fast iteration: flows over a +-12 frame window around t=92 rather than all 181, and training on
that window. Normalisation percentiles still come from the WHOLE movie so the input matches the
full run. Caveat: the model is trained on 25 frames rather than 37 strided over the movie, so
absolute counts here are indicative — the RANKING of parameters is what transfers.
"""
import json, sys
sys.path.insert(0, "/home/dominik/cc-workspace/coastal")
import numpy as np, matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
from skimage import filters, measure, segmentation, exposure
from cecelia.utils import zarr_utils
from coastal.flow import prepare_data_for_unet, normalize_and_project
from coastal.train import train_with_metrics
from coastal.segment import TwoPassSegmentationInference

PROJ="/home/dominik/cecelia-feijoa/projects/zolIMa"; IMG="Dml3RG"; T0=92; R=12
OUT="/home/dominik/Downloads/TMP"; DEAD=("divergence","vorticity","flow_structure_alignment")
lv,_=zarr_utils.open_as_zarr(f"{PROJ}/0/{IMG}/ccidDriftCorrected.ome.zarr",as_dask=True)
a=lv[0]; PX=json.load(open(f"{PROJ}/1/{IMG}/ccid.json"))["meta"]["PhysicalSizeX"]; zc=a.shape[2]//2
CELL_PX = 11.0/PX
print(f"== {IMG} t={T0} z={zc}  cell ~{CELL_PX:.0f} px", flush=True)

raw=np.asarray(a[:,2,zc]).astype(np.float32)
sm=np.stack([filters.gaussian(f,sigma=3.0,preserve_range=True) for f in raw])
_,fp_all=normalize_and_project(sm[:,None])          # global percentiles, as in the full run
lo,hi=T0-R,T0+R+1
prep,_,_,tmx=prepare_data_for_unet(fp_all[lo:hi],temporal_scales=[1,2,4,8],cumulative_window=5,verbose=False)
tm=[{k:v for k,v in d.items() if k not in DEAD} for d in list(tmx)]
CT=T0-lo
m=train_with_metrics(prep,tm,variance_metrics_norm=None,num_epochs=30,intensity_weight=1.0,
  foreground_weight=1.0,temporal_weight=2.0,confetti_weight=0.0,variance_as_input=False,seed=42,device="cuda")
if isinstance(m,tuple): m=m[0]

def run(seed_size, seed_blur, aff=0.3):
    inf=TwoPassSegmentationInference(model=m,device="cuda",prob_threshold=0.3,
        seed_size_large=seed_size,affinity_threshold_large=aff,embedding_blur_sigma_large=1.5,
        seed_blur_sigma_large=seed_blur,merge_max_distance_large=1.5,merge_affinity_threshold_large=0.65,
        seed_size_small=7,affinity_threshold_small=0.4,embedding_blur_sigma_small=1.0,
        seed_blur_sigma_small=0.0,merge_max_distance_small=1.5,merge_affinity_threshold_small=0.60,
        max_iter=200,min_component_size=60,min_component_size_small=6)
    _,inst,_,prov=inf.predict_frame(prep[CT],tm[CT],return_provenance=True)
    inst=np.asarray(inst); prov=np.asarray(prov)
    p1=np.zeros_like(inst)
    for p in measure.regionprops(inst):
        msk=inst==p.label
        if (prov[msk]==1).sum()>=(prov[msk]==2).sum(): p1[msk]=p.label
    props=[p for p in measure.regionprops(p1) if p.area>0]
    e=np.array([2*np.sqrt(p.area*PX**2/np.pi) for p in props]) if props else np.array([0.])
    # a cell is ~11 um; anything past 18 um is two or more cells fused
    return p1, len(props), float(np.median(e)), float(np.percentile(e,95)), int((e>18).sum())

print(f"\n{'seed_size':>9} {'seed_blur':>9} | {'cells':>6} {'med µm':>7} {'p95 µm':>7} {'>18µm (merged)':>15}")
print("-"*62)
best=[]
for ss in (14,20,26,34):
    for sb in (3.0,5.0,8.0):
        lab,n,md,p95,nm_=run(ss,sb)
        best.append((nm_, -n, ss, sb, lab, n, md, p95))
        print(f"{ss:>9} {sb:>9.1f} | {n:>6} {md:>7.1f} {p95:>7.1f} {nm_:>15}", flush=True)

best.sort()
show=[(34,8.0)]+[(b[2],b[3]) for b in best[:2] if (b[2],b[3])!=(34,8.0)]
disp=exposure.rescale_intensity(sm[T0],in_range=tuple(np.percentile(sm[T0],(1,99.7))))
fig,ax=plt.subplots(2,len(show),figsize=(6.4*len(show),12.4))
if len(show)==1: ax=ax.reshape(2,1)
for c,(ss,sb) in enumerate(show):
    lab,n,md,p95,nm_=run(ss,sb)
    tag=" (current)" if (ss,sb)==(34,8.0) else ""
    ax[0,c].imshow(disp,cmap="gray")
    ax[0,c].contour(segmentation.find_boundaries(lab,mode="thick"),levels=[0.5],colors="#00e5ff",linewidths=0.6)
    ax[0,c].set_title(f"seed_size {ss} · seed_blur {sb}{tag}\n{n} cells · med {md:.1f} µm · "
                      f"p95 {p95:.1f} µm · {nm_} over 18 µm",fontsize=9)
    ax[1,c].imshow(disp[300:600,300:600],cmap="gray")
    ax[1,c].contour(segmentation.find_boundaries(lab[300:600,300:600],mode="thick"),
                    levels=[0.5],colors="#00e5ff",linewidths=0.9)
    ax[1,c].set_title("zoom y300:600 x300:600",fontsize=9)
    for r in (0,1): ax[r,c].axis("off")
fig.suptitle(f"{IMG} t={T0} — pass 1 merging. seeds are local maxima in a seed_size window, so two "
             f"cells closer than that\ncan only make ONE seed. Cells are ~{CELL_PX:.0f} px across.",fontsize=11)
fig.tight_layout(rect=(0,0,1,0.94))
fig.savefig(f"{OUT}/flow_14_t92_tuning_{IMG}.png",dpi=105)
print(f"\nfigure: {OUT}/flow_14_t92_tuning_{IMG}.png")
