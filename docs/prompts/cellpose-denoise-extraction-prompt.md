# Investigation: Extract Cellpose 3 Denoising → Coastal

Opus investigation pass. Read everything before producing any plan. No code yet.

## Direction

**Favour the coastal-native implementation.** Vendoring Cellpose 3 denoising is a dead end — it inherits frozen architecture decisions, creates ongoing version pin risk, and produces no real future value for coastal as a package. The dependency compatibility audit (Step 0) still matters, but the answer doesn't change the direction: coastal builds its own denoising module regardless.

Cellpose 3 denoising is a **reference implementation** — understand what it does, what quality it achieves, what training data it used — then build something better. coastal owns the architecture, the weights strategy, and the speed optimizations. No Cellpose dependency, ever.

This is the right approach because:
- coastal may become independently useful to the community (optical flow + denoising as a standalone DL image processing package)
- A clean BSD/MIT implementation with no Cellpose dependency is far more useful than a vendored wrapper
- Speed optimizations can be designed in from the start, not retrofitted
- You're never blocked by Cellpose's versioning decisions again



---

## Temporal denoising — Opus to think independently

Static image denoising is a solved problem. Temporal denoising for intravital time-lapse microscopy is not — and this is where coastal could make a genuine contribution rather than reimplementing something that already exists ten times over.

The scientific context: intravital microscopy captures immune cells moving through living tissue over tens to hundreds of timepoints. The noise is not just spatial (shot noise, detector noise) but temporal — cells move between frames, fluorescence signal fluctuates, the tissue itself drifts slightly. Standard per-frame denoising ignores all temporal information and treats each frame independently. But the temporal dimension is information: a cell that appears bright in frames 1, 2, 3, 5 and dim in frame 4 — is frame 4 actually dimmer, or is it noise?

**Opus: think about whether the denoising approach can be extended to exploit temporal information.** This is an open question, not a specification. Consider:

- **3D+t as 4D**: treating a time-lapse volume as a 4D array (x, y, z, t) and denoising across all four dimensions simultaneously. What architectures support this? What are the memory constraints at typical intravital image sizes?
- **Recurrent / video denoising approaches**: methods like FastDVDnet, EDVR, or temporal shift modules that propagate information across frames. Do any of these have published weights for fluorescence microscopy?
- **Temporal consistency as a loss term**: if coastal trains or fine-tunes a denoising model, temporal consistency (denoised cell trajectory should be smooth) is a natural additional training signal beyond per-frame PSNR
- **Optical flow integration**: coastal already does optical flow. Does optical flow-guided temporal alignment before denoising reduce motion artefacts? (Align frames via optical flow → denoise the aligned stack → unalign.) This would be a coastal-native approach that no static denoising tool can do.
- **What the intravital community currently does**: what methods are actually used for time-lapse denoising in published intravital papers? Is it mostly per-frame Gaussian smoothing, or has anyone published a temporal method?

This is a research question, not an engineering task. Opus should report:
- What temporal denoising methods exist for fluorescence/intravital microscopy specifically
- Whether any have published weights or are practically usable
- Whether the optical flow + temporal alignment approach is novel or has been published
- A honest assessment of whether a temporal denoising module in coastal would be a meaningful contribution or would be duplicating existing work

If the answer is "this is genuinely underexplored and coastal's optical flow integration makes it uniquely positioned to do this well" — flag it as a potential coastal research direction, not just an engineering feature. That's a different kind of output than a port.



The primary motivation for extraction is unblocking Cellpose 4 for segmentation. Before anything else, confirm that the extracted denoising can coexist with Cellpose 4 in the same Pixi environment. This failed when trying to pin both Cellpose 3 and 4 together — the dependency conflict needs to be understood before designing the extraction.

Specifically:
- What are Cellpose 4's pinned dependencies (torch version, numpy version, any others that conflict)?
- What does the Cellpose 3 denoising module actually require at runtime (not at install time — just what it imports)?
- Once extracted, does coastal's denoising need any dependency that Cellpose 4 pins to a conflicting version?

The extraction only delivers value if Cecelia can run Cellpose 4 segmentation AND coastal denoising in the same env. If the denoising architecture requires a torch version that Cellpose 4 conflicts with, the extraction doesn't solve the problem — flag this immediately and report before proceeding to the rest of the investigation.

If the dependency conflict makes vendoring Cellpose 3 denoising impossible alongside Cellpose 4, this is not a dead end — it's the more interesting outcome. Coastal implements its own denoising rather than wrapping Cellpose 3. The Cellpose 3 implementation becomes a reference, not a dependency.

In this case, Step 0 should also investigate:
- What torch version does Cellpose 4 actually require?
- Are there published fluorescence microscopy denoising models (NAFNet, Noise2Void, CARE, DnCNN) with pretrained weights that run on that torch version?
- Could one of those be fine-tuned on Cellpose's published training data or Cecelia's own microscopy data — not retrained from scratch, but fine-tuned from an existing checkpoint?

The goal is a denoising module that:
1. Runs in the same env as Cellpose 4 (no conflict)
2. Produces comparable quality to Cellpose 3 denoising on fluorescence images
3. Is cleanly licensed (BSD/MIT, no Cellpose dependency)
4. Has speed improvements built in from the start

Report both paths after Step 0: (A) vendoring is feasible, here's the plan; or (B) conflict exists, here's the own-implementation path. Opus recommends which to pursue.



Find the Cellpose 3 denoising module in the installed package or PyPI source. Key things to locate:
- The model architecture (UNet-style restoration network — find the class definition)
- How tightly it's coupled to other Cellpose internals (imports, shared utilities, data loading)
- The pretrained weight files: where they're downloaded from, what format, how large
- The inference call chain: what does `denoise_img()` or equivalent actually do end-to-end
- Whether GPU (CUDA/MPS) is already handled or hard-coded to one device

Produce a dependency map: which Cellpose 3 modules does the denoising actually import? Is it self-contained or does it pull in segmentation internals?

---

## Step 2 — Assess extractability

How much of Cellpose 3 would need to come with the denoising to make it standalone?

Best case: the denoising is a separate `denoise.py` module with standard PyTorch imports and a weight download URL — extract it, done.

Worst case: it shares model infrastructure, data augmentation, or training utilities with the segmentation code — extraction means vendoring more than expected.

Report the actual answer from reading the code, not from the Cellpose docs.

---

## Step 3 — Investigate speed improvement opportunities

The Cellpose 3 denoising runs inference on image tiles. Speed bottlenecks are typically:

- **Tiling strategy**: how the image is split into patches, whether patches overlap, whether they're processed sequentially or batched
- **Batch size**: whether the current implementation processes one tile at a time or batches
- **Mixed precision**: whether `torch.autocast` / fp16 inference is used or if it runs full fp32
- **Compilation**: whether `torch.compile()` is applied (PyTorch 2.0+)
- **MPS/CUDA dispatch**: whether Apple Silicon MPS is actually used correctly (cf. the Harmony MPS bug in the codebase — search for it before writing any device selection code)

For each: what does the current Cellpose 3 implementation do, and what's the realistic improvement from changing it? Don't speculate — look at the actual code.

Also: are there published lighter/faster architectures (NAFNet, Restormer, SwinIR) that produce comparable quality on fluorescence microscopy? And if temporal denoising warrants it — is training from scratch a realistic path? What training data exists for intravital microscopy denoising, and what would a minimal viable training set look like?

---

## Step 4 — Read coastal

Read the coastal package thoroughly:
- What DL image processing it already does (optical flow, learned affinity)
- How it structures its Python modules
- Whether it already has a denoising module or anything adjacent
- How it currently handles GPU dispatch (CUDA/MPS/CPU)
- Where a `denoise` module would naturally live

---

## Step 5 — Attribution

Cellpose is BSD licensed. The extracted denoising code and pretrained weights need:
- An entry in `THIRD_PARTY.md` for both Cecelia and coastal
- Inline attribution in any extracted/adapted source file
- The specific Cellpose 3 version the weights were trained on

Also check: does Cellpose 3 denoising itself derive from prior work (CARE, Noise2Void, CSBDeep)? If so, that attribution chain must be preserved.

---

## Step 6 — Produce the plan

**Extraction plan**: exactly which files/classes to extract from Cellpose 3, what minimal dependencies come along, where in coastal they live, how weight download works on first use.

**Speed improvement plan**: which optimizations from Step 3 are worth implementing, in what order, with realistic expected gains.

**Cecelia integration plan**: once denoising lives in coastal, how Cecelia's denoising task module changes — a thin call into coastal, not a reimplementation. Confirm that the Cellpose 3 pin in the FAQ can be removed once this lands.

**Attribution**: exact THIRD_PARTY.md entries for both repos.

---

## Constraints

- Do not move optical flow or segmentation work out of coastal
- GPU dispatch must handle CUDA, Apple MPS, and CPU correctly — check the existing MPS bug pattern in the codebase before writing any device selection code
