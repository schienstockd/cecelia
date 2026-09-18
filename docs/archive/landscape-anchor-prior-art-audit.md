> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# Landscape-anchor prior-art audit — v2 Part 3

Companion to `bidirectional-context-sharing-audit-prompt_v2.md`. A targeted
prior-art sweep for the grid / landscape / mark-scheme mechanism in Part 3
of that brief (roughly lines 456–530 of the original). Produced 2026-09-18
by a WebSearch + WebFetch pass (~25 tool calls) triggered because v2's Part
3 was drafted with limited citations and one of the citations (iSBEM)
didn't resolve. Every non-obvious claim below has a URL. Negative claims
("could not verify X") reflect a targeted search, not exhaustive proof of
absence.

The three findings that changed v2 in the same commit that landed this
file:
- **iSBEM** as named could not be verified — replaced in v2 with SBEMimage
  (Titze et al., *Front Neural Circuits* 2018) + smart-microscopy roadmap
  review (*npj Imaging* 2026) + ZEN Smart Acquisition Toolkit + NIS.ai.
- **The coarse grid overlay ("tile B3") has published precedent** —
  SCAFFOLD / Marked-Grid Scaffold / Grid-Augmented Vision. Reframed in v2
  from "design this" to "pick from these three."
- **Raw SAM is the wrong default region source for fluorescence.** μSAM
  (Archit et al., *Nat Methods* 2025) and Cellpose-SAM (Pachitariu lab,
  bioRxiv 2025) named as the microscopy-appropriate candidates.

---

## 1. v2's existing citations — verify & fix

| v2's citation | Verdict | Notes |
|---|---|---|
| **Set-of-Mark (SoM), Yang et al., MSR, 2023** — beat fully-finetuned referring-segmentation on RefCOCOg zero-shot | **Correct**. arXiv [2310.11441](https://arxiv.org/abs/2310.11441). Yang et al., MSR + academic collaborators, Oct 2023. Overlays SAM-generated regions with alphanumeric marks; purely inference-time (no training). Reported to beat SOTA fully-finetuned referring-seg on RefCOCOg zero-shot. Code: [microsoft/SoM](https://github.com/microsoft/SoM). | v2's summary is accurate; keep. |
| **Micropilot, Conrad et al., Nature Methods 2011** — real-time classifier-driven acquisition | **Correct**. [Nat Methods 8, 246–249](https://www.nature.com/articles/nmeth.1558) (doi:10.1038/nmeth.1558). EMBL, ML-based online cell classification triggering high-resolution follow-up imaging. | Keep. |
| **iSBEM — successor to Micropilot, volume-EM automated targeting** | **Cannot verify as named.** No paper or software called "iSBEM" surfaced across multiple searches. The related, verifiable work in this niche is: **SBEMimage** ([Titze et al., Front Neural Circuits 2018](https://www.frontiersin.org/articles/10.3389/fncir.2018.00054/full)), the open-source SBEM acquisition controller; **CLEM/FIB-SEM fluorescence-guided targeting** (Karreman et al. + successors, e.g. [ultraLM/miniLM, JCB 2016](https://pmc.ncbi.nlm.nih.gov/articles/PMC5234702/)); and **Crosshair** ([eLife 2022](https://elifesciences.org/articles/80899)) for motorised-ultramicrotome semi-automated targeting. | **Fix v2**: replace "iSBEM" with SBEMimage + CLEM/fluorescence-guided FIB-SEM as the actual precedents. |

---

## 2. Broader visual-grounding landscape for VLMs

### 2a. Set-of-Mark family and adjacent visual-prompting

| Method | Idea | Status | Code |
|---|---|---|---|
| **SoM** (Yang 2023) | SAM regions + alphanumerics; VLM references them by mark id | Established, widely reproduced | [github.com/microsoft/SoM](https://github.com/microsoft/SoM) |
| **Red-circle** ([Shtedritski et al., ICCV 2023](https://arxiv.org/abs/2304.06712)) | Draw a red ellipse around the target; CLIP attends there emergently | Original visual-prompting result, elegant, low-cost | [suny-sht/clip-red-circle](https://github.com/suny-sht/clip-red-circle) |
| **ViP-LLaVA** ([CVPR 2024](https://arxiv.org/abs/2312.00784)) | Composite arbitrary prompt shapes (arrow, scribble, rectangle) directly on the RGB image; alpha-blend + multi-layer CLIP features | Training-time, but no bespoke region encoder | [vip-llava.github.io](https://vip-llava.github.io/) |
| **Alpha-CLIP** ([arXiv 2312.03818](https://arxiv.org/abs/2312.03818)) | Extend CLIP with an alpha-mask input channel focusing attention | Requires model modification/training | — |

### 2b. Region-encoding models (heavier — full LLM+region training)

**Kosmos-2**, **Shikra**, **GPT4RoI** ([arXiv 2307.03601](https://arxiv.org/abs/2307.03601)), **Ferret / Ferret-v2** ([arXiv 2404.07973](https://arxiv.org/abs/2404.07973)), **GLaMM** ([CVPR 2024](https://openaccess.thecvf.com/content/CVPR2024/papers/Rasheed_GLaMM_Pixel_Grounding_Large_Multimodal_Model_CVPR_2024_paper.pdf)). All accept coordinates/RoIs as first-class input. Ferret-v2-13B leads on Ferret-Bench (74.9 avg vs Kosmos-2 44.6, Shikra-7B 45.9). **Not relevant here** — Cecelia would call the frontier VLM (Claude), not host its own region-grounded LLM.

### 2c. Grid/labelled-tile schemes — v2's "tile B3" idea has published prior art

- **SCAFFOLD** ([Lei et al., arXiv 2402.12058](https://arxiv.org/abs/2402.12058)) — overlays a uniform dot matrix with labelled (x,y) coordinates; textual answers reference the labels. Extends to 3D (t,x,y) for image sequences. Directly the same idea as v2's tile B3.
- **Marked-Grid Scaffold** (2024 follow-up work in GUI-grounding literature) — overlays an 8×8 or 9×9 labelled grid; discretises coordinate prediction into a classification-over-grid-IDs task.
- **Grid-Augmented Vision** ([Chen 2024, arXiv 2411.18270](https://arxiv.org/abs/2411.18270)) — plain black 9×9 grid overlay, no training, improves localization on GUI/robotics/medical images.

**v2's "coarse spreadsheet-style grid overlay (tile B3)" is not novel** — it is exactly SCAFFOLD/Marked-Grid/Grid-Augmented Vision. Cite these; don't present as original. The design ask changes from "invent this" to "pick which of three published grid schemes to reuse."

---

## 3. Segmentation-first-then-mark (the "landscape resolution" pass)

### 3a. Speed variants of promptable segmentation

| Model | Where measured | Latency shape | Code |
|---|---|---|---|
| **SAM** (ViT-H) | GPU baseline | Reference; heavy | Meta AI |
| **FastSAM** | GPU | ~40 ms/image (~100 img/s) | [github](https://github.com/CASIA-IVA-Lab/FastSAM) |
| **MobileSAM** ([arXiv 2306.14289](https://arxiv.org/abs/2306.14289)) | Mobile / lightweight | ~10 ms (encoder 8 + decoder 4) — ~5× faster than FastSAM | [github](https://github.com/ChaoningZhang/MobileSAM) |
| **EfficientSAM** | GPU | 10–20 img/s (S / Ti variants); EfficientViT-SAM-L0 ~30× faster than SAM-H on GPU, ~50× on CPU | Meta |
| **EdgeSAM** ([arXiv 2312.06660](https://arxiv.org/abs/2312.06660)) | Edge | ~37× faster than SAM on 2080 Ti; 7× faster than MobileSAM on iPhone 14 | — |
| **SAM 2** ([Meta 2024](https://arxiv.org/abs/2408.02924)) | GPU | Hiera-Large ~5× faster than SAM-ViT-H with +4.8 mAP; adds video/temporal propagation | [github](https://github.com/facebookresearch/segment-anything-2) |
| **SEEM** ([Zou et al., NeurIPS 2023, arXiv 2304.06718](https://arxiv.org/abs/2304.06718)) | GPU | Comparable to SAM; unifies text/point/box/scribble prompts + memory | [UX-Decoder/SEEM](https://github.com/UX-Decoder/Segment-Everything-Everywhere-All-At-Once) |

All published latencies assume GPU + ~1 MP natural images. On a 1024×1024 fluorescence tile on Cecelia's typical hardware, MobileSAM or EfficientSAM-Ti is the most realistic "cheap pass" candidate.

### 3b. Classical alternatives (still meaningful for a truly cheap pass)

- **SLIC superpixels** and **Felzenszwalb** — CPU-only, sub-second on 1 MP, no model, no domain-shift issue. See [skimage comparison](https://scikit-image.org/docs/stable/auto_examples/segmentation/plot_segmentations.html).
- **Watershed** — closed-boundary, weak-edge-robust, over-segments in dense scenes.
- Where they still beat SAM variants: (a) no GPU available, (b) truly unfamiliar contrast (SHG, phase, unusual multiplexed panels), (c) when the goal is *visual over-segmentation* to feed a marks-based scheme rather than semantic regions.

### 3c. K-means on raw channel intensities (v2's proposal)

**No published precedent found** for using k-means-on-channels specifically as a candidate-region generator for VLM prompting. Standard k-means on multichannel intensity is a routine unsupervised segmentation baseline (skimage tutorials, decades of use), but as an *input to a Set-of-Mark-style VLM handoff* it appears to be a Cecelia-original combination. Not a red flag — it's a sensible cheap baseline — but worth naming as ad hoc rather than pre-validated.

---

## 4. Microscopy-specific — what actually matters for this domain

### 4a. Bioimage foundation models with promptable segmentation

| Model | Coverage | Prompt types | Availability | Note |
|---|---|---|---|---|
| **μSAM / MicroSAM** (Archit et al., [Nature Methods 22, 579–591, 2025](https://www.nature.com/articles/s41592-024-02580-4)) | Light + electron microscopy; 2D and volumetric via napari plugin | Point / box / mask, iteratively fine-tuned | [github.com/computational-cell-analytics/micro-sam](https://github.com/computational-cell-analytics/micro-sam), napari plugin | **Most relevant single citation for this design.** Generalist LM/EM models trained specifically for microscopy prompting. |
| **CellSAM** (Israel/Van Valen et al., [Nature Methods 2025](https://www.nature.com/articles/s41592-025-02879-w)) | Mammalian, yeast, bacteria across modalities; auto cell detector (CellFinder) prompts SAM | Automatic — no user prompt needed | [cellsam.deepcell.org](https://cellsam.deepcell.org/) | Auto-prompted; strong zero-shot; not necessarily better than μSAM on intravital. |
| **Cellpose-SAM** (Pachitariu lab, [bioRxiv May 2025](https://www.biorxiv.org/content/10.1101/2025.04.28.651001v1)) | Cellular seg; robust to channel shuffling, size, noise, blur | SAM backbone within Cellpose; usual auto flow | [MouseLand/cellpose](https://github.com/MouseLand/cellpose) | "Superhuman generalization" claim; approaches human-consensus. **Strong candidate — you already use Cellpose.** |
| **FluoResFM** ([Nat Comm 2026](https://www.nature.com/articles/s41467-026-70307-4)) | Fluorescence *restoration* (denoise/deblur), not segmentation | Cross-distribution generalist | — | Adjacent, not directly a mark-generator. |

**Semantic-vs-anatomical caveat.** On intravital multichannel data, μSAM regions and Cellpose(-SAM) instances *are* biologically meaningful (cells/nuclei). Raw SAM regions on the same image typically split by intensity/texture — often not on cell boundaries. For Cecelia's use case, **μSAM or Cellpose-SAM as the candidate-region source is materially better than raw SAM**, and this is well-established in the 2024–2025 benchmarking literature (e.g. [Microscopy Cell Segmentation Review & Benchmarking, J. Imaging 2025](https://doi.org/10.3390/jimaging12070297)).

### 4b. Classical automated ROI targeting — the "post-Micropilot" landscape

- **ZEISS ZEN Smart Acquisition Toolkit** — commercial event/target/quality-driven adaptive acquisition; DL-based object detection triggering high-res follow-up. [Product page](https://www.zeiss.com/microscopy/us/products/software/zeiss-zen/smart-acquisition-toolkit.html).
- **Nikon NIS.ai** (part of NIS-Elements) — DL-based inference-time detection for adaptive acquisition. [Product page](https://www.microscope.healthcare.nikon.com/products/software/nis-elements/nis-ai-1).
- **Smart microscopy roadmap** ([Nature *npj Imaging* 2026](https://www.nature.com/articles/s44303-026-00145-y)) — recent review categorising smart-microscopy strategies (quality / event / target / information / outcome). Cite in place of "iSBEM" for the classical-automated-targeting alternative.
- **EAP4EMSIG** ([arXiv 2504.00047](https://arxiv.org/abs/2504.00047)) — event-driven microscopy for microfluidic single-cell — recent worked example.
- **AILA** ("Artificially Intelligent Lab Assistant" — [Uni Jena, 2025](https://www.uni-jena.de/en/366828/the-autonomous-microscope)) — reportedly first end-to-end AI-conducted microscopy experiment.

The commercial world (ZEN, NIS.ai) already ships DL-driven event/target adaptive acquisition. **v2's Part-3 Micropilot-vs-Claude question ("would a classical detector outperform"?) is not a theoretical one** — it's the position ZEN and NIS.ai have shipped into.

### 4c. Interactive "point to region" tools in bioimage analysis — precedent for the drawing layer's UI

- **ilastik pixel classification / carving** ([Berg et al., Nat Methods 2019](https://www.nature.com/articles/s41592-019-0582-9)) — click-based sparse-annotation → RF classifier; canonical.
- **QuPath** cell-classifier UI — click-annotation on WSIs.
- **napari** built-in point / shape layers; multiple community plugins (napari-APOC, Convpaint, Medical-SAM2 GUI [arXiv 2602.22649](https://arxiv.org/abs/2602.22649)) for interactive segmentation-in-viewer.
- **CellProfiler Analyst** — gating classifiers over per-cell measurements.
- **Convpaint** ([bioRxiv 2024](https://www.biorxiv.org/content/10.1101/2024.09.12.610926v1)) — pretrained NN features + user clicks in napari.

These are the design precedents for a paint/annotation layer over a frozen frame. **v2's Part-1 audit already asks for "any existing freeform drawing/markup capability" as a reuse candidate — good; the answer for the wider community is "ilastik/napari-shape-layer/QuPath is the shape of that UI, well understood."**

### 4d. VLMs trained on microscopy/pathology (all with limits)

- **LLaVA-Med** ([Microsoft, 2023](https://github.com/microsoft/LLaVA-Med)) — GPT-4-generated instruction data. General biomedical; research-only.
- **PathChat** ([Lu et al., Nature 2024](https://pmc.ncbi.nlm.nih.gov/articles/PMC11464372/)) — H&E pathology; 78.1% multiple-choice, +63.8% over LLaVA-Med.
- **BioMedCLIP** — CLIP adapted to biomedical.
- **Patho-R1** ([arXiv 2505.11404](https://arxiv.org/html/2505.11404v2)) — RL-based reasoning; 2025.

**All are pathology-slide-shaped**, not intravital / multichannel fluorescence / time-lapse. Do not assume any of these help for Cecelia's core use case.

---

## 5. Watch-outs — where this fails

- **VLM coordinate accuracy is a known weakness.** Comprehensive spatial-reasoning benchmarks (Spatial457, OmniSpatial, SIBench) put 2D coordinate errors ~20% and 3D ~29% on natural images — *before* any microscopy domain shift ([Spatial457, arXiv 2502.08636](https://arxiv.org/abs/2502.08636)). Grid/mark schemes work *precisely because* they replace coordinate regression with discrete label selection.
- **Domain shift is real and quantified.** Multiple papers document SAM performance collapse on fluorescence microscopy without fine-tuning: diffraction-limited resolution, low contrast, densely overlapping organelles ([SAM4MIS](https://github.com/YichiZhang98/SAM4MIS); [PMC review 2025](https://pmc.ncbi.nlm.nih.gov/articles/PMC13412841/)). **Do not build on raw SAM as the region source; use a microscopy-fine-tuned variant.**
- **Semantic mismatch.** A SAM region is a *visual* region, not a biological structure. On dense fluorescence, SAM tends to segment by contrast/texture blobs, which frequently split single cells or merge touching ones — quantified in the same [Digital Pathology zero-shot study (arXiv 2304.04155)](https://arxiv.org/abs/2304.04155) and the [SAM3 pathology evaluation (arXiv 2604.18225)](https://arxiv.org/abs/2604.18225).
- **Small-object / dense-scene failure.** SAM/pathology-SAM struggles on "densely packed instances even with 20 point prompts" — this is the intravital-lymphocyte case exactly.
- **SoM on microscopy specifically — no published evaluation found.** SoM has been widely tried on natural images, GUIs and robotics; a targeted evaluation on fluorescence/intravital microscopy did not surface in this search. **Treat as a real unknown.** The design should include a small internal evaluation (a handful of frames from zolIMa / jFWePN with expert-marked ground-truth ROIs, compared against SoM+SAM, SoM+μSAM, SoM+Cellpose-SAM, and the plain grid overlay), not assume transfer.
- **PathChat/LLaVA-Med hallucinate freely.** Multi-choice accuracy hides the fact that "GPT-4V frequently presents flawed rationales in cases where it makes correct final choices (35.5%)" ([PMC 2024](https://pmc.ncbi.nlm.nih.gov/articles/PMC10896362/)). Don't ship a design that trusts VLM-generated free-text rationale about pixel positions without the mark scheme constraining it.

---

## 6. Recent (2024–2026) developments that shift the picture

- **μSAM in Nature Methods (2025)** — canonical citation now exists; use it, don't rediscover.
- **Cellpose-SAM (2025)** — combines your existing tool with a SAM backbone; single strongest candidate for the region source you already know how to run.
- **CellSAM in Nature Methods (2025)** — CellFinder+SAM prompt engineering; auto mode with no user prompt.
- **Smart-microscopy review** ([npj Imaging 2026](https://www.nature.com/articles/s44303-026-00145-y)) — best current "here's what people ship" survey; use in place of the un-verifiable iSBEM reference.
- **Agentic microscopy platforms landing in 2025–2026** — [AILA](https://www.uni-jena.de/en/366828/the-autonomous-microscope), [SIMBA (PubMed)](https://pubmed.ncbi.nlm.nih.gov/42079268/), [CrystalForge EM2CIF (Sci Adv)](https://www.science.org/doi/10.1126/sciadv.aed0583), [ACMAS](https://academic.oup.com/mam/article/32/Supplement_1/ozag053.977/8742936). This is the peer-pressure evidence that the "Claude Imaging" framing v2 uses isn't isolated ambition.
- **SAM 2 & SAM 3** — video / temporal propagation and text-prompt-native segmentation. If Cecelia does short-clip capture (Part 2), SAM 2's mask propagation is the correct baseline.

---

## 7. Concrete recommendations for v2

**Survive as-is:** Set-of-Mark (Yang 2023); Micropilot (Conrad 2011). Keep both.

**Fix now:**
1. **Remove "iSBEM"** — cannot verify. Replace the "iSBEM and similar volume-EM workflows" phrase with a citation to **SBEMimage** (Titze et al., Front Neural Circuits 2018) *plus* the **smart-microscopy roadmap** review (npj Imaging 2026) as the current-state-of-the-art survey.
2. **Downgrade the "coarse grid overlay (tile B3)" from implicit-novel to explicitly-precedented.** Cite **SCAFFOLD** (Lei et al., arXiv 2402.12058), **Marked-Grid Scaffold**, and **Grid-Augmented Vision** (Chen 2024, arXiv 2411.18270). Reframe v2's ask from "design this" to "pick from these three."
3. **Add μSAM (Archit et al., Nat Methods 2025) and Cellpose-SAM (bioRxiv 2025) as candidate region sources** alongside "a lightweight pretrained general segmentation model (e.g. a fast SAM variant)." The v2 prompt currently biases toward generic SAM — for microscopy this is the wrong default. μSAM/Cellpose-SAM produce biologically-meaningful regions; SAM does not.
4. **Add ZEN Smart Acquisition Toolkit and NIS.ai** to the "automated ROI-targeting in microscopy without any LLM is already mature" list. Micropilot alone dates the argument to 2011; ZEN/Nikon ship this today.

**Add to the evaluation candidates:**
- **ViP-LLaVA** and **Alpha-CLIP** as visual-prompting *design references* (alternate mark-embedding strategies) even though they're not what Claude uses at inference time.
- **SEEM** — same tier as SAM for the region source; unified prompt types.

**Deemphasise / drop from consideration:**
- **LLaVA-Med, PathChat, BioMedCLIP as region-generators** — they're VQA/chat models, not segmentation-region sources; they can't drive the mark scheme. Mention only if v2 also wants a microscopy-VLM future, and be clear they're pathology-slide-shaped, not intravital.
- **Region-encoding LLMs (Ferret, GLaMM, Kosmos-2, GPT4RoI)** — Cecelia won't host these; drop from Part 3's scope entirely.

**The watch-out list the design agent must know:**
1. VLM coordinate regression is broken; mark schemes exist because of this — don't quietly ask Claude for coordinates as a fallback path.
2. Raw SAM regions on fluorescence are contrast-blobs, not cells — use μSAM/Cellpose-SAM.
3. SoM has no published evaluation on fluorescence/intravital microscopy — plan a small internal eval on real Cecelia frames before locking the design.
4. Dense-scene failure of promptable-seg models is well-documented on pathology; intravital lymphocyte swarms will hit the same wall.
5. ZEN/NIS.ai already ship classical event-driven acquisition — the "Claude as ROI-picker" question isn't hypothetical; it's whether language-flexibility is worth losing determinism.

**Novelty check for v2's core idea:** grid overlay = not novel (SCAFFOLD/Marked-Grid/Grid-Augmented Vision). SoM+cheap-semantic-pass as the second layer = combination novel; individual pieces all published. K-means-on-channels as the specific candidate-generator = no direct precedent, plausible cheap baseline. The **framing** — "grid + landscape pass shipped as a standalone Vue feature, not Claude-exclusive" — is the design contribution worth keeping; the underlying primitives are all pre-existing and should cite as such.
