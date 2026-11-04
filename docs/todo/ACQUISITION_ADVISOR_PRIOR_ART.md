# Acquisition advisor: literature and prior-art review

Prepared 2026-11-04 by a Claude research agent for Dominik (Cecelia). **Everything below comes from pages fetched in this session**: Crossref API records, Europe PMC / PMC OAI full text or abstracts, arXiv API, GitHub API (repo trees and files), vendor and facility web pages, and the Micro-Manager device list. If a claim rests on an abstract only, it says so. Vendor claims are marked **[vendor]**. Anything not retrieved is listed in §6. Nobody has run any of the tools.

---

## 1. Summary (10 lines)

1. The guidance on acquisition errors is mature and consistent: North 2006, Waters 2009, Jonkman 2020, Pawley 2006, Laissue/Icha 2017, Kiepas 2020. Most of the rules are already numeric enough to turn into code, so no new knowledge needs to be invented.
2. Validated image-side metrics exist for **saturation** (Percent Maximal) and **blur** (power log-log slope). Bray 2012 validated both, with an F-score, and CellProfiler ships them. Single-image **gain/noise** estimation is published (Heintzmann 2016). Model-free **effective resolution** is published (Descloux 2019).
3. **Nyquist, pinhole and z-step** checks depend only on metadata (pixel size, NA, λ, pinhole µm, RI). Faklaris 2022 gives a usable criterion and the SVI calculator is free. ZEN already has "Optimal"/"Confocal"/"1 AU"/z-"Optimal" buttons, so the advisor's job is to check *whether they were used*, not to re-derive them.
4. **Bleed-through and phototoxicity cannot be settled from one test image.** Bleed-through needs single-label controls or the track layout. Phototoxicity has no image metric that proves it absent, only proxies plus a light-dose record. QUAREP WG13 has published nothing yet; a joint WG1/WG13 manuscript is in progress.
5. **Frame rate for tracking** has no rule-of-thumb paper I could retrieve in full. The Cell Tracking Challenge (Maška 2023) gives computable dataset properties (cell spacing, frame-to-frame overlap, intensity change, SNR) that correlate with tracking and segmentation performance. That is the closest thing to a validated basis.
6. Vendor software already covers part of the job at the microscope. ZEN has Smart Setup (dye → light path, with a predicted crosstalk display), a Range Indicator for saturation, and Nyquist/z-step buttons. **[vendor]** For the FV4000, Evident claims SilVIR HV, gain and offset are fixed, so detector-gain mistakes largely disappear on that scope.
7. **Huygens Image Quality Control** (commercial, SVI) is the closest existing "diagnose an image" product: clipping, hot/cold pixels, crosstalk, chromatic aberration, bleaching, SNR, relative Nyquist. There is a free online version with limited features. I found no validation data.
8. LLM assistants exist. MicroClaw (camera and Micro-Manager only, explicitly no confocal settings) carries a Jonkman-2020 knowledge skill plus per-frame saturation, SNR and focus numbers. Two facilities (Imperial FILM, AIC Turku) give users a hardware-inventory file and a prompt for Claude or ChatGPT; neither looks at images. I found **no validation of LLM-recommended acquisition settings** anywhere.
9. **Nothing found makes the advisor redundant.** No tool combines (a) a test acquisition from an LSM 780/980 or FV4000, (b) metadata-aware computed checks covering confocal settings, intravital issues and tracking cadence, and (c) LLM interpretation. Huygens QC and MicroClaw each cover a slice.
10. Actuation, for later: Micro-Manager has **no adapter** for any of the three scopes. LSM 980 can be driven via ZEN API (gRPC, ZEN ≥3.11, Apache-2.0 client), but the published protos expose tracks/channels on/off, z-stack, time series and experiments, not laser power, gain or pinhole. LSM 780 is reachable via ZEN black VBA/COM (MyPiC). FV4000 via an Evident "RDK" that only a third-party page ties to the FV4000.

---

## 2. Part A: guidance and papers on acquisition errors

### 2.1 Sources, one by one

**North AJ (2006). "Seeing is believing? A beginners' guide to practical pitfalls in image acquisition." *J Cell Biol* 172:9–18. doi:10.1083/jcb.200507103** (open access, PMC2063524; full text read)
- Covers: pinhole and optical section thickness; matching section thickness across channels; gain/offset; saturation; Nyquist; cross-talk; photobleaching; resolution and false colocalisation.
- Actionable rules quoted from the text:
  - "the exposure time and/or gain … and offset … should be adjusted to use the entire dynamic range of the detectors. Too high a gain results in saturated pixels, which cannot be quantified."
  - Use a "range indicator" or "glow scale" LUT.
  - Pinhole is usually 1 Airy Unit, and "for multicolor imaging it is critical to achieve the same optical section thickness in all channels."
  - Sampling intervals "must be more than two times smaller than the smallest resolvable feature."
  - "Single-labeled controls should always be used to assess bleed-through," and on a confocal, image "with each laser line deactivated in turn."
  - "Be particularly suspicious of cross-talk if your two fluorochromes appear to be 100% colocalized."
- Automation: saturation, offset clipping and section-thickness matching (from metadata) are **automatic checks**. The bleed-through test protocol is a **knowledge-base entry** that the advisor asks the user to run.

**Waters JC (2009). "Accuracy and precision in quantitative fluorescence microscopy." *J Cell Biol* 185:1135–1148. doi:10.1083/jcb.200903097** (OA, PMC2712964; full text read)
- Covers detector noise, offset, dynamic range, saturation and SNR, mostly for cameras. Its Table II is a checklist, including "Avoid saturating pixels," "Avoid high camera gain when a large dynamic range is needed," "Consider binning to increase SNR," and minimise excitation before acquisition.
- It also says "choosing to 'crop out' saturated areas is not acceptable."
- It gives the formula for converting intensity to photons from full-well capacity and offset.
- Automation: checklist entries become knowledge-base entries. Photon conversion becomes an automatic SNR check if detector specs are known.

**Jonkman J, Brown CM, Wright GD, Anderson KI, North AJ (2020). "Tutorial: guidance for quantitative confocal microscopy." *Nat Protoc* 15:1585–1611. doi:10.1038/s41596-020-0313-9** (not OA. Verified via Crossref and the Europe PMC abstract only. Publisher page blocked.)
- From the abstract, it covers sample prep, choice of microscope, "configuring the microscope parameters," photobleaching and cross-talk, "instrumentation problems that may prevent the acquisition of quantitative data," and analysis and statistics. There is a companion poster, doi:10.1038/s41596-020-0307-7 (Crossref record seen).
- It is the most comprehensive single confocal source and the one MicroClaw's knowledge skill is condensed from (see §3).
- Automation: the backbone for the knowledge base. Specific numbers must be taken from the paper itself, which I could not read.

**Pawley J (ed.) (2006). *Handbook of Biological Confocal Microscopy*, 3rd ed., Springer. doi:10.1007/978-0-387-45524-2**
- Relevant chapters, verified via Crossref only:
  - Pawley, "Points, Pixels, and Gray Levels: Digitizing Image Data," pp. 59–79, doi:10.1007/978-0-387-45524-2_4
  - Pawley, "Fundamental Limits in Confocal Microscopy," pp. 20–42, doi:10.1007/978-0-387-45524-2_2
  - Art, "Photon Detectors for Confocal Microscopy," pp. 251–264, doi:10.1007/978-0-387-45524-2_12
  - Sheppard et al., "Signal-to-Noise Ratio in Confocal Microscopes," pp. 442–452, doi:10.1007/978-0-387-45524-2_22
- It is the reference text, but paywalled and not read. Cite it for theory. Do not quote numbers from it in the knowledge base until someone reads it.

**Faklaris O et al. (2022). "Quality assessment in light microscopy for routine use through simple tools and robust metrics." *J Cell Biol* 221(11):e202107093. doi:10.1083/jcb.202107093** (OA, PMC9526251; full text read)
- Covers seven instrument QC protocols from 10 core facilities: resolution (PSF), field flatness, chromatic co-registration, illumination power stability, stage drift and repeatability, and camera noise. MetroloJ_QC automates the analysis.
- Useful numbers:
  - Nyquist "considered as met whenever pixel size was equal to or lower than λex/8*NA" for a closed pinhole. "For a 1 A.U. pinhole, the Nyquist criterion allows a 1.6× bigger pixel size," citing svi.nl and Piston 1998.
  - z-step of "0.15 … µm" for NA 1.4 confocal.
  - Bead SBR > 3.
  - Power stability "STAB power >97%."
  - The plugin reports "whether the acquisition meets the Shannon–Nyquist criterion" and discards saturated beads.
- Automation: the Nyquist criterion is a **directly codable metadata check**. The rest is instrument QC (facility side, bead slides), not sample-acquisition advice. It is relevant as context ("is the scope itself healthy?").

**Bray M-A, Fraser AN, Hasaka TP, Carpenter AE (2012). "Workflow and metrics for image quality control in large-scale high-content screens." *J Biomol Screen* (now SLAS Discovery) 17:266–274. doi:10.1177/1087057111420292** (PMC3593271; full text read via PMC OAI)
- Covers focus and saturation QC metrics, validated on HCS images against expert labels using F-score.
- Conclusions: "we recommend PLLS [power log-log slope] to detect blur and PM [Percent Maximal] to detect saturation artifacts." "PM detects saturation artifacts more robustly than the other candidate metrics." "a single QC metric that captures all types of artifacts is not feasible."
- Automation: **automatic checks with published validation**. Caveat: the validation was on widefield HCS, not confocal or intravital.

**CellProfiler MeasureImageQuality module** (manual, CellProfiler 4.2.6, https://cellprofiler-manual.s3.amazonaws.com/CellProfiler-4.2.6/modules/measurement.html)
- Computes: FocusScore (variance/mean, after Sun et al. 2004, *Microsc Res Tech* 65:139–149, doi:10.1002/jemt.20118), LocalFocusScore, Correlation, PowerLogLogSlope, PercentMaximal, PercentMinimal, intensity statistics and an auto-threshold.
- Automation: a reference implementation to reuse or port. PercentMinimal doubles as the offset-clipping check.

**Laissue PP, Alghamdi RA, Tomancak P, Reynaud EG, Shroff H (2017). "Assessing phototoxicity in live fluorescence imaging." *Nat Methods* 14:657–661. doi:10.1038/nmeth.4344** (abstract only)
- Argues for "quantitative, practical assessments and reporting standards" for phototoxicity, and says "a single set of standards … cannot exist."
- Automation: **knowledge base** (what to report, which controls to run). It provides no image metric.

**Icha J, Weber M, Waters JC, Norden C (2017). "Phototoxicity in live fluorescence microscopy, and how to avoid it." *BioEssays* 39:1700003. doi:10.1002/bies.201700003** (abstract only)
- Covers subtle versus obvious phototoxicity and strategies such as "limiting the illumination to the focal plane," plus controls.
- Automation: knowledge base.

**Kiepas A, Voorand E, Mubaid F, Siegel PM, Brown CM (2020). "Optimizing live-cell fluorescence imaging conditions to minimize phototoxicity." *J Cell Sci* 133:jcs242834. doi:10.1242/jcs.242834** (abstract only)
- Introduces "illumination overhead" and a workflow: "(1) determine the maximum image exposure time for a dynamic process, (2) optimize excitation light intensity and (3) assess cell health with mitochondrial markers."
- Mostly about widefield and cameras. The principle (long exposure at low intensity) maps onto dwell time versus laser power.
- Automation: knowledge base, plus a light-dose calculation from metadata.

**QUAREP-LiMi**
- Overview papers:
  - Boehm U et al. (2021) *Nat Methods* 18:1423–1426, doi:10.1038/s41592-021-01162-y
  - Nelson G et al. (2021) *J Microsc* 284:56–73, doi:10.1111/jmi.13041
- Working groups, from https://quarep.org/working-groups/:
  - WG1 Illumination power
  - WG2 Detection system performance
  - WG3 Field homogeneity
  - WG4 Chromatic co-registration
  - WG5 Resolution
  - WG6 Stage and focus
  - WG7 Metadata
  - WG10 Image quality
  - WG11 Publication standards
  - WG13 Phototoxicity
  - …15 in total.
- Outputs relevant here, all from the QUAREP publications page, which lists them as protocols.io items:
  - **WG1**: Gaudreault N et al. (2025) "Illumination Power, Stability, and Linearity Measurements for Confocal, Widefield and Multiphoton Microscopes," protocols.io, https://www.protocols.io/view/illumination-power-stability-and-linearity-measure-n2bvj3m45lk5/v1. It needs an external power meter. It is a facility procedure, not an image check.
  - **WG2**: McFadden D et al. (2025) "Characterization of the Photon Conversion Factor, Noise, and Dynamic Range of Light Microscope Detection Systems," protocols.io, https://www.protocols.io/view/characterization-of-the-photon-conversion-factor-n-dkux4wxn. It has a point-detector sub-protocol (Zeug et al. 2025). This is the community reference for turning gain into photons. Facility side.
  - **WG10 Image quality**: aims to "facilitate the assignment of a microscope- and experiment-specific QC rating to individual images." Meeting notes mention SBR, an "RID" parameter and decorrelation resolution estimation. **No published output yet.** This is the effort most likely to overlap with the advisor's per-image metrics later. Worth watching, not redundant today.
  - **WG13 Phototoxicity** (co-chair P. Laissue): no publications listed. A joint WG1/WG13 manuscript is "planned for journal submission before December 2026." Its stated aim is to "encourage researchers to measure incident light power for all live cell experiments."
  - **Reporting**:
    - Montero Llopis P et al. (2026) "Better reporting is better science: Community-defined minimal reporting requirements for light microscopy." *J Cell Biol* 225(3):e202601032, doi:10.1083/jcb.202601032. Per its abstract, a "bare minimal" checklist with human- and machine-readable input.
    - Schmied C et al. (2023/2024) "Community-developed checklists for publishing images and image analyses." *Nat Methods* 21:170–181, doi:10.1038/s41592-023-01987-9.
- Automation: the WG2 and WG1 protocols are facility calibrations that would *improve* advisor metrics (photon counts, mW) when present. The reporting checklists are a ready source of **"metadata you must capture"** knowledge-base entries.

**Metadata standards**
- **REMBI**: Sarkans U et al. (2021) "REMBI: Recommended Metadata for Biological Images." *Nat Methods* 18:1418–1422, doi:10.1038/s41592-021-01166-8 (PMC8606015 read). A high-level, cross-modality checklist. It hopes "instrument manufacturers … will increasingly support recording of the recommended metadata automatically." Too coarse to drive checks.
- **4DN-BINA-OME**: Hammer M et al. (2021) "Towards community-driven metadata standards for light microscopy: tiered specifications extending the OME model." *Nat Methods* 18:1427–1440, doi:10.1038/s41592-021-01327-9 (PMC9271325 read).
  - "one Descriptive (Tier 1) and two Analytical (Tiers 2 and 3) tiers."
  - Extensions include confocal classes such as PinHoleSettings.
  - It also specifies "minimally accepted calibration metrics (i.e., microscopy quality control metadata)," per the preprint abstract.
  - **This is the best field list for "metadata the metric needs."**
- **Micro-Meta App**: Rigano A et al. (2021) *Nat Methods* 18:1489–1495, doi:10.1038/s41592-021-01315-z. Open-source, implements 4DN-BINA-OME, and "facilitate[s] the extraction and collection" of metadata.
- **MethodsJ2**: Ryan J et al. (2021) *Nat Methods* 18:1414–1416, doi:10.1038/s41592-021-01290-5. Generates methods text from metadata.
- Automation: these are schemas and tools for *recording*, not checking. Reuse the field names. Micro-Meta App/MethodsJ2 show the extract-from-file pattern.

**Nyquist calculators**
- **SVI Nyquist Calculator** (https://svi.nl/NyquistCalculator): free and returns sampling distances in nm. The underlying formula page (https://svi.nl/NyquistRate) is behind a Huygens Imaging Academy login; I could not read it. **[vendor]**
- The Huygens docs say the Parameter Editor colours sampling fields orange or red for moderate or severe undersampling (https://svi.nl/UnderSampling).
- Automation: an automatic check. Implement the criterion ourselves (Faklaris 2022 gives one; the SVI formulas need the login) and *benchmark against* the SVI calculator.

**Cell tracking cadence and quality**
- **Maška M et al. (2023)** "The Cell Tracking Challenge: 10 years of objective benchmarking." *Nat Methods* 20:1010–1020, doi:10.1038/s41592-023-01879-y (PMC10333123; full text read).
  - Defines per-dataset properties: SNR; contrast ratio; heterogeneity; resolution; shape; "cell spacing (Spa) measured as the average distance to the closest neighbor"; "absolute change of the average intensity of the cells with time (Cha)"; "level of cell overlap in consecutive frames (Ove)."
  - For Fluo-2D, tracking performance correlated positively with SNR. Globally, "only the cell overlap (Ove) showed correlation (moderate, rho = 0.4) with the segmentation performance."
  - Automation: **computable properties with published correlation to tracker performance.** This is the best available basis for a frame-rate check (displacement per frame relative to Spa).
- Ulman V et al. (2017) "An objective comparison of cell-tracking algorithms." *Nat Methods* 14:1141, doi:10.1038/nmeth.4473 (abstract). Its finding: "none obtained fully correct solutions."
- **Beltman JB, Marée AFM, de Boer RJ (2009)** "Analysing immune cell migration." *Nat Rev Immunol* 9:789–798, doi:10.1038/nri2638 (abstract only). It describes "artefacts that could affect the estimated positions of the immune cells" and how to recognise and correct them. This is the immunology-specific source on tracking artefacts; full text not read.
- **Wortel IMN et al. (2021)** "CelltrackR…" *ImmunoInformatics* 1–2:100003, doi:10.1016/j.immuno.2021.100003 (OA; full text read). Track QC: "detecting tracking errors … drift correction". It also notes MSD at long timescales "is affected by the limited imaging window (an artifact)." It is GPL-2 R and can be reused for post-hoc checks.

**Intravital-specific**
- **Soulet D, Lamontagne-Proulx J, Aubé B, Davalos D (2020)** "Multiphoton intravital microscopy in small animals: motion artefact challenges and technical solutions." *J Microsc* 278:3–17, doi:10.1111/jmi.12880 (OA; abstract read). Motion from "heartbeat, respiratory cycles, peristalsis, muscle/vascular tone." It is the review for motion-artefact diagnosis.
- **Pinkard H et al. (2021)** "Learned adaptive multiphoton illumination microscopy for large-scale immune response imaging." *Nat Commun* 12 (article number not retrieved), doi:10.1038/s41467-021-22246-5 (OA abstract). Multiphoton "requires precise, sample-dependent increases in excitation power with depth." Demonstrated in mouse lymph node. Relevant to a depth-attenuation check.
- **Vladymyrov M et al. (2016)** "Real-time tissue offset correction system for intravital multiphoton microscopy." *J Immunol Methods* 438:35–41, doi:10.1016/j.jim.2016.08.004 (Crossref only).
- **Secklehner J, Lo Celso C, Carlin LM (2017)** "Intravital microscopy in historic and contemporary immunology." *Immunol Cell Biol* 95:506–513, doi:10.1038/icb.2017.25 (abstract).
- **Pittet MJ, Weissleder R (2011)** "Intravital imaging." *Cell* 147:983–991, doi:10.1016/j.cell.2011.11.004 (abstract).
- Automation: motion artefacts and drift (frame-to-frame registration shift, line jitter) and depth attenuation (signal vs z) are **automatic checks**. The rest is knowledge base.

**Image-side metrics without ground truth**
- **Heintzmann R, Relich PK, Nieuwenhuizen RPJ, Lidke KA, Rieger B (2016)** "Calibrating photon counts from a single image." arXiv:1611.05654. "obtains the gain from the actual image … comparable in precision to traditional gain calibration." This is an **automatic gain/photon check** with no calibration images. It suits a test acquisition.
- **Foi A et al. (2008)** "Practical Poissonian-Gaussian noise modeling and fitting for single-image raw-data." *IEEE TIP* 17:1737–1754, doi:10.1109/TIP.2008.2001399 (Crossref only). An alternative noise-model fit.
- **Descloux A, Grußmayer K, Radenovic A (2019)** "Parameter-free image resolution estimation based on decorrelation analysis." *Nat Methods* 16:918–924, doi:10.1038/s41592-019-0515-7 (abstract). "model-free and does not require any user-defined parameters." It works on diffraction-limited images and is used to "optimize image acquisition." QUAREP WG10 is discussing it. Use it as an **image-side undersampling/blur check**: compare its estimate with the theoretical resolution from metadata.
- **Yang SJ et al. (2018)** "Assessing microscope image focus quality with deep learning." *BMC Bioinformatics* 19, doi:10.1186/s12859-018-2087-4 (OA abstract). Trained on Hoechst/U2OS, "within one defocus level … with 95% accuracy." There is a Fiji plugin (github.com/fiji/microscope-image-quality, seen in search results). Narrow training domain.
- **Koho S et al. (2016)** "Image Quality Ranking Method for Microscopy." *Sci Rep* 6:28962, doi:10.1038/srep28962 (OA abstract). It ranks images by relative quality, validated against subjective scores.

**Colocalisation and bleed-through analysis**
- Bolte S, Cordelières FP (2006) *J Microsc* 224:213–232, doi:10.1111/j.1365-2818.2006.01706.x
- Dunn KW, Kamocka MM, McDonald JH (2011) *Am J Physiol Cell Physiol* 300:C723–C742, doi:10.1152/ajpcell.00462.2010
- Both verified on Crossref only. They are the standard references for checking bleed-through before colocalisation. Knowledge base.

### 2.2 Table: error mode → sources → metric → metadata

"Metadata" means what the check needs from the file. CZI metadata is rated "5 – Outstanding" by Bio-Formats; Olympus OIR metadata is "3 – Good" (bio-formats.readthedocs.io format pages). Whether every field below is actually present in our users' LSM 780 .czi/.lsm and FV4000 .oir files is **unverified**. Check it on real files first.

| Error mode | Best source(s) | Candidate computed metric | Metadata the metric needs |
|---|---|---|---|
| Saturation (high clip) | North 2006; Waters 2009; Bray 2012 (PM validated); CellProfiler MIQ | Percent Maximal per channel/z/t, reported against the *detector* max, not the dtype max (12-bit-in-16 is common). For quantification, any non-zero fraction in the ROI is a fail. | bit depth / valid range, detector type, photon-counting mode (FV4000 SilVIR **[vendor]** claims photon counts up to ~2000 per 2 µs pixel) |
| Offset too low (zero clip) | North 2006; ZEN range-indicator SOPs (blue = 0) | Percent Minimal; fraction of exact zeros in background; background histogram truncated at 0 | offset value, detector mode (zeros are legitimate in photon counting) |
| Gain too high / low SNR | Waters 2009; QUAREP WG2 protocols; Heintzmann 2016; Pawley 2006 ch.12/22 | mean–variance fit → gain (ADU/photon) and photons/pixel; SNR of foreground vs background; flag an HV range known to be noisy | detector type (PMT/GaAsP/HyD/SilVIR), gain/HV, averaging, dwell time |
| Undersampling xy (Nyquist) | Faklaris 2022 (λex/8NA closed pinhole, ×1.6 at 1 AU); SVI calculator; Pawley 2006 ch.4; Descloux 2019 | ratio of pixel size to Nyquist pixel; decorrelation resolution vs theoretical resolution | pixel size, NA, λex/λem, pinhole (AU), immersion RI, 1P/2P mode |
| Wrong z-step | Faklaris 2022 (0.15 µm at NA 1.4 confocal); ZEN "Optimal" z-interval; SVI | z-step / axial Nyquist; inter-slice correlation | z-step, NA, RI, pinhole, λ |
| Wrong z-range | no retrieved source states a rule (our derivation) | per-object signal along z: does it reach background at the first and last slice? fraction of objects touching the top or bottom slice | z positions, stack extent |
| Wrong pinhole | North 2006 (1 AU; match section thickness across channels); ZEN "1 AU" button | pinhole in AU per track, from µm, λem and objective; spread of optical section thickness across channels | pinhole µm per track, λem, NA, magnification (system-specific pinhole-plane factor needed: **unverified per scope**) |
| Photobleaching | Jonkman 2020; North 2006; CTC "Cha" (Maška 2023) | foreground intensity decay over t (fit per channel); within-stack decay vs z (separate from depth attenuation using the scan order) | timestamps, scan order, laser %, dwell, averaging |
| Phototoxicity | Laissue 2017; Icha 2017; Kiepas 2020; QUAREP WG13 (no output yet) | **no direct metric.** Proxies: motility speed or arrest trend over time, rounding or blebbing; light-dose estimate (power × dwell × pixels × frames) | laser power — ideally measured mW (WG1 protocol), not % AOTF; dwell, frame count, zoom |
| Bleed-through / crosstalk | North 2006; Jonkman 2020; Bolte 2006; Dunn 2011; ZEN Smart Setup display; Huygens QC | from the track layout: simultaneous excitation of overlapping dyes; from images: linear component in a channel-vs-channel scatter of single-positive objects; best is a single-label control | track/channel config (sequential vs simultaneous), laser lines per track, detection windows, dye names |
| Frame rate too low for tracking | Maška 2023 (Spa, Ove correlate with performance); Beltman 2009; celltrackR | median displacement per frame ÷ median nearest-neighbour distance; frame-to-frame mask overlap (Ove); also check that the *real* interval (incl. z-stack time) matches the requested one | timestamps per frame (not nominal), pixel size, z-stack duration |
| Blur / out of focus | Bray 2012 (PLLS); CellProfiler MIQ; Yang 2018 | PLLS, FocusScore per slice; decorrelation resolution | none (pixel size helps) |
| Intravital motion / drift | Soulet 2020; Vladymyrov 2016; celltrackR drift correction | frame-to-frame rigid shift (phase correlation) and periodicity (breathing); intra-frame line jitter; global drift of all tracks | timestamps, line/frame time, pixel size |
| Intravital depth attenuation | Pinkard 2021 (LAMI) | signal vs depth of matched objects; SNR at the deepest slice | z positions, laser power vs z (ZEN "Auto Z brightness"; FV "BrightZ" per Evident guide **[vendor]**) |
| Uneven field / instrument fault | Faklaris 2022; QUAREP WG3 | intensity vs radial position of matched objects (weak in sparse samples); otherwise point to the facility's MetroloJ_QC records | zoom, objective, tile layout |

---

## 3. Part B: existing tools

### 3.1 Notes per tool

- **CellProfiler MeasureImageQuality** (open source, BSD). Advises nothing; it measures blur, saturation, intensity and threshold per image. Input: any image. Validation: Bray 2012 (HCS, F-score vs expert labels). **Reuse the metric definitions.**
- **MetroloJ_QC** (Faklaris 2022; Fiji). Instrument QC from bead and flat-field slides: PSF/FWHM, Nyquist met?, field flatness, co-registration, power stability, drift, camera noise. Validated by a 10-facility multi-site study with reference values. **Does not look at sample acquisitions.** Complementary: it answers "is the scope healthy?" I did not retrieve the download location or licence (see §6).
- **Huygens Image Quality Control** (SVI, commercial; free online version "with limited features" per https://svi.nl/Huygens-Quality-Control). **[vendor]**
  - Checks clipping, hot and cold pixels, crosstalk, chromatic aberration, bleaching, SNR, microscope parameters and relative Nyquist, detector alignment and PSF.
  - Input: sample or bead image plus parameters.
  - No validation data on the page.
  - **The most direct overlap with the advisor's image checks. Benchmark against it.**
- **Huygens Parameter Editor undersampling colouring** (commercial). Orange or red for under-sampling against the Nyquist ideal (https://svi.nl/UnderSampling).
- **Google/Fiji focus-quality model** (Yang 2018; open source). Absolute defocus level. Validated on Hoechst/U2OS.
- **NoiSee** (Ferrand A et al. 2019, *Sci Rep* 9 (article number not retrieved), doi:10.1038/s41598-018-37781-3; Crossref only). Fiji workflow for confocal SNR on a standard sample. Facility-side.
- **ZEN (black and blue) built-in helpers**, documented by facility SOPs, not Zeiss:
  - Smart Setup "configures all the system hardware for a chosen set of dyes." It offers "Fastest imaging / Best signal / Smartest." "The graphs display relative values for the expected emission signals and cross-talk."
  - Frame size "Optimal" button gives the "appropriate number of pixels depending on objective N.A. and wavelength."
  - Range Indicator: "Red = saturation (maximum); Blue = zero (minimum)."
  - Offset rule: "Increase the Digital Offset until all blue pixels disappear."
  - Z "Smallest/Optimal" interval "for the given … objective lens, and the pinhole diameter."
  - Sources: Cornell BRC "Zeiss 880 … Detailed User Guide," https://www.biotech.cornell.edu/sites/default/files/2020-06/i880%20Detailed%20User%20Guide.pdf (ZEN black, the same software family as the LSM 780), and ZMB Zurich LSM 980 guide on dozuki ("Click 'Confocal' to optimize pixel size according to Nyquist," "'1 AU'," z "Optimal," "master gain of 650–850V").
  - **These already advise at the moment of acquisition.** The advisor's added value is *checking after the fact whether they were used* and covering what they don't: tracking cadence, bleaching, intravital, and real crosstalk rather than predicted. Validation of Smart Setup's crosstalk prediction: none found.
- **Evident FV4000 / cellSens FV** **[vendor]**:
  - Evident page (https://evidentscientific.com/en/landing/fv4000-laser-confocal-microscope): SilVIR "can capture both in one image without saturation"; "Live TruAI-based noise reduction."
  - Evident technical-sales slides (Okayama Univ., 2025-04-01, https://www.rib.okayama-u.ac.jp/wp-content/uploads/2025/03/FV4000%20guide.pdf, Japanese): "HV, Gain, Offset are fixed values, parameter adjustment unnecessary," "brightness adjusted by laser power only," dynamic range "about 7×" GaAsP at HV500, detectors "automatically assigned," and a Hi-Lo LUT for saturation.
  - **Consequence:** on the FV4000, gain and offset advice is mostly moot. Saturation and laser-power advice remain.
  - TruAI denoising changes the noise statistics. The advisor must know whether a test image was denoised, because the SNR and gain metrics become invalid.
- **MicroClaw** (Marin Z et al. 2026, "Microscope control with a natural language agent," bioRxiv, doi:10.64898/2026.09.15.751723; code https://github.com/Micro-Claw/microclaw, BSD-3-Clause, HEAD `bb3e123`, the same commit the Cecelia plan read). Already covered in `docs/todo/MICROCLAW_LESSONS_PLAN.md`. New for this question:
  - The abstract says it "**advises on experiment- and system-specific parameters**," so yes, it gives acquisition-setting advice.
  - Its `fluorescence-microscopy` skill is "Condensed from one reference paper: [Jonkman2020]."
    - Nyquist rule: "pixels should be 2–3× smaller than the resolution limit"; same in z.
    - "Never clip … `snap_and_analyze` reports `saturated_fraction`; for any quantification, require it to be 0."
    - Sequential acquisition is the default.
    - Bleaching numbers are quoted from Jonkman.
    - It **explicitly omits confocal configuration**: "pinhole/Airy-unit sizing, scan speed and pixel dwell time, point-scanning optical zoom, spectral detectors … stay in the vendor software."
  - `microclaw/image_analysis.py` computes per frame: focus (tenengrad), SNR (with a validity gate), saturated_fraction and signal coverage.
  - Input: a live Micro-Manager camera. None of our three scopes has an MM adapter (see §4).
  - Validation: per the preprint, not re-checked here. Cecelia's plan records that nobody ran it.
  - **Overlap:** its knowledge skill is a ready-made, cited condensation of Jonkman 2020 for the widefield/camera half. **Gap:** it does not do confocal, intravital, tracking cadence or offline test-acquisition analysis.
- **Imperial College FILM "LLM Assistance for Imaging"** (https://www.imperial.ac.uk/medicine/facility-for-imaging-by-light-microscopy/llm-assistance/).
  - Machine-readable hardware reference files plus a checklist, pasted into claude.ai "or another LLM of your choice."
  - Covers lasers and filters, sequential vs simultaneous, z-step from NA, λ and pinhole, objective choice.
  - Scopes: Nikon AX R MP, Leica Stellaris 8 and Stellaris 8 STED. No Zeiss or Evident.
  - It "cannot control the microscope or acquire images" and "does not know the live state of the hardware."
  - **No image input, no validation.** Facility-authored.
- **AIC Turku "Plan your experiments"** (https://aic-turku.github.io/AIC-Turku-database/plan_experiments/; MIT, github.com/AIC-Turku/AIC-Turku-database). An `llm_inventory.json` plus a prompt for "ChatGPT, Claude, Copilot." Planning only, no images, no validation. The inventory includes an LSM 880.
- **Smart/adaptive acquisition (non-LLM).** These act on images during acquisition (feedback), not advice:
  - **AutoMicTools** (EMBL ALMF; Fiji; https://smartmicroscopy.github.io/implementations/academic/embl.html). Bridges to LSM 900/980 via the ZEN blue macro environment (OAD), to LSM 780/880 via MyPiC, and to "FluoView3000/4000" via "XML commands through the RDK module." I did not retrieve the repos or licence.
  - **MyPiC** (github.com/manerotoni/mypic, BSD-2-Clause). A VBA macro for ZEN black, "tested on Zeiss LSM 780 microscopes with ZEN 2010, 2011, and 2012." Autofocus, tracking by centre of mass, adaptive feedback. Supplement to Politi AZ et al. (2018) *Nat Protoc* 13:1445–1464, doi:10.1038/nprot.2018.040.
  - **Pycro-Manager**: Pinkard H et al. (2021) *Nat Methods* 18:226–228, doi:10.1038/s41592-021-01087-6.
  - **Micropilot**: Conrad C et al. (2011) *Nat Methods* 8:246–249, doi:10.1038/nmeth.1558.
  - **Event-driven acquisition**: Mahecic D et al. (2022) *Nat Methods* 19:1262–1267, doi:10.1038/s41592-022-01589-x.
  - **LAMI**: Pinkard 2021, above. Intravital lymph node, learns power vs depth.
  - Review: Morgado L et al. (2024) "The rise of data-driven microscopy powered by machine learning." *J Microsc* 295:85–92, doi:10.1111/jmi.13282 (Crossref only).
  - None of these give mistake advice.
- **Other LLM microscope agents** (none fluorescence-confocal advice):
  - **EIMS**: Huang G et al. (2025) "Embodied Intelligence Unlocks Autonomous Microscopy," bioRxiv, doi:10.1101/2025.08.13.670210. The landing page did not say which hardware it drives and does not mention acquisition-parameter advice.
  - **AILA**: Mandal I et al. (2025) "Evaluating large language model agents for automation of atomic force microscopy," *Nat Commun* 16, doi:10.1038/s41467-025-64105-7 (Crossref). AFM, not fluorescence.
  - **ATOMIC**: Yang J et al. (2025) arXiv:2504.10281. 2D-materials optical microscopy. The abstract mentions robustness to "exposure variations"; it does not advise.
  - **Omega**: Royer LA (2024) *Nat Methods* 21:1371–1373, doi:10.1038/s41592-024-02310-w. Analysis in napari, not acquisition.
  - **Overview**: Ouyang W, Zhang H, "Large Language Models and AI Agents," ch. 3 of *AI in Microscopy: A BioImaging Guide* (https://bioimagingai.janelia.org/3-llms.html). Lists EIMS and AILA as the LLM microscope-control examples.
- **Leica Aperio iQC** (whole-slide pathology rescanning, **[vendor]**). Out of scope; mentioned only because the "iQC" lead resolves to it. I found no general fluorescence tool named "iQC."

### 3.2 Table

| Tool | Advises on | Input needed | Open source? | Validation of advice | Overlap with advisor |
|---|---|---|---|---|---|
| CellProfiler MeasureImageQuality | nothing (measures blur, saturation, intensity) | images | yes (BSD) | Bray 2012, F-score on HCS | metrics to reuse |
| MetroloJ_QC | instrument health (PSF, Nyquist, flatness, power, drift, noise) | bead/flat-field slides | open (Fiji; licence not retrieved) | 10-facility study, reference values | complementary (scope health) |
| Huygens Image QC | clipping, hot pixels, crosstalk, CA, bleaching, SNR, Nyquist | image + parameters | no (free limited online version) | none found **[vendor]** | **closest product; benchmark** |
| Huygens Parameter Editor | undersampling flag | metadata | no | none found | subset |
| Fiji focus-quality (Yang 2018) | defocus level | 2D image | yes | 95% within one level on Hoechst | subset |
| ZEN Smart Setup / Optimal / 1 AU / Range Indicator | dye light path, predicted crosstalk, Nyquist pixels and z, saturation | live, at the scope | no | none found | **partial at acquisition; advisor checks use** |
| FV4000 SilVIR / cellSens FV | removes gain/offset choice; TruAI denoise | at the scope | no | marketing only **[vendor]** | makes gain advice moot on FV4000 |
| MicroClaw | experiment and system parameters (camera/MM); Jonkman-based skill; per-frame saturation, SNR, focus | live MM camera | yes (BSD-3) | preprint; not re-checked | knowledge-skill reuse; no confocal |
| Imperial FILM LLM files | lasers, filters, sequential, z-step | text + hardware file | page public; LLM proprietary | none | planning only; no images |
| AIC Turku LLM inventory | facility options | text + JSON | yes (MIT) | none | planning only |
| AutoMicTools / MyPiC / Pycro-Manager / Micropilot / LAMI | none (adaptive acquisition) | live scope | yes (mostly) | method papers | actuation path, not advice |
| EIMS / AILA / ATOMIC / Omega | control or analysis | various | varies | task benchmarks (AILA: AFMBench) | none |

**Plain answer.**
- Partly solved already: image metrics (CellProfiler, Huygens QC); Nyquist, pinhole and z-step at the scope (ZEN buttons, SVI); widefield/camera knowledge (MicroClaw skill); instrument health (MetroloJ_QC).
- Left to build: a post-hoc reader of the **test acquisition plus its metadata** for the three scopes; confocal-specific checks (pinhole AU, section matching, track layout → crosstalk risk, dwell/averaging → dose); **tracking-cadence** and **intravital** checks (motion, drift, depth); and interpretation in context of the user's question.
- **Redundancy flags:** none blocking.
  - (1) Huygens QC could make a home-grown clipping/crosstalk/bleach check look like duplication for labs that own Huygens. Benchmark rather than compete.
  - (2) On the FV4000, gain and offset advice is mostly moot **[vendor]**.
  - (3) QUAREP WG10 may eventually publish per-image QC ratings. Adopt their parameter definitions when they appear.

---

## 4. Part C: actuation (brief, for later)

| Scope | Micro-Manager / pymmcore-plus | Vendor interface | Documented? | Licence | Verified here |
|---|---|---|---|---|---|
| Zeiss LSM 980 (ZEN blue) | **No adapter** in the MM device list (https://micro-manager.org/Device_Support lists only Zeiss CAN/CAN29 stands and AxioZoom). pymmcore-plus sits on the same MMCore device adapters, so no support either. | **ZEN API**: gRPC via "ZEN API Gateway," "supported by ZEN (blue) and ZEN core starting with version … 3.11"; Python packages per ZEN version (3.13–3.15). **OAD internal IronPython scripting** (AutoMicTools uses the ZEN blue macro environment). TCP/IP and COM (Developer Toolkit page). | Yes: github.com/zeiss-microscopy/OAD/ZEN-API (README, protos, examples) | OAD repo GPL-3.0; `zen_api` packages Apache-2.0 (LICENSE.txt in repo). Gateway installed via the ZEISS Microscopy Installer; no statement about paid licences found. | **Partly.** Proto file names (v2026.11.1) show experiments (load/clone/XML export, start/stop), tracks/channels activate/deactivate, z-stack, time series, tiles, stage/focus, autofocus, camera. **I found no laser-power, detector-gain or pinhole service** in the LM protos. The README says "Internal Scripting offers a lot more functionality." Changing confocal settings via the API would likely go through modifying experiment XML; that is unverified. |
| Zeiss LSM 780 (ZEN black) | No adapter | ZEN black VBA macros, external COM | MyPiC README (VBA, tested LSM 780 ZEN 2010–2012). Developer Toolkit page mentions COM and TCP/IP generally. | MyPiC BSD-2-Clause; ZEN black licensing not found | Partly. The COM object names (Lsm5Vba.Application) appeared only in a search snippet from an Elyra manual, not retrieved; see §6. |
| Evident FV4000 | No adapter (no FluoView entries in the MM list) | "Remote Development Kit" (RDK): "remote control and programming of certain FV3000 functions by … Python, C++ and Matlab" | Evident page covers the **FV3000 only**. The FV4000 link comes only from the Smart Microscopy WG page ("FluoView3000/4000 … XML commands through the RDK module"). | Unknown; sold via quote | **Not verified for FV4000.** |

Bottom line for later: actuation would mean three separate vendor integrations. ZEN API is the only one that is openly documented, and on current evidence it does not expose the settings the advisor would want to change. Advising-only is the right scope.

---

## 5. Conclusion

**Reuse**
- CellProfiler MIQ metric definitions: PercentMaximal/Minimal, PLLS, FocusScore.
- The CTC dataset-property definitions (Spa, Ove, Cha, SNR, CR) for tracking-cadence and bleaching checks.
- The Heintzmann 2016 single-image gain method for photon/SNR estimates.
- Decorrelation resolution (Descloux 2019) as an image-side sampling and blur check.
- celltrackR (GPL-2) drift and track-QC ideas.
- 4DN-BINA-OME field names for the metadata the checks need.
- MicroClaw's Jonkman-condensed skill is a cited model for the widefield half of a knowledge base. Check the licence fit: BSD-3 into GPL-3 is fine. Still verify every number against Jonkman itself.

**Cite (knowledge base)**
- North 2006, Waters 2009 and Jonkman 2020 as the core.
- Pawley 2006 for theory.
- Faklaris 2022 for the Nyquist criterion and instrument QC.
- Laissue 2017, Icha 2017 and Kiepas 2020 for phototoxicity.
- Bolte 2006 and Dunn 2011 for bleed-through.
- Beltman 2009 and Maška 2023 for tracking.
- Soulet 2020 and Pinkard 2021 for intravital.
- Montero Llopis 2026 and Schmied 2023 for reporting.
- QUAREP WG1/WG2 protocols for "measure your power and detector."

**Build**
- A metadata reader per scope (CZI for 780/980; OIR for FV4000). Verify the fields on real files first.
- Confocal checks: pinhole AU per track, section-thickness match, Nyquist xy/z, track layout → crosstalk risk, dwell/averaging/power → dose.
- Image checks: saturation, zero clip, SNR/gain, blur, bleach trend.
- Tracking cadence: displacement/frame vs NN spacing.
- Intravital: motion periodicity, drift, depth attenuation.
- Per-scope caveats: FV4000 fixed gain; TruAI denoising invalidates noise metrics; ZEN 12-bit ranges.

**Benchmark against**
- Huygens Image QC (free online version) on the same test images.
- SVI Nyquist Calculator for sampling numbers.
- CellProfiler MIQ outputs, which should match exactly where the definitions are shared.
- ZEN's own "Optimal" values for pixel and z.
- MetroloJ_QC only where the facility has bead data.

**Redundancy flags**
- None that make the advisor redundant.
- Watch QUAREP WG10 (per-image QC ratings) and the WG1/WG13 phototoxicity manuscript (planned before Dec 2026). Either could supply community definitions to adopt.
- Huygens QC overlaps the generic image checks.
- No one has validated LLM acquisition advice. The facility LLM pages and MicroClaw give no accuracy data. **An advisor evaluation would be new.** That cuts both ways: it is a contribution, and it is a risk if shipped unvalidated.

---

## 6. Could not verify or access

- **Jonkman 2020 full text** (Nat Protoc, paywalled; publisher redirected to login). Only the abstract and Crossref metadata were read. MicroClaw's skill quotes numbers from it (e.g. "Alexa Fluor 488 lost half its intensity in 3 s"). I did **not** check them against the paper.
- **Laissue 2017, Icha 2017, Kiepas 2020, Beltman 2009, Descloux 2019, Pittet 2011, Secklehner 2017 full texts.** Abstracts only. Any specific rules beyond the abstracts are unverified.
- **Pawley 2006 Handbook chapters.** Crossref metadata only, contents not read.
- **SVI Nyquist formulas** (https://svi.nl/NyquistRate is login-only). A search snippet claimed SVI says confocal sampling may be "up to 1.7×" Nyquist and widefield "below 1.5×". The page I retrieved (svi.nl/UnderSampling) did not contain this, so it is **not verified**.
- **Zeiss Smart Setup crosstalk caveat** ("actual … crosstalk … can deviate substantially," LSM 880 manual on manualslib). Search snippet only, not retrieved. The Zeiss Knowledge Base Smart Setup page returned navigation only.
- **ZEN black COM objects** (Lsm5Vba.Application, Zeiss.Micro.AIM.ApplicationInterface) and the "VBA-GUI not backward compatible" note. Elyra 7 manual pages from search snippets only, not retrieved.
- **ZEN API knowledge-base pages** (knowledge.zeiss.com). Fetched, but only navigation came back. ZEN API facts come from the GitHub OAD README and proto file names instead. **ZEN API licensing cost** is not stated anywhere I found. **LSM 980-specific ZEN API coverage** was not confirmed.
- **Evident FV4000 RDK.** Not confirmed on any Evident page; the FV3000 page only. No RDK docs retrieved.
- **FV4000 detector claims** ("HV/Gain/Offset fixed," "~7× dynamic range") come from an Evident sales deck hosted by Okayama University, in Japanese. **[vendor]**; no English Evident spec was retrieved.
- **MetroloJ_QC repository, licence and current release.** Not found in this session.
- **AutoMicTools repositories and licence** (EMBL GitLab). The GitLab API search failed. The repos are listed on smartmicroscopy.github.io only.
- **Bray 2012 PLLS F-score numbers.** The text was read, but I did not extract the figure values.
- **ATOMIC "drives a ZEISS microscope via API"** came from a search summary only. The arXiv abstract I retrieved does not say it.
- **Leica Aperio iQC.** Search snippet only; marked out of scope.
- **QUAREP WG2 protocol contents** (protocols.io). Titles and authors come from the QUAREP publications page; the protocols themselves were not opened.
- **Imperial FILM and AIC Turku pages.** Retrieved, but no publication or evaluation exists to cite. They are facility web pages.
- **Whether CZI (LSM 780 .lsm/.czi, LSM 980 .czi) and OIR (FV4000) files actually carry** pinhole µm, detector gain/HV, offset, laser % or mW, dwell, averaging and track layout. Bio-Formats only gives coarse metadata ratings. **This must be checked on real files before any check is designed.**
