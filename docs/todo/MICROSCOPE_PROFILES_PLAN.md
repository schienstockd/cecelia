# Microscope profiles — acquisition support beyond two-photon

**Status:** **IN PROGRESS** 2026-11-11. P1–P2 in PR #2151 (`feat/microscope-profiles`). Branch `feat/acq-confocal` (stacked): readers record `technology` (+ `modes`) — SP8 `.lif` confocal (sequential), `.czi` Airyscan / spectral / slide-scanner widefield / LSM model name, `.oir`; confocal + Airyscan knowledge (`extends`), SP8 / LSM 980 / Axio Scan.Z1 profiles; diagram, card and dialog per technology (two-photon, confocal, Airyscan, widefield); set check and review prompt say "images" unless every image is a time-lapse; widefield wording for gain/photons/dim/clipped-deep advice. Open: the drawings need the maintainer's eyes in the app; import-time floor on cameras; `.qptiff` read (`qptiff_meta`: single scan, PhenoCycler cycles + `.xpd`, brightfield; PhenoImager Fusion profile) — open: AF correction on scanner-subtracted channels, cycle-per-scan registration; `modes` knowledge files.

**Why.** The acquisition layer (settings readers, `acquisition_knowledge.json`, light-path diagram,
settings card, QC advice text) was built for intravital two-photon, the maintainer's own work. Images
now arrive from other microscopes and fields. The first: project `B8ekO5` "Speck counting", set
`9kUlhM`, Leica `.lif` files from a **Leica DM5500B widefield** (camera DFC365 FX, AF6000 software,
HC PL FLUOTAR 20×/0.50 dry, filter cubes A4 + Y5). The images showed `acquisitionMissing: format`,
the knowledge had nothing for a camera, and QC advice talks about laser power and HV.

**Goal (the maintainer, 2026-11-11):** keep the integration generic, so microscope profiles and their
knowledge can be **added over time** without touching the code paths each time.

## Model

Three layers, each added to independently:

| Layer | What it is | Add one by |
|---|---|---|
| **Reader** (file format) | Reads the settings a vendor file records that Bio-Formats drops, into the normalised `acquisition` dict | a `python/cecelia/utils/<fmt>_meta.py` + one row in the reader registry |
| **Technology** (imaging modality) | `two-photon`, `confocal`, `widefield`, … — what the light path is, its stages, symptoms, sources | one `technologies/<id>.json` in the knowledge directory |
| **Scope** (a microscope model) | Vendor's names for the features that fix a stage, what the lab's files record, `verified` | one `scopes/<id>.json` naming its technology |

The reader stamps `acquisition.technology` from **evidence in the file** (a camera setting block →
widefield; laser lines all ≥ 680 nm → two-photon; visible lasers → confocal), never from the format
alone (an FV4000 `.oir` can be either). `null` when the file does not say.

## Phases

**P1 — Leica reader + one reader registry + `technology`.** BUILT 2026-11-11.
- `python/cecelia/utils/lif_meta.py`: LIF header XML → normalised settings for the image's series
  (a `.lif` holds many; `meta.ori_series` picks one: the series picker's readlif index). Widefield camera
  elements first (`ATLCameraSettingDefinition` + `WideFieldChannelInfo`); a confocal LIF
  (`ATLConfocalSettingDefinition`) returns a reason until a real file is in hand — no guessed parse.
- Per-channel camera fields: `exposureMs`, `gain` (camera), `filterCube`, `illuminationIntensity`;
  image-level `camera` {name, binning, bitDepth, sensorPixelUm}. Rendered by the metadata dialog's
  rows when present.
- One registry, `app/src/tasks/importImages/acquisition_readers.json` (`ext → source, version`), read by the
  Python dispatcher AND the Julia gate/version table — replaces the three hand-synced lists.
- Series index plumbed into the reader call (`ori_series`).

**P2 — knowledge directory + widefield knowledge.** BUILT 2026-11-11 (8 new sources checked on Crossref).
- `mcp/cecelia_mcp/acquisition_knowledge/technologies/{two-photon,widefield}.json`,
  `.../scopes/{fv4000,fvmpe-rs,dm5500b}.json`. `get_acquisition_knowledge(system, technology)`
  resolves system → scope → its technology; no argument → index of technologies + scopes.
- Widefield stages: illumination, filter cube, objective, sample, camera (exposure, gain, binning,
  offset), digitisation. Symptoms each with check / alternatives / cited sources (Crossref-verified).
- Schema test over every file: stage keys exist, source ids resolve, scope names a known technology.

**P3 — technology-aware measurement and advice.** Set check + data-path floor BUILT 2026-11-11; the rest open.
- QC advice text (`app/src/qc/text.jl`): laser/HV/Bright Z advice only for laser-scanning; camera
  gets exposure/gain/binning.
- Floor heuristic (`intensity_utils.floor_stats`): "photon-counted" is a PMT/HyD reading; a camera
  channel must not be called that (seen on a DM5500B channel, "step 9"). **Built:** the metadata
  dialog's data path reads it as no call when `acquisition.camera` is set. **Open:** the import's
  measurement itself still stores the reading (it runs without the settings), and the signal pass
  (`python/cecelia/utils/data_path.py`, the `counted` map) then treats the zeros as no-photon voxels
  in the noise fit — so `noise.snr` / `photons.objects` on a camera channel are suspect until fixed.
- Set check: camera settings that differ between images of one set (exposure 0.12–0.25 s, gain 1–3.1
  across one session) flagged like differing laser settings. **Built:** `camera setting` in
  `acq_check.jl`'s comparison list. **Open:** its finding text still says "movies".
- Light-path diagram + settings card: per technology, widefield after the knowledge settles.

## Beyond widefield — static and live, confocal, PhenoCycler (the maintainer, 2026-11-11)

"Accommodate static and live image wording … make the diagram and everything else accommodate
different microscope setups. There will be normal confocal setups. There will be PhenoCycler data."

### Two axes, not one list

A setup is an **optical technology** (how light reaches the sensor) plus zero or more **modes**
(what is repeated). Mixing them into one list would need a "widefield cyclic tiled" entry per
combination.

| Axis | Values | Comes from | Knowledge file |
|---|---|---|---|
| `technology` | `two-photon`, `confocal`, `widefield` (later `spinning-disk`, `light-sheet`) | the reader, from evidence in the file | `technologies/<id>.json` — light-path stages |
| `modes` | `time-lapse`, `z-stack`, `tiled`, `cyclic` | the store's dims (T, Z) + the reader (tiles, cycles) | `modes/<id>.json` — the stages a mode adds (e.g. cyclic: cycles, blanks, registration) |

PhenoCycler = `widefield` + `cyclic` + `tiled`: its camera, exposure and bleed-through knowledge is
widefield's; what it adds (marker per cycle, blank cycles, background subtraction, stitching, cross-cycle
registration) is the `cyclic` mode's. A confocal time-lapse = `confocal` + `time-lapse`.

### P4 — static vs live wording

Today's acquisition texts say "movie" throughout: `app/src/qc/text.jl` (28 lines), `acq_check.jl` (30),
`api/src/acq_review.jl` (20, the review prompt), `AcqCheckDialog.vue` (17). On a static set
("Speck counting") every one is wrong.
- **One noun per set, from the data:** every image has T > 1 → "movie(s)"; otherwise "image(s)". A
  `{item}` / `{items}` substitution in the texts, chosen once per check (`acq_check`) from the facts —
  not per finding — so a dialog never mixes the two.
- **Time-only checks gate on T, not on wording:** fading over time, drift, frame interval, stopped-early.
  On a static set they say nothing (today drift is already absent without T; fading is a review
  category with no rule). A static set's own version — dimmer in later-acquired fields, by
  `acquiredAt` — is the widefield `fades-with-exposure` symptom; a rule for it is a separate item.
- Category KEYS stay (`acq.cat.movies` is stored); only their text changes.
- The review prompt (`acq_review.jl`) takes the same noun.

### P5 — technology on every reader, and the dispatch

- `oir_meta` / `czi_meta` stamp `technology` from the lasers (all ≥ 680 nm → two-photon, else confocal;
  a transmitted/camera channel noted); `ims_meta` leaves it `null`. Older stored settings: the
  frontend's existing NIR rule (`isMultiphoton`) is the fallback, in ONE helper `acquisitionTechnology(acq)`.
- `modes` derived in one place from the store's dims plus the reader's `tiles` / `cycles`.
- Every surface that asks "is this two-photon?" today asks "which technology?" instead:
  `ImageMetadataDialog.vue`, `acquisitionCard.ts` (`hasAcquisitionCard`), `acquisitionSettings.ts`
  (the "two-photon only so far" notice), the QC advice texts (P3).

### P6 — diagram and settings card per setup

- **Stages come from the technology** (+ modes): the knowledge file's stage keys are the diagram's
  rows, so a symptom and the diagram always name the same stage. `lightPathStages(img)` dispatches to
  one builder per technology (`twoPhoton`, `confocal`, `widefield`); the items helpers stay shared.
  A test asserts each builder's keys equal its knowledge file's stages (the existing two-photon test,
  generalised).
- **Line art per technology** in `AcquisitionLightPath.vue`: two-photon (existing), confocal (adds
  pinhole, per-line lasers, descanned detectors), widefield (lamp/LED, filter cube, camera). Until a
  technology has its art, the same stage list renders without the drawing — never the wrong drawing.
  **Rendering needs the maintainer's eyes before merge** (mock first, side by side).
- **The card** reads the same stages, so it follows. A 40-marker PhenoCycler image does not fit the
  per-channel column layout: channels become a table (marker · cycle · exposure · landed numbers).
  **Built 2026-11-13** (`feat/acq-card-per-tech`): a channel table per technology, page 2 for a long
  one — `ACQUISITION_CARD_PLAN.md` → Status. Readers gained what it needs: `czi_meta` v3 (track, lines
  with power, band, pinhole and beam splitters per channel; an attenuation above 1 left unread instead
  of a negative power), `lif_meta` v3 (the sequence per channel); a registered image keeps each scan's
  run (`cycles.experiments`).

### P7 — confocal

- Readers: `.czi` already reads LSM 780/880/800 (stamp `technology`); `.oir` FV4000 in confocal mode
  (stamp); `.lif` SP8 / Stellaris (`ATLConfocalSettingDefinition`) — **needs a real file** first.
- Knowledge `technologies/confocal.json`: pinhole (sectioning vs signal), per-line laser power,
  PMT/HyD gain, line/frame averaging, sequential vs simultaneous (bleed-through), zoom vs Nyquist,
  bleaching. Sources Crossref-checked; reuse the shared ones (waters2009, north2006, pawley2006).
- Scope files per confocal the lab uses (names + `seenInFiles` from real files).

### P8 — PhenoCycler (cyclic multiplex)

- Reader: Akoya `.qptiff` — **built for a single scan** (`qptiff_meta`, 2026-11-12) from a Fusion 2.4.1
  whole-slide fluorescence scan: one QPI XML per channel page (exposure in µs — Bio-Formats
  `VectraReader`; gain, bit depth, active excitation band of a multiband filter + its emission partner,
  LED light source, `OffsetAndShading` processing, `EnhancedDynamicRange`), stitched (`tiled` when wider
  than one camera field), `Biomarker` → channel name when present. System "PhenoImager Fusion" from the
  `Fusion` software name (the file names no model). **Cyclic runs** (checked on a Fusion 2.4.0 run, five
  more listed): one `.qptiff`, a page per marker with `Biomarker` and `AutofluorescenceSubtracted`; no
  cycle field — each cycle's pages share one TIFF write time, DAPI with the first (blank) cycle; the
  run's `.xpd` (JSON) beside it lists wells = cycles in run order with barcode/reporter/clone/dye/
  planned exposure, blanks as markers "--". Some runs were done one cycle per scan (`ScanN/` each with
  its own `.xpd` and DAPI) — separate images, unregistered. Brightfield RGB scans (`im_Brightfield`) →
  `technology: brightfield`, no knowledge file or drawing yet.
- AF correction on scanner-subtracted channels, cycle-per-scan registration and `modes/cyclic.json`:
  built — see *Multiplex QC* below.
- `modes/cyclic.json` knowledge: blank/background cycles, autofluorescence carried across cycles,
  marker signal falling with cycle number, failed cycles, cross-cycle registration, stitching seams.
- Channel names = markers: the designs in the set check are marker lists already (`acq_design_key`).
- Scale: whole-slide, 40+ channels — the data-path panel and card list channels in a table.
- **Import measures at slide scale — built** (branch `feat/slide-scale-measures`). The probe read a
  whole plane per channel and the photon fit made float copies of it: a 1.2 Gpx four-channel scan
  (0064520) ran the machine out of memory. Now the histogram pass reads a plane past
  `HIST_BLOCK_PX` in row blocks, and the signal pass reads a plane past `SIGNAL_WHOLE_PLANE_PX` as
  4 × 4 patches (`data_path._signal_regions`); a microscope frame is read whole, results unchanged.
  Measured on that scan's cycle 3 (48960 × 24960, 3 channels): 27 s, 0.84 GB peak. Channels the
  scanner processed before writing (`processing`, `afSubtracted`) get no photon fit — reason in
  `photons.fit.untestable` (`photons_untestable` in `saturation.jl`), one note in the data-path panel.
- **Multiplex QC — built** (branch `feat/multiplex-qc`). Tissue mask on a coarse pyramid level
  (`tissue_utils`: Otsu over the scanned pixels — the scanner writes zeros on skipped tiles. Zack's
  triangle, the background threshold elsewhere, was tried first: inside the scanned tiles glass and
  tissue are two peaks of similar height, and triangle anchored on the taller one, cutting above the
  tissue on cycle 2 — 0.13% tissue instead of 5%); the import's signal-pass patches sit on it (`tissue` in
  `meta.saturation`, a data-path note). Cycle-per-scan registration (`editImages.register`, no longer
  `comingSoon`): a whole-slide reference registers on the pyramid (`register_utils`: phase-correlation
  shift → rigid → affine, tiled warp, banded pyramid write) and checks each cycle on the reference's
  tissue — residual offset, tissue covered / retained / only in the cycle, shared-marker signal and
  sharpness ratios — as the task's QC. On 0064520 (three scans, 51120 × 30720): shifts ≈ 3460 px,
  rotation ≤ 0.05°, residual 0.13 µm median, 4.5 min and 1.3 GB peak for 11 channels; one section the
  reference scan skipped shows as `tissueOnlyInCycle` 9–14%. AF correction skips `afSubtracted`
  channels. `modes/cyclic.json` knowledge (served with `get_acquisition_knowledge(modes=)`).
  Many-channel metadata grid: PR #2195. **Open:** a slide probed before the tissue mask is not
  re-probed (`_saturation_is_current` does not know the plane size); the 3-D / small-image register
  path (sitkibex) has no per-cycle checks.

### Series index — one count (found at review, 2026-11-11)

`lif_meta` takes `series` as readlif's index (the series picker's), not a count of its own. Open:
the converter runs `bioformats2raw --series N` with that same N, and Bio-Formats may number a tile
scan (LIF dim 10) differently from readlif — a pre-existing picker/converter question, to check on a
real tiled `.lif` before PhenoCycler-scale tiled data arrives.

### Decided (the maintainer, 2026-11-11)

"Two-photon was the prototype. Now we should support the other technologies in the same way. Line
drawing. QC. And everything. The app is not two-photon exclusive." — every technology gets a reader,
its own line drawing, QC advice in its terms, knowledge, and the card. No "two-photon only so far"
gate survives. Technology × modes as two axes (proposed above, not objected to).

### Files in hand (2026-11-11)

| File (where) | System | Technology / modes | What the reader needs |
|---|---|---|---|
| `.lif` speck trial (staging) | DM5500B + DFC365 FX | widefield | built (P1) |
| `.lif` CD169/Mer (staging, INCITe) | TCS SP8 on DMI6000B-CS | confocal, z-stack, **sequential** (4 sequences, one per channel) | `ATLConfocalSettingDefinition` under `LDM_Block_Sequential_List`: per sequence the active detector (PMT, gain/offset), laser lines with intensity %, emission band (`MultiBand`), dye; image: pinhole (AU + µm), zoom, scan speed, dwell, averaging, bit depth. Sequence → channel by the active detector's LUT colour |
| `11212023_MSi08L…N27.czi` | LSM 880 | confocal, **spectral (lambda) mode**: 33 channels = wavelength bins, z-stack | channel names are wavelengths; AcquisitionMode `SpectralImaging` |
| `1980_TSH_RUN6….czi` | Axio Scan.Z1 + Axiocam 506 | widefield, **tiled** (255 tiles, 37k×32k) | camera channels (exposure); `System` empty → name from `Microscope@Name` |
| `2025-03-20_Rat3….czi`, `vlut1….czi`, `Figure 3 Cingulin.czi` | LSM 980 / 900 / 800 | **Airyscan** (array detector, ~5 AU pinhole), z-stack | `System` says "LSM 800" for all three — the real model is `Device Id="MTBLSMImagingDevice" Name=…`; Airyscan from the detector type |
| `Mouse_1_CD8.czi`, `WT2a-A-3….czi` | LSM 800 / 710 | confocal, tiled | multi-track (already read) |
| `kdrl*.czi` (zebrafish) | LSM 880 | confocal, z-stack (one tiled) | already read; stamp technology |
| `17AHOMI_Scan2.er.qptiff` | PhenoImager Fusion (Fusion 2.4.1), Sony IMX421 | widefield, tiled (whole slide, 106560 × 63360), 3 channels, **not cyclic** | built: `qptiff_meta` |

Found on the way: in the SP8 file the WT and Mer images were acquired with **different laser %
and PMT gain** (638 nm 13.5 % vs 5.5 %) — the set check must flag that once confocal is read.

Technologies from these: `two-photon`, `confocal` (point-scanning, PMT/HyD/GaAsP), `airyscan` (confocal
light path, array detector — its own drawing stage and pinhole meaning), `widefield`. Modes:
`z-stack`, `time-lapse`, `tiled`, `spectral`, `sequential`, `cyclic`.

### Drawings

`AcquisitionLightPath.vue` keeps the stage list and legend; the figure becomes one component per
technology (`LightPathFigure<Technology>.vue`), each with its own stage markers, same line-art style
(theme tokens, Okabe–Ito excitation/emission, numbered markers). Mocks go to the maintainer before
any is built.

### Build order

1. Mocks of the confocal, Airyscan and widefield drawings → the maintainer.
2. P5 + P7 readers (SP8 `.lif` confocal, `.czi` model name / Airyscan / spectral / camera, technology stamped by every reader) + confocal and Airyscan knowledge, while the mocks are reviewed.
3. P4 wording.
4. P6 drawings + card per technology, from the approved mocks; QC advice text per technology.
5. P8 PhenoCycler: `.qptiff` reader built (single scan, cyclic run + `.xpd`, brightfield); open items above.

## Open (the maintainer's calls)

- Technology + modes as two axes (above), or PhenoCycler as its own technology.
- Sample files: a confocal `.lif` (SP8/Stellaris) and a PhenoCycler `.qptiff` — readers wait for them.
- Line art per technology, or a stage list without art for the non-two-photon setups.
- Whether a scope file can be added by a user (GUI) or only committed — committed for now.
