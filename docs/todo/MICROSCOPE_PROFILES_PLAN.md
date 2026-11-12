# Microscope profiles — acquisition support beyond two-photon

**Status:** **IN PROGRESS** 2026-11-11 (branch `feat/microscope-profiles`) — P1 + P2 built; P3 set check and data-path floor built, QC advice text and the import-time floor open. P4–P8 (static/live wording, technology dispatch, diagram per setup, confocal, PhenoCycler) planned.

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

### P7 — confocal

- Readers: `.czi` already reads LSM 780/880/800 (stamp `technology`); `.oir` FV4000 in confocal mode
  (stamp); `.lif` SP8 / Stellaris (`ATLConfocalSettingDefinition`) — **needs a real file** first.
- Knowledge `technologies/confocal.json`: pinhole (sectioning vs signal), per-line laser power,
  PMT/HyD gain, line/frame averaging, sequential vs simultaneous (bleed-through), zoom vs Nyquist,
  bleaching. Sources Crossref-checked; reuse the shared ones (waters2009, north2006, pawley2006).
- Scope files per confocal the lab uses (names + `seenInFiles` from real files).

### P8 — PhenoCycler (cyclic multiplex)

- Reader: Akoya `.qptiff` — **format to confirm on a real file** (per-channel marker names, exposure,
  cycle; whether it is already stitched and background-subtracted). No guessed parse.
- `modes/cyclic.json` knowledge: blank/background cycles, autofluorescence carried across cycles,
  marker signal falling with cycle number, failed cycles, cross-cycle registration, stitching seams.
- Channel names = markers: the designs in the set check are marker lists already (`acq_design_key`).
- Scale: whole-slide, 40+ channels — the data-path panel and card list channels in a table;
  per-channel measurements at import must stay bounded (check `cleanupFacts` cost on one file).

### Series index — one count (found at review, 2026-11-11)

`lif_meta` takes `series` as readlif's index (the series picker's), not a count of its own. Open:
the converter runs `bioformats2raw --series N` with that same N, and Bio-Formats may number a tile
scan (LIF dim 10) differently from readlif — a pre-existing picker/converter question, to check on a
real tiled `.lif` before PhenoCycler-scale tiled data arrives.

### Build order

P4 (wording, small, no new data needed) → P5 (technology everywhere) → P6 (diagram/card, with mocks)
→ P7 and P8 each when a real file is in hand.

## Open (the maintainer's calls)

- Technology + modes as two axes (above), or PhenoCycler as its own technology.
- Sample files: a confocal `.lif` (SP8/Stellaris) and a PhenoCycler `.qptiff` — readers wait for them.
- Line art per technology, or a stage list without art for the non-two-photon setups.
- Whether a scope file can be added by a user (GUI) or only committed — committed for now.
