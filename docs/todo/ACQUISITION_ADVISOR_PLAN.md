# Acquisition advisor — plan

**Status:** **DRAFT** (2026-11-04) — nothing built; decisions being worked through with the maintainer one at
a time. Brief: [`docs/archive/ACQUISITION_ADVISOR_PROMPT.md`](../archive/ACQUISITION_ADVISOR_PROMPT.md).

## Goal

Claude helps people at the microscope avoid simple, common acquisition mistakes (saturation,
undersampling, wrong pinhole, bleaching, bleed-through, wrong z-range or frame rate for the question).
It advises; it does not drive hardware. Scopes: Zeiss LSM 780 (ZEN black), LSM 980 (ZEN blue),
Evident FV4000.

## Verified facts (origin/main fbcfb4d8)

- **Entry point.** `pixi run chat` always attaches to a project — pick one or create one
  (`chat.py` docstring). No "file in, advice out" path exists.
- **Metadata.** CZI/OIR are read through Bio-Formats (`api/src/routes/image.jl`, `showinf` for
  dims; `bioformats2raw` on import). `ome_xml_utils.py` reads dims, pixel size and time increment
  only. No laser power, gain, pinhole, scan speed or detector settings are parsed anywhere.
- **Acquisition QC that already runs on every import** (`qc.jl`, `importImages/saturation_run.py`,
  `intensity_utils.saturation_stats`):
  - `import.channel_saturated` — clipping at the detector ceiling. Threshold
    `_SATURATION_WARN_SIGNAL_FRAC = 1e-2` is a deliberate smoke alarm, ~140× above anything measured
    (worst seen 0.007 % of signal voxels; 4 of 9 real movies clip a channel). Per-cell clipping is
    deferred until segmentation is dependable (`docs/FUTURE.md` → *Flag clipping per CELL*).
  - `import.photon_limited` — high zero fraction.
  - Cohort comparison: `nChannelsSaturated` / `maxClippedSignalFrac` per set (`qc_cohort.jl`).
- **Bleed-through** is estimated by AF correction (`maxBleedthrough`, `cleanupImages.afCorrect`) —
  a cleanup step on a project image, not a standalone metric.
- **Drift** is estimated by `drift-correct` (Tested tier, synthetic-shift tests).
- **Not measured anywhere:** SNR as a number, Nyquist vs. objective, bleaching across timepoints.
- **Blackboard** is per-project (`<proj>/blackboard/`, travels in `.ccbundle`;
  `docs/ARCHITECTURE.md` → *Project memory*: no store outside `<proj>/`). Entry state is
  `open | resolved | parked` plus snapshots. No provenance tiers, review queue or version tags.
- **Maturity tiers** exist; Experimental is the floor (`FEATURE_MATURITY_PLAN.md`).
- **Roadmap** is in hardening (`docs/ROADMAP.md`).
- **Prior art already in the repo:** `MICROCLAW_LESSONS_PLAN.md` — MicroClaw (Marin et al. 2026), a
  Claude agent driving microscopes through Micro-Manager. Agent reading only; not run, not read by
  the maintainer.

## Worked example — "my image gets darker with depth" (the maintainer, 2026-11-04)

A user acquires a z-stack in tissue without Bright Z, because they don't know it exists, and sees
the image get darker with depth. What each piece contributes:

1. **Measured (import, point 4).** Per-plane numbers show the fall-off: "CH2 signal drops N-fold
   from top to 80 µm; below 90 µm it is photon-limited".
2. **Read from the file (kind 1, point 3).** Bright Z is off and laser % is constant at every
   depth. This needs the raw `.oir` reader: Bio-Formats drops `brightnessAdjustmentParam`. When it
   is on, the ramp is stored as (depth, %) pairs. Checked on zolIMa 20armH: MaiTai 14.4 % at 0 µm →
   21.0 % at 60 µm. The file also records the automatic correction-collar setting
   (`correctionCollarAutoAdjustmentParam`, objective `XLPLN25XWMP2`), another depth setting.
3. **Shown on the diagram (point 11).** The finding sits at the sample ("darker with depth"); the
   cause sits at power control ("Bright Z off · 12 % at every depth").
4. **Explained (kind 2, generic two-photon knowledge with citations).** Tissue scatters the
   excitation light, two-photon signal depends on excitation intensity squared so losses compound,
   and the emitted light is scattered on the way back. Pinkard 2021 for depth-dependent power.
5. **Action (kind 3, small).** "Turn on Bright Z: set power at the top and bottom of the stack."
   This needs only the feature's name and rough place on this scope, one hand-written line per
   scope marked unverified until the facility checks it. No manual processing. The same idea has a
   different name on other vendors' scopes.
6. **The lab's own history (kind 4, optional).** "Your other stacks in this project used Bright Z
   and kept signal to 120 µm at a similar laser %", from the same reader run over past images.

What the advisor must not get wrong:
- **Fewer cells deeper is biology, not optics.** Intensity alone can't tell them apart. SHG or
  background trends and object-level intensity may help; until checked, say "consistent with" and
  not "caused by".
- **Darker over time is a different cause** (immersion water drying, focus drift, bleaching), not
  depth. The per-timepoint numbers separate the two.
- **Bright Z has a cost.** More power at depth means more photodamage, so the advice includes
  "don't ramp further than needed", and that part is reasoned, not measured.

What it shows about the knowledge store: this example needs kinds 1 and 2 (repo, cited) and one
line of kind 3, but no manual-processing pipeline.

## Decisions

Worked through in order; each is open until marked agreed.

1. **Who is it for — Cecelia users first** — agreed (the maintainer, 2026-11-04). People already analysing
   in Cecelia: the project profile supplies the question (goal, cell types, channels), the test
   acquisition is imported into their project and gets the import QC, and the pilot runs their own
   steps. Facility users who don't use Cecelia are out of scope for now; serving them is a separate
   file-centred tool, closer to MicroClaw and facility QC tools.
2. **Entry point** — follows from 1: the test acquisition is imported into the user's project; no
   project-less path. Open: whether test acquisitions need marking so they stay out of cohort
   analysis.
3. **Settings source** — settled: key map + raw `.oir` reader, two-photon first (see end of this point). Checked 2026-11-04 on the OME-XML bioformats2raw kept at import
   (`<proj>/0/<uid>/ccidImage.ome.zarr/OME/METADATA.ome.xml`) for two OIRs, both two-photon:
   FV4000 (software 3.2.1.85, project zolIMa) and FVMPE-RS (2.3.2.169, 2021 T-cell set).
   - **Standard OME fields carry little.** Objective (model, NA, magnification, immersion), pixel
     size, z-step, laser wavelength and, on the FV4000 only, `PinholeSize`. `LightSourceSettings`
     and `DetectorSettings` exist but are empty: no attenuation, no gain.
   - **The values are in the vendor keys** (`OriginalMetadata`): laser transmissivity (% power,
     e.g. 14.9), zoom, scanner type (galvo/resonant), line/frame integration and Kalman count,
     pinhole diameter, system name and software version, and on the FVMPE-RS detector HV (550).
   - **FV4000 detector sensitivity is not recorded** in either form: gain 1, offset 0, voltage 0 on
     every channel. That's the setting saturation advice needs most, so it has to be asked.
   - **Not found in either file:** pixel dwell time. `TimeIncrement` is missing on the FVMPE-RS file.
   - **Unchecked:** LSM 780 and LSM 980. No CZI is among the `ori_path`s in the dev projects.
   - **Scope list vs. data:** the lab's own OIRs are two-photon (FVMPE-RS, FV4000), while the brief
     lists confocal scopes (LSM 780/980, FV4000). **Two-photon first** (the maintainer, 2026-11-04); CZI
     and confocal-only error modes such as the pinhole wait.
   - **The raw OIR holds more than Bio-Formats passes on.** Checked by grepping the embedded XML of
     the FV4000 `.oir` (first 60 MB): `commonparam:pixelSpeed` (dwell, e.g. 2 µs) and `lineSpeed`;
     `lsmparam:brightnessAdjustmentParam` / `zBrightnessAdjustment` (depth-dependent laser and
     detector settings; transmissivity varies 14.4–21 % across the file); `lsmparam:autoHV = true`;
     `fvLsmparam:maxPhoton` 8/32/128 (photon-count dynamic range). None of these reach the OME-XML.
   - **FV4000 "voltage 0" may not be missing data.** With `autoHV` on and a photon-count dynamic
     range, there may be no HV to record. Unverified; question for the facility or Evident.
   - **So no import flags are needed.** Two layers: (a) a key map per vendor and software version
     over the `OriginalMetadata` bioformats2raw already keeps; (b) a small reader for the OIR's
     embedded XML, for what Bio-Formats drops (dwell, line time, depth ramp, autoHV, dynamic
     range). The output goes into ccid `meta` beside the existing import facts.
4. **Metrics (two-photon intravital)** — agreed for import (below); pilot-side metrics follow point 6. The import saturation probe
   (`importImages/saturation_run.py`) already streams every plane once (~3 s/GB) but sums them into
   one histogram per channel (`intensity_utils.channel_histograms`). Keeping a few numbers **per
   plane** from that same pass gives most of the list at no extra read cost:

   | Metric | From | Needs from metadata |
   |---|---|---|
   | Clipping by depth and time (where it clips, not just whether) | per-plane top-bin count, same pass | z-step, Bright Z ramp |
   | Depth where signal ends (photon-limited by depth) | per-plane zero fraction + signal percentile | z-step, laser % per depth |
   | Signal fall-off with depth, and whether Bright Z compensated it | per-plane background-subtracted percentile | Bright Z ramp |
   | Signal loss over time (bleaching, or drifting out of focus) | per-timepoint percentile | frame interval |
   | Lateral and axial sampling vs. the optics | metadata only | pixel size, z-step, NA, wavelength. Formula from the prior-art review, not memory |
   | Drift | drift-correct's estimator, in the pilot | — |
   | Cross-excitation and bleed-through | AF correction's `maxBleedthrough`, in the pilot | lasers per channel |
   | Frame interval vs. cell speed | tracks, in the pilot | frame interval |

   **Not measurable from pixels:** photodamage. The advisor reasons from laser % at depth, dwell
   time and interval, and says it is reasoning, not measuring. Bleaching vs. focus drift can't be
   told apart from intensity alone; drift in the pilot helps separate them.

   Store per-plane summaries (a few percentiles, zero fraction, top-bin count), not per-plane
   histograms.

   **Agreed (the maintainer, 2026-11-04): the per-plane numbers go into the normal import check**, for
   every user and not only the advisor. They're advisory only and never block an import.
5. **Knowledge store — repo files, no manuals** — agreed (the maintainer, 2026-11-04). Three small parts,
   all in the repo:
   - **Key map** per vendor and software version (kind 1). Code, tested against real files.
   - **Two-photon explanations** (kind 2): what each stage does and which error causes which
     symptom, in our own words with citations from `ACQUISITION_ADVISOR_PRIOR_ART.md`.
   - **Feature names per scope** (kind 3, minimal): one hand-written line each ("FV4000: Bright Z,
     set power at the top and bottom of the stack"), tagged `unverified` until the facility checks
     it.

   **Manual processing dropped** (brief decision 5). No manual was found on the institute's
   intranet. Without one there is nothing to process, and the worked example shows Milestone 1
   doesn't need it. That removes the review queue, the staleness handling and the licensing
   question. Revisit if a manual turns up, or if the live session (Milestone 3) produces questions
   the three parts can't answer. Cross-project lab experience (kind 4) stays a later option, built
   on Blackboard lab knowledge (`POST /api/blackboard/knowledge`).
6. **Pilot analysis** — leaning: offline, in Milestone 1. The pilot is the project's existing chain
   run on a crop of the test acquisition; existing task QC and result review report what broke,
   and the advisor maps each failure to a stage. Timed 2026-11-04 on a copy of zolIMa 20armH with
   its real chain (crop → stack align → drift correct → denoise → coastal segment+measure →
   track), one run each:
   - 256×256 × 31 z × 41 t (10 min of acquisition): crop 41 s, align 66 s, drift 53 s, denoise
     407 s, segment+measure 244 s, track 65 s, **≈ 15 min in total**.
   - 512×512 × 41 t: align 134 s, drift 316 s; stopped during denoise.
   - 256 × 10 t without denoise: segment+measure took 2283 s (38 min), against 244 s for 41 t
     with denoise. Not explained; noted, not diagnosed.

   So a pilot is not real-time at the microscope. Denoise and segmentation dominate.
7. **Screenshot lane** — deferred to Milestone 3. Offline, the file holds the settings; a screenshot can only confirm display-scaled values, never measure.
8. **Evaluation + stop/go** — open. No published validation of LLM acquisition advice exists
   (prior-art review), so this set would be the first. **The maintainer is supplying known-bad two-photon
   acquisitions** (2026-11-04). Each needs a known diagnosis written down *before* the advisor sees
   it.
9. **Prior-art and literature review** — done (2026-11-04), full record in
   [`ACQUISITION_ADVISOR_PRIOR_ART.md`](ACQUISITION_ADVISOR_PRIOR_ART.md). One agent's reading of
   fetched pages; abstract-only sources are marked there. It was briefed on the confocal scopes
   before the two-photon pivot, so its intravital coverage is thin. What carries over:
   - **Nothing makes the advisor redundant.** No tool combines a test acquisition, two-photon or
     tracking-aware checks and LLM interpretation. Vendor software advises at the scope (ZEN Smart
     Setup, range indicator, "Optimal" buttons). Huygens Image Quality Control is the closest
     "diagnose an image" product: commercial, with a free online version, and no validation data
     found.
   - **LLM assistants exist but don't look at images or confocal settings.** MicroClaw handles
     cameras through Micro-Manager and leaves pinhole, dwell and zoom to the vendor software.
     Imperial FILM and AIC Turku give users a hardware file plus a prompt. **Nobody has validated LLM
     acquisition advice**, so point 8's evaluation would be new.
   - **Metrics with published validation:** Percent Maximal (saturation) and the power log-log slope
     (blur), both from Bray 2012 and shipped in CellProfiler `MeasureImageQuality`. Gain from a
     single image: Heintzmann 2016. Resolution by decorrelation: Descloux 2019.
   - **Depth attenuation (two-photon):** Pinkard 2021 (adaptive excitation with depth, lymph node).
     Motion artefacts: Soulet 2020.
   - **Tracking cadence:** no published frame-rate rule was found. The Cell Tracking Challenge
     (Maška 2023) computes displacement and overlap per dataset and correlates them with tracker
     performance. That's the basis for "displacement per frame vs. nearest-neighbour spacing".
   - **FV4000 [vendor claim]:** SilVIR detector HV, gain and offset are fixed. That matches the
     `autoHV` and voltage 0 seen in point 3, so gain advice is largely moot there. TruAI denoising
     would invalidate noise metrics on a denoised image.
   - **Gaps for two-photon:** the Nyquist criterion it found (Faklaris 2022) is for confocal; a
     two-photon criterion still needs sourcing. QUAREP's phototoxicity group (WG13) has published
     nothing yet.
10. **Phasing vs. hardening** — drafted below (Phases), for review.
11. **Light-path diagram (brainstorm, the maintainer 2026-11-04 — not a spec yet).** Users find the settings
    confusing because it isn't clear where in the acquisition each one acts. One small, generic
    diagram per imaging technology (two-photon first) would place every setting at its point in the
    excitation and detection path. Each stage would show this image's values from the point-3 key
    map, and each import finding would sit at the stage that causes it, so "this is why my image
    looks off" is visible without reading a list. A two-photon sketch, to be checked:
    laser (wavelength) → power control (transmissivity %, Bright Z ramp with depth) → scanner (galvo
    or resonant, zoom → pixel size, dwell, line/frame time) → objective (NA, immersion) → sample
    (depth, scattering, photodamage) → emission filters and dichroics (bleed-through, cross-excitation)
    → detector (HV/gain/offset or photon-count range) → digitisation (bit depth, averaging,
    integration) → stack and time (z-step, frame interval).
    Open: where it lives, how findings attach to stages, and whether it is also the advisor's
    "here's what I read" echo-back.

    **Sketch (2026-11-04):** an upright two-photon microscope drawn as line art in the style of the
    guide modal's schematics (`ClaudeWaysIn.vue`: theme tokens, round strokes), with numbered stages
    and this image's values beside it. Colours are Okabe–Ito, as in `share/console_palette.json`
    and the `.cc-dot` tones, and none relies on colour alone (build from `plots/palettes.json` → `okabe-ito`):
    - excitation is a solid vermillion line, emission a dashed blue one. Bluish green was tried
      first; under red–green colour blindness both paths turned olive and only the dash separated
      them;
    - each value source has its own shape: OME circle, vendor-key square, raw-`.oir` diamond,
      not-recorded hollow circle, measured-at-import triangle.

    Checked under simulated deuteranopia, protanopia and tritanopia (Machado 2009): colour alone
    merged vendor with measured (red–green) and OME with vendor (tritan); the shapes keep them apart.
    The optics are generic, not the FV4000's real layout; the detector-to-channel split is unmapped.
12. **Lab knowledge beyond one project** — open, **keep visible** (the maintainer, 2026-11-04: "we should
    not let this aspect get buried"). The case: "CD169-Kat bleaches after 30 min at 20 %" or
    "lymph node needs Bright Z from 40 µm", learned in one project, should reach the next one and
    the advisor.
    - **What exists (AGENT_RUN_REVIEW_PLAN P4, built):** a Blackboard entry can be marked as lab
      knowledge (`POST /api/blackboard/knowledge`); a section can become a "Lesson" that a person
      rewrites in general form; the session briefing lists knowledge entries first.
    - **What conflicts:** that plan's Decision 11 keeps lab knowledge **inside its own project** and
      rejects a machine-level store, because it would drift from the project, be missing from an
      export, and mix in other users' knowledge on a shared machine. "General lab knowledge" across
      projects has to answer those three objections, not ignore them.
    - **Not decided:** how knowledge crosses projects. Candidates to weigh: an explicit "bring in
      lab knowledge from project X" copy, which travels with export and is owned per user; or a
      dedicated lab-knowledge project that others reference. Also open: who promotes, and whether
      the advisor may *suggest* a promotion ("this keeps coming up — make it lab knowledge?").
    - **Leaning: a lab-knowledge project** (the maintainer, 2026-11-04, "probably"). How it would meet
      Decision 11's objections: *drift*, because projects reference its entries instead of copying
      them; *mixing users*, because it is an ordinary project with `owners` in `project.json`;
      *export* is the gap, since a project export would not carry the referenced entries unless
      export snapshots the ones it used. Not designed yet.

## Phases

**Draft (2026-11-04).** Effort figures are guesses until a phase starts.

**Ships before 1.0, as Experimental** (the maintainer, 2026-11-04: "shouldn't we ship the advisor before —
not wait for 1.0"). This overrides the brief's decision 6 and is an explicit exception to the roadmap's
"hardening only" (`docs/ROADMAP.md` → Phase 3). It stays compatible with "no new headline surface":
the diagram lives in the existing image metadata dialog, and the advisor is Claude reading through
the MCP, not a new page. Milestones 0 and 1 below both land before the freeze; P1–P2 first because
they and the diagram (P4) are useful without Claude.

### Milestone 0 — during hardening (no new page; useful without Claude)

- **P1 · Per-plane import numbers** (point 4, agreed). The import saturation pass keeps per-plane
  and per-timepoint summaries (a few percentiles, zero fraction, top-bin count) in ccid `meta`,
  with findings for clipping by depth, signal ending at a depth, and loss over time. Advisory only.
  Pure, unit-tested finding helpers (`qc.jl` pattern) and cohort metrics in `qc_cohort.jl`. Shown
  through `get_image_info` (MCP), which already carries the saturation facts.
  *Effort:* small–medium. *Risk:* touches a step every import runs; the thresholds need real data
  (the maintainer's bad images), as `_SATURATION_WARN_SIGNAL_FRAC` did.
- **P2 · OIR acquisition-settings reader** (point 3). Key map over `OriginalMetadata` plus a reader
  for the raw `.oir` XML (dwell, line time, Bright Z ramp pairs, auto HV, correction collar), for
  FV4000 3.2 and FVMPE-RS 2.3. Written to ccid `meta` at import and listed plainly in
  `ImageMetadataDialog.vue`. Follow the Imaris pattern (`python/cecelia/utils/ims_meta.py`, run from
  import and from `resync_ome_meta!` in `importImages/omezarr/ccid_sync.jl`), so already-imported
  OIRs get their settings by resync without re-importing. Reads `ori_path`, so the raw file must
  still be reachable; when it isn't, only the `OriginalMetadata` half is available. No raw-OIR parser exists
  yet: `ims_meta._from_oir_time_step` reads an Olympus group inside an Imaris `.ims` (h5py), not a
  `.oir`. So the reader is a new per-format runner beside `read_ims_time_interval_run.py`, called
  from import and `resync_ome_meta!` like `_recover_ims_time_increment!`. The key map parses through
  `ome_xml_utils.load_ome_xml`, never `METADATA.ome.xml` raw. Fixtures are the XML header cut from real files (CLAUDE.md: copied
  from real output, never written to match the parser).
  *Effort:* medium. *Risk:* key names change with software versions; an unknown version reports
  "not mapped" instead of guessing.

- **P4 · Light-path diagram** (point 11; moved here, the maintainer 2026-11-04: "same for the diagram").
  Its own inline-SVG component (house shape: `DatasetAskFigure.vue`, `ClaudeWaysIn.vue`), mounted
  by `ImageMetadataDialog.vue`, with the stage→setting/finding mapping in a tested `utils/*.ts`.
  It shows P2's settings and P1's findings at their stages. Line and marker colours come from the
  canonical qualitative palette `plots/palettes.json` → `okabe-ito`, not the status-dot tokens. Needs no Claude, so it doesn't wait for the advisor's evaluation. Gate: the maintainer's visual
  pass and a dark-mode check before it ships; generic optics are labelled as a schematic, not the
  scope's real layout.

Why these fit hardening: all three make an existing result easier to trust ("was this acquisition fit
for the analysis?") without a new page, and both are useful without Claude.

### Milestone 1 — offline advisory core (Experimental, before 1.0)

- **P3 · Knowledge files** (point 5): two-photon explanations with citations, feature-name lines
  per scope, in the repo.
  **Built:** `mcp/cecelia_mcp/acquisition_knowledge.json` — per light-path stage, symptoms with
  explanation, check (setting or measured number), alternatives and sources. Sources checked on
  Crossref: Helmchen & Denk 2005, Theer 2003, Pinkard 2021, Patterson & Piston 2000, Hopt & Neher
  2001, Waters 2009, North 2006, Pawley 2006 ch. 4. Wording is ours; the files' claims are kept to what
  each source is known for, no numbers quoted from them. Feature lines for FV4000 and FVMPE-RS
  (Bright Z, Auto HV), all `verified: false`.
- **P5 · Advisor**: guidance text and any MCP additions (`server.py` + `guidance.py`) so Claude
  reads settings, per-plane numbers and the knowledge files. The echo-back is the diagram. Rules:
  "consistent with" until a cause is separated, reasoning marked as reasoning, unverified feature
  names flagged.
  **Built:** `get_acquisition_knowledge(system)` + a briefing paragraph (`guidance.py`): read
  `acquisition` (asked) against `cleanupFacts` (landed), "consistent with" until alternatives are out,
  reasoning marked, sources cited by id, unverified names flagged, untestable numbers left unknown,
  the question asked before judging sampling, the metadata dialog as the echo-back. Not yet
  evaluated (P7).
- **P6 · Offline pilot** (point 6): run the project chain on a crop and map task QC and result
  review findings to stages. About 15 min for 256² × 41 t on zolIMa.
- **P7 · Evaluation** (point 8): the maintainer's bad acquisitions with diagnoses written beforehand, good
  counterparts, the biology-not-optics case and a time-effect case.
  **Stop/go before Milestone 2** (proposed, to agree): the main cause is right on most cases; never
  "caused by" on the biology case; unverified directions flagged every time; the maintainer's judgement
  that the advice was useful.
- **Demo:** one bad and one good acquisition from the set, diagram + advice side by side.
- **Ship gate (proposed):** the evaluation has run at least once and the advisor is never
  confidently wrong on it (no "caused by" on the biology case; unverified directions flagged).
  Usefulness can still be improving: Experimental means "not checked extensively". The stop/go above
  gates Milestone 2, not shipping.

### Milestone 2 — lab knowledge across projects (point 12)

Design the lab-knowledge project against AGENT_RUN_REVIEW_PLAN Decision 11 (drift, export, mixing
users), including the export gap. Then the advisor reads it and may suggest promotions. Deliberately
its own milestone so it is not absorbed into "later".

### Milestone 3 — at the microscope

Live session: save/transfer/load at the scope, screenshot lane (point 7, deferred), the fast
feedback loop (import numbers + diagram; pilot stays offline unless timing improves), adoption test
with a few real users on locked-down acquisition PCs. Confocal scopes (LSM 780/980, CZI reader) join
here or later.

### Later, optional

Actuation (brief decision 1): only after the advisor has proven useful. Notes for then are in
`ACQUISITION_ADVISOR_PRIOR_ART.md` §4.
