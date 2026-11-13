# Acquisition card — plan

**Status:** **P0–P3 BUILT** (2026-11-06, branch `acquisition-card`) — P4 (a person re-acquires from a card) open.
**Per technology (2026-11-13, branch `feat/acq-card-per-tech`):** the "This run" columns are gone (they
overflowed or were too thin); the channel table is laid out per technology — Zeiss tracks / Leica
sequences with each group's lines and power, a lambda run as one row, the Airyscan mode, the camera
per channel, a PhenoCycler's markers by cycle (clone, reporter, band, exposure vs plan); the target
keeps only the columns the image has (no depth on a single plane); the caveat names the technology's
own causes; a card longer than a page continues on page 2 (`utils/acquisitionCardPages.ts`). Mocks
agreed with the maintainer first. Layout agreed with the maintainer on a mock (below). Audit done against `origin/main` bd436545 on one
FV4000 image (zolIMa `20armH`) and one FVMPE-RS source file (TCELL_TYPES 2021-12-01 `z180_0001.oir`,
read-only). Brief: [`docs/archive/ACQUISITION_CARD_PROMPT.md`](../archive/ACQUISITION_CARD_PROMPT.md).
Part of [`ACQUISITION_ADVISOR_PLAN.md`](ACQUISITION_ADVISOR_PLAN.md); lands Experimental.

## Goal

A one-page card made from an existing image, which a person at the microscope uses to acquire it
again: the settings in the scope's own order and words, blanks where the file is silent, and what
landed last time as the target for the first snap. A record, not a command — no hardware, no scope API.

## Verified facts

Checked in code and on the two files above. "Stored" = in `meta["acquisition"]` / `cleanupFacts`;
"in the file" = in the `.oir` XML but not parsed today.

1. **Settings come from one reader.** `python/cecelia/utils/oir_meta.py` (`acquisition_settings`) →
   `meta["acquisition"]` at import or Refresh metadata (`_resync_acquisition_settings!`,
   `app/src/tasks/importImages/omezarr/acquisition.jl`). `.oir` only; any other format has
   `acquisitionMissing` with a reason. Served on the image payload (`api/src/routes/helpers.jl`).
2. **Only 3 of ~40 `.oir` images in the dev projects have settings stored** (zolIMa `20armH`,
   `Dml3RG`, `R8m1Yo`, all FV4000). The rest were imported before the reader and need Refresh
   metadata. Every FVMPE-RS image is in that group, but their sources are reachable.
3. **What the reader gives, per scope:**

   | Field | FV4000 (`20armH`) | FVMPE-RS (`z180_0001`) |
   |---|---|---|
   | System · software | FV4000 · 3.2.1.85 | FVMPE-RS · 2.3.2.169 |
   | Objective | XLPLN25XWM, 25×, NA 1.05, water | same |
   | Lasers · λ · % | MaiTai 900 nm 14.4 %; InSight X3 1140 nm 14.9 % | MaiTai 850 nm 15 %; InSight 1200 nm 15 % |
   | Bright Z | **on**, 2 points (0 → 60 µm) | **off**, 2 points (0 → 35 µm) stored anyway |
   | Scanner · zoom · dwell | Resonant · 1.5 · 0.033 µs | Galvano · 2.0 · 2.0 µs |
   | Integration | Frame, count 2, not average; Kalman 5/2 | type "None", count 2 (ambiguous) |
   | Auto HV · HV | on · HV 0 every detector | on · HV 550 every detector |
   | Gain · offset | 1 · 0 | 1 · 1 |
   | Bit depth · pinhole | 16 · 50 µm | not recorded · not recorded |
   | Channel → laser, detector, dye preset | yes | yes (one preset empty) |
   | z-step · planes | 2 µm · 31 (`zStack`) | 5 µm · 8 |
   | Interval · frames | 15 s · 181 | 10 s · 180 |
4. **In the file, not parsed:** `base:creationDateTime` (acquisition date — stored nowhere today),
   `fvCommonphase:filterName` per channel (barrier filters: BA410-455, BA495-540, ET585/40M,
   ET720/60M on the FV4000; BA415-485 … BA642-709 on the FVMPE-RS), `firstMirrorTurretIndex` (a
   dichroic *position*), microscope body (`BX63LF`). **So "Dichroics and filters not recorded" in the
   Light path panel was wrong for `.oir`: the emission filters are recorded, the reader dropped
   them.** *Correction found while building P0:* the dichroics ARE named too — the phase light path
   holds the IR laser dichroic (`LCDM690-1050` FV4000, `LCDM690-870` FVMPE-RS) and the frame's mirror
   cubes (`DM760`, both). What each slot does is our reading of the element names; dichroics between
   the non-descanned detectors are not in the file.
5. **`zStack` and `scanner.kalman` are stored but untyped** in `AcquisitionSettings`
   (`frontend/src/utils/acquisitionSettings.ts`). Bright Z `hv` is keyed by channel UUID (FV4000) or
   detector id (FVMPE-RS) — two key schemes for one field.
6. **The landed numbers are not there for most images.** `img_cleanup_facts` (`app/src/qc.jl`) reads
   `profile`, `range`, `floor`, `noise` from `meta["saturation"]`; on `20armH` (live app, MCP
   `get_image_info`) all four are `null` and `zeroPct` is `null`. They need Refresh metadata with
   `measure: true` (`ensure_saturation_meta!`). Only `topValue` and `clippedPct` are there.
7. **No trend number exists.** `app/src/qc/profile.jl` states a change ("brightest signal falls 46 %
   over the movie") as an `info` finding, deliberately with no cause: bleaching, drift and cells
   leaving look alike. The card prints the same measurement, not "bleaching".
8. **The zero-floor wording exists.** `intensity_utils.floor_stats` classifies photon-counted /
   clipped / indeterminate, and the knowledge file has `background-at-zero` (offset only for a clipped
   floor) and `zeros-are-no-photon`. The brief's "related fix" is done; the card reuses that wording.
9. **Scope lines.** `acquisition_knowledge.json` → `scopes[]`: FV4000 Bright Z + Auto HV, FVMPE-RS
   Bright Z, each `verified: false` with `seenInFiles` (#1964). The frontend does not read this file
   today (only the MCP and Kiwi do).
10. **Export paths.** One PDF path: `plots/pdf.ts` `exportTabsToPdf` (pdf-lib, Analysis board).
    No print stylesheet anywhere in `frontend/src`. No QR dependency.
11. **Where it would hang.** `ImageMetadataDialog.vue` already shows the Acquisition section, the
    Light path diagram and the Data path panel, and has the Refresh metadata button.
12. **The modal already builds most of the card.** Two formats, picked by `isMultiphoton` (a laser
    ≥ 680 nm): two-photon images get `lightPathStages()` (`acquisitionLightPath.ts`) — nine stages in
    light order, every value tagged with its source, measured depth/time numbers placed in; everything
    else gets `acquisitionRows()` (`acquisitionSettings.ts`), a flat grid with no source markers, no
    gain/offset and no measured numbers. The two duplicate the objective / scanner / integration text.
    Wording faults the card would inherit: filters hard-coded "not recorded" (fact 4); "HV 0" listed
    as a detector value with "Auto HV on" on a separate line; FVMPE-RS integration "None, count 2"
    shown as "No integration" (count dropped); acquisition date, Kalman, bit depth, pinhole never shown.

## Scope of this design

**Two-photon `.oir` only — FV4000 and FVMPE-RS.** It rests on the light-path stages, which only
two-photon images get. **Rethink when multiplex images and other confocal scopes land** (the
maintainer, 2026-11-06): one-photon confocal brings pinhole, per-line lasers and dichroic choices that
matter, multiplex brings many rounds/channels per image, and neither fits a four-row detector table.
Until then a non-two-photon image gets no card button rather than a half-right card — it gets the
"Request your microscope" action instead (D10), and those requests are the evidence for which scope
to support next.

**`.czi` landed alongside (#1966).** A two-photon Zeiss file (LSM 780 NLO) passes the same rule and
gets a card. `czi_meta` reads no filters, dichroics or date, so those rows print "not recorded"; HV is
per detector (no Auto HV); the depth ramp is named "Auto Z brightness". Reader versions are per
`source` (`ACQUISITION_READER_VERSIONS`), so `.czi` settings are not re-read on every refresh.

## Decisions

Agreed on the mock (2026-11-06): light-path order, the "This run" column, channel names only.
D2 and D7 are proposed.

- **D1 · The card reuses the modal's content, not a new builder.** The settings come from
  `lightPathStages()`, the target block from `acquisitionDataPath.ts` / `cleanupFacts`. A thin
  `acquisitionCard(img)` in `frontend/src/utils/acquisitionCard.ts` only regroups those items into the
  card's sections and adds the fill-in fields. So the modal and the card can never disagree, and a
  wording fix lands in both.
- **D2 · Renderer = a print view of a Vue component**, `window.print()` → the browser's "Save as PDF",
  `@media print` stylesheet, A4, black and white. No new dependency. Rejected: pdf-lib (hand-placed
  text runs for what is a table).
- **D3 · Light-path order, no per-scope templates.** Same order as the modal's diagram, for every
  scope. Rejected for now: the scope software's own order and words — needs a template per scope and
  facility input, and the modal already teaches the light-path order. Scope feature lines (Bright Z,
  Auto HV) print where they act, with their `verified` flag.
- **D4 · Never fill a hole.** Every row carries the modal's source marker as a shape (◆ microscope
  file, ● image metadata, ▲ measured, ○ not recorded), plus `?` for unverified. Not measured prints
  "not measured" with one line "Refresh metadata in Cecelia before printing" — never hidden, never
  estimated.
- **D5 · Bright Z prints its state first,** with on/off tick boxes. FVMPE-RS case: "off — depth points
  stored but not applied".
- **D6 · HV under Auto HV is not a value.** Under Auto HV the HV column is one merged cell, "Auto HV
  on: file records HV <n>, maybe not the HV used ?"; otherwise each detector shows its HV. Gain and
  offset are always per channel, in their own column: merging them hid differing offsets (found by
  result review; `acquisitionCardGain.test.ts`). Same wording in `lightPathStages()`, so the modal
  says the same.
- **D7 · Link back = project + image uid as text** in the header. No QR (no dependency, and no
  Cecelia at the scope PC).
- **D8 · A "This run" column** down the right of the settings, detectors and target tables. The person
  writes what they set and what the first snap showed; the filled sheet is the record of the rerun
  and the P3 evaluation log.
- **D9 · Channel names are Cecelia's** (SHG, nuc-GFP, …). The scope's CH1–CH4 are not printed.
- **D10 · No card ⇒ the existing "Request your microscope" action.** Today `acquisitionNotice()`
  (`acquisitionSettings.ts`) offers it only for reason `format` (no settings reader for the file
  type); a file whose settings ARE read but is not two-photon gets "Light-path diagram: two-photon
  only so far" with no action — and under this plan also no card. One rule instead: no reader, or no
  diagram → the same action, in the place of the card button. `acquisitionSupportRequestUrl()`
  (`lib/links.ts`) takes the recorded `system` + `softwareVersion` when the file has them (today the
  title says "microscope: …") and what is missing (settings reader vs diagram and card).

## Layout (agreed mock)

One A4 page, black and white. Mock rendered from `20armH`'s real values (date and filters read from
the `.oir`, i.e. assuming P0):

1. **Header:** image name · scope, software, body · acquired date · source file; corner: Experimental,
   project + image uid, printed date. Marker legend under it.
2. **Settings, in light-path order** — rows numbered by stage: lasers (λ → channels), power, Bright Z
   (state first, ramp, unverified feature line), scanner (type, zoom, dwell), pixel size, objective,
   dichroics (as recorded, per slot), digitisation (integration, bit depth,
   Kalman), stack, time. Columns: setting · marker · last time · **This run**.
3. **Detectors, per channel** — channel · dye preset · laser · detector · emission filter · HV (one
   merged Auto HV cell, D6) · gain and offset (one column) · **This run**.
4. **Target — check your first snap against this** — per channel: clipped % of signal · zero floor
   kind (`floor_stats` wording) · brightest 1 % top → bottom · brightest 1 % first → last frame ·
   **Your snap**.
5. **This run** — sample, date, question, operator, notes, "unclear on this card".
6. **Caveat box:** same settings ≠ same image (alignment, laser age, tissue, depth); the settings are
   where to start, the target is the check.

Missing-value text per row: as the modal's (`lightPathStages`), plus "date not recorded", "not
calibrated" for pixel size, and "21 of 45 planned" for a stopped-early time-lapse.

## Phases

- **P0 · Reader gaps — BUILT.** `oir_meta.parse_settings` returns `acquiredAt`, `microscopeBody`,
  per-channel `emissionFilter` (matched by channel id) and `dichroics` (`{slot, name}`, Empty slots
  left out), with `readerVersion` 2. Refresh metadata re-reads settings from an older reader
  (`ACQUISITION_READER_VERSIONS`, per reader `source`; the `.oir` one kept in step with
  `oir_meta.READER_VERSION` by a test); with the
  source gone, the older settings stay. `zStack` / `kalman` typed.
- **P1 · Modal wording — BUILT** (the card inherits it, D1). In `lightPathStages()`: dichroics and
  per-channel filters from P0; the D6 HV cell; integration printed as recorded ("None × 2", `?`);
  date, Kalman, bit depth shown. Tests in `acquisitionLightPath.test.ts`, extending its real fixture.
  D10: the request action on the not-two-photon notice, and the request URL carrying system and what
  is missing (vitest for both).
- **P2 · Card content — BUILT.** `acquisitionCard.ts` regrouping P1's stages + the data path. Tests: golden
  content from the `20armH` fixture; missing values (no `acquisition`, no `profile`) print as missing;
  unverified flags survive; Bright Z off; non-two-photon image → no card.
- **P3 · Print view — BUILT.** "Settings card" button in the dialog's Acquisition section (two-photon only) →
  print component, Experimental badge; where there is no card, the D10 request action. PDF of `20armH` for the maintainer's eyes before merge.
- **P4 · Evaluation.** One person re-acquires one image from a printed card, ideally on the FVMPE-RS,
  and fills the This-run columns. Check the new image's clipped % and depth/time p99 change against
  the target block; log what was unclear from the card's own field. Needs facility time.

P4 waits for a scope session.

## Open questions

For the maintainer:
- Refresh metadata (with measure) on the FVMPE-RS projects and on `20armH` — writes `ccid.json`, so his
  call; until then only 3 FV4000 images have settings, and none has the target numbers.
- Reachable outside the dialog too (image table row menu)?

For facility staff (do not guess):
- FVMPE-RS: integration "None, count 2" — what ran? HV 550 under Auto HV — the value used, or a
  leftover?
- Which recorded dichroic slots matter for re-acquiring (laser combiner, frame cube), and are the
  detector-unit dichroics fixed per scope?
- Which fields people actually set by hand, so the card can stay one page.
