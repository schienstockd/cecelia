> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/ACQUISITION_ADVISOR_PLAN.md`](../todo/ACQUISITION_ADVISOR_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-06, `origin/main` bd436545). Read against main, three
> premises had moved:
> - **The "related fix" already landed, in a different shape.** `background-at-zero` now gives offset
>   advice only when the panel reads "likely background clipped at 0", and photon-counted zeros got
>   their own entry, `zeros-are-no-photon`, rather than an extra `alternatives` line.
>   `intensity_utils.floor_stats` classifies the floor (photon-counted / clipped / indeterminate).
>   A card's "landed" block would reuse that wording, not the "zero-heavy" coined here.
> - **The scope feature lines are checked against the lab's files (#1964, `seenInFiles`).** FV4000:
>   7 of 7 files record Auto HV on and HV 0, so the HV hole is real. FVMPE-RS: Bright Z depths set
>   but switched off in 7 of 7, so a card must print the on/off state, not only the depth points.
>   `verified` stays false.
> - **"An add-on, not a new surface" has no slot in the advisor plan.** No phase mentions a card or
>   printout. The only PDF path is `plots/pdf.ts` (`exportTabsToPdf`, pdf-lib, analysis canvas).
>   `docs/todo/ACQUISITION_CARD_PLAN.md` was never written.

# ACQUISITION_CARD_PROMPT

Plan first, then implement only what the plan says fits now. Self-contained: do not assume other context. Output of the planning step: `docs/todo/ACQUISITION_CARD_PLAN.md`.

## Goal

A **printable settings card** generated from an existing image, so that a person at the microscope can re-acquire an image with the same settings. The card is a record, not a command: no hardware control, no scope API, no MHS.

## Background (existing work, verify in the repo)

- The metadata dialog already shows what the scope was **asked** to do (`acquisition`: lasers, power incl. Bright Z ramp, scanner, objective, detectors per channel, digitisation, stack and time) and what **landed** in the file (`cleanupFacts`: clipping, zero fraction, per-channel range, per-depth and per-frame numbers, noise where measurable).
- Values carry source markers: image metadata / microscope file / measured at import / not recorded.
- `mcp/cecelia_mcp/acquisition_knowledge.json` holds per-stage knowledge and per-scope feature lines (FV4000, FVMPE-RS: Bright Z, Auto HV), all `verified: false`.
- The acquisition advisor plan is `docs/todo/ACQUISITION_ADVISOR_PLAN.md`. This card is an add-on to it, not a new surface.

## What the card contains (one page, A5 or A4, printable PDF)

1. **Header:** image name, acquisition date, scope and software, source file, and a short ID or QR that leads back to the image in Cecelia.
2. **Settings in the order and terms of the scope's own UI**, per scope template. Cover at least: lasers and wavelengths, power (including Bright Z top and bottom values or depth points), zoom, scanner type, dwell, pixel size, objective, z-step and plane count, frame interval and count, detector per channel (HV, gain, offset), digitisation (frame sum or average and count). Each value carries a marker: recorded / not recorded / unverified.
3. **Known holes with blanks to fill in by hand:** filters and dichroics when not recorded; detector HV when Auto HV is on (the file may record HV 0). Say what to do instead, e.g. "start from Auto HV, then check clipping".
4. **What landed last time, as a target:** per channel percent clipped and percent at zero, brightest 1 % by depth, bleaching trend over time, only where measured. Values that were not measurable are printed as "not measured", never filled in. After the first snap the person compares against these to see whether the rerun matches.
5. **Fill-in fields:** sample, the question being asked, date, operator.
6. **A short fixed caveat:** the same settings do not guarantee the same image. Laser power at the sample varies with alignment and ageing, and tissue and depth differ. The settings are a starting point; the landed numbers are the check.

## Constraints

- Generate from data already extracted. No new acquisition features, no hardware control, no scope-specific API code.
- Deterministic and testable: a function taking an image (or its stored metadata) and returning the card content, then a renderer. Same input, same card. Keep content and layout separate so a card can also be shown in the GUI.
- Missing inputs are explicit ("not recorded", "not measured", "unverified"). Never guess or back-fill. Unverified scope feature wording is printed with its flag.
- Per-scope templates: start with the scopes in the knowledge file (FV4000, FVMPE-RS) and make adding a scope a data change, not a code change. A scope without a template gets a generic card, clearly marked as such.
- Land as **Experimental** under the maturity tiers. Keep the change small and reviewable. No new headline surface during hardening.
- Follow existing repo conventions for PDF or figure export; check what already exists before adding a renderer or dependency.

## Related fix to schedule with this work

`acquisition_knowledge.json`, entry `background-at-zero` (stage `detectors`), explains a high zero fraction as offset cutting the background at 0. Its `alternatives` list only post-acquisition subtraction. For resonant two-photon data, zeros may equally be true zeros from photon-limited or sparse signal, where raising the offset does not help. Add that alternative, make the offset advice conditional on the histogram shape (truncated analog background vs. discrete photon-count values), and keep the card's "landed" block consistent with this: report a high zero fraction as "zero-heavy" rather than "background clipped". See `ACQUIRED_DATA_PANEL_REVIEW.md` for the reasoning, if present in the repo.

## Plan requirements

1. Audit what exists: which metadata and `cleanupFacts` fields are available per scope, what is recorded vs. not for FV4000 and FVMPE-RS images, and any existing export or print path. Reference files and PRs.
2. Card content spec: field-by-field table with source, marker, and fallback text when missing.
3. Per-scope template format and where it lives.
4. Renderer choice and layout (one page, legible at the scope, black-and-white friendly), with a sample output for one real image.
5. Tests: golden-file test for the card content from a fixture image; a test that missing values print as missing; a test that unverified flags survive.
6. Evaluation: have a person rerun one acquisition from a card. State what to check (match of landed numbers) and how to log what was unclear on the card.
7. Phasing and effort, with what can start during hardening and what waits. Separate verified facts from assumptions and open questions for facility staff (UI wording per scope, which fields people actually need).
