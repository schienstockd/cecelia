# User booklets — plan

**Status:** **DRAFT** (2026-11-14) — nothing built; D1–D5 locked. Picks up after the three throwaway drafts in
[`GUIDE_SCREENSHOTS_PLAN.md`](GUIDE_SCREENSHOTS_PLAN.md) and their review
[`../archive/BOOKLET_RESERVATIONS.md`](../archive/BOOKLET_RESERVATIONS.md) (banner first). #2216 (viewer
rebuild) merged, so captures can start from `origin/main`. Output repo: `schienstockd/cecelia-booklets`
(created, empty, public; see D1).

## Goal

Short PDF booklets a person can hand to a new user, follow at the bench or the workstation, and trust.
They should be brief, clear and clean, and still complete enough to follow without help. Every booklet has the same frame and the same building blocks. Each one is free inside its steps.

Three things run through every booklet:

1. **Acquisition decides what you can quantify.** Aim for images you can measure, not images that only look good. Catch
   problems at the microscope, not by bending the analysis afterwards to rescue weak data.
2. **The app already shows what you acquired:** the metadata dialog, the QC flags, the set check and the
   acquisition card. Booklets point at these instead of explaining physics.
3. **"How do I use AI here?"** has one simple answer. The AI toggle is off by default. There are two ways in, Kiwi and Claude Code, and a booklet only says what each one can do at that step.

The microscopy facility is moving to three training sessions: (1) intro, (2) at the instrument, setting up
imaging properly, (3) analysis, ideally in Cecelia. The booklet series is planned around sessions 2 and 3.

## Series

| # | Booklet | Session | Fixture (candidate) | Endpoint |
|---|---|---|---|---|
| **A1** | **Images you can measure**: what each setting decides later | 2 | one good and one flawed image per failure: saturated, undersampled, short z-range, slow frame interval, bleed-through | Read the metadata dialog, QC flags and acquisition card; know when to re-image |
| B1 | Few-marker tissue (confocal) | 3 | `4rNbMp` / FtGoJO | Gated population on the image, count and share |
| B2 | Multiplex tissue (PhenoCycler) | 2 + 3 | `mjphFQ` / bPkrUs | Part 1: registered, checked, viewed (microscope users). Part 2: segment → cluster → spatial |
| B3 | Intravital timelapse (2P) | 3 | `4kS67f` / yFNlw2 | Tracks, behaviour, board |
| B4 | Live cells and structures | 3 | open | from `recipes.ts` `live-structures` |
| B5 | In vitro migration | 3 | open | from `recipes.ts` `in-vitro-migration` |
| **C1** | **Using AI in Cecelia**: one page | any | none | Toggle, Kiwi vs Claude Code, what leaves the machine, stamps |
| C2 | Connect Claude to Cecelia | any | none (terminal + app) | A user's own Claude Code session sees their project (answers the user question "how do I connect claude to cecelia?") |
| C3 | Ask Kiwi about what you see | 3 | B1's fixture | Point → ask → keep; check a claim's ref |

B1–B5 are the five recipes in `frontend/src/lib/guides/recipes.ts`, one booklet each, so the booklet and the in-app guide follow the same path. The order to build them: A1 · B1 · C1 · C2 · B2 · B3 · C3 · B4 · B5. A1 and C2 come early because they answer the facility's question and the user's question.

## The frame (fixed in every booklet)

1. **Cover:** title, *what you'll get*, *what you need*, who it is for, where it ends, the tissue or sample in
   one line.
2. **Before you image** (B-series): 3–5 rows of *setting → what it limits later → where Cecelia shows
   it*. Drawn from the acquisition advisor's bands, not invented per booklet.
3. **Steps:** each one is *Do* (one pointer) → *You'll see* (the crop) → *If it goes wrong* (one line), plus an
   optional *Why* line for any setting that isn't obvious. Steps are numbered by the build.
4. **Ask AI** box: one prompt per booklet that works, marked "optional, AI is off by default".
5. **Next:** the booklet that follows on from this one.
6. **Stamp:** app version, commit, capture date.

**Free within the frame:** the number of steps, extra figures (a deliberately bad example, a before and after),
side boxes, how deep a booklet goes. **Not free:** the order of the frame's sections, the pointer style, the print scale, the
words used for UI controls (always the app's own label, checked by the build).

## Shared modules (written once, filled in per technology)

| Module | Content | Used by |
|---|---|---|
| `import` | Add images → the import QC flags | B* |
| `metadata` | **The image metadata dialog** (`ImageMetadataDialog.vue`): channels, pixel size, acquisition settings, where to find the rest | A1, B* (right after import) |
| `channels` | Naming channels; what a name is used for later | B* |
| `qc` | A QC flag → what it means → what to do | A1, B* |
| `viewer` | Open, channels, contrast, zoom to 100 µm, outlines | B* |
| `ai-box` | The Ask AI box and its fine print | all |

A module is a spec fragment plus its caption text, with slots for the technology (for example, which flags an `.oir` image shows).

## Pipeline

```
spec (per booklet, declarative)  ─┐
modules                           ├─► capture ─► compose ─► check ─► PDF ─► release on cecelia-booklets
fixture project (scripted)       ─┘
```

- **Spec:** one entry per step: preconditions (route, task function, image, version, panel), pointer
  `data-guide` id, crop anchors, masks, caption (Do / See / If wrong / Why), and **asserted labels** (the
  function name and button text the caption cites). Typed (`.ts`) so the build can check it.
- **Capture:** `gui_capture.mjs` manifest mode (P1 of the screenshots plan). One fresh context per shot. Each step
  **sets its function and captures the state before it is done** (the review's main capture failure). Viewer shots run
  headed with the ANGLE Vulkan flags, so on a workstation, not in CI.
- **Compose:** an HTML template rendered to PDF by the same Chromium. The pointer is drawn from the recorded rects in the
  GuideBubble style. Print scale is 0.19 mm per CSS px.
- **Check (build fails):** a missing anchor; an active function or button label that doesn't match the spec; a
  callout that overlaps a cell of a table; text below the minimum print height; a volatile string (relative times, ids)
  visible inside the crop. Counts in captions are **not** asserted: a person checks them on the rendered PDF.
- **Fixtures:** one clean project per booklet on an isolated instance (own `CECELIA_DEV_DIR`). That way the import
  step really starts empty and no other images or internal names leak into a shot. **Reuse, don't write new:**
  `scripts/agent_eval/app_project.py` (copies chosen images of a source project as a fresh project holding only
  raw `default`; the source is never written) and `setup_project.jl` (imports through the real import task).
  The recipe→fixture map is `scripts/agent_eval/guide_projects.json`, extended with B1–B5. It already maps
  `intravital-timelapse` to `tSJpBI`/`k58SK7`; whether B3 uses that or `4kS67f` gets settled in P2, and either way there is one map.
- **Where things live:** spec, modules, style guide and scripts go in **this repo** (`user-docs/booklets/`), because
  they depend on `data-guide` anchors and must change in the same PR as the UI. Built PDFs go to
  the public **cecelia-booklets** repo as release assets, plus an index README. The app's Help menu links to that
  index (D1), after B1 ships, not before.

## Style guide (`user-docs/booklets/STYLE.md`, to write in P1)

Extends [`docs/ui/COPY.md`](../ui/COPY.md) → *House style* (cites it, does not restate it) with what is
specific to print, condensed from the review's §8: short sentences, the app's own labels, one term per idea,
every term defined at first use or avoided, a *Why* line for every setting that isn't obvious, a failure example for every
check step, experimental features marked as such, and no internal names. Plus the pre-ship checklist from the review.

## Phases

- **P0 (app bug, before B2):** fix the PhenoCycler channel grid (`acquisitionChannelTable.ts:23`, `chName`
  indexes the stacked name list by a per-cycle `order`). Small, separate PR.
- **P1 skeleton:** style guide, spec schema, compose template and the `ai-box` module. Built with **C1** (no
  screenshots needed) to settle the frame and print look. **Needs eyes.**
- **P2 capture:** manifest mode in `gui_capture.mjs`, rect recording, the build checks, and fixture scripts.
- **P3 first pathway:** **B1** end to end, including the `import`, `metadata`, `channels`, `qc` and `viewer` modules.
  **Needs eyes.**
- **P4:** **A1** (needs a flawed image for each failure) and **C2** (trace the real connect flow first:
  `pixi run chat`, the terminal plugin "Set up", Kiwi's "Start in terminal").
- **P5:** B2 and B3 rebuilt on the modules; C3.
- **P6 publish:** release workflow to cecelia-booklets; Help menu link to the index (D1).
- **P7:** B4 and B5.

## Decisions

- **D1 Data and hosting — LOCKED: lab data, never public datasets, and the PDFs are public.** Public datasets
  (for example Immunemap) have not been processed and carry less complete metadata, and re-analysing public data
  was done once already for the Nat Comms paper. Screenshots of lab data in a public PDF are fine: only pixels in a
  booklet leave the lab, never the image data itself. So `cecelia-booklets` stays **public**, and the Help menu links straight to
  its latest release (or an index page). Nothing is bundled in the app.
- **D2 Audience — LOCKED:** a new lab member who knows microscopy but not Cecelia.
- **D3 Experimental features — LOCKED:** documented, marked Experimental.
- **D4 A1 shape — LOCKED:** one booklet, one page per technology.
- **D5 Sources — LOCKED:** spec, modules, style guide and scripts live in this repo; `cecelia-booklets` holds
  built PDFs only.
