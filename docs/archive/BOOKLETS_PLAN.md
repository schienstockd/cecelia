> **ARCHIVED — not authoritative, do not act on this.** A frozen record of the user-booklets plan, ended
> by D9 (2026-11-14): no PDFs at all. It is not a description of how the code works now.
>
> **Outcome.** Built and merged: the spec, the PDF build and C1 (#2234), capture and fixtures (#2236). The
> showcase frame and B1 (#2238) were dropped by D7–D9 before merging. What survives is the screenshot
> engine, re-homed for the user docs: `user-docs/shots/` (shot sets, `README.md` for how to write one) and
> `scripts/docshots/` (`pixi run doc-fixture`, `doc-shots`, `doc-images`). C1 is now the generated page
> `user-docs/using-ai.md` (`frontend/src/utils/usingAiDoc.ts`). Pointer from
> [`../todo/GUIDE_SCREENSHOTS_PLAN.md`](../todo/GUIDE_SCREENSHOTS_PLAN.md). The `cecelia-booklets` repo
> was left empty, for the maintainer to keep or delete.

# User booklets — plan

**Status:** **ENDED** (2026-11-14, D9): no booklets. P1 and P2 were built; D1–D9 locked. Picked up after the
three throwaway drafts in [`GUIDE_SCREENSHOTS_PLAN.md`](../todo/GUIDE_SCREENSHOTS_PLAN.md) and their review
[`BOOKLET_RESERVATIONS.md`](BOOKLET_RESERVATIONS.md) (banner first). #2216 (viewer
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
| **C1** | **Using AI in Cecelia**: one page | any | none | Toggle, Kiwi vs Claude Code, what leaves the machine, stamps |
| C2 | Connect Claude to Cecelia — *`user-docs/` page* | any | none (terminal + app) | A user's own Claude Code session sees their project (answers the user question "how do I connect claude to cecelia?") |
| C3 | Ask Kiwi about what you see — *in-app guide, not booklets work* | 3 | — | Point → ask → keep; check a claim's ref |

Only **A1** and **C1** are PDFs (D7). The per-recipe B-series (one per recipe in `recipes.ts`) is dropped
entirely, outcome images included (D8). The order to build: A1 · C2 as a page · publish A1 and C1.

**If the B-series is ever reopened**, two fixtures were set up and their numbers measured: B1 on
`4rNbMp`/FtGoJO (6 channels, 26 z-planes; a P14 gate of 1244 cells, 2.5 %; open: debris gate, tiling artefact,
plane readout) and B3 on `4kS67f`/k3Tx90 (52 T and 23 B tracks over 75 timepoints at 30 s; each population's
`qc` gate and the B movement clusters recreated from the source's own). B1's entry is in git history
(`user-docs/booklets/b1-few-marker-tissue.ts`, #2238).

## The frame (fixed in every booklet)

Every PDF (A1, C1) has the frame below. D6's second kind, the **showcase**, is withdrawn (D7, D8).

1. **Cover:** title, *what you'll get*, *what you need*, who it is for, where it ends, the tissue or sample in
   one line.
2. **Before you image** (where a booklet has it): 3–5 rows of *setting → what it limits later → where Cecelia shows
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
With the B-series dropped (D8), `metadata` and `qc` are the modules still in use, for A1; import, channels and
viewer were for the recipe booklets.

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
  visible inside the crop; a viewer shot whose state, read back after setting it, is not the one asked for
  (image, plane, channels, outlines, gated). **Numbers** are the fixture's: a caption writes
  `{{project.image.fact}}` (planes, channels, a gate's count and %), the fixture measures them through the
  app, the capture checks the shot shows the same number, and a hand-typed count fails the build.
- **Fixtures:** one clean project per booklet on an isolated instance (own `CECELIA_DEV_DIR`). That way the import
  step really starts empty and no other images or internal names leak into a shot. **Reuse, don't write new:**
  `scripts/agent_eval/app_project.py` (copies chosen images of a source project as a fresh project holding only
  raw `default`, or with `--with-results` its segmentation and cell tables too; the source is never written) and
  `setup_project.jl` (imports through the real import task). Gates are never copied: the fixture recreates the
  ones a booklet shows, through the population manager's route.
- **Where things live:** spec, modules, style guide and scripts go in **this repo** (`user-docs/booklets/`), because
  they depend on `data-guide` anchors and must change in the same PR as the UI. Built PDFs go to
  the public **cecelia-booklets** repo as release assets, plus an index README. The app's Help menu links to that
  index (D1), once A1 ships (P6).

## Style guide (`user-docs/booklets/STYLE.md`, to write in P1)

Extends [`docs/ui/COPY.md`](../ui/COPY.md) → *House style* (cites it, does not restate it) with what is
specific to print, condensed from the review's §8: short sentences, the app's own labels, one term per idea,
every term defined at first use or avoided, a *Why* line for every setting that isn't obvious, a failure example for every
check step, experimental features marked as such, and no internal names. Plus the pre-ship checklist from the review.

## Phases

- **P0 (app bug, before B2):** fix the PhenoCycler channel grid (`acquisitionChannelTable.ts:23`, `chName`
  indexes the stacked name list by a per-cycle `order`). Small, separate PR.
- **P1 skeleton — BUILT** (2026-11-14, branch `docs/booklets-p1`): `user-docs/booklets/` (`spec.ts` with the
  frame rules, `index.ts`, `booklet.css`, `STYLE.md`), `scripts/booklets/build.mjs` (`pixi run booklets`),
  `scripts/lib/chromium.mjs` (shared with `gui_capture.mjs`), CI check `frontend/src/utils/booklets.test.ts`, and
  **C1** (two A4 pages; the app's own AI text imported, not retyped). The Ask AI box is part of the template. **Needs eyes** on the print look.
- **P2 capture — BUILT** (2026-11-14, #2236): shots declared per booklet (`Shot` in `spec.ts`),
  `scripts/booklets/capture.mjs` (`pixi run booklet-shots`) on shared page helpers `scripts/lib/app_page.mjs`
  (also used by `gui_capture.mjs`); crop + pointer composed by the build (`figure.ts`; the pointer's wording
  and side come from the spec at build time). Checks: missing anchor, cited label, stale time in the crop,
  crop too wide, spec changed since capture. Fixtures (`Booklet.fixture`, `pixi run booklet-fixture`): lab
  images copied raw into the isolated instance, profiles with AI on/off, one Kiwi question asked once on
  `default` and saved, so shots never show a live turn. **C1 has all five shots.** Capturing step 1 found
  two app bugs (opt-in dialog behind Preferences; the switch showing on while asking), fixed in #2235.
  **Open:** the viewer (headed Chromium with the ANGLE flags, with P3); the logged-in Account row and a
  tagged entry with a ✦ gate (both left out: a real account would show, and the tag would change the
  fixture on every run).
- **P3 first showcase — BUILT (#2238), then dropped by D8.** What carried over to the shared tooling: viewer
  shots (headed WebGPU on the second screen, state read back), fixture facts (`{{project.image.fact}}`, a
  hand-typed count fails the build), fixture gates recreated through the app (a box in raw values, or the
  source's own gate or filter), copied results whose pooled runs name the copy. The showcase frame, B1 and B3
  were removed.
- **P4 A1** (needs a flawed image for each failure; one page per technology, D4).
- **P5 C2** as a `user-docs/` page (trace the real connect flow first: `pixi run chat`, the terminal plugin "Set
  up", Kiwi's "Start in terminal").
- **P6 publish:** release workflow to cecelia-booklets for A1 and C1; Help menu link to the index (D1).

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
- **D6 Two kinds; B-series are showcases — LOCKED (2026-11-14).** A PDF fits where the reader is not in the
  app or the content is frozen: A1 (at the microscope), C1 (the policy table people forward), C2 (terminal
  setup, which an in-app guide cannot drive), C3. Step-by-step for an analysis workflow would duplicate the
  in-app recipe guides (`lib/guides/recipes.ts`), which point at the live control, and two copies of the same
  steps drift. So B1–B5 are **showcases**: outcome first, what it takes, a few capability shots, honest limits,
  then the in-app guide for the doing. One booklet is never both. The capture pipeline stays the shared base:
  the same scripted shots can feed the in-app guides (`GUIDE_SCREENSHOTS_PLAN.md`). Spec: `Booklet.kind`
  (`guide` | `showcase`), each with its own frame rules in `checkBooklets` (P3). **Its showcase half is
  superseded by D7.**
- **D7 PDFs are A1 and C1; a recipe's showcase is one image on its card — LOCKED (2026-11-14).** The B-series
  showcase PDFs and the Guides dialog's recipe cards (feat/guides-next-step) were waiting on each other: the cards
  on the B shots, the PDFs on the dialog's Help path. One outcome image per recipe, on its card in the dialog's
  "What are you working with?" view, is the whole showcase, and the in-app guide does the rest. So no
  B-series PDF. C2 becomes a `user-docs/` page and C3 an in-app guide. The capture pipeline, fixtures and checks
  stay: they now feed A1 and the outcome images. A session-3 handout, if wanted later, is generated from
  `recipes.ts` plus the image, never hand-written. **Its outcome-image half is superseded by D8.**
- **D8 No B-series at all — LOCKED (2026-11-14).** Supersedes D7's outcome images: preparing an honest example
  for each recipe (B1's debris gate, tiling artefact and plane readout, then recapturing as the app changes)
  costs too much for what it adds, since the Guides dialog's "Is this you?" column already answers which recipe
  fits. `recipes.ts` gets no `outcome` field. The series is C1, A1 and C2 (a `user-docs/` page); C3 is an in-app
  guide and not booklets work. The capture pipeline, fixture tooling and checks stay for A1. **Superseded by D9.**
- **D9 No booklets — LOCKED (2026-11-14).** No PDFs, A1 and C1 included. A1's loop is microscope →
  workstation → back: the workstation part is the Acquisition check in the app, which gets an in-app guide,
  and the paper that goes back to the microscope is the per-image acquisition card, which already prints. C1's
  reader outside the app (the PI, ethics or IT person it is forwarded to) is served as well by a `user-docs/`
  page on GitHub. So C1 becomes `user-docs/using-ai.md`, generated from the app's own text with its
  screenshots; the PDF build, the booklet frame and its print rules go; the capture engine (capture,
  fixtures, figure composition and its checks) stays, re-homed so a shot targets a user-docs image. Its next
  uses are decided separately.
