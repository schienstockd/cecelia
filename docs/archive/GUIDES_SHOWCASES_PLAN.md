> **ARCHIVED — not authoritative, do not act on this.** A frozen record of a design discussion: notes,
> not decisions, written from the throwaway booklet drafts and one screenshot of the Guides dialog. It is
> not a description of how the code works now, and not instructions to re-run. Current design lives in
> [`docs/todo/BOOKLETS_PLAN.md`](../todo/BOOKLETS_PLAN.md) and
> [`docs/todo/GUIDE_SCREENSHOTS_PLAN.md`](../todo/GUIDE_SCREENSHOTS_PLAN.md).
>
> **Outcome:** archived 2026-11-14 (`origin/main` 2cb2a501); the §7 brief was not run as written. Read
> against main and the booklets work then in flight:
> - **§1 (three kinds; B-series as showcases; PDFs only where the app is not open) became BOOKLETS_PLAN
>   D6**, with `Booklet.kind` (`guide` | `showcase`) and its frame checks in `user-docs/booklets/spec.ts`.
>   At archiving these were on the unmerged `docs/booklets-p3` branch (P3), not yet on main.
> - **The recipe already holds most of what §4 assigns it.** `frontend/src/lib/guides/recipes.ts` steps
>   carry `why`, `optional`, `assist`, and either a `guide` id or a guide-less `page` + `label` (the
>   "no guide" case); maturity lives on each `GuideDef`, not the step. *Choose-when* is the recipe's
>   `whenThisIsYou`; there is no per-step *if it goes wrong*.
> - **§5's "two indexes" is real:** `GuidesDialog.vue` shows `GuideMap` (recipes, page map, maturity)
>   above an accordion of the same `GuideDef`s grouped by sidebar section (`guidesByGroup`). The plan
>   missed that each set already stores a recipe (`utils/setRecipe.ts`: suggestion, `resumeStepOf`), which
>   the dialog ignores; a declutter mock opening on it was drafted, nothing built.
> - **§3's assertions:** captures check a control's anchor and label text (`expect`); the other
>   assertions listed (active panel, cited counts, callout overlap, volatile text) did not exist.

# Guides, showcases and PDFs: discussion summary and reservations

Status: discussion notes, not decisions. Written from the booklet test drafts (intravital, confocal, PhenoCycler, C1), a screenshot of the current Guides modal, and Opus's reply on the B-series. Anything about the codebase is second-hand and needs checking (see section 7).

## 1. Where we landed

- **The recipe is the single source of truth for process.** The in-app guide, the click-through PDF and any showcase summary all read step titles, why-lines, anchors, optional flags and maturity from it. Nobody retypes step text.
- **Three document types, not one booklet type.**
  - **Guide:** linear, live, anchored to controls. Answers "what do I do next".
  - **Showcase:** short, outcome-first. Answers "is this worth doing". Ends with "Start guide".
  - **Banner:** one image, one line, for social (Bluesky). Answers "what does Cecelia do".
- **PDFs are a build target, not the product.** Keep real PDFs only where the app isn't open or the document is forwarded: A1 (microscope), C1 (what is sent to Anthropic), C2 (terminal setup), anything for the paper or supplement.
- **B1–B5 become two-page showcases** that link to the in-app recipe. No parallel step-by-step PDFs.
- **Don't bundle screenshots.** Recipe guides point at the live control, so they need none. Showcase and PDF images are fetched on demand from versioned, hash-keyed hosted URLs and cached. Offer an optional offline pack. Measure real sizes before deciding.

## 2. Reservations (carried over from the test drafts)

The drafts were tests. The defects fall into a few classes, and these apply to every future output.

**Text and screenshot disagree.**
- Segment step showed the Measure labels panel; caption said Cellpose.
- "Run on the set" vs "Run on 1 image"; "ten sections" vs nine visible; "cell count and share" not visible.
- PhenoCycler acquisition grid showed cycle 1's markers for every cycle. Probably an app bug, and the capture exposed it.

**Capture problems.**
- Screenshots taken from the end state; relative times ("77d", "12h") baked into pixels.
- Callouts covered cited values; tables unreadable at print size.
- Fixtures leaked other images, internal names, and unrelated tabs. Some results looked wrong (gate off the density cloud, unreadable outline density, alignment shots that can't show alignment).

**Content problems.**
- Jargon undefined (P14, CTDR, logicle, MSD, HMM, ref, Blackboard).
- No "choose this when", no failure cases or success/failure contrast.
- Overclaim: "every claim links to data it read" vs red refs for unread data. "Checked" needs one definition, used everywhere.
- Experimental features shown as defaults.
- Fixture facts disagree across booklets (26 z-planes vs 13/25; different sample names).

**Process problems.**
- Names differ between the PDFs and the app (Confocal / PhenoCycler / Intravital two-photon vs Few-marker / Multiplex / Intravital timelapse).
- Step lists differ (intravital draft had 11 steps; the Intravital recipe is different).
- Numbering typed by hand.

## 3. Guidelines to apply going forward

1. One spec per step drives caption, state, crop and assertions.
2. Assert every checkable claim in a caption at build time. Existing control-and-label checks are not enough. Add assertions for: active function or panel, cited counts and values, population names, and callout overlap with data.
3. Set app state explicitly before each capture; capture the before-state unless the result is the point.
4. Crop to the target; no volatile text (timestamps, IDs, job banners); legible at print size.
5. One minimal, deterministic fixture per output, chosen so the result looks right and is easy to judge. Pin Kiwi replies as recorded fixtures.
6. Define audience and endpoint; define terms at first use; say why, not only what; show how to tell success from failure; flag experimental features.
7. Shared steps are modules, written once.
8. Treat capture anomalies as app bugs. Triage into app bug, capture bug and writing bug.
9. Stamp every output with app version and date; regenerate on every release; human review of the rendered output.

## 4. Showcase and recipe: how they stay distinct

| Fact | Owner | Used by |
|---|---|---|
| Step title, why-line, anchor, optional flag, maturity | Recipe | Guide, PDF, showcase |
| "Choose this when", "if it goes wrong" | Recipe | Guide, PDF |
| Outcome image, claim, "you need" | Showcase | Showcase only |
| Counts, durations | Fixture run | Showcase, assertions |
| Step screenshots | Capture | PDF only |

- A showcase refers to recipe steps by id and may quote titles and why-lines only, so it can never carry advice the guide doesn't.
- Counts and time are measured from the fixture run and asserted against the claim. No hand-typed numbers.
- A renamed or removed step fails the showcase build.

## 5. Declutter the Guides modal

From the current screenshot, the modal shows several things at once: five recipe tabs plus the AI help toggle, a nine-step list with a description and button on each row, a page-map grid with a maturity legend, a collapsed Maturity section, and then START and DATA lists that look like the same guides indexed a second way. Someone who thinks "I don't know what to do" has to choose among all of it.

**Principle:** the first screen should hold one choice and one button. Everything else is one click deeper.

1. **Start with one question.** "What are you working with?" then big choices for the five recipes, worded as the user's data ("Few markers on tissue", "Many markers, several cycles", "Time-lapse of moving cells"…). Preselect from the loaded image's metadata when possible (the Next panel already suggests a modality).
2. **After choosing, show only the next step.** One card: title, one line, a single **Start**. Done steps collapse to a tick; later steps collapse under "All 9 steps".
3. **Optional and "no guide" steps** sit in a collapsed "Optional" group, or are hidden until relevant. Don't show a disabled-looking row in the main path.
4. **One index, not two.** The recipe tabs and the START/DATA lists appear to cover the same guides. Keep the recipe view as the default and move the by-page catalogue behind "Browse all guides".
5. **Move the page map and maturity legend out of the first view.** Put the page map under "Browse", and show maturity only where it matters: a small badge on Experimental or Coming soon steps, with Beta and Tested left off the default view.
6. **Show descriptions only for the current step,** or on expand. Titles carry the list.
7. **First-run shortcut.** In an empty project the only visible step is Import images.
8. **Showcase strip** (outcome image, needs, time, PDF) appears when a recipe is chosen, and collapses after first view.
9. **Check by clicks:** from "open Guides" to the first real action should take at most two clicks, with nothing to read in between.

Caveat: this is from one screenshot. How the modal is built, and whether the two lists are really the same data, needs checking (section 7).

## 6. Open questions

- How much of the recipe may a showcase quote? Proposal: titles and why-lines only.
- What does the PDF generator do with optional steps and "no guide" steps? Proposal: include optional steps marked as such; skip "no guide" or mark "no guide yet".
- Which maturity levels does a showcase show? Proposal: always show Experimental and Coming soon.
- Which names win (panel names vs booklet names)? Proposal: the panel's names, everywhere.
- Are fixtures publishable (lab data, unpublished results, any human samples)? Needed before the banner or any hosted images.
- Do hosted images go on the docs site, a CDN, or the release assets? Does a network fetch need a setting for strict labs?
- Bluesky banner: which placements and sizes? Check current specs before fixing dimensions. Alt text generated from the spec.

## 7. Brief for Opus: check before planning

Please review what exists, then report how the pieces fit before any B1 work starts. I'd like findings, gaps and a proposed order, not a rewrite.

**Check what exists**
- `lib/guides`, the recipes and the `data-guide` anchors: what a recipe step currently holds, and which of these exist already: why-line, choose-when, if-it-goes-wrong, optional, maturity, "no guide".
- How the Guides modal gets its data. Are the START/DATA lists and the recipe tabs the same source? Where do the page map and maturity legend come from?
- The capture pipeline and current checks: exactly what fails a build today (renamed control, changed label) and what doesn't.
- How PDFs are generated and stamped today, and whether the booklets (A1, C1, C2, B-series) share a spec.
- The C1 content: is the "what is sent" table generated from the app's AI-feature config, or typed?

**Check how it fits together**
- Can a showcase spec reference recipe steps by id without changing the recipe schema much? What would change?
- Can the PDF click-through be generated straight from the recipe, so the B-series needs no separate step prose?
- Where would the showcase strip live in the modal, and what data does it need?
- Do the recipe names match the booklet names, and what is the cheapest way to align them?
- Is the in-app guide's step list the same as the PDF's for each modality? Where do they differ (intravital had 11 PDF steps)?

**Check the gaps this discussion found**
- Which of these assertions exist, and which need building before B1: active function or panel matches caption, cited counts and states match the app, callout overlap with data, capture-state (before vs end), no volatile text in pixels.
- Is the PhenoCycler acquisition grid a real app bug? Reproduce it and file it if so.
- Fixture reconciliation: one sample, one name, one set of counts across booklets and viewer.
- Measure the real image count and size for the showcase and PDF assets before choosing a hosting approach.

**Report back with**
- What already exists and what is missing, per item above.
- A recommended order of work, with the assertion gaps as a prerequisite for B1.
- A decision list for section 6, with your recommendation on each.
- A proposed first-screen design for the Guides modal, checked against how it is built.
