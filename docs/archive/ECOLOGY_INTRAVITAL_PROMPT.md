> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> track-behaviour design lives in [`docs/todo/MOTILITY_METRICS_PLAN.md`](../todo/MOTILITY_METRICS_PLAN.md)
> and [`docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md`](../todo/TRACK_BEHAVIOUR_AXES_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-26); no `docs/todo/ecology-intravital/` exists.
> Premise drift at archiving: `docs/todo/IMMUNEMAP_IMPORT_PROMPT.md` is now
> [`IMMUNEMAP_IMPORT_PROMPT.md`](IMMUNEMAP_IMPORT_PROMPT.md) here, and the importer is built (#2046;
> [`../todo/IMMUNEMAP_IMPORT_PLAN.md`](../todo/IMMUNEMAP_IMPORT_PLAN.md)), so "do not build the
> importer" is moot. The tumour/SHG-collagen framing (data source, anchor question 1) came from the
> drafting model, not the maintainer: the intent is behaviour analysis and regional landscapes in
> general, not one project's collagen question. "Julia + Vue direction as well as the current
> stack" — Julia + Vue *is* the current stack. Continuous behaviour axes (#1821–#1823) and
> dwell/bouts (MOTILITY P3) are the "already tracked" items it names.

# ECOLOGY_INTRAVITAL_PROMPT

Cecelia already borrows one idea from movement ecology: hidden Markov models for track behaviour. Find out how much more movement and behaviour analysis from ecology could be integrated into the framework for intravital data. This is the main search, so go deep.

Quantification and analysis only, **not image processing**. Do not modify application code. Write only under `docs/todo/ecology-intravital/`.

This is an experiment. If it surfaces nothing new, say so plainly. That is a valid result.

## Data

1. **In-house:** T cell tracks from the tumor/collagen T cell project (tracks, positions over time, available channels such as SHG collagen, treatment timepoints). Locate them from the repo, docs, and scripts.
2. **Immunemap fallback:** when a method cannot be tested on in-house data (for example it needs vessels, a second cell type, longer tracks, a covariate we lack, or more animals), look for suitable datasets on Immunemap (immunemap.org). Read `docs/todo/IMMUNEMAP_IMPORT_PROMPT.md` and any importer code first. Check what is actually accessible. If automated access fails, **list candidate datasets with links and what each would test, and stop there**. Do not invent datasets or contents. Respect licenses and terms of use. Download tables for analysis outside the app only. Do not build the importer here.

**Start from segmented cells and tracks, not from existing populations, clusters, or HMM states.** Those are the maintainer's own answers and would bias the exploration. Derive what you need from raw tracks.

## Anchor questions

Use these as hypotheses where the data allow, and add your own:

- Do T cells change speed, turning, or persistence near aligned collagen (SHG), and does this differ across treatment timepoints?
- Do behaviour states (searching vs transit vs arrest) depend on local environment, and can they be found without hard clusters?
- Do cells interact (encounter, avoid, follow) beyond chance?

If the maintainer adds more questions below this line, prioritise them:

<!-- MAINTAINER QUESTIONS -->

## Phase 0: What Cecelia already does

Write `docs/todo/ecology-intravital/ANALYSIS_MAP.md`: every track and behaviour analysis Cecelia offers (track measures, HMM states, Leiden/UMAP on tracks, tracks sub-population gating, plots), with where it lives, the data structure it consumes, and its limits. Read `docs/todo/` ideas and audits so you do not re-propose tracked ideas (diffmap/continuous behaviour axes, track sub-population gating, and similar). List those as "already tracked."

## Phase 1: Method catalogue from movement ecology

Extend and verify this seed list, dropping anything that does not hold up:

- Step-selection and resource-selection functions: next-step choice vs local covariates (collagen density/alignment, vessels, neighbours, distance to structures)
- Behavioural state models beyond a standard HMM: hidden semi-Markov (state duration), covariate-driven transition probabilities, multi-scale states, change-point segmentation
- Random-walk family: correlated random walk, biased and bi-directional walks, Lévy vs Brownian, anomalous diffusion exponents
- First-passage time and area-restricted search
- Utilisation distributions, home-range or territory estimation per cell or per region
- Encounter, avoidance, and attraction analysis between individuals or cell types
- Movement-covariate coupling: time-varying drivers of speed and turning
- Individual vs population variation: mixed-effects movement models, repeatability across tracks
- Measurement-error handling: state-space and Kalman-based track models, effect of localisation error and track gaps

Sources: movement-ecology reviews, package docs (for example amt, momentuHMM, moveHMM, adehabitat, bayesmove, ctmm), and papers that already applied these to immune or tumor cells. For each method record: what question it answers, required inputs, primary reference with URL, maintained package and license, and whether it has been applied to intravital data before (link or "none found").

## Phase 2: Mapping to Cecelia

For each method: which Cecelia structure feeds it, whether the covariates exist or must be computed, whether it is already covered or tracked, and the biological question in one sentence an immunologist would care about. Drop methods already covered or whose data need cannot be met on in-house or Immunemap data.

## Phase 3: Test on real data (the key step)

For the top 6 to 8 methods, run them **outside the app** on exported tables, with scripts saved under `docs/todo/ecology-intravital/scripts/`:

1. **Simulate first.** Simulate tracks from a known model, apply the method, and confirm it recovers the known parameters or states. If it cannot recover them at realistic track length and noise, say so. Movement methods are easy to misfit.
2. State a hypothesis **before** running on real data.
3. Run on in-house data, or on Immunemap data where needed. Use permutation or null controls. Treat tracks from one animal, movie, or field as non-independent.
4. Produce one figure and one paragraph: what it shows, robustness, what would break it.
5. Score: **Insight** (new / confirms known / nothing), **Robustness** (low/medium/high), **Effort to integrate** (S/M/L).

Report weak and failed runs honestly. Do not tune until something looks significant. Save all figures in `docs/todo/ecology-intravital/CONTACT_SHEET.md` so the maintainer can skim them quickly.

## Phase 4: Integration design

For methods with insight, describe the minimal integration:

- new inputs needed (covariate layers such as collagen orientation, distance-to-structure, per-step features)
- where it fits (new track analysis task, new per-track or per-step feature, extension of the HMM, new plot)
- reuse of existing structures versus new ones
- dependency, license, runtime cost, and whether it works in the Julia + Vue direction as well as the current stack
- a one-line success metric a rerun on the same data could check

## Phase 5: Report

Write `docs/todo/ecology-intravital/ECOLOGY_INTRAVITAL_REPORT.md`:

1. **Verdict** (3 to 5 lines): did this surface anything new and useful? What was robust? Would you run it again, on what?
2. **Ranked methods** (top 5 to 8): question, evidence on real data, robustness, integration effort, what Cecelia needs.
3. **Data gaps:** methods that could not be tested in-house, with Immunemap candidates (links, what they would test) or why none exist.
4. **Already covered or tracked** appendix.
5. **Rejected** appendix with reasons.
6. **Cautions:** small n, track length, tracking errors, motion artefacts, multiple comparisons, pseudoreplication.
7. **Domain review needed:** every result whose statistical or biological validity needs the maintainer's judgement.

## Rules

- Cite every method and claim with a URL or reference. Quotes under 15 words, otherwise paraphrase.
- State hypotheses before running. Report all runs, including failures.
- No biological conclusions beyond what the data show.
- Do not fill gaps from memory. If a source or dataset is unreachable, say so and move on.
- Reproducible scripts: fixed seeds, recorded package versions, input paths.
- Stop when the report is written, or if two consecutive phases produce nothing usable. Then write the verdict explaining why.

## Budget

At most 20 candidate methods in Phase 1, 8 analyses in Phase 3, one pass. If you hit a limit, stop and report what you have.
