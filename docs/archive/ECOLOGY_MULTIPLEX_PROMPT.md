> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> spatial design lives in [`docs/todo/SPATIAL_REGIONS_PLAN.md`](../todo/SPATIAL_REGIONS_PLAN.md) and
> [`docs/todo/SPATIAL_PATTERN_PLAN.md`](../todo/SPATIAL_PATTERN_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-26); no `docs/todo/ecology-multiplex/` exists.
> Premise drift at archiving: several seed methods are already built — co-occurrence with a
> permutation null (`spatial_utils.pairwise_contact_logodds`, *Neighbour stats*), point-pattern
> statistics (*Spatial pattern*: NN ratio and a K-ratio pair curve under random labelling, no edge
> correction needed), composition vs distance from a structure (*Proximity profile*, #2318), and
> region clustering / composition (#2364). Diversity indices, Moran's I / Geary's C, landscape
> metrics and beta diversity are not built. The HuBMAP sections it means are the PhenoCycler tissue
> in projects mjphFQ / bdRjpG.

# ECOLOGY_MULTIPLEX_PROMPT

Decide which community and landscape ecology methods are worth integrating into Cecelia for multiplex tissue images (PhenoCycler/CODEX). Many of these already exist in packages, so for each method the question is **integrate or not**, not "is it new."

Quantification and analysis only, **not image processing**. Do not modify application code. Write only under `docs/todo/ecology-multiplex/`.

This is an experiment. "Nothing worth integrating" is a valid result.

## Data

Use the HuBMAP PhenoCycler sections available to the maintainer (segmented cells with positions and marker intensities). Locate them from the repo, docs, and scripts. If another public multiplex dataset is needed to test a method, find one, check its license, and record the source. Do not assume Immunemap has multiplex data. Check before relying on it.

**Start from segmented cells and marker intensities, not from existing populations, clusters, or gating.** Those are the maintainer's own answers. Derive cell types with a simple standard method of your own if you need them, and say which.

## Phase 0: What Cecelia already does

Write `docs/todo/ecology-multiplex/ANALYSIS_MAP.md`: every spatial and composition analysis Cecelia offers (neighbourhoods, spatial measures, populations, plots), with where it lives, the data it consumes, and its limits. Read `docs/todo/` ideas and audits and list tracked ideas as "already tracked."

## Phase 1: Method catalogue

Extend and verify this seed list:

- Co-occurrence and association models with null-model testing
- Point-pattern statistics: Ripley's K/L, pair-correlation, marked point processes, with edge correction
- Diversity indices (Shannon, Simpson, Hill numbers) as local heterogeneity maps
- Landscape metrics on tissue regions: patch size, edge density, connectivity, fragmentation
- Niche and gradient analysis: ordination, composition shift with distance from a structure
- Spatial autocorrelation (Moran's I, Geary's C) on marker or cell-type fields
- Beta diversity and occupancy or co-abundance models between regions and sections

For each: question answered, required inputs, primary reference with URL, and the packages that implement it with licenses (for example spatstat, vegan, landscapemetrics, squidpy, scimap, scanpy, pysal). Record whether it has been applied to multiplex or spatial-omics data before (link or "none found").

## Phase 2: Integrate-or-not decision

For each method, give one verdict:

- **Already covered:** by Cecelia or a tracked idea.
- **Wrap existing package:** a maintained, compatibly licensed package does it. State the package, the input it needs, and what glue is required.
- **Build:** no good package, but the method is simple and valuable.
- **Skip:** low value, poor data fit, heavy dependency, or a license problem.

Justify each verdict in two sentences: biological question it answers (in terms an immunologist or tissue biologist would care about), and cost versus value.

## Phase 3: Test on real data

For the methods with verdict wrap or build (at most 6), run them **outside the app** on exported tables, with scripts under `docs/todo/ecology-multiplex/scripts/`:

1. State a hypothesis before running (for example, "cell types A and B co-occur at short range beyond chance").
2. Use null models or permutation controls. Treat sections from one donor or sample as non-independent. Say whether edge effects or tissue holes could create artefacts.
3. Produce one figure and one paragraph: what it shows, robustness, what would break it.
4. Score: **Insight** (new / confirms known / nothing), **Robustness**, **Effort to integrate** (S/M/L).

Report weak and failed runs honestly. Do not tune until something looks significant. Put all figures into `docs/todo/ecology-multiplex/CONTACT_SHEET.md`.

## Phase 4: Report

Write `docs/todo/ecology-multiplex/ECOLOGY_MULTIPLEX_REPORT.md`:

1. **Verdict** (3 to 5 lines): is anything worth integrating? Which methods, and why?
2. **Decision table:** method, verdict (covered / wrap / build / skip), package and license, biological question, effort, evidence from real data.
3. **Integration notes** for wrap and build items: inputs, where it fits (task, plot, feature), dependency cost, success metric.
4. **Rejected** with reasons.
5. **Cautions:** pseudoreplication, segmentation and typing error, edge effects, multiple comparisons.
6. **Domain review needed:** results whose validity needs the maintainer's judgement.

## Rules

- Cite every method and claim with a URL or reference. Quotes under 15 words, otherwise paraphrase.
- State hypotheses before running. Report all runs, including failures.
- No biological conclusions beyond what the data show.
- Do not fill gaps from memory. If a source or dataset is unreachable, say so and move on.
- Reproducible scripts: fixed seeds, recorded package versions, input paths.
- Stop when the report is written, or if two consecutive phases produce nothing usable. Then write the verdict explaining why.

## Budget

At most 15 candidate methods in Phase 1, 6 analyses in Phase 3, one pass. If you hit a limit, stop and report what you have.
