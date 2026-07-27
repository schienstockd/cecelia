# Stats annotations on summary plots

Status: planning · no branch yet · supersedes `docs/prompts/stats-on-plots-prompt.md`

## Goal

Compute pairwise / omnibus hypothesis tests server-side and render them as **marks inside the
existing Observable Plot summary charts** — so the annotation rides the ONE registry-driven plot
pipeline (`docs/PLOTS.md`), the ONE CSV export, and the ONE board-SVG export. Result is structured
data reusable by the What's New card format ([`WHATS_NEW_PLAN.md`](WHATS_NEW_PLAN.md)) and the
sketch engine ([`SKETCH_ENGINE_PLAN.md`](SKETCH_ENGINE_PLAN.md)).

## Corrections to the Sonnet prompt

- **Not the Population Manager.** `SummaryCanvas` does not consume the pop manager; it uses
  `SeriesPicker` + `useSummaryData` (`SummaryCanvas.vue:3-5`, `useSummaryData.ts:18-60`). Stats UI
  lives on the summary canvas.
- **Not a new `/api/stats/compare` route.** `POST /api/plot_data` is the ONE aggregator
  (`docs/PLOTS.md:3-17`); extend `PlotDataResponse` with `comparisons?`.
- **Not a post-render SVG overlay.** `plots/overlays.ts` is HTML-only (legend/title). Marks belong
  inside the Plot spec so they export as vector automatically via `PlotChart.toImageURL('svg')`.
- **Not a bespoke CSV serialiser.** `frontend/src/plots/plot.ts:829` `plotDataToCsv` is the ONE
  serialiser — extend, don't fork.
- **Not `write_qc`.** Stats are cross-series and ephemeral, with no `value_name` — the QC rail is
  for per-image sidecars. Add an explicit exemption comment.

## Genuinely new (scope)

1. `HypothesisTests.jl` added to `app/Project.toml` — **triple-instantiate** `app/`, `api/`,
   `pluto/` (per CLAUDE.md → *Adding a Julia dependency to `app/`*).
2. `app/src/plotting/stats.jl` — pure Julia stats module with `StatsResult`.
3. `PlotDataResponse.comparisons?: Comparison[]` — optional field on the existing response.
4. Marks builder in `frontend/src/plots/plot.ts` — brackets + p-value/star text as native Plot
   marks (`Plot.ruleY`, `Plot.text`).
5. `SummaryPanel` gets a small "Compare groups" section (test dropdown + on/off toggle) —
   state via `useViewState`-persisted `statsConfig` in the panel's shared bag.
6. `plotDataToCsv` case: `# Stats: …` comment block preceding the tidy table (Prism-compatible).

## Decisions (2026-07-27)

1. **Tests.**
   - 2 groups → **Mann-Whitney U** default; **Welch's t-test** opt-in.
   - >2 groups → **Kruskal-Wallis** default; **one-way ANOVA** opt-in.
   - Pairwise post-hoc with **Bonferroni** p-adjustment (uncontroversial, no extra dep).
     Dunn / Tukey deferred until requested.
   - Significance display: `ns / * / ** / *** / ****` from the adjusted p.
2. **Where compute lives**: `app/src/plotting/stats.jl` — small pure module,
   `run_stats(groups::Dict{String,Vector{Float64}}; test::Symbol=:auto) → StatsResult`. Imported
   from `plot_data.jl` only when the request opts in.
3. **Contract — `StatsResult` JSON** (locked; consumed by Plans 1 & 3):
   ```jsonc
   {
     "test": "mannwhitney",
     "groups": ["WT", "KO"],
     "n": [8, 7],
     "means": [4.2, 6.1],
     "medians": [4.0, 6.3],
     "statistic": 12.0,
     "p_value": 0.003,
     "significance": "**",
     "method_note": "Mann-Whitney U (two-sided)",
     "comparison_pairs": [["WT", "KO", 0.003, "**"]]   // (a, b, p_adj, sig)
   }
   ```
4. **Extend `POST /api/plot_data`, don't add a route.** Request adds an optional
   `stats: { enabled: bool, test: "auto"|"ttest"|"mannwhitney"|"anova"|"kruskal", showNs?: bool }`;
   response adds an optional `comparisons: StatsResult`.
5. **Rendering = Plot marks, not SVG overlay.** Extend `buildPlotOptions` in `plot.ts` to append
   bracket rules + text marks when `r.comparisons` is present. The existing PlotChart SVG export
   picks them up automatically (no changes to `PlotChart.vue`).
6. **UI**: a "Compare groups" collapsible in `SummaryPanel.vue` — test dropdown + on/off toggle +
   "show `ns`" toggle. State via `useViewState` in the panel's bag; NOT in the pop manager.
7. **CSV**: extend `plotDataToCsv` with a leading `# Stats: …` block when `comparisons` present.
   Cover in `plotCsv.test.ts`.
8. **Lab log** on compute: one `[Cecelia]` line via `lab_log.jl` (mirrors the cohort-check
   pattern in `CohortCheckButton.vue`). Not per-image QC.
9. **Clutter control**: hide non-significant brackets by default; the `showNs` toggle reveals them.
10. **QC exemption comment** in `stats.jl`: "no `write_qc` — cross-series, ephemeral, no value_name".

## Phases (independently shippable)

- **S0 — Prism-parity audit (do this first).** Biologists know Prism. Before writing test code,
  screenshot / list what Prism does for the four plot kinds we render (bar + SEM, box, violin,
  scatter): bracket placement, star vs `p =` text, offset from top of highest bar/whisker, "ns"
  behaviour, colour, multi-comparison layout (stacked brackets vs sloped). Pick the closest
  Prism-alike as our default for each. Deliverable: a short list of decisions ("brackets black,
  1px, `p = 0.003 **` centred, positioned at 1.05× tallest whisker per pair") that S3's marks
  builder implements verbatim. Also cross-check: does the current bar/box/violin/scatter cover
  everything Prism does that biologists reach for, or is there a chart type we don't render yet
  that we'd need? If yes, note it — but stats annotation still lands only on what we render.
- **S1** — Julia: add `HypothesisTests` to `app/Project.toml`; triple-instantiate (`app/`, `api/`,
  `pluto/`) and commit all three manifests. `app/src/plotting/stats.jl` + `run_stats` +
  golden-value tests in `app/test/runtests.jl` (Mann-Whitney vs a known reference, Kruskal-Wallis,
  Bonferroni).
- **S2** — Wire into `plot_data.jl`: when `stats.enabled`, group the aggregated data by the same
  key as the chart's colour/facet series and compute pairwise. Extend `PlotDataResponse`.
- **S3** — Frontend: extend `PlotDataResponse` TS type; marks builder in `plot.ts` (brackets +
  significance text); unit test the mark placement with a stub dataset.
- **S4** — `SummaryPanel` "Compare groups" section + `useViewState`-persisted `statsConfig`.
- **S5** — `plotDataToCsv` case + `plotCsv.test.ts`.
- **S6** — Lab-log entry on compute (single `[Cecelia]` line).

## Verify

- 2 treatment groups → bracket + `p = 0.00X **` renders on the plot.
- 3+ groups → only significant pairs shown; toggle reveals `ns`.
- CSV starts with `# Stats: Mann-Whitney U, p = …` block; remainder unchanged; opens in Prism.
- Vector SVG export contains the annotations (via existing `PlotChart` path — no new export code).
- `StatsResult` JSON is the exact shape referenced by `WhatNewCard.statsAnnotation` (Plan 1) and
  `DrawBracketAct` input (Plan 3).
- All three manifests resolve after adding `HypothesisTests`.

## Out of scope

- Linear models / mixed effects — separate future phase.
- Normality-driven auto test selection — assume non-parametric default.
- Dunn / Tukey post-hoc — Bonferroni suffices for v1.
- Persistence — cross-series ephemeral; recompute on request.

## References

- `INVENTORY.md:19,71-72,114` — plot flow, aggregator, sole serialiser.
- `docs/PLOTS.md:3-17` — the ONE plot hosting path.
- `docs/MODULES.md` → *QC — REQUIRED for every new task* — exemption noted here.
- `app/src/plotting/plot_data.jl` — server-side aggregator.
- `api/src/plotting_api.jl:164-299` — `POST /api/plot_data` handler.
- `frontend/src/plots/plot.ts:829` — `plotDataToCsv` (extend, don't fork).
- `frontend/src/components/plots/PlotChart.vue` — Plot spec renderer + SVG export.
- `frontend/src/composables/useSummaryData.ts:18-60` — shared canvas bag.
- `frontend/src/composables/useViewState.ts` — the persistence primitive.
- `CLAUDE.md` → *Adding a Julia dependency to `app/`* — triple-manifest rule.
- `WHATS_NEW_PLAN.md`, `SKETCH_ENGINE_PLAN.md` — downstream consumers of `StatsResult`.
