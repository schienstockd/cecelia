# Stats annotations on summary plots

Status: planning · no branch yet · supersedes `docs/archive/stats-on-plots-prompt.md`

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
   `pluto/` (per docs/ARCHITECTURE.md → *Adding a Julia dependency to `app/`*).
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
   `run_stats(groups; test::Symbol=:auto) → StatsResult`, where `groups` is any iterable of
   `label => values` pairs (Vector, tuple, or `AbstractDict`). Plain `Dict` iteration order is
   implementation-defined, so callers that need a stable order pass a `Vector{<:Pair}` or an
   `OrderedDict`. Imported from `plot_data.jl` only when the request opts in.
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
   `stats: { enabled: bool, test?: "auto"|"ttest"|"mannwhitney"|"anova"|"kruskal" }`; response
   adds an optional `comparisons: StatsResult`. `showNs` and `useStars` are purely client-side
   rendering flags (not server input) — the server always returns the full result and the
   frontend decides what to draw.
5. **Rendering = Plot marks, not SVG overlay.** Extend `buildPlotOptions` in `plot.ts` to append
   bracket rules + text marks when `r.comparisons` is present. The existing PlotChart SVG export
   picks them up automatically (no changes to `PlotChart.vue`).
6. **UI**: a "Compare groups" collapsible in `SummaryPanel.vue` — test dropdown + on/off toggle +
   "show `ns`" toggle. State via `useViewState` in the panel's bag; NOT in the pop manager.
7. **CSV**: keep the existing points CSV pristine (per-datapoint, Prism-loadable). Ship the
   stats as a **separate** `{name}.stats.csv` sibling — one row per pairwise comparison, with a
   short header block (test, method note, n per group). New helper
   `plotStatsToCsv(r: PlotDataResponse)` in `frontend/src/plots/plot.ts`; board export writes one
   `.stats.csv` per plot that has `comparisons` (nothing when a plot has no stats). Cover in
   `plotCsv.test.ts`.
8. **Lab log** on compute: one `[Cecelia]` line via `lab_log.jl` (mirrors the cohort-check
   pattern in `CohortCheckButton.vue`). Not per-image QC.
9. **Clutter control**: `ns` brackets **shown by default** (matches Prism, per S0 audit). Users
   can toggle `showNs=false` to hide them; when many groups clutter the chart, Compact Letter
   Display would help but is deferred (see S0-6).
10. **QC exemption comment** in `stats.jl`: "no `write_qc` — cross-series, ephemeral, no value_name".

## Phases (independently shippable)

- **S0 — Prism-parity audit (done; decisions locked).** Sources: GraphPad Prism 10/11 docs
  (Pairwise Comparisons, Decimal formatting of P values, FAQ 978, Compact Letter Display, How to
  report P values in journals) and the `ggprism` R theme that mirrors Prism's defaults.
  Findings — Prism's annotation layer is chart-type-agnostic (same brackets on bar+SEM / box /
  violin / column-scatter), so ONE renderer covers all four. Locked defaults for cecelia's marks
  builder:

  1. **Star ladder** (GP-style, default): `ns` / `*` / `**` / `***` / `****` at
     p > 0.05 / ≤ 0.05 / ≤ 0.01 / ≤ 0.001 / ≤ 0.0001. NEJM/APA styles cap at three stars —
     expose as a future toggle if asked; default GP.
  2. **Bracket geometry**: squared bracket (horizontal line with short vertical drops), **1 pt
     black** stroke, anchored on each group's **X-centre** (bar centre / box centre / violin
     centre / column centre — NOT whisker tip).
  3. **Placement**: first bracket sits ~5 % of plot height above the tallest data element
     (bar+error, whisker top, violin extreme, or point) in the compared pair. Stacked brackets
     use a vertical gap of ~4 % of plot height (≈ 1.5 × the annotation text height).

     **The measure-axis domain must reserve that band.** The annotations are placed in DATA
     coordinates, but the domain was derived from the data alone (+5 % headroom) — so the topmost
     annotation landed on the frame and its pixel offset (`dx: 8` rotated, `dy: -6` upright) pushed the
     glyph outside the plot, where it was clipped. Compact letters showed it worst: one row at exactly
     the 5 % headroom, i.e. exactly on the edge. `statsBandFraction` (in `plots/plot.ts`, unit-tested)
     reports how far the annotations reach — one row for a CLD, one per shown pair for a bracket stack —
     and `buildPlotOptions` reserves it in BOTH the domain and a pixel margin (`marginRight` when
     rotated, `marginTop` otherwise). Over-reserve rather than under-reserve: spare headroom is
     cosmetic, a clipped annotation is a wrong figure.
  4. **Text**: sans-serif **bold ~14 pt**, black, centred above the bracket on a single line.
     **Numeric-p default** (`p = 0.003`) — GP style, three-significant-figure format;
     `p < 0.001` when the value rounds below that. Journals want p-values; stars without a legend
     are confusing. Asterisks (`**`) are opt-in via the stats config (`useStars=true`). Prism
     itself doesn't ship the `p = 0.003 **` combo — either numeric OR stars, not both.
  5. **`ns` visibility**: shown by default. Provide a "hide non-significant" toggle in the stats
     config with a p-cutoff field (`hideNsAbove: 0.05` by convention).
  6. **Multi-group layout**: stack brackets vertically — no sloped / tree / nested lines. No
     auto-optimiser: order pairs by ascending span (closest pairs first, widest at the top). For
     "many groups" (n ≥ 5) the clutter gets brutal — Prism's own recommendation is Compact Letter
     Display (letters `a`/`b`/`ab` above each group). CLD is **deferred** to a later phase
     (STATS_ANNOTATIONS_PLAN doesn't include CLD in v1).
  7. **Test-name annotation on the chart**: **NO**. Prism omits it and pushes the test identity
     into the figure legend / methods section. Match that. Method note reaches the user via the
     CSV comment lines (S5) and the `[Cecelia]` lab-log entry (S6), not the plot itself.
     *(Follow-up: with `auto` the test is resolved server-side from the group count, so the
     picker also can't show it. `SummaryPanel` reports the result's `methodNote` up via a
     `stats-note` emit and the shared `PlotOptions` → Stats block echoes it under the Test select
     for the ACTIVE plot — a control-panel readout, still nothing on the chart.)*
  8. **Colour**: black lines, black text. Do NOT tint by `--cc-*` tokens — Prism's convention is
     black-on-white for the annotation layer regardless of theme. On the dark theme, keep the
     annotation black on the plot's own light background (cecelia plots are light-themed for
     export anyway).

  Everything above is what S3's marks builder implements verbatim.
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
- `docs/ARCHITECTURE.md` → *Adding a Julia dependency to `app/`* — triple-manifest rule.
- `WHATS_NEW_PLAN.md`, `SKETCH_ENGINE_PLAN.md` — downstream consumers of `StatsResult`.
