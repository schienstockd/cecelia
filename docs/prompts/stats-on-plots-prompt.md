# Prompt 2: Statistical Annotations on Plots

Sonnet execution prompt. Read `INVENTORY.md`, `docs/UI.md`, `docs/PLOTS.md`, `docs/POPULATION.md`, and `CLAUDE.md` before writing any code. Check what already exists before building anything.

---

## What this builds

Statistical test results (t-test, Mann-Whitney U, one-way ANOVA) computed server-side in Julia and displayed as annotations on existing Observable Plot summary charts. Results are also available as structured data — designed to be consumed by the sketch animation engine (Prompt 3) and the What's New card system (Prompt 1).

---

## Step 1 — Julia stats engine

In `Cecelia.jl`, using `HypothesisTests.jl`:

```julia
# Supported tests
run_stats(data::Dict{String,Vector{Float64}}; test::Symbol) -> StatsResult

struct StatsResult
  test::Symbol              # :ttest, :mannwhitney, :anova, :kruskal
  groups::Vector{String}    # group labels
  n::Vector{Int}            # n per group
  means::Vector{Float64}    # mean per group
  medians::Vector{Float64}  # median per group
  p_value::Float64
  statistic::Float64
  significance::String      # "ns", "*", "**", "***", "****"
  comparison_pairs::Vector{Tuple{String,String,Float64}}  # for multi-group: pairwise p-values
  method_note::String       # "Welch's t-test", "Mann-Whitney U", etc.
end
```

Test selection logic:
- 2 groups → Mann-Whitney U (non-parametric default, appropriate for small n microscopy data)
- 2 groups, user requests parametric → Welch's t-test
- >2 groups → Kruskal-Wallis (non-parametric default)
- >2 groups, user requests parametric → one-way ANOVA

Pairwise post-hoc for >2 groups: Dunn's test (non-parametric) or Tukey HSD (parametric), Bonferroni-corrected p-values.

**No linear models for now.** LM/mixed models are a separate phase. Keep scope narrow.

---

## Step 2 — API endpoint

`POST /api/stats/compare`
```json
{
  "projectUid": "...",
  "imageUids": ["uid1", "uid2"],
  "measure": "live.cell.speed",
  "groupBy": "attr:treatment",   // or "pop:cd4+" or "image"
  "popType": "live",
  "pops": ["tcells/tracked"],
  "test": "auto"                 // or "ttest", "mannwhitney", "anova", "kruskal"
}
```

Returns `StatsResult` as JSON. Computation is fast (< 1s for typical n) — synchronous, no task queue needed.

---

## Step 3 — Population manager Stats panel

The population manager already has rendering options. Add a "Stats" section (collapsible, after the existing options):

- **Compare groups by**: dropdown — `None`, `Image`, `Attribute: [attr name]`, `Population`
- **Test**: `Auto (recommended)`, `Mann-Whitney U`, `Welch's t-test`, `Kruskal-Wallis`, `ANOVA`
- **Show on plot**: toggle — when on, fetches stats and passes result to the active plot

The population manager provides `statsConfig` (the test parameters) to any connected plot via the existing plot context. Check how the population manager currently passes data to plots before designing this — do not create a new data flow.

---

## Step 4 — Observable Plot annotations

When a `StatsResult` is available, overlay significance annotations on the existing bar/box/violin plots in `PlotChart.vue`:

For 2 groups:
```
Group A    Group B
  │           │
  └─────┬─────┘
        p = 0.003 **
```

For >2 groups: bracket pairs for significant comparisons only (suppress `ns` pairs to avoid clutter). Use `significance` string ("*", "**", "***", "****", "ns").

Implementation: annotations are SVG elements overlaid on the Observable Plot SVG. `plots/statsAnnotations.ts` — pure function: `annotate(plotNode: SVGElement, result: StatsResult, opts) → void`. Called in `PlotChart.vue` after `Plot.plot()` renders.

The annotation style:
- Thin lines, `--cc-text-dim` colour
- p-value text: `cc-fs-2xs cc-muted`
- Significance stars: `cc-fs-xs` accent colour
- No brackets for `ns` pairs (clutter) unless user toggles "show all"

**Weave with Prompt 1 (What's New cards):** `StatsResult` is the `statsAnnotation` field type in `WhatNewCard`. When a tip card shows a plot with stats, the annotation data comes from here.

**Weave with Prompt 3 (sketch engine):** `StatsResult` is the input data for sketch annotations — "cells A have significantly different behaviour" becomes a sketch bracket + stars drawn by Rough.js. The `StatsResult` JSON format is the contract between this prompt and Prompt 3. Do not change it after Prompt 3 is built.

---

## Step 5 — CSV export of stats

Add stats results to the existing plot CSV export:

```csv
# Stats: Mann-Whitney U, p = 0.003 **
# Groups: WT (n=8, mean=4.2), KO (n=7, mean=6.1)
label,value,group,...
```

Comment lines at the top of the CSV, so it's importable by Prism without modification.

---

## Verify

- Open a summary plot with 2 treatment groups → population manager Stats section appears
- Enable "Show on plot" → brackets + p-value appear on the plot
- >2 groups → only significant pairs shown with brackets
- CSV export includes stats comment lines
- Stats result logged to lab log as `[Cecelia]` entry when run: "Stats: Mann-Whitney U on live.cell.speed (WT vs KO): p = 0.003 **"
- `StatsResult` JSON structure matches the `statsAnnotation` field type in `WhatNewCard` (Prompt 1)

---

## Out of scope

- Linear models, mixed effects models — later
- Multiple comparison methods beyond Bonferroni/Dunn/Tukey — later
- Automatic test selection based on normality testing — later (assume non-parametric by default)
