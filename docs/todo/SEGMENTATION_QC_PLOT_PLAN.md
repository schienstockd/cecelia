# Segmentation Integrity (QC) Plot — module page + whiteboard node

**Status:** planned (2026-07-04). Build target: a segmentation-integrity/consistency QC plot rendered
BOTH on the segment module page and as a whiteboard **plot node**, in one effort.

## Goal

After segmentation + measurement, give the user a fast "did this segment sensibly?" read — per image
**and per timepoint** — so under/over-segmentation, debris, merges, and temporal instability are
obvious at a glance. Then make the same plot a first-class **whiteboard plot node** so it drops into a
chain after `segment.cellposeMeasure` and refreshes as images clear that stage.

## Inspiration — `~/cc-workspace/coastal`

Coastal (a dropped DL segmentation/tracking optimiser) frames segmentation quality largely as
**temporal consistency**: `WarpConsistencyLoss` warps frame *t+1* onto *t* via optical flow and
penalises embedding drift; it learns temporal metric embeddings. We can't reuse the pixel-embedding
losses, but the principle transfers directly to QC: **segmentation should be stable across
timepoints** — cell count and morphology distributions shouldn't jump frame-to-frame. This is why the
plot is per-timepoint, not just per-image: a time series exposes drops/spikes/drift that a
whole-movie summary hides.

## Metrics (all requested)

Source: the measured `.h5ad` written by `segment.measureLabels` (`measure_utils.py` regionprops).
Per cell we have: `area`, `eccentricity`, `perimeter`, `solidity`, `extent`, `equivalent_diameter`,
`aspect_ratio`, `perimeter_to_area`, and per-channel `mean_intensity_{c}`. Timepoint is in
`obsm/temporal` (column `"t"`, only for timecourse images).

| QC dimension | Metric | Per image | Per timepoint |
|---|---|---|---|
| **Count** | # cells (row count per group) | bar per image | line over `t` (the headline consistency signal) |
| **Size** | `area` distribution | violin/hist per image | median±IQR ribbon over `t` |
| **Shape** | `solidity`, `aspect_ratio`, `eccentricity` distribution | violin per image | median over `t` |
| **Intensity** | `mean_intensity_{c}` per channel | violin per image | median over `t` |

Non-timecourse (static) images: only the per-image column applies (no `t`); the time-series view is
hidden/empty.

## Locked decisions

1. **Reuse the summary-plot framework — do NOT build a bespoke chart.** `plot_data.jl` +
   `/api/plot_data` + `PlotChart.vue` (Observable Plot) already render distributions per
   `(value_name, pop)` series, split `by_image` (uID) and by a generic categorical `group_col`.
   Per-timepoint = **`group_col="t"`** (the temporal column, already surfaced by
   `label_props.temporal_columns` and included in views). This is the same reuse rule that governs
   gating/cluster plots (see `docs/PLOTS.md`, and CLAUDE.md "use existing framework").

2. **Cell count is the one new aggregation.** Count = rows per group, not a per-cell measure the
   summary aggregator understands. Add a `"count"` chart/agg to `_summary_agg` (rows per series /
   per `group_col` bucket) rather than faking a measure. This also feeds the per-timepoint count
   line.

3. **Per-timepoint chart type = ordered line/ribbon over `t`.** The summary framework groups on a
   categorical X; timepoints are discrete ordered integers, so a `line` chart type over the `t`
   buckets (count, or median of a measure) is the addition. Distributions per image reuse existing
   violin/hist unchanged.

4. **Data path is a thin preset over `plot_data`, not a new endpoint** where possible. The QC plot
   selects the segmentation's whole population (`{vn}/`, `pop_type` = segmentation/`flow` root),
   measures = the QC set, `by_image=true`, `group_col="t"` for the time view. A small
   `segmentation_qc_data(img|imgs; value_name)` helper in `app/src/plotting/` assembles the presets
   and calls the existing aggregator, so REPL + API + tests share one path.

5. **Whiteboard plot node reuses the declarative plot + the existing incremental scheduler node.**
   The scheduler already supports `scope="incremental"` (debounced recompute on upstream
   `node:done`) and `set` (final barrier) — that half is built and tested (`docs/SCHEDULER.md`).
   Missing pieces: (a) a real plot **task** (`plots/segmentation_qc`) that returns a declarative
   plot spec, (b) frontend **plot-node rendering** on the whiteboard (the "Plots — coming soon"
   palette section) that mounts `PlotChart` inside a VueFlow node. One plot spec, rendered
   identically on the module page and the node (Step 9 shared-source principle).

6. **Same component both places.** The module-page QC panel and the whiteboard plot node render the
   SAME `PlotChart` from the SAME spec — no second chart implementation (mirrors the image-selection
   / DynamicWidget reuse rule).

## Two DISTINCT plot mechanisms (decided 2026-07-04)

QC plots and user-dragged plots are **different things** — don't merge them into one node type.

### 1. QC plots — automatic, module-declared, always available
- A task declares a default QC plot via an optional JSON field `"qcPlot": "plots.segmentationQc"`
  (a plot id in the plot registry). A task with a `qcPlot` has a canonical "did this go well?" view.
- **Not a chain node.** QC is an overlay on the **Live view**, tied to the processing node that
  produced it — no dragging, no wiring. It "always comes up".
- **Live layout = a QC row.** A band **above/below the image grid** holds one **aggregate** QC
  thumbnail per QC-producing column (cross-image distribution, refreshing as images complete). This
  keeps it uncluttered for many images and reads as "the QC for this stage".
- **Show/hide toggle** on the Live tab (like the existing viewer-reload / auto-refresh toggles).
- **Thumbnail → expand.** Click a QC thumbnail → large overlay; an option there switches to the
  **per-image small-multiples** breakdown. Thumbnail itself is compact — a clean mini-plot (e.g.
  the count-over-t sparkline) or headline numbers (n cells, median area).

### 2. User plots — first-class draggable nodes (separate, later)
- Dragged from the palette **like module task nodes**, wired into the chain, configured with a
  plot + measures. On the Live they appear as **thumbnails** (numbers or a clean simple plot), more
  detail on click — exactly like a task node.
- Per-image (or configurable) — the researcher's own exploratory plots. Reuses the SAME plot
  registry + `PlotChart`; some of the QC plots defined here can be offered as user plots too.
- **Out of scope for the segmentation-QC work** — captured here so the QC design doesn't
  accidentally implement the wrong (merged) model. Build after QC lands.

## Plot registry

Plots are addressed by id (e.g. `plots.segmentationQc`) in a registry paralleling the task registry,
served like `/api/tasks/definitions`. A registry entry declares: id, label, the data kind it needs
(e.g. `measures` from a segmentation), and its measure/param options. `qcPlot` on a task references
an entry. The Live QC row, the module-page panel, and (later) the user-plot picker all read this
registry — one source of truth for "what plots exist and what they need".

## Architecture (by layer)

```
measure .h5ad ──(pop_df + temporal "t")──> plot_data._summary_agg (add "count" + "line")
     │                                             │
     ├── app/src/plotting/segmentation_qc.jl  ─────┤  preset: QC measures, by_image, group_col="t"
     │        (REPL + test entry)                  │
     ├── GET /api/plots/segmentation-qc  ──────────┘  thin API wrapper (or reuse /api/plot_data)
     │
     ├── SegmentModule.vue  → QC panel (PlotChart) below the image table   [module page]
     └── whiteboard plot node: plots/segmentation_qc task (declarative spec)
              + ChainPlotNode.vue (mounts PlotChart in a VueFlow node)      [whiteboard]
              refreshed by the incremental scheduler node on segment node:done
```

## Build phases

**Status (2026-07-04):** P1 ✅ (count agg + `segmentation_qc` preset + tests). P2 ✅ (route
`/api/plots/segmentation-qc` + `SegmentationQcPanel` on the segment module page). P3 ✅ (Live QC row:
`qcPlot` field on segment tasks, `ChainQcNode` thumbnail band above the grid, show/hide toggle,
click-to-expand `SegmentationQcPanel`, incremental refetch as images clear the stage). Also landed:
**Live-view run-history loading** (`/api/chains/runs` + `/api/chains/run`) so past runs load from
disk. User-dragged plot nodes (mechanism #2) remain a separate follow-up.


- **P1 — backend data.** `count` aggregation + `line`-over-`group_col` in `plot_data.jl`;
  `segmentation_qc.jl` preset (measures, `by_image`, `group_col="t"`); tests over the fixture
  `.h5ad` (count per t, area median per t, static-image fallback). Docs: PLOTS.md.
- **P2 — API + module page.** Wrapper route; QC panel on `SegmentModule.vue` reusing `PlotChart` +
  the picker (measure toggle, per-image vs per-timepoint). Docs: MODULES.md, ANALYSIS.md.
- **P3 — Live QC row.** `qcPlot` field on `segment.cellposeMeasure` (+ registry entry); a QC band
  above/below the Live image grid rendering one aggregate QC thumbnail per QC-producing column,
  reusing `PlotChart`; show/hide toggle on the Live tab; click → expand overlay with a per-image
  small-multiples option; refresh as images clear the stage (chain `node:done`). Docs: SCHEDULER.md
  (Live QC row), UI.md, MODULES.md (`qcPlot` field).
- **P4 — verify.** Run 3P end-to-end; confirm the QC row aggregates as segmentation clears, the
  expand shows per-image, and the module-page panel matches; static + timecourse both correct.

> User-dragged plot nodes (mechanism #2) are a **separate follow-up**, not part of P1–P4.

## Open questions (resolve during P1/P3)

- Count line: absolute count vs count normalised to t0 (drift view)? Offer both via a toggle.
- Node rendering: mount live `PlotChart` (interactive) vs a rasterised thumbnail in the node with
  click-to-expand? Lean interactive `PlotChart`, capped size, expand-on-click.
- Reuse `/api/plot_data` directly vs a dedicated `/api/plots/segmentation-qc`? Prefer reuse; add a
  dedicated route only if the preset assembly is too awkward client-side.
