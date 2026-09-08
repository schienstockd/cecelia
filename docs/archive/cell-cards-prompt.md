> **ARCHIVED — original planning brief.** Superseded by [`docs/todo/CELL_CARDS_PLAN.md`](../todo/CELL_CARDS_PLAN.md). The brief predates the LAYOUT-model analysis canvas (`ANALYSIS_CANVAS_PLAN`, 2026-07-02) and the registry-declared rail (`CANVAS_MANAGER_RAIL_PLAN`, 2026-08-08); its "Card Table = new canvas + new panel type" framing is not the shipped shape. Cards are a registry-declared InteractiveView on the existing Analysis board (`rail: 'clusterPops'`), with a per-card detail floater. See the plan for the reframe.

# Prompt: Cell Cards / Card Table — Architecture & Planning Pass

## Context

Cecelia is a bioimage analysis platform (Julia backend via Cecelia.jl, Vue 3 + Pinia
frontend) undergoing a full rewrite from R/Shiny (codename "pineapple"). The name
"Cecelia" originated from an early idea: poker-card-sized cards drawn by hand for
T cells and dendritic cells, Pokemon-style, showing each cell type's morphology and
behaviour as strengths/weaknesses. We now want to build this as a real feature.

Relevant existing architecture to work from (do not redesign unless necessary):
- Population Manager: manages population types (flow/live/clust/region) over AnnData
- Floating plots: draggable/resizable panel canvas already in the frontend, backed
  by AlgebraOfGraphics.jl + CairoMakie for plot generation
- TrackMeasures.jl: cell track measures (ported from celltrackR)
- Clustering already produces behaviour clusters over tracks
- Vue Flow-based pipeline whiteboard exists as a separate canvas concept — Cell
  Cards should NOT reuse this; it should reuse the floating plots panel/canvas
  system instead

## Feature: Cell Cards

For each behaviour cluster (from track clustering), generate a "card":
- A representative track for that cluster (medoid track — nearest to cluster
  centroid in feature space, not a synthetic average)
- A cropped image sequence/thumbnail of that specific cell, for that track's
  frame range
- A stat block underneath: cluster characteristics (e.g. speed, turning angle,
  displacement, contact time, or whatever measures TrackMeasures.jl already
  produces for that cluster) styled like a trading-card stat sheet

## Board: "Card Table"

- A canvas for arranging generated cards, analogous to the floating plots canvas
- Should reuse the floating plots panel/drag/zoom/persistence infrastructure —
  a card is effectively a new panel type, not a new canvas
- Auto-arrange generated cards in a grid on creation; user can drag/rearrange
  after, same interaction model as floating plots

## Task

Produce an architecture and implementation plan covering:

1. **Data pipeline (Julia side)**
   - How to identify the medoid track per cluster from existing clustering output
   - How to extract/crop the image region for that track's frame range (source:
     OME-Zarr; check what crop/ROI utilities already exist before proposing new ones)
   - What stat fields are already computed per cluster vs. need new aggregation
   - Data contract for a "card" object (fields, image reference format, stats schema)

2. **Frontend (Vue side)**
   - New `CellCard` panel type for the floating plots canvas — reuse vs. extend
     existing panel component; note what needs to change in the shared panel
     abstraction (if anything) to support a non-plot content type
   - Card layout: image thumbnail + stat block, consistent sizing across cards
   - "Card Table" as a named canvas/mode: is this the same canvas as floating
     plots with a filter, or a separate route/view? Recommend one, with tradeoffs
   - Grid auto-arrange behavior on generation; persistence of user rearrangement
     (localStorage, same pattern as view profiles, if applicable)

3. **Integration point**
   - Where "generate cards" is triggered from (e.g. Population Manager, a
     clustering results view) — propose the UX entry point
   - One card per cluster on generation

4. **Open questions to flag, not resolve**
   - Naming: "Card Table" vs. alternatives — pick one and justify briefly, not a
     bikeshed
   - Whether cards need live recompute (if clustering changes) or are static
     snapshots
   - Performance: image cropping/thumbnail generation cost at scale (many clusters)

## Deliverable

A phase-separated implementation plan in the same style as prior Cecelia.jl
planning docs — architecture decisions first, then sequential Claude Code phase
prompts (Phase 0: setup/contracts, Phase 1: backend data pipeline, Phase 2:
frontend panel type, Phase 3: integration/wiring). Flag unknowns explicitly
rather than guessing; check the existing repo/inventory (`INVENTORY.md`,
canonical shared components) before proposing new abstractions.
