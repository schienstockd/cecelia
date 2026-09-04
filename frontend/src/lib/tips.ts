// Tip-of-the-day catalogue for the What's New modal (WHATS_NEW_PLAN.md W4).
//
// Static array. `pickDailyTip(now)` returns the tip for today (deterministic mod day-of-year).
// The launch trigger lives in App.vue; it only opens the modal once per day and only if the user
// hasn't opted out. The opt-out lives on the card itself as a checkbox.
//
// Tips should read like a one-paragraph hint — brief description, optional 2-4 steps, no walls of
// text. `sketchAnimation.id` points at a feijoa sketch (rendered inline by WhatNewCard); an id
// that isn't in feijoa's catalogue falls back to the "coming soon" placeholder.
import type { WhatNewCard } from './whatsNew'

// First entry is the welcome card (kind 'about') — brief intro to cecelia; the rest are tips in
// pipeline-arc order (segmentation is the entry point; analysis features read from it), with the
// assist surfaces (Claude/MCP) last.
export const TIPS: WhatNewCard[] = [
  {
    id: 'about-cecelia',
    kind: 'about',
    title: 'Cecelia',
    description: 'Immunological image analysis: segment cells, track them, gate populations, cluster phenotypes, and measure fibrous networks — all from a single project. Cycle through the sketches to see what it does.',
    sketchAnimation: { id: 'logo' },
    // The welcome card gets the orientation tour, so "Show me" on the first card a new user sees
    // leads somewhere. Every other card's guide teaches a pipeline step and needs data; this one
    // points at chrome only and runs on an empty project. App.vue also starts it unprompted the
    // first time this dialog is closed — the button is for everyone who skipped that.
    guideId: 'find-your-way-around',
  },
  {
    id: 'tip-segmentation',
    guideId: 'segment-an-image',
    kind: 'tip',
    title: 'Segmentation is the entry point',
    description: 'Cellpose turns fluorescence into per-cell labels — each with an id, centroid, and every regionprops/intensity measure. Everything downstream (tracking, gating, clustering, HMM) reads from the segmentation.',
    steps: [
      'Import an image (Import module).',
      'Run Segment → Cellpose on the right channel.',
      'Check the QC panel — it flags low counts, weird sizes, edge cells.',
    ],
    sketchAnimation: { id: 'segmentation' },
  },
  {
    id: 'tip-tracking',
    guideId: 'track-cells',
    kind: 'tip',
    title: 'Cell tracking',
    description: 'btrack links segmentation labels across frames into tracks — each cell gets a track_id and its position at every timepoint. Speed / angle measures pop out for free.',
    steps: [
      'Segment every frame first.',
      'Run Track → btrack; tune the config if links look wrong.',
      'Open in viewer — tracks show as trailing polylines.',
    ],
    sketchAnimation: { id: 'tracking' },
  },
  {
    id: 'tip-hmm-behaviour',
    guideId: 'behaviour-states',
    kind: 'tip',
    title: 'HMM behaviour states',
    description: 'Track movement gets classified into hidden states (arrested, directed, meandering) via a Gaussian HMM. States land as a categorical measure on each track.',
    steps: [
      'Segment + track cells first.',
      'Run Behaviour → HMM to fit states.',
      'Colour tracks by the new state column in viewer.',
    ],
    sketchAnimation: { id: 'hmm' },
  },
  {
    id: 'tip-gate-then-viewer',
    guideId: 'gate-populations',
    kind: 'tip',
    title: 'Gate a population, view it in viewer',
    description: 'Populations from the Gate module can be shown as coloured cell centroids in viewer — great for cross-referencing gating decisions against the raw image.',
    steps: [
      'Draw a polygon gate in the Gate module.',
      'Open the image in viewer.',
      'Toggle the population\'s "Show" in the Viewer panel.',
    ],
    sketchAnimation: { id: 'gating' },
  },
  {
    id: 'tip-population-hierarchy',
    guideId: 'gate-populations',
    kind: 'tip',
    title: 'Populations form a tree',
    description: 'A gate is applied INSIDE its parent population — so populations form a tree (root/CD4+/effector). Any child pop can itself be gated further; the address is the "/-separated" path.',
    steps: [
      'Select the parent population in the Population Manager.',
      'Draw the next gate — it operates on that parent only.',
      'The new pop appears indented beneath its parent.',
    ],
    sketchAnimation: { id: 'hierarchy' },
  },
  {
    id: 'tip-cluster-to-pop',
    guideId: 'cluster-cells',
    kind: 'tip',
    title: 'Cluster labels → populations',
    description: 'Leiden clusters aren\'t populations by default — but you can promote any cluster (or a set of them) into a named population that behaves like any other.',
    steps: [
      'Run Cluster cells (or Cluster tracks).',
      'Open the Cluster panel, select clusters that share a phenotype.',
      'Save as population — the new pop appears in the manager.',
    ],
    sketchAnimation: { id: 'clusters' },
  },
  {
    id: 'tip-region-clustering',
    kind: 'tip',
    title: 'Region clustering',
    description: 'Cluster cells by their SPATIAL NEIGHBOURHOOD rather than by their own phenotype. Each region shares a local cell-mix; cells keep their own type and gain a region label — great for CytoMAP-style tissue-architecture summaries.',
    steps: [
      'Segment + phenotype your cells (gate or cluster them).',
      'Run Spatial → Region clustering.',
      'Regions appear as a new categorical measure per cell.',
    ],
    sketchAnimation: { id: 'region_clustering' },
  },
  {
    id: 'tip-branching',
    kind: 'tip',
    title: 'Branching networks',
    description: 'Not everything is a cell. Skeletonise a segmentation into a network of branches — measures each branch\'s length, tortuosity, endpoints. Built for SHG collagen, FRC/CCL19 reticular meshes, vessels, nerves.',
    steps: [
      'Segment the fibrous / reticular structure.',
      'Run Segment → Branching on that segmentation.',
      'Plot per-branch measures like any other population.',
    ],
    sketchAnimation: { id: 'branching' },
  },
  {
    id: 'tip-stats-on-plots',
    kind: 'tip',
    title: 'Stats on plots',
    description: 'Turn on Stats in the plot options — the server selects the right between-group test (Mann-Whitney / Kruskal-Wallis by default), Bonferroni-adjusts pairwise, and draws brackets + stars on the chart. Compact letters for many groups.',
    steps: [
      'Open the Population Manager\'s Stats panel.',
      'Enable "Compare groups"; select a test or leave auto.',
      'Brackets + stars land on the summary plot in place.',
    ],
    sketchAnimation: { id: 'stats' },
  },
  {
    id: 'tip-analysis-board',
    guideId: 'build-plots',
    kind: 'tip',
    title: 'Analysis boards',
    description: 'The Analysis board arranges plots into A4 "comic plates" — cross-image summary, gating strategy, cluster heatmaps, image tiles. Publish-ready. Export the whole board as PDF or SVG, individual plots as CSV.',
    steps: [
      'Open Analysis; add plots into a plate.',
      'Drag to lay them out; each plate is one A4 page.',
      'Export the tab as PDF (raster) or SVG (vector).',
    ],
    sketchAnimation: { id: 'analysis_board' },
  },
  {
    id: 'tip-notebooks',
    guideId: 'create-a-notebook',
    kind: 'tip',
    title: 'Notebooks Playground',
    description: 'Every project gets its own Pluto notebook workspace for pure-Julia downstream analysis. Use CeceliaNb helpers to load pop_df / clusters, then DataFrames + Plots or AlgebraOfGraphics — versioned per project.',
    steps: [
      'Open the Notebooks module; create a notebook.',
      'Load populations with CeceliaNb.load_pop_df("root/…").',
      'Snapshot when the analysis is worth pinning.',
    ],
    sketchAnimation: { id: 'notebooks' },
  },
  {
    id: 'tip-tracks-in-viewer',
    kind: 'tip',
    title: 'Track stats + tracks in viewer',
    description: 'Summary stats live in the app (behaviour state proportions, speed distributions); the tracks themselves are best viewed as coloured polylines over the raw image in viewer — the two views work together.',
    steps: [
      'Run Behaviour → HMM on tracked images.',
      'Open the summary plots for the stats.',
      'Open the image in viewer; tracks show as polylines coloured by state.',
      'Add a Tracks plot on the Track page or the Analysis board for a figure of the paths.',
    ],
    sketchAnimation: { id: 'viewer_tracks' },
  },
  {
    id: 'tip-claude-mcp',
    kind: 'tip',
    title: 'Claude Code can read your project',
    description: 'The cecelia-observer MCP server gives a Claude Code session read-only access to this project — images, QC, cohort outliers, lineage, populations, behaviour, clusters. It points out what looks off and names the knob worth trying; you make the change and run it. Ask, and it notes the decision in the lab log or builds you a Pluto notebook.',
    steps: [
      'Lab log → Set up my terminal (once).',
      'Run claude in a terminal.',
      'Ask: "check my current project in cecelia".',
    ],
    sketchAnimation: { id: 'claude_mcp' },
  },
]

/** Deterministic daily index — same tip everywhere on the same date. Returns -1 on empty catalogue. */
export function todayTipIndex(now: Date = new Date()): number {
  if (TIPS.length === 0) return -1
  const start = new Date(Date.UTC(now.getUTCFullYear(), 0, 0))
  const dayOfYear = Math.floor((now.getTime() - start.getTime()) / 86_400_000)
  return Math.abs(dayOfYear) % TIPS.length
}

/** Deterministic daily pick — same tip everywhere on the same date. Empty catalogue → null. */
export function pickDailyTip(now: Date = new Date()): WhatNewCard | null {
  const i = todayTipIndex(now)
  return i < 0 ? null : TIPS[i]
}

/** Today's date as YYYY-MM-DD for comparing against the persisted `tipsLastShown`. */
export function todayKey(now: Date = new Date()): string {
  return now.toISOString().slice(0, 10)
}
