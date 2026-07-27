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

// Tip order roughly follows the pipeline arc — segmentation is the entry point; the analysis
// features (gating / clustering / behaviour) read from it. Add new tips in the stage they belong.
export const TIPS: WhatNewCard[] = [
  {
    id: 'tip-segmentation',
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
    kind: 'tip',
    title: 'Cell tracking',
    description: 'btrack links segmentation labels across frames into tracks — each cell gets a track_id and its position at every timepoint. Speed / angle measures pop out for free.',
    steps: [
      'Segment every frame first.',
      'Run Track → btrack; tune the config if links look wrong.',
      'Open in napari — tracks show as trailing polylines.',
    ],
    sketchAnimation: { id: 'tracking' },
  },
  {
    id: 'tip-hmm-behaviour',
    kind: 'tip',
    title: 'HMM behaviour states',
    description: 'Track movement gets classified into hidden states (arrested, directed, meandering) via a Gaussian HMM. States land as a categorical measure on each track.',
    steps: [
      'Segment + track cells first.',
      'Run Behaviour → HMM to fit states.',
      'Colour tracks by the new state column in napari.',
    ],
    sketchAnimation: { id: 'hmm' },
  },
  {
    id: 'tip-gate-then-napari',
    kind: 'tip',
    title: 'Gate a population, view it in napari',
    description: 'Populations from the Gate module can be shown as coloured cell centroids in napari — great for cross-referencing gating decisions against the raw image.',
    steps: [
      'Draw a polygon gate in the Gate module.',
      'Open the image in napari.',
      'Toggle the population\'s "Show" in the Viewer panel.',
    ],
    sketchAnimation: { id: 'gating' },
  },
  {
    id: 'tip-population-hierarchy',
    kind: 'tip',
    title: 'Populations form a tree',
    description: 'A gate is applied INSIDE its parent population — so populations form a tree (root/CD4+/effector). Any child pop can itself be gated further; the address is the "/-separated" path.',
    steps: [
      'Pick the parent population in the Population Manager.',
      'Draw the next gate — it operates on that parent only.',
      'The new pop appears indented beneath its parent.',
    ],
    sketchAnimation: { id: 'hierarchy' },
  },
  {
    id: 'tip-cluster-to-pop',
    kind: 'tip',
    title: 'Cluster labels → populations',
    description: 'Leiden clusters aren\'t populations by default — but you can promote any cluster (or a set of them) into a named population that behaves like any other.',
    steps: [
      'Run Cluster cells (or Cluster tracks).',
      'Open the Cluster panel, pick clusters that share a phenotype.',
      'Save as population — the new pop appears in the manager.',
    ],
    sketchAnimation: { id: 'clusters' },
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
    description: 'Turn on Stats in the plot options — the server picks the right between-group test (Mann-Whitney / Kruskal-Wallis by default), Bonferroni-adjusts pairwise, and draws brackets + stars on the chart. Compact letters for many groups.',
    steps: [
      'Open the Population Manager\'s Stats panel.',
      'Enable "Compare groups"; pick a test or leave auto.',
      'Brackets + stars land on the summary plot in place.',
    ],
    sketchAnimation: { id: 'stats' },
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
