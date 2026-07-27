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

export const TIPS: WhatNewCard[] = [
  {
    id: 'tip-hmm-behaviour',
    kind: 'tip',
    title: 'HMM behaviour states',
    description: 'Track movement gets classified into hidden states (arrested, directed, meandering) via a Gaussian HMM. States land as a categorical measure on each track.',
    steps: [
      'Segment + track cells (Segment → Track).',
      'Run Behaviour → HMM to fit states.',
      'Colour tracks by the new state column in napari to see them.',
    ],
    sketchAnimation: { id: 'hmm' },
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
]

/** Deterministic daily pick — same tip everywhere on the same date. Empty catalogue → null. */
export function pickDailyTip(now: Date = new Date()): WhatNewCard | null {
  if (TIPS.length === 0) return null
  const start = new Date(Date.UTC(now.getUTCFullYear(), 0, 0))
  const dayOfYear = Math.floor((now.getTime() - start.getTime()) / 86_400_000)
  return TIPS[Math.abs(dayOfYear) % TIPS.length]
}

/** Today's date as YYYY-MM-DD for comparing against the persisted `tipsLastShown`. */
export function todayKey(now: Date = new Date()): string {
  return now.toISOString().slice(0, 10)
}
