// THE guide catalogue. `GuidesDialog` lists this; `stores/guide.ts` runs it.
//
// Ordered by the pipeline arc, grouped like the sidebar — so the picker reads in the same order as the
// app you are learning. A guide's `group` must be one of GROUP_ORDER or it lands in an unlabelled
// group at the end (the test pins this).
//
// Adding a guide:
//   1. If the page is a ModuleLayout + TaskRunner one, add a `moduleTaskGuide({…})` call in
//      `taskGuides.ts` / `extraGuides.ts` — do NOT hand-write the five standard steps (plan D8).
//   2. Otherwise write a `GuideDef` in its own file here.
//   3. Register it below.
//   4. Anchor ids must exist in the markup as `data-guide` attributes — `guides.test.ts` fails if not.

import type { GuideDef } from './types'
import { importImagesGuide } from './importImages'
import { driftCorrectGuide, segmentGuide, trackCellsGuide } from './taskGuides'
import { gatePopulationsGuide } from './gatePopulations'
import { notebooksGuide } from './notebooks'
import { plotsGuide } from './plots'
import {
  fixMetadataGuide, behaviourStatesGuide, clusterCellsGuide, runChainGuide,
} from './extraGuides'

export const GUIDES: GuideDef[] = [
  // Data — get images in and ready
  importImagesGuide,
  fixMetadataGuide,
  driftCorrectGuide,
  segmentGuide,
  // Populations — define who the cells are
  gatePopulationsGuide,
  trackCellsGuide,
  clusterCellsGuide,
  // Explore — use those populations
  behaviourStatesGuide,
  // Analysis — free-form surfaces
  plotsGuide,
  notebooksGuide,
  // Pipeline — do it at scale
  runChainGuide,
]

// The sidebar's own grouping, so the picker's sections match the nav's.
export const GROUP_ORDER = ['Data', 'Populations', 'Explore', 'Analysis', 'Pipeline'] as const

export function guideById(id: string): GuideDef | undefined {
  return GUIDES.find(g => g.id === id)
}

// Grouped for rendering, groups in GROUP_ORDER and guides in catalogue order within each. Any group
// not in GROUP_ORDER is appended rather than dropped — a mis-typed group should look wrong, not
// vanish.
export function guidesByGroup(): { group: string; guides: GuideDef[] }[] {
  const seen = new Map<string, GuideDef[]>()
  for (const g of GUIDES) {
    const list = seen.get(g.group) ?? []
    list.push(g)
    seen.set(g.group, list)
  }
  const ordered: { group: string; guides: GuideDef[] }[] = []
  for (const group of GROUP_ORDER) {
    const guides = seen.get(group)
    if (guides) { ordered.push({ group, guides }); seen.delete(group) }
  }
  for (const [group, guides] of seen) ordered.push({ group, guides })
  return ordered
}

export type { GuideDef, GuideStep, GuideCtx, Prereq } from './types'
