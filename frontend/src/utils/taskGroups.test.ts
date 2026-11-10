import { describe, it, expect } from 'vitest'
import { groupTaskDefs, UNGROUPED } from './taskGroups'
import type { TaskDef } from '../tasks/types'

const def = (label: string, group?: string): TaskDef =>
  ({ fun_name: `m.${label}`, task: label, label, category: 'M', env: ['local'], params: [], group })

describe('groupTaskDefs', () => {
  it('orders groups by workflow, not alphabetically', () => {
    const g = groupTaskDefs([
      def('Correct labels', 'Correct'), def('Snapshot', 'Correction tools'),
      def('Measure labels', 'Measure'), def('Cellpose', 'Segment'),
    ])!
    expect(g.map(x => x.title)).toEqual(['Segment', 'Measure', 'Correct', 'Correction tools'])
  })

  // Each page opens on the first group's first def, so the first group is the recipe's first step
  // (docs/todo/GUI_CLARITY_PLAN.md, Decision 12).
  it('opens Segment on segmentation, not on importing labels', () => {
    const g = groupTaskDefs([
      def('Import labels', 'Import'), def('Cellpose segment + measure', 'Segment'),
      def('Measure labels', 'Measure'), def('Correct labels', 'Correct'),
    ])!
    expect(g.map(x => x.title)).toEqual(['Segment', 'Measure', 'Correct', 'Import'])
    expect(g[0].defs[0].label).toBe('Cellpose segment + measure')
  })

  it('opens Cleanup on drift correction, before autofluorescence', () => {
    const g = groupTaskDefs([
      def('AF correction', 'Autofluorescence'), def('Drift correction', 'Align'), def('Smoothing', 'Denoise'),
    ])!
    expect(g.map(x => x.title)).toEqual(['Align', 'Autofluorescence', 'Denoise'])
  })

  it('still opens Manage images on importing', () => {
    const g = groupTaskDefs([
      def('Export as OME-TIFF', 'Export'), def('Measure signal', 'Manage'), def('Convert to OME-ZARR', 'Import'),
    ])!
    expect(g.map(x => x.title)).toEqual(['Import', 'Manage', 'Export'])
  })

  it('sorts defs by label within a group', () => {
    const g = groupTaskDefs([def('Ridge', 'Segment'), def('Cellpose', 'Segment'), def('M', 'Measure')])!
    expect(g[0].defs.map(d => d.label)).toEqual(['Cellpose', 'Ridge'])
  })

  it('returns null when there are fewer than two groups', () => {
    expect(groupTaskDefs([def('A', 'Segment'), def('B', 'Segment')])).toBeNull()
    expect(groupTaskDefs([def('A'), def('B')])).toBeNull()
    expect(groupTaskDefs([])).toBeNull()
  })

  it('puts unknown groups after known ones and ungrouped defs last', () => {
    const g = groupTaskDefs([def('P'), def('Z', 'Zeta'), def('Y', 'Alpha'), def('S', 'Segment')])!
    expect(g.map(x => x.title)).toEqual(['Segment', 'Alpha', 'Zeta', UNGROUPED])
  })
})

