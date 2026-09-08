import { describe, it, expect } from 'vitest'
import {
  selectionKind,
  labelSelectionSummary,
  trackSelectionSummary,
  labelChipOptions,
  trackChipOptions,
  parseLabelChipValues,
} from './correctionSelections'

describe('selectionKind', () => {
  it('routes tracks → tracks; labels + review → labels', () => {
    expect(selectionKind('tracks')).toBe('tracks')
    expect(selectionKind('labels')).toBe('labels')
    // Review shares the /Pick selection with Labels — this was the F1/screenshot bug:
    // the summary line fell through to the tracks branch in Review mode.
    expect(selectionKind('review')).toBe('labels')
  })
})

describe('labelSelectionSummary', () => {
  it('reports "viewer not ready" when t is null even if picks exist', () => {
    expect(labelSelectionSummary([1, 2], null)).toBe('Viewer not ready')
  })

  it('names the frame when nothing is picked', () => {
    expect(labelSelectionSummary([], 12)).toBe('No label picked (frame 12)')
  })

  it('spells out up to four label ids, collapses beyond that', () => {
    expect(labelSelectionSummary([9], 3)).toBe('Label 9 @ frame 3')
    expect(labelSelectionSummary([1, 2, 3, 4], 0)).toBe('Labels 1, 2, 3, 4 @ frame 0')
    expect(labelSelectionSummary([1, 2, 3, 4, 5], 0)).toBe('5 labels picked @ frame 0')
  })
})

describe('trackSelectionSummary', () => {
  it('reports the empty state', () => {
    expect(trackSelectionSummary([], null)).toBe('No track selected')
  })

  it('shows the split frame only when one track is selected', () => {
    expect(trackSelectionSummary(['42'], 7)).toBe('Track 42 @ frame 7')
    expect(trackSelectionSummary(['42'], null)).toBe('Track 42')
    // Two-track branch — a Join needs two ids and no split frame; keep the split-frame text out.
    expect(trackSelectionSummary(['1', '2'], 5)).toBe('Tracks 1 + 2')
  })

  it('collapses beyond two', () => {
    expect(trackSelectionSummary(['1', '2', '3'], null)).toBe('3 tracks selected')
  })
})

describe('labelChipOptions', () => {
  it('builds one option per id with a click-to-drop tooltip', () => {
    const opts = labelChipOptions([10, 20])
    expect(opts.length).toBe(2)
    expect(opts[0].value).toBe('10')
    expect(opts[0].label).toBe('10')
    expect(opts[0].tip).toMatch(/click to drop from pick/)
    // no accent when there's no focus target
    expect(opts[0].accent).toBeUndefined()
  })

  it('highlights the focused id via accent so Review\'s cursor is visible', () => {
    const opts = labelChipOptions([10, 20, 30], 20)
    expect(opts[1].accent).toBe('var(--cc-accent)')
    expect(opts[1].tip).toMatch(/focused/)
    expect(opts[0].accent).toBeUndefined()
    expect(opts[2].accent).toBeUndefined()
  })
})

describe('trackChipOptions', () => {
  it('emits one option per track with a drop tooltip', () => {
    const opts = trackChipOptions(['a', 'b'])
    expect(opts.map(o => o.value)).toEqual(['a', 'b'])
    expect(opts[0].tip).toMatch(/click to drop from pick/)
  })
})

describe('parseLabelChipValues', () => {
  it('drops non-numeric, zero, and negative entries', () => {
    expect(parseLabelChipValues(['1', '2', 'x', '0', '-3', '10'])).toEqual([1, 2, 10])
  })

  it('floors decimals defensively', () => {
    expect(parseLabelChipValues(['3.7'])).toEqual([3])
  })
})
