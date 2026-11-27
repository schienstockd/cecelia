import { describe, expect, it } from 'vitest'
import {
  PICK_SELECTION_PATH, pickBroadcastAction, pickLayerSelections, pickOutlineLabels, treeHasPickSelection,
} from './pickSelection'

const sel = { valueName: 'seg', labels: [3, 5] }

describe('treeHasPickSelection', () => {
  it('finds the transient top-level pick pop, nothing else', () => {
    expect(PICK_SELECTION_PATH).toBe('/Pick selection')
    expect(treeHasPickSelection({ populations: [{ name: 'A' }, { name: 'Pick selection', transient: true }] })).toBe(true)
    expect(treeHasPickSelection({ populations: [{ name: 'Pick selection' }] })).toBe(false)   // a user pop of that name
    expect(treeHasPickSelection({ populations: [] })).toBe(false)
    expect(treeHasPickSelection(null)).toBe(false)
  })
})

describe('pickBroadcastAction', () => {
  it('a tree with the selection re-reads it', () => {
    expect(pickBroadcastAction(null, 'seg', true)).toBe('fetch')
    expect(pickBroadcastAction(sel, 'other', true)).toBe('fetch')
  })
  it('a tree without it clears only a selection on the same segmentation', () => {
    expect(pickBroadcastAction(sel, 'seg', false)).toBe('clear')
    expect(pickBroadcastAction(sel, 'other', false)).toBe('ignore')
    expect(pickBroadcastAction(null, 'seg', false)).toBe('ignore')
  })
})

describe('pickLayerSelections', () => {
  it('the picked segmentation gets the labels, every other layer an empty set', () => {
    const [a, b, c] = pickLayerSelections(sel, ['seg', 'other', 'seg'])
    expect([...a!]).toEqual([3, 5])
    expect(b!.size).toBe(0)
    expect([...c!]).toEqual([3, 5])
  })
  it('no selection, an empty one, or none of its segmentation drawn: nothing dims', () => {
    expect(pickLayerSelections(null, ['seg'])).toEqual([undefined])
    expect(pickLayerSelections({ valueName: 'seg', labels: [] }, ['seg'])).toEqual([undefined])
    expect(pickLayerSelections(sel, ['other', 'third'])).toEqual([undefined, undefined])
  })
})

describe('pickOutlineLabels', () => {
  it('outlines only on the picked segmentation\'s mask', () => {
    expect(pickOutlineLabels(sel, 'seg')).toEqual([3, 5])
    expect(pickOutlineLabels(sel, 'other')).toEqual([])
    expect(pickOutlineLabels(sel, '')).toEqual([])
    expect(pickOutlineLabels(null, 'seg')).toEqual([])
  })
})
