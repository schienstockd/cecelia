import { describe, it, expect } from 'vitest'
import { suggestUrl, conflictOptions, applyChoices, type HistoryConflict } from './chainFromHistory'
import type { ChainTemplate } from '../tasks/types'

describe('suggestUrl', () => {
  it('lists the exemplars and omits keep when every output is wanted', () => {
    const u = new URL(suggestUrl('P', ['a', 'b']), 'http://x')
    expect(u.searchParams.get('imageUids')).toBe('a,b')
    expect(u.searchParams.has('keep')).toBe(false)
  })
  it('sends an explicit keep (even empty) and retrain', () => {
    const u = new URL(suggestUrl('P', ['a'], { keep: [], retrain: true }), 'http://x')
    expect(u.searchParams.get('keep')).toBe('')
    expect(u.searchParams.get('retrain')).toBe('1')
  })
})

const conflict: HistoryConflict = { nodeId: 'trk', param: 'missRate', values: { a: 0.1, b: 0.2, c: 0.1 } }

describe('conflictOptions', () => {
  it('one option per distinct value, naming the exemplars that used it', () => {
    const o = conflictOptions(conflict, u => `img-${u}`)
    expect(o.map(x => x.value)).toEqual([0.1, 0.2])
    expect(o[0].label).toBe('0.1 — img-a, img-c')
  })
})

describe('applyChoices', () => {
  const tpl = {
    name: 't', edges: [],
    nodes: [{ id: 'trk', fn: 'f', scope: 'image', params: { missRate: 0.2, other: 1 } },
            { id: 'seg', fn: 'g', scope: 'image', params: { x: 1 } }],
  } as unknown as ChainTemplate
  it('writes a picked value into its node and leaves the rest alone', () => {
    const key = conflictOptions(conflict)[0].key
    const out = applyChoices(tpl, { trk: { missRate: key } })
    expect(out.nodes[0].params).toEqual({ missRate: 0.1, other: 1 })
    expect(out.nodes[1]).toBe(tpl.nodes[1])
    expect(tpl.nodes[0].params).toEqual({ missRate: 0.2, other: 1 })   // input untouched
  })
})
