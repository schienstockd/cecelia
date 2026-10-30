import { describe, it, expect } from 'vitest'
import { suggestUrl, conflictOptions, applyChoices, groupSteps, defaultKeep, groupLeftOut, uniqueChainName,
         type HistoryConflict, type HistoryFinal } from './chainFromHistory'
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

const fin = (id: string, fn: string, at: string, multi = false, variant = ''): HistoryFinal =>
  ({ id, fn, at, multi, variant, output: '', imageUids: [], kept: true })

describe('groupSteps + defaultKeep', () => {
  const finals = [fin('r1', 'regions', '2026-07-30T12:36'), fin('r2', 'regions', '2026-07-30T12:48'),
                  fin('t1', 'track', '2026-10-11T15:39', true, '/qc/frags'),
                  fin('t2', 'track', '2026-10-11T15:40', true, '/qc/cells')]
  const groups = groupSteps(finals)
  it('one group per task, newest version first', () => {
    expect(groups.map(g => g.fn)).toEqual(['regions', 'track'])
    expect(groups[0].versions.map(v => v.id)).toEqual(['r2', 'r1'])
  })
  it('keeps the newest of alternatives, and every version of a fork', () => {
    expect(defaultKeep(groups).sort()).toEqual(['r2', 't1', 't2'])
  })
})

describe('groupLeftOut', () => {
  it('folds runs per reason, naming the tasks and counting the images', () => {
    const g = groupLeftOut([
      { fn: 'a', imageUid: 'i1', why: 'gone' }, { fn: 'b', imageUid: 'i1', why: 'gone' },
      { fn: 'a', imageUid: 'i2', why: 'gone' }, { fn: 'c', imageUid: 'i1', why: 'old' }])
    expect(g).toEqual([{ why: 'gone', tasks: ['a', 'b'], images: 2 }, { why: 'old', tasks: ['c'], images: 1 }])
  })
})

describe('uniqueChainName', () => {
  it('never returns a name a chain already has', () => {
    expect(uniqueChainName('From history', [])).toBe('From history')
    expect(uniqueChainName('From history', ['From history', 'From history 2'])).toBe('From history 3')
  })
})
