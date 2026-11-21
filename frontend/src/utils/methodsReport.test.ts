import { describe, it, expect } from 'vitest'
import { methodsReportUrl, methodsFileBase } from './methodsReport'
import { historyParamsUrl } from './chainFromHistory'

const params = (u: string) => new URL(u, 'http://x').searchParams

describe('methodsReportUrl', () => {
  it('asks for a chain run by its id', () => {
    const q = params(methodsReportUrl('P', { kind: 'chainRun', runId: 'r 1' }))
    expect(q.get('projectUid')).toBe('P')
    expect(q.get('runId')).toBe('r 1')
    expect(q.has('imageUids')).toBe(false)
  })
  it('asks for a set by its uid', () => {
    const q = params(methodsReportUrl('P', { kind: 'set', setUid: 'S' }))
    expect(q.get('setUid')).toBe('S')
    expect(q.has('runId')).toBe(false)
  })
  it('sends a board the same scope its Parameters CSV sends, plus its name', () => {
    const scope = { pops: ['live::B/a, b'], clusterRuns: ['trackclust:movement'] }
    const u = methodsReportUrl('P', { kind: 'board', imageUids: ['a', 'b'], scope, board: 'Fig 1' })
    const q = params(u)
    const csv = params(historyParamsUrl('P', ['a', 'b'], scope))
    for (const k of ['projectUid', 'imageUids', 'pops', 'clusterRuns']) expect(q.get(k)).toBe(csv.get(k))
    expect(JSON.parse(q.get('pops')!)).toEqual(['live::B/a, b'])   // JSON: a pop name may hold a comma
    expect(q.get('board')).toBe('Fig 1')
    expect(u.startsWith('/api/analysis/methods?')).toBe(true)
  })
  it('leaves the board name out when there is none', () => {
    const q = params(methodsReportUrl('P', { kind: 'board', imageUids: ['a'], scope: { pops: [], clusterRuns: [] } }))
    expect(q.has('board')).toBe(false)
  })
})

describe('methodsFileBase', () => {
  it('makes a file-safe name', () => {
    expect(methodsFileBase('Fig 1: T cells')).toBe('Fig_1_T_cells_methods')
    expect(methodsFileBase('  ')).toBe('cecelia_methods')
    expect(methodsFileBase('Board (cropped).')).toBe('Board_cropped_methods')
  })
})
