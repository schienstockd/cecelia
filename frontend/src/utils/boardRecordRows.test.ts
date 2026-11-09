// RESULT-REVIEW: result-e4879591
// RESULT-REVIEW: result-d08f7be8
// The board record view's steps (`recordRows`): which run a recorded step is, and whether it changed
// since the record's time. A record holds the frozen rows plus `now (differs)` rows for steps that ran
// again before it was made; the figure's plots are live, so the `now` row is the one that was drawn.
import { describe, it, expect } from 'vitest'
import { recordRows, recordRowsCsv, rowParamTree, type ProvenanceRun } from './boardProvenance'
import type { HistoryParamsRow } from './chainFromHistory'

const run = (ids: string[], recorded: string, over: Partial<ProvenanceRun> = {}): ProvenanceRun => ({
  step: 'Track', task: 'tracking.bayesian_track_measures', output: '', relation: 'ran on', lastRun: '2026-11-01T10:00:00',
  taskIds: ids, note: '', recorded, params: 'popsToTrack=/qc/cells', ...over,
})
const today = (ids: string[], over: Partial<HistoryParamsRow> = {}): HistoryParamsRow => ({
  step: 'Track', task: 'tracking.bayesian_track_measures', output: '', images: '1/1', lastRun: '2026-11-08T10:00:00',
  changedFromDefault: '', conflicts: '', note: '', taskIds: ids, ...over,
})

describe('a figure file: steps against today', () => {
  it('a step that re-ran between freezing and export is the newer run, not "re-ran since"', () => {
    // frozen t1 on 11-01, re-ran as t2 on 11-08, exported 11-09: the figure shows t2, nothing re-ran after
    const rows = recordRows([run(['t1'], 'frozen 2026-11-01T10:00:00.000Z'), run(['t2'], 'now (differs)')], [today(['t2'])])
    expect(rows.map(r => r.status)).toEqual(['same'])
    expect(rows).toHaveLength(1)
  })
  it('re-ran after the export, or gone today', () => {
    expect(recordRows([run(['t1'], 'frozen x')], [today(['t9'])]).map(r => r.status)).toEqual(['reran'])
    expect(recordRows([run(['t1'], 'frozen x')], []).map(r => r.status)).toEqual(['absent'])
  })
  it('a step recorded without task ids is not flagged on a guess', () => {
    expect(recordRows([run([], 'rebuilt')], [today(['t9'])]).map(r => r.status)).toEqual(['same'])
  })
})

describe('the board on screen: steps against its frozen parameters', () => {
  it('a step added or re-run since the freeze is flagged, the frozen one it replaced left out', () => {
    const rows = recordRows([
      run(['t1'], 'frozen x'),
      run(['s1'], 'frozen x', { step: 'Segment', task: 'segment.cellpose', output: 'B' }),
      run(['t2'], 'now (differs)'),
      run(['n1'], 'now (differs)', { step: 'Contacts', task: 'spatial.contacts', output: 'c' }),
    ], null)
    expect(rows.map(r => [r.step, r.status])).toEqual([['Segment', 'same'], ['Track', 'changed'], ['Contacts', 'changed']])
  })
  it('a board with nothing re-run since shows nothing flagged', () => {
    expect(recordRows([run(['t1'], 'frozen x')], null).map(r => r.status)).toEqual(['same'])
    expect(recordRows([run(['t1'], 'rebuilt')], null).map(r => r.status)).toEqual(['same'])
  })
})

describe('the steps as CSV', () => {
  it('one row per setting, one header, a board column; a step with none keeps one row', () => {
    const rows = recordRows([run(['t1'], 'frozen x', { params: 'popsToTrack=/qc/cells; tiling={"overlap":0}' }),
                             run(['s1'], 'frozen x', { step: 'Segment', task: 'segment.cellpose', output: 'B', params: '' })],
                            [today(['t9']), today(['s1'], { step: 'Segment', task: 'segment.cellpose', output: 'B' })])
    const lines = recordRowsCsv([{ title: 'A', rows }, { title: 'B', rows: rows.slice(1) }], s => (s === 'reran' ? 're-ran' : ''))
      .trim().split(/\r?\n/)
    expect(lines[0]).toBe('board,step,task,output,parameter,value,last_run,since')
    expect(lines.filter(l => l.startsWith('board,'))).toHaveLength(1)
    expect(lines.slice(1).map(l => l.split(',')).map(c => [c[0], c[4], c[5]]))
      .toEqual([['1 · A', 'popsToTrack', '/qc/cells'], ['1 · A', 'tiling.overlap', '0'], ['1 · A', '', ''], ['2 · B', '', '']])
    expect(lines[1]).toContain('re-ran')
  })
})

describe('settings that differ between images', () => {
  it('are listed with their spread, never read as defaults', () => {
    const r = run(['s1'], 'frozen x', { params: '', differs: 'cellprobThreshold: 0.5 (1) | -1 (1); diameter: 12 (1) | 20 (1)' })
    const [row] = recordRows([r], null)
    expect(rowParamTree(row)).toEqual([
      { key: 'cellprobThreshold', value: '0.5 (1) | -1 (1) — differs between images' },
      { key: 'diameter', value: '12 (1) | 20 (1) — differs between images' }])
    const csv = recordRowsCsv([{ title: 'A', rows: [row] }], () => '')
    expect(csv).toContain('cellprobThreshold')
    expect(csv).toContain('differs between images')
  })
  it('share a key path with the settings that agree, and survive an unbalanced bracket in a value', () => {
    const [row] = recordRows([run(['s1'], 'frozen x', {
      params: 'models[0].seedSize=8', differs: 'popsToTrack: /qc/[a (1) | /qc/b (1); models[0].diameter: 12 (1) | 20 (1)' })], null)
    expect(rowParamTree(row)).toEqual([
      { key: 'models', children: [{ key: '0', children: [
        { key: 'seedSize', value: '8' }, { key: 'diameter', value: '12 (1) | 20 (1) — differs between images' }] }] },
      { key: 'popsToTrack', value: '/qc/[a (1) | /qc/b (1) — differs between images' }])
  })
})
