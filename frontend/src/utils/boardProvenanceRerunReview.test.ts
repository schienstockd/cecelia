// RESULT-REVIEW: result-2a65c9be
// Plots stay live, so after a step re-runs the figure is drawn from the new run. The figure's
// provenance record must name that run, not only the frozen one.
import { it, expect } from 'vitest'
import { snapshotScope, paramsScopeKey, buildProvenanceRecord } from './boardProvenance'

const row = (t: string, at: string) => ({ step: 'Segment', task: 'segment.x', output: 'Bcells', images: '1/1', lastRun: at,
  changedFromDefault: 'seed=8', conflicts: '', note: '', relation: 'made' as const, taskIds: [t] })

it('names the re-run that drew the figure alongside the frozen run', () => {
  const sc = snapshotScope(['imgA'], { pops: ['live::Bcells/qc/cells'], clusterRuns: [] })
  const snap = { takenAt: 'T0', key: paramsScopeKey(sc, ['summary:a']), scope: sc, rows: [row('tA', '2026-10-01T12:00:00')] }
  const rec = buildProvenanceRecord([{ title: 'F', snapshot: snap, scope: sc, server: { ceceliaVersion: 'v',
    project: { uid: 'P', name: 'P' }, images: [{ uid: 'imgA', name: 'A' }], populations: [],
    runs: [row('t9', '2026-11-05T08:00:00')], author: {} } }], 'now')!
  expect(rec.boards[0].runs.map(r => r.taskIds)).toEqual([['tA'], ['t9']])
  expect(rec.boards[0].runs[1].recorded).toBe('now (differs)')
  expect(rec.boards[0].parameters.differs).toBe(true)
})
