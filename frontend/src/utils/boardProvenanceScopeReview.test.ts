// RESULT-REVIEW: result-fe996103
// A snapshot frozen for image A must not be exported as what a board now showing image B was made
// with: the shared image selection moved, the snapshotter had not re-taken it yet, and A's task id was
// exported under B with `differs: false`.
import { it, expect } from 'vitest'
import { snapshotScope, paramsScopeKey, resolveParamsExport, buildProvenanceRecord } from './boardProvenance'

const row = (t: string, at: string) => ({ step: 'Segment', task: 'segment.x', output: 'Bcells', images: '1/1', lastRun: at,
  changedFromDefault: 'seed=8', conflicts: '', note: '', relation: 'made' as const, taskIds: [t] })

it('does not export a snapshot taken for other images as frozen', () => {
  const scA = snapshotScope(['imgA'], { pops: ['live::Bcells/qc/cells'], clusterRuns: [] })
  const scB = snapshotScope(['imgB'], { pops: ['live::Bcells/qc/cells'], clusterRuns: [] })
  const snap = { takenAt: 'T0', key: paramsScopeKey(scA, ['summary:a']), scope: scA, rows: [row('tA', '2026-10-01T12:00:00')] }
  const nowB = [row('tB', '2026-10-01T12:00:00')]   // same step, same params, same chain-run time
  const out = resolveParamsExport(snap, nowB, scB)
  expect(out.source).toBe('rebuilt')
  expect(out.rows.map(r => r.taskIds)).toEqual([['tB']])
  const rec = buildProvenanceRecord([{ title: 'F', snapshot: snap, scope: scB, server: { ceceliaVersion: 'v',
    project: { uid: 'P', name: 'P' }, images: [{ uid: 'imgB', name: 'B' }], populations: [], runs: nowB, author: {} } }], 'now')!
  expect(rec.boards[0].runs.map(r => r.taskIds)).toEqual([['tB']])
  // and without a scope, the task id still tells the two rows apart
  expect(resolveParamsExport(snap, nowB).differs).toBe(true)
})
