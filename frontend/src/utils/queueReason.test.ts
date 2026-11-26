import { describe, it, expect } from 'vitest'
import { queueReason, type QueueReasonContext } from './queueReason'
import type { InFlightTaskRow } from './runningTasks'
import { DEFAULT_PROFILE_DISPLAY_NAME } from './profileApi'

// Rows shaped like `list_tasks()` (app/src/tasks/scheduler/reporting.jl) emits them.
const NOW = Date.parse('2026-11-26T03:12:00Z')

function row(over: Partial<InFlightTaskRow>): InFlightTaskRow {
  return {
    id: 'x', fun_name: 'segment.cellpose', pool_name: 'gpu', image_uid: 'imgA', image_uids: ['imgA'],
    project_uid: 'P1', chain_run_id: '', chain_node_id: '', by: 'default', status: 'running',
    queued_at: '2026-11-26T03:00:00Z', started_at: '2026-11-26T03:00:00Z',
    hpc_stage: '', hpc_job: '',
    ...over,
  }
}

const ctx = (over: Partial<QueueReasonContext> = {}): QueueReasonContext => ({
  selfProfile: 'alice',
  currentProjectUid: 'P1',
  imageName: uid => ({ imgA: 'img_01', imgB: 'img_02' })[uid],
  projectName: uid => ({ P2: 'Lymph node' })[uid],
  labelFor: f => ({ 'segment.cellpose': 'Cellpose' })[f] ?? '',
  now: NOW,
  ...over,
})

const mine = { id: 'me', status: 'queued' as const, pool: 'gpu' }
const selfRow = row({ id: 'me', status: 'queued', by: 'alice', image_uid: 'imgB',
                      queued_at: '2026-11-26T03:10:00Z', started_at: '' })

describe('queueReason', () => {
  it('names another profile’s run in another project that holds the pool', () => {
    const r = queueReason(mine, [row({ id: 'other', project_uid: 'P2', by: 'bob' }), selfRow], ctx())
    expect(r?.short).toBe('gpu busy: Cellpose (bob)')
    expect(r?.tip).toBe('Waiting for a free gpu slot. Running: Cellpose on Lymph node, bob, 12m 00s.')
  })

  it('calls this profile’s own run "you", and names the image when it is in the open project', () => {
    const r = queueReason(mine, [row({ id: 'other', by: 'alice' }), selfRow], ctx())
    expect(r?.short).toBe('gpu busy: Cellpose (you)')
    expect(r?.tip).toContain('Cellpose on img_01, you')
  })

  it('names the default profile by its display name, as everywhere else', () => {
    expect(queueReason(mine, [row({ id: 'other', by: 'default' }), selfRow], ctx())?.short)
      .toBe(`gpu busy: Cellpose (${DEFAULT_PROFILE_DISPLAY_NAME})`)
  })

  it('summarises several running and counts those queued ahead', () => {
    const r = queueReason(mine, [
      row({ id: 'a', by: 'bob' }), row({ id: 'b', by: 'alice' }),
      row({ id: 'c', status: 'queued', started_at: '', queued_at: '2026-11-26T03:05:00Z' }),
      row({ id: 'd', status: 'queued', started_at: '', queued_at: '2026-11-26T03:11:00Z' }), // behind
      selfRow,
    ], ctx())
    expect(r?.short).toBe('gpu busy: 2 running (bob, you) · 1 ahead')
  })

  it('ignores other pools', () => {
    expect(queueReason(mine, [row({ id: 'o', pool_name: 'cpu' }), selfRow], ctx())).toBeNull()
  })

  it('takes the pool from the snapshot when the row has none', () => {
    const r = queueReason({ id: 'me', status: 'queued' }, [row({ id: 'o', by: 'bob' }), selfRow], ctx())
    expect(r?.short).toBe('gpu busy: Cellpose (bob)')
  })

  it('matches a chain row by its scheduler id', () => {
    const r = queueReason({ id: 'run::node::imgB', backendTaskId: 'me', status: 'queued' },
      [row({ id: 'o', by: 'bob' }), selfRow], ctx())
    expect(r?.short).toBe('gpu busy: Cellpose (bob)')
  })

  it('says nothing for a running row, a cluster run, or an empty snapshot', () => {
    expect(queueReason({ ...mine, status: 'running' }, [row({ id: 'o' })], ctx())).toBeNull()
    expect(queueReason({ ...mine, hpcStage: 'queued' }, [row({ id: 'o' })], ctx())).toBeNull()
    expect(queueReason(mine, [], ctx())).toBeNull()
  })
})
