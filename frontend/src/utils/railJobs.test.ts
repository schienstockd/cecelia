import { describe, it, expect } from 'vitest'
import { adoptsRailJob, railJobLabel, railJobRow } from './railJobs'

const frame = (over = {}) => ({ taskId: 'hpc-env-install', status: 'running', fun: 'hpc:install-env', pool: 'job',
                                startedAt: new Date('2026-10-27T10:00:00Z'), ...over })

describe('railJobs', () => {
  it('names the backend-started jobs', () => {
    expect(railJobLabel('hpc:install-env')).toBe('Install cluster environment')
    expect(railJobLabel('system:install-env:cellpose-v3')).toBe('Install cellpose-v3 environment')
    expect(railJobLabel('system:fetch-weights:cpsam')).toBe('Download cpsam weights')
    expect(railJobLabel('hpc:cleanup')).toBe('Clean up cluster')
    expect(railJobLabel('hpc:delete-all')).toBe('Delete Cecelia files on cluster')
    expect(railJobLabel('movie:batch')).toBe('movie:batch')
  })

  it('makes a row from a starting job with none, and only from a job', () => {
    expect(adoptsRailJob(frame())).toBe(true)
    expect(adoptsRailJob(frame({ status: 'done' }))).toBe(false)       // a finish alone is not a start
    expect(adoptsRailJob(frame({ pool: 'cpu' }))).toBe(false)          // scheduler tasks have their own path
    expect(adoptsRailJob(frame(), { status: 'running' })).toBe(false)  // its row already shows it
  })

  it('replaces the fixed id\'s finished row only for a run that started after it', () => {
    const old = { status: 'cancelled' as const, finishedAt: new Date('2026-10-27T09:00:00Z') }
    expect(adoptsRailJob(frame(), old)).toBe(true)
    // a late frame of the run that was cancelled must not bring it back
    expect(adoptsRailJob(frame({ startedAt: new Date('2026-10-27T08:00:00Z') }), old)).toBe(false)
    expect(adoptsRailJob(frame({ startedAt: undefined }), old)).toBe(false)
  })

  it('is a projectless row that cannot be re-run', () => {
    const r = railJobRow(frame())
    expect(r).toMatchObject({ id: 'hpc-env-install', label: 'Install cluster environment', projectUid: '',
                              imageUid: '', pool: 'job', status: 'running', paramsUnknown: true })
  })
})
