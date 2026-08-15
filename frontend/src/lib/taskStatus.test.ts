import { describe, it, expect } from 'vitest'
import { TASK_STATUS, rollupTaskStatus, type RollupTask } from './taskStatus'
import type { TaskStatus } from '../stores/tasks'

const ALL: TaskStatus[] = ['queued', 'running', 'done', 'failed', 'cancelled']
const t = (status: TaskStatus, finishedAt?: string, seq?: number): RollupTask =>
  ({ status, ...(finishedAt ? { finishedAt: new Date(finishedAt) } : {}), ...(seq ? { seq } : {}) })

describe('task status model', () => {
  it('every state ships an icon + label (colour never alone)', () => {
    const icons = new Set<string>()
    for (const k of ALL) {
      expect(TASK_STATUS[k].icon).toMatch(/pi-/)
      expect(TASK_STATUS[k].label.length).toBeGreaterThan(0)
      icons.add(TASK_STATUS[k].icon)
    }
    expect(icons.size).toBe(ALL.length)   // distinct shapes, not one shape × five hues
  })
})

describe('rollupTaskStatus', () => {
  it('is null with nothing to roll up', () => {
    expect(rollupTaskStatus([])).toBeNull()
  })

  it('passes a single task straight through', () => {
    for (const s of ALL) expect(rollupTaskStatus([t(s)])).toBe(s)
  })

  it('live beats terminal, whatever the terminal outcome was', () => {
    expect(rollupTaskStatus([t('done', '2026-08-15T10:00:00Z'), t('running')])).toBe('running')
    expect(rollupTaskStatus([t('failed', '2026-08-15T10:00:00Z'), t('queued')])).toBe('queued')
    // …and the order it is handed them must not matter
    expect(rollupTaskStatus([t('queued'), t('failed', '2026-08-15T10:00:00Z')])).toBe('queued')
  })

  it('running beats queued', () => {
    expect(rollupTaskStatus([t('queued'), t('running')])).toBe('running')
    expect(rollupTaskStatus([t('running'), t('queued')])).toBe('running')
  })

  it('among terminal states the most recent run wins', () => {
    const failedFirst = [t('failed', '2026-08-15T10:00:00Z'), t('done', '2026-08-15T11:00:00Z')]
    // a successful re-run clears the badge — a severity order would leave it Failed all session
    expect(rollupTaskStatus(failedFirst)).toBe('done')
    expect(rollupTaskStatus([...failedFirst].reverse())).toBe('done')
    // …and a failure AFTER a success is what the user has to see
    expect(rollupTaskStatus([t('done', '2026-08-15T10:00:00Z'), t('failed', '2026-08-15T11:00:00Z')]))
      .toBe('failed')
  })

  it('falls back to seq, then to severity, when finish times tie', () => {
    const at = '2026-08-15T10:00:00Z'
    expect(rollupTaskStatus([t('done', at, 1), t('failed', at, 2)])).toBe('failed')
    expect(rollupTaskStatus([t('failed', at, 1), t('done', at, 2)])).toBe('done')
    // same instant, same seq (or none): the problem is what gets reported
    expect(rollupTaskStatus([t('done', at), t('failed', at)])).toBe('failed')
    expect(rollupTaskStatus([t('cancelled', at), t('failed', at)])).toBe('failed')
    expect(rollupTaskStatus([t('done', at), t('cancelled', at)])).toBe('cancelled')
  })

  it('a run with no finish time never outranks one that has one', () => {
    expect(rollupTaskStatus([t('done', '2026-08-15T10:00:00Z'), t('failed')])).toBe('done')
  })
})
