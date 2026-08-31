import { describe, it, expect } from 'vitest'
import { notebooksState, previewState, stateInfo, formatUptime } from './serviceStatus'

describe('notebooksState', () => {
  it('maps the /api/notebooks/status payload', () => {
    expect(notebooksState({ running: true })).toBe('running')
    expect(notebooksState({ running: false, starting: true })).toBe('starting')
    expect(notebooksState({ running: false, starting: false })).toBe('stopped')
    expect(notebooksState(null)).toBe('stopped')
  })
})

describe('formatUptime', () => {
  it('formats seconds / minutes / hours', () => {
    expect(formatUptime(45)).toBe('45s')
    expect(formatUptime(59)).toBe('59s')
    expect(formatUptime(60)).toBe('1m')
    expect(formatUptime(12 * 60)).toBe('12m')
    expect(formatUptime(3 * 3600 + 4 * 60)).toBe('3h 4m')
  })
  it('returns — for missing/invalid', () => {
    expect(formatUptime(null)).toBe('—')
    expect(formatUptime(undefined)).toBe('—')
    expect(formatUptime(-5)).toBe('—')
    expect(formatUptime(NaN)).toBe('—')
  })
})

describe('stateInfo', () => {
  it('gives a label + tone per state', () => {
    expect(stateInfo('running')).toEqual({ label: 'Running', tone: 'ok' })
    expect(stateInfo('starting')).toEqual({ label: 'Starting…', tone: 'warn' })
    expect(stateInfo('stopped')).toEqual({ label: 'Stopped', tone: 'idle' })
  })
})

describe('previewState', () => {
  it('reduces the preview worker payload', () => {
    // /api/preview/status returns {alive, starting}. Kept as a shared reducer (aliveState) so a
    // future service with the same payload shape can reuse it without a second copy.
    expect(previewState({ alive: true, starting: false })).toBe('running')
    expect(previewState({ alive: false, starting: true })).toBe('starting')
    expect(previewState({ alive: false, starting: false })).toBe('stopped')
    expect(previewState(null)).toBe('stopped')      // backend unreachable → stopped, never "running"
    expect(previewState(undefined)).toBe('stopped')
  })
})
