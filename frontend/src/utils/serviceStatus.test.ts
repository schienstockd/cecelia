import { describe, it, expect } from 'vitest'
import { napariState, notebooksState, stateInfo, formatUptime } from './serviceStatus'

describe('napariState', () => {
  it('maps the /api/napari/status payload', () => {
    expect(napariState({ alive: true, starting: false })).toBe('running')
    expect(napariState({ alive: false, starting: true })).toBe('starting')
    expect(napariState({ alive: false, starting: false })).toBe('stopped')
    // starting wins even if a stale alive flag lingers
    expect(napariState({ alive: true, starting: true })).toBe('starting')
  })
  it('treats a missing/failed payload as stopped', () => {
    expect(napariState(null)).toBe('stopped')
    expect(napariState(undefined)).toBe('stopped')
    expect(napariState({})).toBe('stopped')
  })
})

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
