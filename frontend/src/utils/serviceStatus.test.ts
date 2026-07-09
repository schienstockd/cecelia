import { describe, it, expect } from 'vitest'
import { napariState, notebooksState, stateInfo } from './serviceStatus'

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

describe('stateInfo', () => {
  it('gives a label + tone per state', () => {
    expect(stateInfo('running')).toEqual({ label: 'Running', tone: 'ok' })
    expect(stateInfo('starting')).toEqual({ label: 'Starting…', tone: 'warn' })
    expect(stateInfo('stopped')).toEqual({ label: 'Stopped', tone: 'idle' })
  })
})
