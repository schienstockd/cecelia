import { describe, it, expect } from 'vitest'
import { isObserverTrigger, OBSERVER_AUTO_FRAME_TYPES } from './observerAuto'

describe('isObserverTrigger', () => {
  it('fires on terminal module-page task outcomes', () => {
    expect(isObserverTrigger({ type: 'task:status', status: 'done' })).toBe(true)
    expect(isObserverTrigger({ type: 'task:status', status: 'failed' })).toBe(true)
  })

  it('ignores non-terminal task states', () => {
    expect(isObserverTrigger({ type: 'task:status', status: 'queued' })).toBe(false)
    expect(isObserverTrigger({ type: 'task:status', status: 'running' })).toBe(false)
    expect(isObserverTrigger({ type: 'task:status', status: 'cancelled' })).toBe(false)
  })

  it('fires on terminal chain nodes (both launch paths trigger)', () => {
    expect(isObserverTrigger({ type: 'chain:node:done' })).toBe(true)
    expect(isObserverTrigger({ type: 'chain:node:failed' })).toBe(true)
    expect(isObserverTrigger({ type: 'chain:node:running' })).toBe(false)
  })

  it('ignores unrelated frames', () => {
    expect(isObserverTrigger({ type: 'task:progress' })).toBe(false)
    expect(isObserverTrigger({ type: 'chain:log' })).toBe(false)
    expect(isObserverTrigger({})).toBe(false)
  })

  it('subscribes to exactly the terminal frame types', () => {
    expect(OBSERVER_AUTO_FRAME_TYPES).toEqual(['task:status', 'chain:node:done', 'chain:node:failed'])
  })
})
