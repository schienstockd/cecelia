import { describe, it, expect, beforeEach } from 'vitest'
import { activePlotLoads, beginPlotLoad, endPlotLoad, waitForPlotsIdle } from './plotReady'

// waitForPlotsIdle ends with requestAnimationFrame; node has none, so stub it.
;(globalThis as unknown as { requestAnimationFrame: (cb: (t: number) => void) => void })
  .requestAnimationFrame = (cb) => { setTimeout(() => cb(0), 0) }

beforeEach(() => { activePlotLoads.value = 0 })

describe('plotReady counter', () => {
  it('increments, decrements, and never goes negative', () => {
    beginPlotLoad(); beginPlotLoad()
    expect(activePlotLoads.value).toBe(2)
    endPlotLoad()
    expect(activePlotLoads.value).toBe(1)
    endPlotLoad(); endPlotLoad()          // extra end() is clamped, not negative
    expect(activePlotLoads.value).toBe(0)
  })
})

describe('waitForPlotsIdle', () => {
  it('resolves after the settle window when already idle', async () => {
    await expect(waitForPlotsIdle({ settleMs: 60, timeoutMs: 1000, pollMs: 10 })).resolves.toBeUndefined()
  })

  it('does not resolve while a load is active, then resolves after it ends + settles', async () => {
    beginPlotLoad()
    let done = false
    const p = waitForPlotsIdle({ settleMs: 60, timeoutMs: 3000, pollMs: 10 }).then(() => { done = true })
    await new Promise(r => setTimeout(r, 80))
    expect(done).toBe(false)              // still loading → not resolved
    endPlotLoad()
    await p
    expect(done).toBe(true)
  })

  it('gives up after timeout even if a load never ends', async () => {
    beginPlotLoad()
    await expect(waitForPlotsIdle({ settleMs: 60, timeoutMs: 150, pollMs: 10 })).resolves.toBeUndefined()
    endPlotLoad()
  })
})
