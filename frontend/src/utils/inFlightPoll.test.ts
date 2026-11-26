import { describe, it, expect, vi, afterEach } from 'vitest'
import { subscribeInFlightPoll, inFlightPollRunning, inFlightSnapshot } from './inFlightPoll'

afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers() })

describe('inFlightPoll', () => {
  it('runs one interval while anyone is subscribed, and clears the snapshot after the last leaves', async () => {
    vi.useFakeTimers()
    const fetchMock = vi.fn(async () => new Response(JSON.stringify([{ id: 't1', status: 'running' }]), { status: 200 }))
    vi.stubGlobal('fetch', fetchMock)
    const a = subscribeInFlightPoll()
    const b = subscribeInFlightPoll()
    expect(inFlightPollRunning()).toBe(true)
    await vi.advanceTimersByTimeAsync(0)
    expect(inFlightSnapshot.value.map(r => r.id)).toEqual(['t1'])
    a(); a()                                   // a double release must not drop b's subscription
    expect(inFlightPollRunning()).toBe(true)
    b()
    expect(inFlightPollRunning()).toBe(false)
    expect(inFlightSnapshot.value).toEqual([])
  })
})
