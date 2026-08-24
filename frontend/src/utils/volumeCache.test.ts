import { describe, it, expect } from 'vitest'
import {
  cacheCapacity, prefetchWindow, prefetchDepth, lruEvictions, stripCells, playbackAdvance,
  playbackIntervalMs,
} from './volumeCache'

describe('cacheCapacity', () => {
  it('divides the budget by a timepoint', () => {
    expect(cacheCapacity(1000e6, 100e6)).toBe(10)
    expect(cacheCapacity(2048e6, 351e6)).toBe(5)      // the real target on a 2 GB budget
  })
  it('never drops to one slot', () => {
    // With one slot the outgoing texture is destroyed before the incoming one lands, so every step is
    // both a cold load and a black frame.
    expect(cacheCapacity(100e6, 351e6)).toBe(2)
    expect(cacheCapacity(0, 351e6)).toBe(2)
    expect(cacheCapacity(1000e6, 0)).toBe(2)
    expect(cacheCapacity(1000e6, NaN)).toBe(2)
  })
})

describe('prefetchWindow', () => {
  it('asks for the current timepoint first', () => {
    expect(prefetchWindow(50, 1, 181, 8)[0]).toBe(50)
  })
  it('leans in the direction of travel, keeping two for a reversal', () => {
    const fwd = prefetchWindow(50, 1, 181, 8)
    expect(fwd).toEqual([50, 51, 49, 52, 48, 53, 54, 55])
    const back = prefetchWindow(50, -1, 181, 8)
    expect(back).toEqual([50, 49, 51, 48, 52, 47, 46, 45])
  })
  it('spends a SMALL budget ahead of the playhead, not behind it', () => {
    // The case that matters most: a 351 MB timepoint on a 1 GB budget is capacity 2. Reserving a flat
    // two slots for the reversal inverted it — every slot went behind, so playing forward missed on
    // every frame while the cache held frames already watched.
    expect(prefetchWindow(50, 1, 181, 2)).toEqual([50, 51])
    expect(prefetchWindow(50, 1, 181, 3)).toEqual([50, 51, 52])
    expect(prefetchWindow(50, 1, 181, 4)).toEqual([50, 51, 49, 52])
    expect(prefetchWindow(50, -1, 181, 2)).toEqual([50, 49])
  })
  it('always leans ahead on balance, at every capacity', () => {
    for (const cap of [2, 3, 4, 5, 6, 8, 12, 40]) {
      const w = prefetchWindow(50, 1, 181, cap).slice(1)
      const ahead = w.filter(x => x > 50).length
      const behind = w.filter(x => x < 50).length
      expect(ahead).toBeGreaterThanOrEqual(behind)
      expect(behind).toBeLessThanOrEqual(2)
    }
  })
  it('never asks for more than the cache can hold', () => {
    // Otherwise every arrival evicts something this same window still wants, and it thrashes at the
    // moment it matters most.
    for (const cap of [2, 3, 4, 10, 40]) {
      expect(prefetchWindow(50, 1, 181, cap).length).toBeLessThanOrEqual(cap)
    }
    expect(prefetchWindow(0, 1, 3, 99)).toHaveLength(3)   // …nor more than the movie has
  })
  it('recovers after a loop-wrap, where the direction reads backward', () => {
    // Playback wrapping 180 → 0 computes dir = -1, i.e. backward, at the exact moment it is about to
    // go forward. Everything behind t=0 is out of range, so the outward fill has to end up ahead —
    // otherwise the first second after every loop is all misses.
    expect(prefetchWindow(0, -1, 181, 6)).toEqual([0, 1, 2, 3, 4, 5])
  })
  it('stays in range at both ends, and spends the freed budget outward', () => {
    const start = prefetchWindow(0, -1, 181, 6)
    expect(start.every(t => t >= 0 && t < 181)).toBe(true)
    expect(start).toHaveLength(6)                          // not 4, with two slots wasted below zero
    expect(start[0]).toBe(0)
    const end = prefetchWindow(180, 1, 181, 6)
    expect(end.every(t => t >= 0 && t < 181)).toBe(true)
    expect(end).toHaveLength(6)
  })
  it('has no duplicates', () => {
    const w = prefetchWindow(1, 1, 4, 12)
    expect(new Set(w).size).toBe(w.length)
  })
  it('degenerates safely', () => {
    expect(prefetchWindow(0, 1, 1, 8)).toEqual([0])
    expect(prefetchWindow(0, 1, 0, 8)).toEqual([])
    expect(prefetchWindow(99, 1, 10, 8)).toEqual([])       // out of range → asks for nothing
  })
})

describe('lruEvictions', () => {
  it('drops the least recently used first', () => {
    expect(lruEvictions([1, 2, 3, 4, 5], 3, 5)).toEqual([1, 2])
  })
  it('never evicts the frame on screen, wherever it sits in the order', () => {
    // Dropping it destroys the texture the next draw binds — an intermittent black frame under memory
    // pressure, which is close to unattributable afterwards.
    expect(lruEvictions([1, 2, 3, 4, 5], 3, 1)).toEqual([2, 3])
    expect(lruEvictions([1, 2, 3], 1, 1)).toEqual([2, 3])
  })
  it('evicts nothing when there is room', () => {
    expect(lruEvictions([1, 2], 5, 2)).toEqual([])
    expect(lruEvictions([], 5, 0)).toEqual([])
  })
  it('spares SEVERAL timepoints — the one on screen is not the one being loaded', () => {
    // The crash case. While timepoint 90 loads, 5 is still on screen and its texture is what the bind
    // group reads; sparing only 90 destroyed 5 under the live bind group, which took Firefox down
    // rather than producing a bad frame.
    expect(lruEvictions([5, 6, 7, 8, 90], 3, [90, 5])).toEqual([6, 7])
    // Order of the spare list is irrelevant, and a duplicate or an absent entry is harmless — the
    // caller passes `[keep, boundT]` and those are frequently the same, or -1 before the first paint.
    expect(lruEvictions([5, 6, 7], 1, [7, 7])).toEqual([5, 6])
    expect(lruEvictions([5, 6, 7], 1, [7, -1])).toEqual([5, 6])
  })
})

describe('stripCells', () => {
  const set = (...v: number[]) => new Set(v)
  it('bounds the DOM regardless of how long the movie is', () => {
    expect(stripCells(5000, set(), set(), 0, 120)).toHaveLength(120)
    expect(stripCells(7, set(), set(), 0, 120)).toHaveLength(7)   // never MORE cells than timepoints
  })
  it('covers every timepoint exactly once', () => {
    const cells = stripCells(181, set(), set(), 0, 40)
    expect(cells[0].from).toBe(0)
    expect(cells[cells.length - 1].to).toBe(180)
    for (let i = 1; i < cells.length; i++) expect(cells[i].from).toBe(cells[i - 1].to + 1)
  })
  it('takes the most interesting state in a bucket, not the majority', () => {
    // The strip answers "will scrubbing there be instant" — a half-resident bucket must not read as
    // cached, and a bucket holding the current frame must show it whatever else is in there.
    const cells = stripCells(100, set(10, 11, 12, 13, 14), set(20), 55, 10)
    expect(cells[1].state).toBe('resident')      // 10-19, partly resident
    expect(cells[2].state).toBe('loading')       // 20-29, one in flight beats the rest
    expect(cells[5].state).toBe('current')       // 50-59 holds t=55
    expect(cells[9].state).toBe('absent')
  })
  it('has nothing to draw for an empty movie', () => {
    expect(stripCells(0, set(), set(), 0)).toEqual([])
  })
})

describe('playbackAdvance', () => {
  const all = () => true
  const none = () => false
  it('advances a frame at a time', () => {
    expect(playbackAdvance(5, 181, false, all)).toEqual({ t: 6, next: 6, stalled: false, ended: false })
  })
  it('WAITS for a frame instead of skipping to whatever is cached', () => {
    // Skipping would hold the frame rate by silently dropping timepoints — a movie playing at the right
    // speed while omitting data, with nothing on screen saying so.
    expect(playbackAdvance(5, 181, true, none)).toEqual({ t: 5, next: 6, stalled: true, ended: false })
    // …and it waits for the NEXT frame specifically, not for any frame
    expect(playbackAdvance(5, 181, true, t => t === 6)).toEqual({ t: 6, next: 6, stalled: false, ended: false })
    expect(playbackAdvance(5, 181, true, t => t === 7)).toEqual({ t: 5, next: 6, stalled: true, ended: false })
  })
  it('names the frame it is WAITING for, so the caller prefetches that and not where it is', () => {
    // The stall this exists for: at the end of a loop, `t` and `next` are the one pair that disagree.
    // A prefetch window centred on t=180 fills BACKWARDS and never asks for frame 0, so playback waits
    // for something nothing is fetching — "3D play doesn't loop back, it just stays stuck".
    const stuck = playbackAdvance(180, 181, true, none)
    expect(stuck).toEqual({ t: 180, next: 0, stalled: true, ended: false })
    expect(prefetchWindow(stuck.t, 1, 181, 6)).not.toContain(stuck.next)   // the bug…
    expect(prefetchWindow(stuck.next, -1, 181, 6)).toContain(stuck.next)   // …and the fix
  })
  it('wraps when looping and stops when not', () => {
    expect(playbackAdvance(180, 181, true, all)).toEqual({ t: 0, next: 0, stalled: false, ended: false })
    expect(playbackAdvance(180, 181, false, all)).toEqual({ t: 180, next: -1, stalled: false, ended: true })
  })
  it('ends immediately on a still', () => {
    expect(playbackAdvance(0, 1, true, all).ended).toBe(true)
    expect(playbackAdvance(0, 0, true, all).ended).toBe(true)
  })
})

describe('playbackIntervalMs', () => {
  it('converts fps to a period', () => {
    expect(playbackIntervalMs(10)).toBe(100)
    expect(playbackIntervalMs(25)).toBe(40)
  })
  it('cannot be made to spin the timer', () => {
    expect(playbackIntervalMs(0)).toBe(1000)
    expect(playbackIntervalMs(-5)).toBe(1000)
    expect(playbackIntervalMs(NaN)).toBe(1000)
    expect(playbackIntervalMs(100000)).toBeCloseTo(1000 / 60)
  })
})

describe('prefetchDepth', () => {
  const PLANE = 8.8e6, VOLUME = 326e6            // one timepoint of Dml3RG, measured
  it('reads the whole window ahead while a timepoint is cheap', () => {
    expect(prefetchDepth(170, PLANE, false)).toBe(170)
    expect(prefetchDepth(170, PLANE, true)).toBe(170)
  })
  it('reads NOTHING ahead when a timepoint costs 1.5 s and cannot be played anyway', () => {
    // The window held four volumes, so entering the 3D view spent ~6 s pre-paying for a read-ahead the
    // view can never use. The frame asked for still arrives at its own cost.
    expect(prefetchDepth(4, VOLUME, false)).toBe(1)
  })
  it('buffers ahead regardless once playback is running — that is what lets it advance', () => {
    expect(prefetchDepth(4, VOLUME, true)).toBe(4)
  })
  it('does not depend on a measurement it has not got', () => {
    expect(prefetchDepth(4, 0, false)).toBe(4)   // before the first slab, behave as before
  })
})
