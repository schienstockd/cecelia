import { describe, it, expect } from 'vitest'
import { playHealthSummary, trimSamples, type PlayHealthSample } from './playHealth'

const s = (timeMs: number, displayT: number, boundT: number, missing: number): PlayHealthSample =>
  ({ timeMs, displayT, boundT, missingAtDisplay: missing })

describe('playHealthSummary', () => {
  it('returns zeros / NaN fps on an empty window', () => {
    const r = playHealthSummary([])
    expect(r.count).toBe(0)
    expect(r.avgMissingAtDisplay).toBe(0)
    expect(r.p95BoundTLag).toBe(0)
    expect(Number.isNaN(r.achievedFps)).toBe(true)
  })

  it('averages missing@display across the window', () => {
    const r = playHealthSummary([s(0, 0, 0, 0), s(100, 0, 0, 4), s(200, 1, 1, 8)])
    expect(r.avgMissingAtDisplay).toBeCloseTo(4)
    // p95 with 3 samples picks the top (nearest-rank ceil(0.95 * 3) = 3 → idx 2)
    expect(r.p95MissingAtDisplay).toBe(8)
  })

  it('reports 95th-percentile boundT lag — the stall size a smooth average would hide', () => {
    // 100 samples: 90 with zero lag, 10 with a 40-frame stall. Average = 4, p95 catches the stall.
    // (With nearest-rank p95, the outliers must occupy AT LEAST the top 5% of the sorted array to
    // be captured — a single outlier in 20 samples is p95=0, which was the test's original bug.)
    const many: PlayHealthSample[] = []
    for (let i = 0; i < 90; i++) many.push(s(i * 100, i, i, 0))
    for (let i = 0; i < 10; i++) many.push(s(9000 + i * 100, 90 + i, 90 + i + 40, 0))
    const r = playHealthSummary(many)
    expect(r.p95BoundTLag).toBe(40)
  })

  it('derives achievedFps from displayT advances over the window span, not sample count', () => {
    // 10 samples at 100 ms apart: span 900 ms, 9 advances → 9 × 1000 / 900 = 10 fps.
    const advancing: PlayHealthSample[] = []
    for (let i = 0; i < 10; i++) advancing.push(s(i * 100, i, i, 0))
    expect(playHealthSummary(advancing).achievedFps).toBeCloseTo(10)

    // Same 10 samples but displayT stuck at 0 (playback stalled — Frankenstein tick) → 0 fps.
    const stalled: PlayHealthSample[] = []
    for (let i = 0; i < 10; i++) stalled.push(s(i * 100, 0, i, 5))
    expect(playHealthSummary(stalled).achievedFps).toBe(0)
  })

  it('returns NaN fps when the window spans zero time — one sample or duplicate timestamps', () => {
    expect(Number.isNaN(playHealthSummary([s(100, 0, 0, 0)]).achievedFps)).toBe(true)
    expect(Number.isNaN(playHealthSummary([s(100, 0, 0, 0), s(100, 1, 1, 0)]).achievedFps)).toBe(true)
  })

  it('clamps lag to zero — a negative boundT-displayT (loop wrap) is not a lag', () => {
    // Playback loops: boundT resets to 0 while displayT lingers at 180 during the wrap tick.
    // The raw difference is -180; that's not a stall to report — clamp at 0.
    const r = playHealthSummary([s(0, 180, 0, 0)])
    expect(r.p95BoundTLag).toBe(0)
  })
})

describe('trimSamples', () => {
  it('drops samples older than the window', () => {
    const now = 1000
    const samples = [s(100, 0, 0, 0), s(600, 1, 1, 0), s(900, 2, 2, 0)]
    // window 500 ms → keep everything with timeMs >= 500
    expect(trimSamples(samples, now, 500, 100)).toEqual([s(600, 1, 1, 0), s(900, 2, 2, 0)])
  })

  it('caps at maxSamples even when everything is within the window — memory guard for a stuck loop', () => {
    const samples: PlayHealthSample[] = []
    for (let i = 0; i < 200; i++) samples.push(s(i, i, i, 0))
    const trimmed = trimSamples(samples, 199, 10_000, 50)
    expect(trimmed).toHaveLength(50)
    // Kept the LAST 50 (freshest), not the first 50
    expect(trimmed[0].timeMs).toBe(150)
    expect(trimmed[49].timeMs).toBe(199)
  })
})
