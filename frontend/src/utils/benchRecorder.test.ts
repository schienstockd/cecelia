import { describe, it, expect } from 'vitest'
import {
  median, percentile95, summarize, buildBlob, benchFilename,
  type BenchSample, type BenchMeta,
} from './benchRecorder'

describe('median', () => {
  it('returns null for an empty input', () => {
    expect(median([])).toBeNull()
  })
  it('picks the middle value for an odd length', () => {
    expect(median([3, 1, 2])).toBe(2)
  })
  it('averages the two middle values for an even length', () => {
    expect(median([1, 2, 3, 4])).toBe(2.5)
  })
})

describe('percentile95', () => {
  it('returns null for an empty input', () => {
    expect(percentile95([])).toBeNull()
  })
  it('returns the top value when there are only a few samples', () => {
    // Fewer than 20 samples → nearest-rank floors at the last index. A CPU-time bench with 10
    // samples where the p95 must be the max is what a real short session looks like.
    expect(percentile95([1, 2, 3, 4, 5])).toBe(5)
  })
  it('returns a rank within the sorted distribution', () => {
    const xs = Array.from({ length: 100 }, (_, i) => i + 1)  // 1..100
    expect(percentile95(xs)).toBe(95)
  })
})

const sample = (atMs: number, drawMs: number): BenchSample => ({ atMs, drawMs })

describe('summarize', () => {
  it('reports zero-frame sessions honestly (all nulls, count 0)', () => {
    const s = summarize([], 500)
    expect(s.nFrames).toBe(0)
    expect(s.drawMedianMs).toBeNull()
    expect(s.framesPerSecond).toBeNull()
  })
  it('computes fps from frames and session length in ms', () => {
    const frames = Array.from({ length: 60 }, (_, i) => sample(i * 16, 4))
    const s = summarize(frames, 1000)
    expect(s.framesPerSecond).toBe(60)
  })
  it('carries mean/median/p95 through', () => {
    const frames = [sample(0, 1), sample(1, 2), sample(2, 3), sample(3, 4)]
    const s = summarize(frames, 100)
    expect(s.drawMedianMs).toBe(2.5)
    expect(s.drawMeanMs).toBe(2.5)
  })
})

const meta: BenchMeta = {
  imageUid: 'fXgbTl', valueName: 'smoothed',
  nT: 31, nC: 4, nZ: 32, nY: 420, nX: 441,
  nLevels: 1, bytesPerVoxel: 1,
}

describe('buildBlob', () => {
  it('normalises frame times to be relative to t0', () => {
    const b = buildBlob({
      mode: 'brick', meta, t0: 1000, savedAt: 5000, isoDate: '2026-08-29T10:00:00Z',
      firstFrameMs: 210,
      frames: [sample(1050, 3), sample(1100, 4)],
      bytesFetched: 12345, vram: null,
    })
    expect(b.frames[0]!.atMs).toBe(50)  // wall-clock 1050 minus t0 1000
    expect(b.frames[1]!.atMs).toBe(100)
    expect(b.sessionMs).toBe(4000)
  })
  it('picks up the session identity so a paste-back is self-explaining', () => {
    const b = buildBlob({
      mode: 'flat', meta, t0: 0, savedAt: 100, isoDate: '2026-08-29T10:00:00Z',
      firstFrameMs: null,
      frames: [], bytesFetched: 0, vram: null,
    })
    expect(b.mode).toBe('flat')
    expect(b.meta.imageUid).toBe('fXgbTl')
    expect(b.version).toBe(1)
  })
})

describe('benchFilename', () => {
  it('encodes mode + image + iso date, safe for shell paste', () => {
    const f = benchFilename('brick', 'fXgbTl', '2026-08-29T10:15:23.456Z')
    expect(f).toBe('bench-fXgbTl-brick-2026-08-29_10-15-23-456Z.json')
  })
})
