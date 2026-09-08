/**
 * `cardVis` — the shape of each card's figure. The claims worth pinning are the ones a user reading
 * the picker would take away: each card looks the way its regime looks, the figures are
 * deterministic (no re-shuffle on reload), the animated cards actually cycle, and the ordered id
 * list stays authoritative so a re-order in the picker cannot silently drift from the producer.
 */
import { describe, expect, it } from 'vitest'
import { CARD_IDS, N, cardFigure, isAnimated } from './cardVis'

/** The maximum cell value in a frame. */
const peakOf = (f: number[][]) => Math.max(...f.map(r => Math.max(...r)))
/** The mean cell value in a frame — a rough "how bright is this figure overall". */
const meanOf = (f: number[][]) => {
  let s = 0, n = 0
  for (const r of f) for (const v of r) { s += v; n++ }
  return s / n
}

describe('cardFigure — shape + determinism', () => {
  it.each(CARD_IDS)('%s is deterministic (no PRNG re-seed drift)', id => {
    expect(cardFigure(id)).toEqual(cardFigure(id))
  })

  it.each(CARD_IDS)('%s frames are N x N with values in [0, 1]', id => {
    const fig = cardFigure(id)
    expect(fig.frames.length).toBeGreaterThan(0)
    for (const f of fig.frames) {
      expect(f.length).toBe(N)
      for (const row of f) {
        expect(row.length).toBe(N)
        for (const v of row) {
          expect(v).toBeGreaterThanOrEqual(0)
          expect(v).toBeLessThanOrEqual(1)
        }
      }
    }
  })
})

describe('cardFigure — per-card regime signature', () => {
  it('resonance: sparse strikes on a dark field — mostly-black overall, a few hot pixels', () => {
    const fig = cardFigure('resonance')
    for (const f of fig.frames) {
      // Mostly dark — the mean is dominated by the low-noise background, not the strikes.
      expect(meanOf(f)).toBeLessThan(0.20)
      // At least one strike is visibly bright.
      expect(peakOf(f)).toBeGreaterThan(0.7)
    }
  })

  it('resonance: strike positions REFRESH between frames — implies single-photon events', () => {
    const fig = cardFigure('resonance')
    expect(fig.frames.length).toBeGreaterThanOrEqual(2)
    expect(fig.frames[0]).not.toEqual(fig.frames[1])
  })

  it('resonance: strikes CLUSTER on the same scene galvo shows — coherent, not uniform noise', () => {
    // Sum resonance frames and compare "bright where the scene is bright" against galvo. A
    // uniformly-scattered field would show no correlation; a photon-limited sampling of the same
    // scene should.
    const res = cardFigure('resonance').frames
    const galvo = cardFigure('galvo').frames[0]
    const stack = zeros()
    for (const f of res) for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) stack[y][x] += f[y][x]
    // Mean intensity in galvo-bright cells (>0.4) vs galvo-dark cells (<0.1).
    let bs = 0, bn = 0, ds = 0, dn = 0
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) {
        if (galvo[y][x] > 0.4)      { bs += stack[y][x]; bn++ }
        else if (galvo[y][x] < 0.1) { ds += stack[y][x]; dn++ }
      }
    }
    expect(bn).toBeGreaterThan(0); expect(dn).toBeGreaterThan(0)
    const brightMean = bs / bn, darkMean = ds / dn
    // Resonance strikes on the scene should be at least 3× brighter than the dark background.
    expect(brightMean).toBeGreaterThan(darkMean * 3)
  })

  function zeros(): number[][] { return Array.from({ length: N }, () => new Array(N).fill(0)) }

  it('galvo: multiple distinct bright regions on a low-noise field (clean cellular scene)', () => {
    const fig = cardFigure('galvo')
    expect(fig.frames.length).toBe(1)
    const f = fig.frames[0]
    expect(peakOf(f)).toBeGreaterThan(0.8)
    // Count "bright regions" — a cell above 0.5 that dominates its 3x3 neighbourhood is a local
    // peak. The regime's point is "you see cells (plural)", not one blob.
    let localPeaks = 0
    for (let y = 1; y < N - 1; y++) {
      for (let x = 1; x < N - 1; x++) {
        const v = f[y][x]
        if (v < 0.5) continue
        let isPeak = true
        for (let dy = -1; dy <= 1 && isPeak; dy++) {
          for (let dx = -1; dx <= 1 && isPeak; dx++) {
            if (dx === 0 && dy === 0) continue
            if (f[y + dy][x + dx] > v) isPeak = false
          }
        }
        if (isPeak) localPeaks++
      }
    }
    expect(localPeaks).toBeGreaterThanOrEqual(3)
    // Cleaner than resonance — the "clean signal" claim is about smoothness, not brightness.
    // Neighbour-to-neighbour intensity change is small (gaussian falloffs, no grain).
    let dSum = 0, dN = 0
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N - 1; x++) { dSum += Math.abs(f[y][x + 1] - f[y][x]); dN++ }
    }
    const galvoRoughness = dSum / dN
    const rf = cardFigure('resonance').frames[0]
    let rSum = 0, rN = 0
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N - 1; x++) { rSum += Math.abs(rf[y][x + 1] - rf[y][x]); rN++ }
    }
    const resonanceRoughness = rSum / rN
    expect(galvoRoughness).toBeLessThan(resonanceRoughness)
  })

  it('spinning_disk: multiple frames, adjacent frames DIFFER (puncta have drifted)', () => {
    const fig = cardFigure('spinning_disk')
    expect(fig.frames.length).toBeGreaterThanOrEqual(2)
    expect(fig.frames[0]).not.toEqual(fig.frames[1])
    // Each frame has visible puncta, not a uniform field.
    for (const f of fig.frames) expect(peakOf(f)).toBeGreaterThan(0.7)
  })

  it('deep_3d: frames drift LINEARLY across the stack — a Z-shear, not a wobble', () => {
    const fig = cardFigure('deep_3d')
    expect(fig.frames.length).toBeGreaterThanOrEqual(3)
    // Intensity-weighted x centroid of each frame — a rough centre-of-mass. Under a linear
    // intra-stack shear it drifts monotonically from first frame to last; a wobble would
    // return to its start. Using a centroid rather than the single brightest cell so a
    // focus-fade-driven change in *which* object is brightest cannot flip the reading.
    const cxs: number[] = []
    for (const f of fig.frames) {
      let sx = 0, sw = 0
      for (let y = 0; y < N; y++) {
        for (let x = 0; x < N; x++) { sx += x * f[y][x]; sw += f[y][x] }
      }
      cxs.push(sx / sw)
    }
    for (let i = 1; i < cxs.length; i++) {
      expect(cxs[i]).toBeGreaterThan(cxs[i - 1])
    }
  })

  it('custom / unknown ids: a black grid — the picker draws its own escape glyph over this', () => {
    for (const id of ['custom', 'not-a-card']) {
      const fig = cardFigure(id)
      expect(fig.frames.length).toBe(1)
      expect(peakOf(fig.frames[0])).toBe(0)
    }
  })
})

describe('isAnimated', () => {
  it('flags cards that need the frame timer', () => {
    expect(isAnimated('resonance')).toBe(true)
    expect(isAnimated('spinning_disk')).toBe(true)
    expect(isAnimated('galvo')).toBe(false)
    expect(isAnimated('deep_3d')).toBe(true)
    expect(isAnimated('custom')).toBe(false)
  })
})

describe('CARD_IDS is the one authority', () => {
  it('contains the four opinionated cards plus custom, in display order', () => {
    expect([...CARD_IDS]).toEqual(['resonance', 'galvo', 'spinning_disk', 'deep_3d', 'custom'])
  })
})
