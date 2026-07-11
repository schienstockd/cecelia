import { describe, it, expect } from 'vitest'
import { densityGrid, pointDensities, outlierPoints, DENSITY_GRID } from './density'

const ext = { xMin: 0, xMax: 1, yMin: 0, yMax: 1 }

// a tight cluster of many points near one corner + a couple of lone points far away
function cluster(n: number, cx: number, cy: number, spread = 0.01): number[] {
  const out: number[] = []
  for (let i = 0; i < n; i++) out.push(cx + (i % 3) * spread, cy + ((i * 7) % 3) * spread)
  return out
}

describe('densityGrid', () => {
  it('is normalised to a max of 1 and peaks where points concentrate', () => {
    const g = densityGrid(new Float32Array(cluster(300, 0.2, 0.2)), ext)
    expect(g.length).toBe(DENSITY_GRID * DENSITY_GRID)
    expect(Math.max(...g)).toBeCloseTo(1, 6)
    // the peak cell should be near (0.2,0.2): gx=gy≈floor(0.2*G) (± a couple of cells for the blur)
    const expected = Math.floor(0.2 * DENSITY_GRID)
    let peak = 0, pi = 0
    g.forEach((v, i) => { if (v > peak) { peak = v; pi = i } })
    const gy = Math.floor(pi / DENSITY_GRID), gx = pi % DENSITY_GRID
    expect(Math.abs(gx - expected)).toBeLessThanOrEqual(3)
    expect(Math.abs(gy - expected)).toBeLessThanOrEqual(3)
  })
  it('skips non-finite and out-of-range points without throwing', () => {
    const g = densityGrid(new Float32Array([NaN, 0.5, 5, 5, 0.3, 0.3]), ext)
    expect(Math.max(...g)).toBeGreaterThan(0)   // only the (0.3,0.3) point counted
  })
})

describe('pointDensities', () => {
  it('is one 0..1 value per point, highest in the dense core, lowest in the sparse tail', () => {
    const pts = new Float32Array([...cluster(400, 0.5, 0.5), 0.02, 0.02])   // dense core + 1 lone tail point
    const t = pointDensities(pts, ext)
    expect(t.length).toBe(pts.length / 2)
    for (const v of t) { expect(v).toBeGreaterThanOrEqual(0); expect(v).toBeLessThanOrEqual(1) }
    expect(t[0]).toBeGreaterThan(t[t.length - 1])   // a core point is denser than the lone tail point
  })
  it('gives non-finite points 0 density', () => {
    const t = pointDensities(new Float32Array([NaN, NaN, 0.5, 0.5]), ext)
    expect(t[0]).toBe(0)
  })
})

describe('outlierPoints', () => {
  it('returns the sparse-tail points, not the dense core', () => {
    const pts = new Float32Array([...cluster(400, 0.5, 0.5), 0.02, 0.02, 0.97, 0.95])  // core + 2 far outliers
    const out = outlierPoints(pts, ext)
    const has = (x: number, y: number) => {  // Float32 → compare with tolerance
      for (let i = 0; i < out.length; i += 2) if (Math.abs(out[i] - x) < 1e-3 && Math.abs(out[i + 1] - y) < 1e-3) return true
      return false
    }
    expect(has(0.02, 0.02)).toBe(true)      // the two lone corner points are below the outermost contour → kept
    expect(has(0.97, 0.95)).toBe(true)
    expect(has(0.5, 0.5)).toBe(false)       // the dense core is NOT an outlier
    expect(out.length / 2).toBeLessThan(50) // far fewer than the 402 input points
  })
  it('is empty for a single uniform blob with no tail', () => {
    // all points in one cell → that cell is the max (density 1), nothing below the level
    const out = outlierPoints(new Float32Array(cluster(200, 0.5, 0.5, 0)), ext)
    expect(out.length).toBe(0)
  })
})
