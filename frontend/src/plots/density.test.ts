import { describe, it, expect } from 'vitest'
import { densityGrid, pointDensities, outlierPoints, DENSITY_GRID, DOT_R, VIS_POINT_DEFAULT, dotRadiusFor, valueGrid } from './density'

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

describe('dotRadiusFor (the board shares one point-size slider)', () => {
  it('leaves the default look untouched, so an existing board figure does not change', () => {
    expect(dotRadiusFor(VIS_POINT_DEFAULT)).toBe(DOT_R)
    expect(dotRadiusFor(undefined)).toBe(DOT_R)
    expect(dotRadiusFor(null)).toBe(DOT_R)
  })
  it('scales as a RATIO of that slider, so it means "bigger/smaller" on a plot whose natural dot is much smaller', () => {
    expect(dotRadiusFor(4)).toBeCloseTo(DOT_R * 2)
    expect(dotRadiusFor(1)).toBeCloseTo(DOT_R / 2)
  })
  it('never collapses to an invisible dot', () => {
    expect(dotRadiusFor(0)).toBeGreaterThan(0)
  })
})

describe('valueGrid (the binned colour-by field)', () => {
  const G = 4
  const ext4 = { xMin: 0, xMax: 4, yMin: 0, yMax: 4 }
  // no blur, so a cell's mean is exactly its own points
  const grid = (pts: number[], vals: number[]) =>
    valueGrid(new Float32Array(pts), new Float32Array(vals), ext4, G, 0, 0)
  const at = (g: { mean: Float32Array }, gx: number, gy: number) => g.mean[gy * G + gx]

  it('averages the value of the points in each cell', () => {
    const g = grid([0.5, 0.5, 0.6, 0.6, 2.5, 2.5], [10, 20, 99])
    expect(at(g, 0, 0)).toBeCloseTo(15)      // (10 + 20) / 2
    expect(at(g, 2, 2)).toBeCloseTo(99)
  })
  it('leaves a cell with no events NaN, so empty space stays empty', () => {
    expect(at(grid([0.5, 0.5], [7]), 3, 3)).toBeNaN()
  })
  it('skips a point whose measure is missing instead of counting it as zero', () => {
    const g = grid([0.5, 0.5, 0.6, 0.6], [NaN, 4])
    expect(at(g, 0, 0)).toBeCloseTo(4)       // not 2
    expect(g.count[0]).toBe(1)               // and it does not claim two events
  })
  it('reports the RAW per-cell count, so a caller can paint only cells that hold events', () => {
    const g = grid([0.5, 0.5, 0.6, 0.6, 2.5, 0.5], [1, 1, 1])
    expect(g.count[0]).toBe(2)
    expect(g.count[2]).toBe(1)
    expect(g.count[5]).toBe(0)
  })
  it('weights the smoothing by COUNT — a dense cell dominates its sparse neighbour, not the reverse', () => {
    // 100 events at 0 in cell (0,0); one event at 10 in cell (1,0); blur radius 1
    const pts: number[] = [], vals: number[] = []
    for (let i = 0; i < 100; i++) { pts.push(0.5, 0.5); vals.push(0) }
    pts.push(1.5, 0.5); vals.push(10)
    const g = valueGrid(new Float32Array(pts), new Float32Array(vals), ext4, G, 1, 1)
    expect(at(g, 1, 0)).toBeLessThan(1)      // a mean-of-means would have said ~5
  })
})
