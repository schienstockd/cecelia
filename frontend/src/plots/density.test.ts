import { describe, it, expect } from 'vitest'
import { densityGrid, densityImageData, outlierPoints, DENSITY_GRID } from './density'

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

describe('densityImageData', () => {
  it('returns G×G RGBA with empty cells transparent and dense cells opaque + coloured', () => {
    const img = densityImageData(new Float32Array(cluster(400, 0.5, 0.5)), ext)
    expect(img.width).toBe(DENSITY_GRID)
    expect(img.height).toBe(DENSITY_GRID)
    expect(img.data.length).toBe(DENSITY_GRID * DENSITY_GRID * 4)
    // a corner far from the (0.5,0.5) blob → empty → fully transparent
    const cornerA = (0 * DENSITY_GRID + 0) * 4
    expect(img.data[cornerA + 3]).toBe(0)
    // the centre cell (rows flipped: destRow = G-1-gy, gy≈G/2 → still ≈centre) is opaque + non-black
    const mid = Math.floor(DENSITY_GRID / 2)
    const o = (mid * DENSITY_GRID + mid) * 4
    expect(img.data[o + 3]).toBe(255)
    expect(img.data[o] + img.data[o + 1] + img.data[o + 2]).toBeGreaterThan(0)
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
