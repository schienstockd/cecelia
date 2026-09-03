import { describe, it, expect } from 'vitest'
import { projectAxes, formatZoom } from './axesGizmo'

// Golden values are hand-derived from mipShader.ts's `camera()` fn:
//   fwd   = (cp*sy, sp, cp*cy)
//   right = normalize(cross((0,1,0), fwd))
//   up    = cross(right, fwd)
// A change in either has to update this test AT THE SAME TIME — that is the whole point of a
// second copy of the formula in TypeScript: the drift is the failure, not the divergence.
const near = (a: number, b: number, eps = 1e-9) => Math.abs(a - b) < eps

describe('projectAxes — face-on (yaw=0, pitch=0)', () => {
  const tips = projectAxes(0, 0, 1)
  const by = (k: string) => tips.find(t => t.key === k)!

  it('+X goes right, -X goes left', () => {
    expect(near(by('+X').x, 1)).toBe(true)
    expect(near(by('+X').y, 0)).toBe(true)
    expect(near(by('-X').x, -1)).toBe(true)
  })

  it('+Y goes DOWN on the SVG (image convention: row axis is down)', () => {
    expect(near(by('+Y').x, 0)).toBe(true)
    expect(near(by('+Y').y, 1)).toBe(true)          // SVG y grows downward
    expect(near(by('-Y').y, -1)).toBe(true)
  })

  it('+Z is the front (near the viewer); -Z is the back', () => {
    // Both z tips project to the centre at rest; the tell is the depth.
    expect(by('+Z').depth).toBeGreaterThan(by('-Z').depth)
    expect(near(by('+Z').x, 0)).toBe(true)
    expect(near(by('+Z').y, 0)).toBe(true)
  })
})

describe('projectAxes — 90° yaw around Y', () => {
  // Looking FROM +X toward the origin: world Z now runs along screen X.
  const tips = projectAxes(Math.PI / 2, 0, 1)
  const by = (k: string) => tips.find(t => t.key === k)!

  it('world +X collapses to a point in front of the camera', () => {
    expect(near(by('+X').x, 0)).toBe(true)
    expect(near(by('+X').y, 0)).toBe(true)
    expect(by('+X').depth).toBeGreaterThan(by('-X').depth)
  })

  it('world +Z now points LEFT on screen (right-handed rotation)', () => {
    expect(near(by('+Z').x, -1)).toBe(true)
    expect(near(by('-Z').x, 1)).toBe(true)
  })
})

describe('projectAxes — painter ordering + centre + radius', () => {
  it('tips arrive back-to-front so a naive draw stacks correctly', () => {
    const tips = projectAxes(0.7, 0.4, 1)
    for (let i = 1; i < tips.length; i++) {
      expect(tips[i].depth).toBeGreaterThanOrEqual(tips[i - 1].depth)
    }
  })

  it('centre shifts every tip by the same offset', () => {
    const a = projectAxes(0.3, 0.2, 1)
    const b = projectAxes(0.3, 0.2, 1, { x: 40, y: 40 })
    for (const t of a) {
      const t2 = b.find(u => u.key === t.key)!
      expect(near(t2.x - t.x, 40)).toBe(true)
      expect(near(t2.y - t.y, 40)).toBe(true)
    }
  })

  it('radius scales linearly', () => {
    const a = projectAxes(0.5, 0.3, 10)
    const b = projectAxes(0.5, 0.3, 30)
    for (const t of a) {
      const t2 = b.find(u => u.key === t.key)!
      expect(near(t2.x, t.x * 3)).toBe(true)
      expect(near(t2.y, t.y * 3)).toBe(true)
    }
  })
})

describe('formatZoom', () => {
  it('shows fit as 1.0×', () => { expect(formatZoom(1)).toBe('1×') })
  it('shows zoomed-in with one decimal', () => { expect(formatZoom(2.437)).toBe('2.4×') })
  it('shows zoomed-out with two decimals', () => { expect(formatZoom(0.6249)).toBe('0.62×') })
  it('drops trailing decimals for round numbers', () => { expect(formatZoom(12)).toBe('12×') })
  it('replaces NaN / non-finite with an em-dash', () => {
    expect(formatZoom(NaN)).toBe('—')
    expect(formatZoom(Infinity)).toBe('—')
    expect(formatZoom(0)).toBe('—')
  })
})
