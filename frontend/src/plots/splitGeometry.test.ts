import { describe, it, expect } from 'vitest'
import { pointInPoly } from '../utils/drawGeometry'
import type { GateSpec } from '../stores/gating'
import {
  sliceLeftPolygon, quadrantPolygon, regionPolygon, dividerSegments, quadrantOf, armsCcw, transposeSplit,
  positiveSide, sliceRegions, quadrantRegions, splitLabelPoint, clipToBox, withDivider, defaultArms,
} from './splitGeometry'

// Expected regions come from the definition (left of the line's direction; sector from arm q to arm q+1
// counter-clockwise) — the same cases app/test/suite/split_gates.jl pins for the Julia membership.
type Pt = [number, number]
const box = { xMin: -10, xMax: 10, yMin: -10, yMax: 10 }
const base = { x_channel: 'x', y_channel: 'y', x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } } as const
const slice = (vertices: Pt[], side = 1): GateSpec => ({ ...base, kind: 'slice', vertices, side })
const quad = (centre: Pt, arms: Pt[], quadrant = 1): GateSpec => ({ ...base, kind: 'quadrant', centre, arms, quadrant })
const AXIS_ARMS: Pt[] = [[1, 0], [0, 1], [-1, 0], [0, -1]]

describe('slice regions', () => {
  it('a straight line: left of its direction, extended past its ends', () => {
    const left = sliceLeftPolygon([[-1, 0], [1, 0]], box)
    expect(pointInPoly([0, 1], left)).toBe(true)
    expect(pointInPoly([8, 3], left)).toBe(true)       // beyond the drawn end, still above
    expect(pointInPoly([0, -1], left)).toBe(false)
  })
  it('a bent line: right along y = 0, then up x = 0 → the upper-left quadrant', () => {
    const v: Pt[] = [[-1, 0], [0, 0], [0, 1]]
    const left = regionPolygon(slice(v, 1), box), right = regionPolygon(slice(v, -1), box)
    for (const [p, l] of [[[-1, 1], true], [[1, 1], false], [[-1, -1], false], [[1, -1], false], [[-5, 8], true]] as [Pt, boolean][]) {
      expect(pointInPoly(p, left)).toBe(l)
      expect(pointInPoly(p, right)).toBe(!l)
    }
  })
  it('the divider runs from border to border', () => {
    const segs = dividerSegments(slice([[-1, 0], [0, 0], [0, 1]]), box)
    expect(segs[0][0]).toEqual([-10, 0])
    expect(segs[segs.length - 1][1]).toEqual([0, 10])
    expect(segs).toHaveLength(4)
  })
})

describe('quadrant regions', () => {
  it('axis-aligned arms: sectors 1–4 are ++, -+, --, +-', () => {
    expect([[1, 1], [-1, 1], [-1, -1], [1, -1]].map(p => quadrantOf([0, 0], AXIS_ARMS, p as Pt))).toEqual([1, 2, 3, 4])
    for (let q = 1; q <= 4; q++) {
      const poly = quadrantPolygon([0, 0], AXIS_ARMS, q, box)
      const probe: Pt[] = [[5, 5], [-5, 5], [-5, -5], [5, -5]]
      expect(probe.map(p => pointInPoly(p, poly))).toEqual(probe.map((_, i) => i === q - 1))
    }
  })
  it('a tilted arm (spider) moves the boundary', () => {
    const arms: Pt[] = [[1, 1], [0, 1], [-1, 0], [0, -1]]
    expect(quadrantOf([0, 0], arms, [9, 0.5])).toBe(4)
    expect(quadrantOf([0, 0], arms, [1, 3])).toBe(1)
    expect(pointInPoly([9, 0.5], quadrantPolygon([0, 0], arms, 4, box))).toBe(true)
  })
  it('refuses arms that cross', () => {
    expect(armsCcw([0, 0], AXIS_ARMS)).toBe(true)
    expect(armsCcw([0, 0], [[1, 0], [-1, 0], [0, 1], [0, -1]])).toBe(false)
    expect(armsCcw([0, 0], [[0, 0], [0, 1], [-1, 0], [0, -1]])).toBe(false)
  })
  it('a new quadrant reaches the border along the axes', () => {
    expect(defaultArms([2, 3], box)).toEqual([[10, 3], [2, 10], [-10, 3], [2, -10]])
  })
})

describe('axis swap', () => {
  // swapping the axes must keep every point in the region it was in (the Julia project_gate rule)
  const sw = (p: Pt): Pt => [p[1], p[0]]
  const probes: Pt[] = [[3, 1], [-2, 4], [-5, -1], [2, -6], [0.5, 0.2], [7, 7], [-8, 2]]
  it('a slice keeps each point on its side', () => {
    const g = slice([[-1, -0.5], [0.3, 0.2], [1, 1.5]], 1), t = transposeSplit(g)
    const a = regionPolygon(g, box), b = regionPolygon(t, box)
    for (const p of probes) expect(pointInPoly(sw(p), b)).toBe(pointInPoly(p, a))
  })
  it('a quadrant keeps each point in its sector', () => {
    const arms: Pt[] = [[1, 0.4], [0, 1], [-1, -0.3], [0.5, -1]]
    for (let q = 1; q <= 4; q++) {
      const t = transposeSplit(quad([0.2, 0.1], arms, q))
      for (const p of probes)
        expect(quadrantOf(t.centre!, t.arms!, sw(p)) === t.quadrant).toBe(quadrantOf([0.2, 0.1], arms, p) === q)
    }
  })
})

describe('naming', () => {
  it('+ is the side toward higher values', () => {
    expect(positiveSide([[0, 0], [5, 1]])).toBe(1)      // runs right: + is above = left
    expect(positiveSide([[5, 1], [0, 0]])).toBe(-1)     // runs left: above is its right
    expect(positiveSide([[0, 0], [1, 5]])).toBe(-1)     // runs up: + is to the right
    expect(positiveSide([[1, 5], [0, 0]])).toBe(1)
    const [p, m] = sliceRegions('Kat', [[0, 0], [5, 1]])
    expect(p).toEqual({ name: 'Kat+', region: 1 })
    expect(m).toEqual({ name: 'Kat-', region: -1 })
  })
  it('quadrants are named by sign on each axis, no path separator', () => {
    expect(quadrantRegions('CD4', 'CD8/b').map(r => r.name)).toEqual(['CD4+ CD8b+', 'CD4- CD8b+', 'CD4- CD8b-', 'CD4+ CD8b-'])
  })
})

describe('labels and linked drags', () => {
  it('a label sits inside its region, in view', () => {
    const p = splitLabelPoint(quad([0, 0], AXIS_ARMS, 3), box)!
    expect(p[0]).toBeLessThan(0); expect(p[1]).toBeLessThan(0)
    // a region wholly out of view has no label
    const at = (c: Pt): Pt[] => AXIS_ARMS.map(a => [c[0] + a[0], c[1] + a[1]])
    expect(splitLabelPoint(quad([20, 20], at([20, 20]), 1), box)).toBeNull()
    // a centre ON the view's border: the sector beyond it is out of view, the one inside is labelled —
    // not the whole box for both (an arm of zero length closed round the entire border)
    expect(splitLabelPoint(quad([10, 0], at([10, 0]), 1), box)).toBeNull()
    const inView = splitLabelPoint(quad([10, 0], at([10, 0]), 2), box)!
    expect(inView[0]).toBeLessThan(10); expect(inView[1]).toBeGreaterThan(0)
  })
  it('clipping keeps only the part inside the box', () => {
    const c = clipToBox([[-20, -20], [20, -20], [20, 0], [-20, 0]], box)
    expect(Math.min(...c.map(p => p[0]))).toBe(-10)
    expect(Math.max(...c.map(p => p[1]))).toBe(0)
  })
  it('a group member follows the dragged divider and keeps its region', () => {
    const a = { ...slice([[0, 0], [1, 0]], 1), group: 'g' }, b = { ...slice([[0, 0], [1, 0]], -1), group: 'g' }
    const moved = withDivider(b, { ...a, vertices: [[0, 2], [1, 2]] })
    expect(moved.vertices).toEqual([[0, 2], [1, 2]]); expect(moved.side).toBe(-1)
    expect(withDivider({ ...b, group: 'other' }, { ...a, vertices: [[0, 2], [1, 2]] }).vertices).toEqual([[0, 0], [1, 0]])
  })
})
