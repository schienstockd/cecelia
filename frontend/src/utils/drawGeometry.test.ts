import { describe, it, expect } from 'vitest'
import {
  MIN_DRAG_PX, isClickNotDrag, polygonAreaPx, isDegeneratePolygon,
  beginRect, updateRect, finishRect,
  beginPoly, addVertex, updateCursor, isNearFirst, finishPoly,
  beginStroke, extendStroke, simplifyRDP, finishStroke,
  type Point,
} from './drawGeometry'

describe('primitives', () => {
  it('MIN_DRAG_PX gates click-vs-drag identically', () => {
    // one number in one place — if this line ever needs to change, both jobs (annotation + gating)
    // have to move together
    expect(MIN_DRAG_PX).toBe(3)
    expect(isClickNotDrag([100, 100], [100, 100])).toBe(true)
    expect(isClickNotDrag([100, 100], [102, 101])).toBe(true)
    expect(isClickNotDrag([100, 100], [140, 130])).toBe(false)
    expect(isClickNotDrag([100, 100], [100, 140])).toBe(false)   // one-axis sliver is deliberate
  })
  it('shoelace area is orientation-independent', () => {
    expect(polygonAreaPx([[0, 0], [10, 0], [10, 10], [0, 10]])).toBe(100)
    expect(polygonAreaPx([[0, 0], [10, 0], [10, 10], [0, 10]].reverse() as Point[])).toBe(100)
    expect(polygonAreaPx([[0, 0], [10, 0]] as Point[])).toBe(0)   // not a polygon yet
  })
  it('degenerate polygons are anything below the jitter square', () => {
    expect(isDegeneratePolygon([[0, 0], [5, 0], [10, 0]])).toBe(true)          // colinear
    expect(isDegeneratePolygon([[0, 0], [1, 0], [1, 1]])).toBe(true)           // closed on the spot
    expect(isDegeneratePolygon([[0, 0], [20, 0], [20, 20]])).toBe(false)
  })
})

describe('rectangle state machine', () => {
  it('starts as a zero-area at the seed point', () => {
    const d = beginRect([10, 20])
    expect(d.start).toEqual([10, 20])
    expect(d.cur).toEqual([10, 20])
  })
  it('normalises min/max regardless of drag direction', () => {
    const d = updateRect(beginRect([50, 60]), [10, 20])
    const r = finishRect(d, [10, 20])
    expect(r).toEqual({ xMin: 10, yMin: 20, xMax: 50, yMax: 60 })
  })
  it('drops zero-area / jitter releases (isClickNotDrag)', () => {
    const d = beginRect([100, 100])
    expect(finishRect(d, [100, 100])).toBeNull()
    expect(finishRect(d, [101, 102])).toBeNull()
  })
})

describe('polygon state machine', () => {
  it('accumulates vertices; cursor is separate from the committed list', () => {
    let d = beginPoly()
    d = addVertex(d, [0, 0]); d = addVertex(d, [10, 0]); d = addVertex(d, [10, 10])
    d = updateCursor(d, [5, 15])
    expect(d.vertices).toEqual([[0, 0], [10, 0], [10, 10]])
    expect(d.cursor).toEqual([5, 15])
  })
  it('isNearFirst needs ≥3 vertices AND jitter-close to vertex 0', () => {
    let d = beginPoly()
    d = addVertex(d, [0, 0]); d = addVertex(d, [10, 0])
    expect(isNearFirst(d, [1, 1])).toBe(false)   // only 2 vertices — closing here would be a mis-click
    d = addVertex(d, [10, 10])
    expect(isNearFirst(d, [1, 1])).toBe(true)    // within MIN_DRAG_PX of [0,0]
    expect(isNearFirst(d, [50, 50])).toBe(false)
  })
  it('finishPoly drops degenerates and returns a copy', () => {
    expect(finishPoly({ vertices: [[0, 0], [1, 0]], cursor: null })).toBeNull()
    expect(finishPoly({ vertices: [[0, 0], [5, 0], [10, 0]], cursor: null })).toBeNull()   // colinear
    const out = finishPoly({ vertices: [[0, 0], [20, 0], [20, 20]], cursor: null })!
    expect(out).toEqual([[0, 0], [20, 0], [20, 20]])
    // returned array is independent of the input — mutating one must not touch the other
    out[0][0] = 999
    const again = finishPoly({ vertices: [[0, 0], [20, 0], [20, 20]], cursor: null })!
    expect(again[0]).toEqual([0, 0])
  })
})

describe('freehand state machine', () => {
  it('thins points closer than minStepPx from the last kept one', () => {
    let s = beginStroke([0, 0])          // default minStepPx = 2
    s = extendStroke(s, [1, 0])          // 1px — dropped (below threshold)
    s = extendStroke(s, [2, 0])          // 2px — kept (threshold is strict-less-than, so 2 passes)
    s = extendStroke(s, [3, 0])          // 1px from [2,0] — dropped
    s = extendStroke(s, [3, 0])          // no motion — dropped
    s = extendStroke(s, [4, 1])          // sqrt(5)≈2.24px from [2,0] — kept
    expect(s.pts).toEqual([[0, 0], [2, 0], [4, 1]])
  })
  it('simplifyRDP keeps endpoints and drops points inside tolerance', () => {
    const straight: Point[] = [[0, 0], [1, 0.5], [2, -0.4], [3, 0.2], [10, 0]]
    expect(simplifyRDP(straight, 1)).toEqual([[0, 0], [10, 0]])
    const corner: Point[] = [[0, 0], [5, 0], [5, 5]]
    expect(simplifyRDP(corner, 0.5)).toEqual([[0, 0], [5, 0], [5, 5]])
  })
  it('simplifyRDP handles degenerate segments (zero-length) via point-distance fallback', () => {
    // if beginStroke's minStepPx let a self-loop through, the perpendicular formula would divide
    // by zero — the fallback keeps the outlier
    const loop: Point[] = [[5, 5], [5, 5], [10, 0], [5, 5]]
    const out = simplifyRDP(loop, 0.1)
    expect(out).toContainEqual([10, 0])
  })
  it('finishStroke returns a simplified copy from a real stroke', () => {
    let s = beginStroke([0, 0])
    for (let x = 0; x <= 30; x += 3) s = extendStroke(s, [x, Math.sin(x / 10)])
    const out = finishStroke(s, 0.5)
    expect(out[0]).toEqual([0, 0])
    expect(out[out.length - 1][0]).toBe(30)
    expect(out.length).toBeLessThan(s.pts.length)
  })
})
