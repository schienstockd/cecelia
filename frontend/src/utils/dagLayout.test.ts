import { describe, it, expect } from 'vitest'
import {
  topoOrder, layerLanes, layoutDag, LAYOUT_VARIANTS, EDITOR_GRID, COMPACT_GRID,
  type DagNode, type DagEdge,
} from './dagLayout'

const N = (...ids: string[]): DagNode[] => ids.map(id => ({ id }))
const E = (...pairs: [string, string][]): DagEdge[] => pairs.map(([from, to]) => ({ from, to }))

describe('topoOrder', () => {
  it('orders a linear chain', () => {
    expect(topoOrder(N('c', 'a', 'b'), E(['a', 'b'], ['b', 'c']))).toEqual(['a', 'b', 'c'])
  })

  it('keeps cyclic / unreachable nodes instead of dropping them', () => {
    // A malformed template must still RENDER — a node that silently vanished from the canvas would be
    // far worse than one drawn in the wrong place.
    const out = topoOrder(N('a', 'b'), E(['a', 'b'], ['b', 'a']))
    expect(out.sort()).toEqual(['a', 'b'])
  })

  it('ignores edges naming an unknown node', () => {
    expect(topoOrder(N('a'), E(['ghost', 'a'], ['a', 'ghost']))).toEqual(['a'])
  })
})

describe('layerLanes', () => {
  it('uses the LONGEST path, so a late join sits right of its slowest input', () => {
    //  a → b → c
    //   \_______↗     d joins c directly, but c must still sit after b
    const { layer } = layerLanes(N('a', 'b', 'c'), E(['a', 'b'], ['b', 'c'], ['a', 'c']))
    expect([layer.get('a'), layer.get('b'), layer.get('c')]).toEqual([0, 1, 2])
  })

  it('gives fan-out siblings the same layer and different lanes', () => {
    const { layer, lane, maxLane } = layerLanes(N('root', 'x', 'y'), E(['root', 'x'], ['root', 'y']))
    expect(layer.get('x')).toBe(layer.get('y'))
    expect(new Set([lane.get('x'), lane.get('y')])).toEqual(new Set([0, 1]))
    expect(maxLane).toBe(2)
  })

  it('reports one lane for a linear chain', () => {
    const { maxLane, countByLayer } = layerLanes(N('a', 'b'), E(['a', 'b']))
    expect(maxLane).toBe(1)
    expect(countByLayer.get(0)).toBe(1)
  })
})

describe('layoutDag', () => {
  it('lays a linear chain out as one row (LR) — same as the old hardcoded fallback', () => {
    const pos = layoutDag(N('a', 'b', 'c'), E(['a', 'b'], ['b', 'c']))
    expect(pos.a).toEqual({ x: EDITOR_GRID.originX, y: EDITOR_GRID.originY })
    expect(pos.b.x - pos.a.x).toBe(EDITOR_GRID.depth)
    expect(new Set([pos.a.y, pos.b.y, pos.c.y]).size).toBe(1)   // one row
  })

  it('splits a fan-out across the flow axis', () => {
    const pos = layoutDag(N('root', 'x', 'y'), E(['root', 'x'], ['root', 'y']))
    expect(pos.x.x).toBe(pos.y.x)          // same column…
    expect(pos.x.y).not.toBe(pos.y.y)      // …different rows: the branch is visible
  })

  it('centres the trunk against its branches', () => {
    // Top-aligning made a two-way split look like the trunk belonged to the upper branch.
    const pos = layoutDag(N('root', 'x', 'y'), E(['root', 'x'], ['root', 'y']))
    expect(pos.root.y).toBeCloseTo((pos.x.y + pos.y.y) / 2)
  })

  it('TB swaps the axes', () => {
    const lr = layoutDag(N('a', 'b'), E(['a', 'b']), 'LR')
    const tb = layoutDag(N('a', 'b'), E(['a', 'b']), 'TB')
    expect(lr.b.x).toBeGreaterThan(lr.a.x)
    expect(lr.b.y).toBe(lr.a.y)
    expect(tb.b.y).toBeGreaterThan(tb.a.y)
    expect(tb.b.x).toBe(tb.a.x)
  })

  it('covers every node, including one in a cycle', () => {
    const pos = layoutDag(N('a', 'b', 'c'), E(['a', 'b'], ['b', 'c'], ['c', 'b']))
    expect(Object.keys(pos).sort()).toEqual(['a', 'b', 'c'])
    for (const p of Object.values(pos)) {
      expect(Number.isFinite(p.x) && Number.isFinite(p.y)).toBe(true)
    }
  })

  it('handles an empty graph without throwing', () => {
    expect(layoutDag([], [])).toEqual({})
  })
})

describe('LAYOUT_VARIANTS', () => {
  it('offers every direction × spacing combination as one flat, clickable row', () => {
    expect(LAYOUT_VARIANTS.map(v => v.id)).toEqual(['LR', 'LR-compact', 'TB', 'TB-compact'])
    for (const v of LAYOUT_VARIANTS) {
      expect(v.label.length).toBeGreaterThan(0)
      expect(v.icon).toMatch(/^pi-/)
      expect(['LR', 'TB']).toContain(v.direction)
    }
  })

  it('compact tightens spacing without letting node boxes touch', () => {
    // A task node is up to ~182px wide (ChainTaskNode: min-width 140, label max 160, +22 padding), so
    // the flow axis has little room — the saving has to come from `across`. A uniform scale factor
    // would have overlapped horizontally; this asserts the asymmetry is deliberate.
    expect(COMPACT_GRID.depth).toBeLessThan(EDITOR_GRID.depth)
    expect(COMPACT_GRID.depth).toBeGreaterThan(182)
    expect(COMPACT_GRID.across).toBeLessThan(EDITOR_GRID.across * 0.7)
  })

  it('a compact variant really does place nodes closer together', () => {
    const nodes = N('root', 'x', 'y')
    const edges = E(['root', 'x'], ['root', 'y'])
    const normal = layoutDag(nodes, edges, 'LR', EDITOR_GRID)
    const compact = layoutDag(nodes, edges, 'LR', COMPACT_GRID)
    expect(Math.abs(compact.x.y - compact.y.y)).toBeLessThan(Math.abs(normal.x.y - normal.y.y))
    expect(compact.x.x - compact.root.x).toBeLessThan(normal.x.x - normal.root.x)
  })
})
