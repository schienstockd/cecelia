import { describe, it, expect } from 'vitest'
import {
  buildMergeOp, buildRemoveOp, buildSplitOp, buildCentroidSplitOp, labelActions,
  opLabel, opDescription, undoLast,
  type LabelOp,
} from './labelCorrection'

describe('buildMergeOp', () => {
  it('needs at least two distinct ids — returns null otherwise', () => {
    // an empty pick, a single-cell pick, and a duplicated-single pick are all "not enough to merge"
    expect(buildMergeOp(0, [])).toBeNull()
    expect(buildMergeOp(0, [3])).toBeNull()
    expect(buildMergeOp(0, [3, 3])).toBeNull()
  })

  it('defaults into = min(ids); accepts an explicit into ∈ ids', () => {
    expect(buildMergeOp(2, [7, 3, 5])).toEqual({ op: 'label.merge', t: 2, ids: [3, 5, 7], into: 3 })
    expect(buildMergeOp(2, [7, 3, 5], 5)).toEqual({ op: 'label.merge', t: 2, ids: [3, 5, 7], into: 5 })
    // an into NOT in ids falls back to the default — a UI that offered it would be a bug, but the
    // engine's validator would reject it anyway; producing a bad op here would just double-fail.
    expect(buildMergeOp(2, [7, 3], 99)).toEqual({ op: 'label.merge', t: 2, ids: [3, 7], into: 3 })
  })

  it('drops background (0) and non-positive ids — the engine rejects them anyway', () => {
    // 0 = background; the mask can't have a negative id. Round through Number(id).
    expect(buildMergeOp(0, [0, 3, 5])).toEqual({ op: 'label.merge', t: 0, ids: [3, 5], into: 3 })
    expect(buildMergeOp(0, [3, -1])).toBeNull()   // single valid id left
  })
})

describe('buildRemoveOp', () => {
  it('needs at least one valid id', () => {
    expect(buildRemoveOp(0, [])).toBeNull()
    expect(buildRemoveOp(0, [0])).toBeNull()      // background is not a valid input
    expect(buildRemoveOp(0, [3])).toEqual({ op: 'label.remove', t: 0, ids: [3] })
    expect(buildRemoveOp(4, [5, 3, 3])).toEqual({ op: 'label.remove', t: 4, ids: [3, 5] })
  })
})

describe('labelActions', () => {
  it('empty selection blocks both actions with helpful reasons', () => {
    const [merge, remove] = labelActions(0, [])
    expect(merge.op).toBeNull()
    expect(remove.op).toBeNull()
    expect(merge.blocked).toMatch(/two or more/)
    expect(remove.blocked).toMatch(/at least one/)
  })

  it('one id: Merge blocked (needs a second), Remove enabled', () => {
    const [merge, remove] = labelActions(3, [7])
    expect(merge.op).toBeNull()
    expect(merge.blocked).toMatch(/second label/)
    expect(remove.op).toEqual({ op: 'label.remove', t: 3, ids: [7] })
    expect(remove.blocked).toBe('')
  })

  it('two+ ids: both enabled — merge defaults to smallest id', () => {
    const [merge, remove] = labelActions(0, [5, 3])
    expect(merge.op).toEqual({ op: 'label.merge', t: 0, ids: [3, 5], into: 3 })
    expect(remove.op).toEqual({ op: 'label.remove', t: 0, ids: [3, 5] })
    expect(merge.blocked).toBe('')
    expect(remove.blocked).toBe('')
  })
})

describe('opLabel + opDescription', () => {
  it('short label for the button, sentence for the tooltip', () => {
    expect(opLabel({ op: 'label.merge', t: 0, ids: [3, 5], into: 3 })).toBe('Merge')
    expect(opLabel({ op: 'label.remove', t: 0, ids: [3] })).toBe('Remove')
    expect(opLabel({ op: 'label.split', t: 0, id: 5, xs: [1, 10], ys: [2, 2] })).toBe('Split')
    expect(opDescription({ op: 'label.merge', t: 2, ids: [3, 5], into: 3 }))
      .toBe('Merge label 5 into 3 at frame 2')
    expect(opDescription({ op: 'label.merge', t: 2, ids: [3, 5, 7], into: 3 }))
      .toBe('Merge labels 5, 7 into 3 at frame 2')
    expect(opDescription({ op: 'label.remove', t: 4, ids: [7] }))
      .toBe('Remove label 7 at frame 4')
    expect(opDescription({ op: 'label.remove', t: 4, ids: [7, 9] }))
      .toBe('Remove labels 7, 9 at frame 4')
    expect(opDescription({ op: 'label.split', t: 3, id: 12, xs: [10, 30, 50], ys: [20, 25, 20] }))
      .toBe('Split label 12 along a 3-point polyline at frame 3')
  })
})

describe('buildSplitOp', () => {
  it('needs a positive id and >=2 vertices of matching xs/ys length', () => {
    expect(buildSplitOp(0, 0, [1, 2], [3, 4])).toBeNull()          // id=0 (background)
    expect(buildSplitOp(0, -1, [1, 2], [3, 4])).toBeNull()         // id<0
    expect(buildSplitOp(0, 5, [1], [3])).toBeNull()                // 1 vertex
    expect(buildSplitOp(0, 5, [1, 2], [3])).toBeNull()             // xs/ys length mismatch
    expect(buildSplitOp(0, 5, [1, -2], [3, 4])).toBeNull()         // negative coord
    expect(buildSplitOp(0, 5, [1, 2], [3, 4]))
      .toEqual({ op: 'label.split', t: 0, id: 5, xs: [1, 2], ys: [3, 4] })
  })
  it('floors non-integer coords (client may pass float centroids)', () => {
    expect(buildSplitOp(0, 5, [1.7, 2.9], [3.1, 4.5]))
      .toEqual({ op: 'label.split', t: 0, id: 5, xs: [1, 2], ys: [3, 4] })
  })
})

describe('buildCentroidSplitOp', () => {
  it('horizontal cut runs left-right through the centroid y', () => {
    const op = buildCentroidSplitOp(2, 7, 100, 50, 'horizontal', 30)!
    expect(op.op).toBe('label.split')
    expect(op.id).toBe(7)
    expect(op.ys).toEqual([50, 50])                                // same y at both endpoints
    expect(op.xs[0]).toBeLessThan(op.xs[1])                        // increasing x
  })
  it('vertical cut runs top-bottom through the centroid x', () => {
    const op = buildCentroidSplitOp(2, 7, 100, 50, 'vertical', 30)!
    expect(op.xs).toEqual([100, 100])
    expect(op.ys[0]).toBeLessThan(op.ys[1])
  })
  it('clamps the leading endpoint to 0 (never negative image px)', () => {
    const op = buildCentroidSplitOp(0, 3, 5, 5, 'horizontal', 100)!
    expect(op.xs[0]).toBe(0)
  })
  it('returns null for a NaN centroid — a label with no overlay row', () => {
    expect(buildCentroidSplitOp(0, 3, NaN, 5, 'horizontal')).toBeNull()
  })
})

describe('undoLast', () => {
  it('returns a copy with the last op removed — immutable', () => {
    const q: LabelOp[] = [
      { op: 'label.remove', t: 0, ids: [1] },
      { op: 'label.merge',  t: 0, ids: [2, 3], into: 2 },
    ]
    const undone = undoLast(q)
    expect(undone).toHaveLength(1)
    expect(undone[0].op).toBe('label.remove')
    expect(q).toHaveLength(2)     // input untouched
  })
})
