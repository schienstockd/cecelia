import { describe, it, expect } from 'vitest'
import {
  buildMergeOp, buildRemoveOp, labelActions,
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
    expect(opDescription({ op: 'label.merge', t: 2, ids: [3, 5], into: 3 }))
      .toBe('Merge label 5 into 3 at frame 2')
    expect(opDescription({ op: 'label.merge', t: 2, ids: [3, 5, 7], into: 3 }))
      .toBe('Merge labels 5, 7 into 3 at frame 2')
    expect(opDescription({ op: 'label.remove', t: 4, ids: [7] }))
      .toBe('Remove label 7 at frame 4')
    expect(opDescription({ op: 'label.remove', t: 4, ids: [7, 9] }))
      .toBe('Remove labels 7, 9 at frame 4')
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
