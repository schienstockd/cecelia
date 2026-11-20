import { describe, it, expect } from 'vitest'
import { toggleStep, recordStep, takeStep, writePlan, sameIds, ASSIGN_HISTORY_LIMIT,
         type AssignHistory, type Assign } from './clusterAssignHistory'

const fresh = (): AssignHistory => ({ undo: [], redo: [] })
// apply a write plan to a state, refusing a cluster in two pops at once (as the server does)
function apply(state: Assign, plan: Array<[string, number[]]>): Assign {
  const s = { ...state }
  for (const [p, ids] of plan) {
    for (const id of ids)
      if (Object.keys(s).some(q => q !== p && s[q].includes(id))) throw new Error(`cluster_taken ${id}`)
    s[p] = ids
  }
  return s
}

describe('toggleStep', () => {
  const cur: Assign = { a: [1, 2], b: [5] }
  it('adds a free cluster to the target only', () => {
    expect(toggleStep(cur, 'b', 3)).toEqual({ before: { b: [5] }, after: { b: [3, 5] } })
  })
  it('takes a cluster out when the target already holds it', () => {
    expect(toggleStep(cur, 'a', 2)).toEqual({ before: { a: [1, 2] }, after: { a: [1] } })
  })
  it('a move touches both populations', () => {
    expect(toggleStep(cur, 'b', 1)).toEqual({ before: { b: [5], a: [1, 2] }, after: { b: [1, 5], a: [2] } })
  })
})

describe('takeStep', () => {
  it('undo returns the before lists, redo the after lists', () => {
    const h = fresh(), step = toggleStep({ a: [1], b: [] }, 'b', 1)
    recordStep(h, step)
    expect(takeStep(h, 'undo', { a: [], b: [1] })).toEqual({ a: [1], b: [] })
    expect(h.redo).toHaveLength(1)
    expect(takeStep(h, 'redo', { a: [1], b: [] })).toEqual({ a: [], b: [1] })
  })
  it('drops a step (and the rest of its stack) when the pops changed since', () => {
    const h = fresh()
    recordStep(h, toggleStep({ a: [] }, 'a', 1))
    recordStep(h, toggleStep({ a: [1] }, 'a', 2))
    // someone else set a to [7] after the last click
    expect(takeStep(h, 'undo', { a: [7] })).toBeNull()
    expect(h.undo).toHaveLength(0)
    expect(h.redo).toHaveLength(0)
  })
  it('a deleted population makes its step stale', () => {
    const h = fresh()
    recordStep(h, toggleStep({ a: [] }, 'a', 1))
    expect(takeStep(h, 'undo', {})).toBeNull()
  })
  it('a new click clears redo; the stack is capped', () => {
    const h = fresh()
    recordStep(h, toggleStep({ a: [] }, 'a', 1))
    takeStep(h, 'undo', { a: [1] })
    recordStep(h, toggleStep({ a: [] }, 'a', 2))
    expect(h.redo).toHaveLength(0)
    for (let i = 0; i < ASSIGN_HISTORY_LIMIT + 5; i++) recordStep(h, toggleStep({ a: [] }, 'a', i))
    expect(h.undo).toHaveLength(ASSIGN_HISTORY_LIMIT)
  })
})

describe('writePlan', () => {
  it('undoing a move never puts a cluster in two pops (drop before add)', () => {
    // click moved 1 from a → b; undo must take 1 out of b BEFORE giving it back to a,
    // whichever order the step lists the populations in
    const now: Assign = { a: [2], b: [1, 5] }
    for (const target of [{ b: [5], a: [1, 2] }, { a: [1, 2], b: [5] }]) {
      const out = apply(now, writePlan(now, target))
      expect(sameIds(out.a, [1, 2]) && sameIds(out.b, [5])).toBe(true)
    }
  })
  it('a swap between two pops goes through', () => {
    const now: Assign = { a: [1], b: [2] }
    const out = apply(now, writePlan(now, { a: [2], b: [1] }))
    expect(out).toEqual({ a: [2], b: [1] })
  })
  it('skips pops already at their target', () => {
    expect(writePlan({ a: [1], b: [2] }, { a: [1], b: [2, 3] })).toEqual([['b', [2, 3]]])
  })
})
