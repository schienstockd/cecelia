import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useLinkedSelectionStore } from './linkedSelection'

describe('useLinkedSelectionStore', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('starts empty', () => {
    const s = useLinkedSelectionStore()
    expect(s.isEmpty).toBe(true)
    expect(s.bag).toBeNull()
  })

  it('set + clear + isEmpty flip', () => {
    const s = useLinkedSelectionStore()
    s.set({ scope: 'tracks', ids: [1, 2, 3], source: 'test' })
    expect(s.isEmpty).toBe(false)
    expect(s.bag).toEqual({ scope: 'tracks', ids: [1, 2, 3], source: 'test' })
    s.clear()
    expect(s.isEmpty).toBe(true)
    expect(s.bag).toBeNull()
  })

  it('an empty ids array clears the bag rather than storing {ids: []}', () => {
    // Producers with a brush that happens to select nothing get idle-state semantics without a
    // special branch. The dim rule only fires on non-empty; storing `[]` would be a distinct
    // "cleared but not idle" the plan explicitly rejected (Decision 5).
    const s = useLinkedSelectionStore()
    s.set({ scope: 'tracks', ids: [], source: 'test' })
    expect(s.bag).toBeNull()
    expect(s.isEmpty).toBe(true)
  })

  it('set defensive-copies ids', () => {
    // A producer that mutates its input array after set() must NOT retroactively change what
    // subscribers see. Same idiom as `TrackHighlight` setters in `stores/viewer.ts`.
    const s = useLinkedSelectionStore()
    const src = [1, 2, 3]
    s.set({ scope: 'tracks', ids: src, source: 'test' })
    src.push(4)
    expect(s.bag?.ids).toEqual([1, 2, 3])
  })

  it('scope + sourcePlotId round-trip', () => {
    const s = useLinkedSelectionStore()
    s.set({ scope: 'cells', ids: [10], source: 'panel', sourcePlotId: 'p1' })
    expect(s.bag?.scope).toBe('cells')
    expect(s.bag?.sourcePlotId).toBe('p1')
  })

  it('sourcePlotId is omitted from the bag when not passed', () => {
    // Absent-when-unset (same shape rule as `outcome` in the Blackboard list API) — the
    // affordance can render "unknown source" as an absent chip rather than an empty one.
    const s = useLinkedSelectionStore()
    s.set({ scope: 'tracks', ids: [1], source: 'button' })
    expect(s.bag).not.toHaveProperty('sourcePlotId')
  })

  it('set overwrites — a fresh brush replaces the previous selection', () => {
    const s = useLinkedSelectionStore()
    s.set({ scope: 'tracks', ids: [1], source: 'a' })
    s.set({ scope: 'cells', ids: [99, 100], source: 'b' })
    expect(s.bag?.scope).toBe('cells')
    expect(s.bag?.ids).toEqual([99, 100])
    expect(s.bag?.source).toBe('b')
  })
})
