import { describe, it, expect } from 'vitest'
import { useCanvasShareSelection } from './useCanvasShareSelection'

describe('useCanvasShareSelection', () => {
  it('starts inactive with no selection', () => {
    const s = useCanvasShareSelection()
    expect(s.active.value).toBe(false)
    expect(s.count.value).toBe(0)
    expect(s.has(1)).toBe(false)
  })

  it('begin() enters share mode with an empty set, end() exits', () => {
    const s = useCanvasShareSelection()
    s.begin()
    expect(s.active.value).toBe(true)
    expect(s.count.value).toBe(0)
    s.end()
    expect(s.active.value).toBe(false)
  })

  it('add / remove / toggle track the set', () => {
    const s = useCanvasShareSelection()
    s.begin()
    s.add(1); s.add(2); s.add(1)
    expect(s.count.value).toBe(2)
    expect(s.has(1)).toBe(true)
    expect(s.has(2)).toBe(true)
    s.remove(1)
    expect(s.has(1)).toBe(false)
    s.toggle(2); s.toggle(3)
    expect(s.has(2)).toBe(false)
    expect(s.has(3)).toBe(true)
  })

  it('set(iter) replaces the selection wholesale (drag commit)', () => {
    const s = useCanvasShareSelection()
    s.begin()
    s.add(1); s.add(2)
    s.set([3, 4, 5])
    expect(s.count.value).toBe(3)
    expect(s.has(1)).toBe(false)
    expect(s.has(3)).toBe(true)
  })

  it('begin() resets the selection — previous entry does not leak', () => {
    const s = useCanvasShareSelection()
    s.begin(); s.add(1); s.add(2)
    s.end()
    s.begin()
    expect(s.count.value).toBe(0)
  })

  it('mutations are no-ops when inactive (defensive against a stray drag)', () => {
    const s = useCanvasShareSelection()
    s.add(1); s.toggle(2); s.remove(3)
    expect(s.active.value).toBe(false)
    expect(s.count.value).toBe(0)
  })
})
