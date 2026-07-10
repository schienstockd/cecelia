import { describe, it, expect } from 'vitest'
import { childGateSignature, type FlatPopLite } from './childGateSig'

// The signature drives when GatePlotPanel re-fetches its (server-projected) gate outlines: it must
// move exactly when the DIRECT children of the displayed parent, or their gates, change — and stay
// put for anything else (siblings, grandchildren, the parent's own gate). Deleting a child pop that
// left its outline lingering was the bug this guards.
describe('childGateSignature', () => {
  const g = (x: number) => ({ kind: 'rectangle', x_min: 0, x_max: x })
  const pops: FlatPopLite[] = [
    { path: '/A', parent: 'root' },
    { path: '/A/x', parent: '/A', gate: g(1) },
    { path: '/A/y', parent: '/A', gate: g(2) },
    { path: '/A/x/deep', parent: '/A/x', gate: g(9) },   // grandchild of /A
    { path: '/B', parent: 'root', gate: g(5) },           // sibling subtree
  ]

  it('includes only the direct children of the parent', () => {
    const sig = childGateSignature(pops, '/A')
    expect(sig).toContain('/A/x')
    expect(sig).toContain('/A/y')
    expect(sig).not.toContain('/A/x/deep')   // grandchild excluded
    expect(sig).not.toContain('"path":"/B"')
    expect(sig).not.toContain('/A:')         // the parent itself excluded
  })

  it('is empty when the parent has no children', () => {
    expect(childGateSignature(pops, '/A/y')).toBe('')
    expect(childGateSignature([], 'root')).toBe('')
  })

  it('changes when a child is deleted (the reported bug)', () => {
    const before = childGateSignature(pops, '/A')
    const after = childGateSignature(pops.filter(p => p.path !== '/A/y'), '/A')
    expect(after).not.toBe(before)
  })

  it('changes when a child is added', () => {
    const before = childGateSignature(pops, '/A')
    const after = childGateSignature([...pops, { path: '/A/z', parent: '/A', gate: g(3) }], '/A')
    expect(after).not.toBe(before)
  })

  it("changes when a child's gate geometry changes", () => {
    const before = childGateSignature(pops, '/A')
    const edited = pops.map(p => p.path === '/A/x' ? { ...p, gate: g(42) } : p)
    expect(childGateSignature(edited, '/A')).not.toBe(before)
  })

  it("does NOT change when a grandchild's or the parent's own gate changes", () => {
    const before = childGateSignature(pops, '/A')
    const grandEdit = pops.map(p => p.path === '/A/x/deep' ? { ...p, gate: g(99) } : p)
    const parentEdit = pops.map(p => p.path === '/A' ? { ...p, gate: g(7) } : p)
    expect(childGateSignature(grandEdit, '/A')).toBe(before)
    expect(childGateSignature(parentEdit, '/A')).toBe(before)
  })

  it('serialises a gate-less (filter/cluster) pop stably as null', () => {
    const filterPops: FlatPopLite[] = [{ path: '/A/c', parent: '/A' }]
    expect(childGateSignature(filterPops, '/A')).toBe('/A/c:null')
  })
})
