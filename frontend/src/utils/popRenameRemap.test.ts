import { describe, it, expect } from 'vitest'
import { makePopPathRemap, remapPopKeys, type PopIdent } from './popRenameRemap'

const P = (key: string, uid: string): PopIdent => ({ key, uid })

describe('makePopPathRemap', () => {
  it('preserves a rename by following the uid', () => {
    const old = [P('/A', 'u1'), P('/B', 'u2')]
    const next = [P('/A2', 'u1'), P('/B', 'u2')]
    const remap = makePopPathRemap(old, next)
    expect(remap('/A')).toBe('/A2')
    expect(remap('/B')).toBe('/B')
  })

  it('drops a delete (uid vanished)', () => {
    const old = [P('/A', 'u1'), P('/B', 'u2')]
    const next = [P('/A', 'u1')]
    const remap = makePopPathRemap(old, next)
    expect(remap('/B')).toBeNull()
  })

  it('does not surface an add — nothing to remap because add is not in the input', () => {
    const old = [P('/A', 'u1')]
    const next = [P('/A', 'u1'), P('/C', 'u3')]
    const remap = makePopPathRemap(old, next)
    // /A survives; /C is not a key we can be asked about (nothing highlights it yet).
    expect(remap('/A')).toBe('/A')
    // an unknown key falls through path fallback: /C IS in new keys, so it survives — but the
    // rename-remap contract does not "auto-highlight" adds; the caller supplies the highlight
    // list, which by definition does not include /C. This assertion pins the fallback shape.
    expect(remap('/C')).toBe('/C')
  })

  it('falls back to path presence for an unknown key (first mount with persisted highlights)', () => {
    // On first arrival of the popmap, oldItems is empty — every persisted highlight is unknown.
    // Pre-uid pruner behaviour: intersect with the new set. Preserved here.
    const remap = makePopPathRemap([], [P('/A', 'u1'), P('/B', 'u2')])
    expect(remap('/A')).toBe('/A')
    expect(remap('/gone')).toBeNull()
  })

  it('falls back to path presence for synthetic pops with no uid', () => {
    // SegmentationPops docstring: uid is empty for `/labels`, the "all cells" root, `_tracked` sets.
    const old = [P('/labels', ''), P('/A', 'u1')]
    const next = [P('/labels', ''), P('/A', 'u1')]
    const remap = makePopPathRemap(old, next)
    expect(remap('/labels')).toBe('/labels')
  })

  it('drops a synthetic pop whose path vanished', () => {
    const old = [P('/labels', '')]
    const next: PopIdent[] = []
    const remap = makePopPathRemap(old, next)
    expect(remap('/labels')).toBeNull()
  })

  it('handles the reparent case (path changes, uid preserved) the same as rename', () => {
    // move_pop! also rewrites the path but keeps the uid — the remap treats it identically.
    const old = [P('/qc/A', 'u1')]
    const next = [P('/A', 'u1')]
    const remap = makePopPathRemap(old, next)
    expect(remap('/qc/A')).toBe('/A')
  })
})

describe('remapPopKeys', () => {
  it('remaps, drops deletes, drops unknown-and-absent, preserves order', () => {
    const remap = makePopPathRemap(
      [P('/A', 'u1'), P('/B', 'u2'), P('/C', 'u3')],
      [P('/A2', 'u1'),                 P('/C', 'u3')],
    )
    expect(remapPopKeys(['/A', '/B', '/C'], remap)).toEqual(['/A2', '/C'])
  })

  it('is a no-op when nothing changed', () => {
    const items = [P('/A', 'u1'), P('/B', 'u2')]
    const remap = makePopPathRemap(items, items)
    expect(remapPopKeys(['/A', '/B'], remap)).toEqual(['/A', '/B'])
  })
})
