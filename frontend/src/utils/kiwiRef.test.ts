import { describe, expect, it } from 'vitest'
import schema from '../lib/kiwiRef.schema.json'
import { KIWI_REF_KINDS, refsFromCaptureAddress, type KiwiRef } from './kiwiRef'

// One MINIMAL (required fields only) and one FULL (every field) sample per kind, each typed as KiwiRef
// — so the TS union must accept them (compile time), and the schema must agree on exactly those
// fields (below). A field added to either side without the other fails here.
const MINIMAL: KiwiRef[] = [
  { kind: 'project' },
  { kind: 'set', setUid: 's' },
  { kind: 'image', imageUid: 'i' },
  { kind: 'population', imageUid: 'i', valueName: 'B', popPath: '/a' },
  { kind: 'cells', imageUid: 'i', valueName: 'B', labelIds: [1] },
  { kind: 'tracks', imageUid: 'i', valueName: 'B', trackIds: [1] },
  { kind: 'viewer', imageUid: 'i' },
  { kind: 'plot', plotId: 'p' },
  { kind: 'tile', imageUid: 'i', valueName: 'B', cellId: 'B3' },
  { kind: 'capture', captureId: 'cap-1' },
  { kind: 'task', funName: 'f' },
  { kind: 'ui', anchor: 'viewer.play' },
  { kind: 'blackboard', entryId: 'bb-1' },
]
const FULL: KiwiRef[] = [
  ...MINIMAL.filter((r) => !['viewer', 'plot', 'tile', 'blackboard'].includes(r.kind)),
  { kind: 'viewer', imageUid: 'i', t: 0, z: 0 },
  { kind: 'plot', plotId: 'p', u: 0.5, v: 0.5 },
  { kind: 'tile', imageUid: 'i', valueName: 'B', cellId: 'B3', t: 0, z: 0 },
  { kind: 'blackboard', entryId: 'bb-1', version: 2 },
]

type Def = { required: string[]; properties: Record<string, unknown> }
const defs = schema.definitions as unknown as Record<string, Def>
const sorted = (xs: string[]) => [...xs].sort()

describe('KiwiRef — TS mirror of lib/kiwiRef.schema.json', () => {
  it('lists every schema kind, and the samples cover each once', () => {
    expect(sorted(KIWI_REF_KINDS)).toEqual(sorted(MINIMAL.map((r) => r.kind)))
    expect(sorted(KIWI_REF_KINDS)).toEqual(sorted(FULL.map((r) => r.kind)))
  })

  it('required fields match the minimal samples', () => {
    for (const r of MINIMAL) expect(sorted(defs[r.kind].required), r.kind).toEqual(sorted(Object.keys(r)))
  })

  it('all fields match the full samples', () => {
    for (const r of FULL) expect(sorted(Object.keys(defs[r.kind].properties)), r.kind).toEqual(sorted(Object.keys(r)))
  })
})

describe('refsFromCaptureAddress', () => {
  it('a viewer capture becomes a viewer ref at its position', () => {
    expect(refsFromCaptureAddress({ projectUid: 'p', imageUid: 'i', t: 4, z: 2 }))
      .toEqual([{ kind: 'viewer', imageUid: 'i', t: 4, z: 2 }])
  })
  it('a slab capture takes the start of its t range', () => {
    expect(refsFromCaptureAddress({ projectUid: 'p', imageUid: 'i', t: [3, 9] }))
      .toEqual([{ kind: 'viewer', imageUid: 'i', t: 3 }])
  })
  it('a UI capture becomes a ui ref; a plot spec is not a live plot ref', () => {
    expect(refsFromCaptureAddress({ projectUid: 'p', domAnchor: 'nav:/analysis', plotSpec: { specId: 'summary' } }))
      .toEqual([{ kind: 'ui', anchor: 'nav:/analysis' }])
  })
  it('nothing addressable → no refs', () => {
    expect(refsFromCaptureAddress({ projectUid: 'p' })).toEqual([])
  })
})
