import { describe, it, expect } from 'vitest'
import { POP_TYPE_REGISTRY, STORED_CELL_POP_TYPES, TRACK_FAMILY_POP_TYPES, popTypeInfo } from '../plots/popTypeRegistry'
import { CELL_POP_TYPES } from './overlayAutoShow'
import { isClusterPopType, isGatingPopType, clusterMeasurePrefix } from './clusterMeasure'
import { isTrackPopType } from './measureGroups'
import { popTypeLabel } from '../plots/popTypes'

// The frontend's pop-type questions all answer from the registry (popTypeRegistry.json, written by
// Julia's `write_pop_type_registry_json()`; app/test/suite/pop_type_registry.jl pins the JSON to it).
describe('pop-type registry', () => {
  it('answers the predicates the hand-written lists used to', () => {
    expect(POP_TYPE_REGISTRY.filter(i => isTrackPopType(i.name)).map(i => i.name)).toEqual(['track', 'trackclust'])
    expect(POP_TYPE_REGISTRY.filter(i => isClusterPopType(i.name)).map(i => i.name))
      .toEqual(['clust', 'trackclust', 'region'])
    expect(POP_TYPE_REGISTRY.filter(i => isGatingPopType(i.name)).map(i => i.name)).toEqual(['flow', 'track'])
    expect(clusterMeasurePrefix('region')).toBe('regions.')
    expect(clusterMeasurePrefix('trackclust')).toBe('clusters.')
    expect(clusterMeasurePrefix('flow')).toBe('clusters.')
    expect(isTrackPopType(undefined)).toBe(false)
    expect(isClusterPopType('bogus')).toBe(false)
  })

  it('keeps the track plots picker order (the first is the default)', () => {
    expect(TRACK_FAMILY_POP_TYPES).toEqual(['live', 'track', 'trackclust'])
  })

  it('labels every type, branch included', () => {
    expect(popTypeLabel({ popType: 'flow', granularity: 'cell' })).toBe('Gated')
    expect(popTypeLabel({ popType: 'trackclust', granularity: 'track' })).toBe('Track clusters')
    expect(popTypeInfo('branch')?.label).toBe('Branches')
    expect(POP_TYPE_REGISTRY.every(i => i.label.length > 0)).toBe(true)
  })

  it('the viewer point-overlay types are the stored cell-grained types', () => {
    expect([...CELL_POP_TYPES]).toEqual(STORED_CELL_POP_TYPES)
  })
})

// ── Ratchet: no new hand-written list of pop types ───────────────────────────────────────────────
// A comma-separated run of two pop-type literals, or an `=== 'a' || x === 'b'` chain, is a list that
// will drift from the registry. Read the registry instead; a deliberate subset says why on its line
// (or within the 3 lines above) with `POP-TYPES-OK: <reason>`. `labels` is left out of the pattern — it is also
// a field and a scope name, and no list of it alone has drifted.
const PT = "(?:flow|live|track|clust|trackclust|region|branch)"
const LIST = new RegExp(`'(${PT})'\\s*,\\s*'(${PT})'`)
const CHAIN = new RegExp(`===\\s*'(${PT})'\\s*\\|\\|\\s*[\\w.]+\\s*===\\s*'(${PT})'`)

export function popTypeListOffenders(sources: Record<string, string>): string[] {
  const out: string[] = []
  for (const [path, src] of Object.entries(sources)) {
    if (path.endsWith('.test.ts') || path.endsWith('/plots/popTypeRegistry.ts')) continue
    const lines = src.split('\n')
    lines.forEach((line, i) => {
      const t = line.trim()
      if (t.startsWith('//') || t.startsWith('*') || t.startsWith('/*')) return
      const code = line.replace(/\/\/.*$/, '')        // a trailing comment is prose, not a list
      const m = LIST.exec(code) ?? CHAIN.exec(code)
      if (!m || m[1] === m[2]) return
      if (lines.slice(Math.max(0, i - 3), i + 1).some(l => l.includes('POP-TYPES-OK:'))) return
      out.push(`${path}:${i + 1}: ${t}`)
    })
  }
  return out
}

describe('pop-type registry ratchet', () => {
  const SRC = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>

  it('the glob resolved', () => { expect(Object.keys(SRC).length).toBeGreaterThan(100) })

  it('flags a list and a chain, honours the marker', () => {
    expect(popTypeListOffenders({ '/src/a.ts': "const X = ['flow', 'track']" })).toHaveLength(1)
    expect(popTypeListOffenders({ '/src/a.ts': "return p === 'clust' || p === 'region'" })).toHaveLength(1)
    expect(popTypeListOffenders({ '/src/a.ts': "const X = ['flow', 'track'] // POP-TYPES-OK: why" })).toHaveLength(0)
    expect(popTypeListOffenders({ '/src/a.ts': "// POP-TYPES-OK: why\nconst X = ['flow', 'track']" })).toHaveLength(0)
    expect(popTypeListOffenders({ '/src/a.ts': "const t: 'clust' | 'trackclust' = 'clust'" })).toHaveLength(0)
  })

  it('no hand-written pop-type list outside the registry', () => {
    expect(popTypeListOffenders(SRC)).toEqual([])
  })
})
