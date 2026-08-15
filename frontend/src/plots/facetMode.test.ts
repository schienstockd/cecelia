import { describe, it, expect } from 'vitest'
import { facetMode, defaultVis, type VisProps } from './plot'

// `facet` used to be a boolean toggle meaning "one small-multiple panel per series". It became a
// three-way choice once faceting BY IMAGE existed — the cross-image comparison, where each panel is
// one movie and the segmentations/populations overlay inside it.
//
// Two things have to hold, and neither announces itself when broken: a canvas persisted with the old
// boolean must still come up faceted (a plot that silently un-facets on upgrade is the kind of
// regression nobody reports), and the new field must win once it is set.
const vis = (o: Partial<VisProps>): VisProps => ({ ...defaultVis(), ...o })

describe('facetMode', () => {
  it('reads the explicit mode', () => {
    expect(facetMode(vis({ facetBy: 'none' }))).toBe('none')
    expect(facetMode(vis({ facetBy: 'image' }))).toBe('image')
    expect(facetMode(vis({ facetBy: 'series' }))).toBe('series')
  })

  // the migration: a canvas saved before `facetBy` existed carries only the boolean
  it('migrates the legacy boolean — true meant per-series', () => {
    const legacy = { ...defaultVis(), facet: true } as VisProps
    delete (legacy as { facetBy?: unknown }).facetBy
    expect(facetMode(legacy)).toBe('series')

    const legacyOff = { ...defaultVis(), facet: false } as VisProps
    delete (legacyOff as { facetBy?: unknown }).facetBy
    expect(facetMode(legacyOff)).toBe('none')
  })

  // if both are present the NEW field wins — otherwise a user switching a migrated plot to Image
  // would be dragged back to Series by the stale boolean still sitting beside it
  it('prefers facetBy over a stale legacy boolean', () => {
    expect(facetMode({ facetBy: 'image', facet: true } as VisProps)).toBe('image')
    expect(facetMode({ facetBy: 'none', facet: true } as VisProps)).toBe('none')
  })

  it('defaults to none for an absent or empty vis', () => {
    expect(facetMode(null)).toBe('none')
    expect(facetMode(undefined)).toBe('none')
    expect(facetMode({} as VisProps)).toBe('none')
    expect(facetMode(defaultVis())).toBe('none')
  })
})
