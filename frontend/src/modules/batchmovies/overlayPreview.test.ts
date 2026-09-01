import { describe, it, expect } from 'vitest'
import {
  buildOverlayScene, derivedOverlayFlags, popsForConfig, renderOverlayPreview,
  ALL_TRACKS_GREY, PREVIEW_PALETTE,
} from './overlayPreview'

// The rules the preview mirrors — locked ALSO in the backend
// (`api/test/runtests.jl` → "movie rail — offline overlay-config translator"). If either drifts
// the two fixtures move together or one of them fails, which is the point of duplicating them.

describe('derivedOverlayFlags — locks the PR #751 rules', () => {
  it('allTracks = showTracks && !showPops (pops wins when both)', () => {
    // both on → pops wins; whole-seg grey is dropped
    expect(derivedOverlayFlags({ showTracks: true, showPopulations: true }).allTracks).toBe(false)
    // tracks alone → whole-seg mode active
    expect(derivedOverlayFlags({ showTracks: true }).allTracks).toBe(true)
    // pops alone → not whole-seg
    expect(derivedOverlayFlags({ showPopulations: true }).allTracks).toBe(false)
    // neither → not whole-seg
    expect(derivedOverlayFlags({}).allTracks).toBe(false)
  })

  it('includeTracks fires under EITHER showTracks or showGatedTracks', () => {
    expect(derivedOverlayFlags({ showTracks: true }).includeTracks).toBe(true)
    expect(derivedOverlayFlags({ showGatedTracks: true }).includeTracks).toBe(true)
    expect(derivedOverlayFlags({ showTracks: true, showGatedTracks: true }).includeTracks).toBe(true)
    expect(derivedOverlayFlags({ showPopulations: true }).includeTracks).toBe(false)
  })

  it('showPoints fires under showPops OR showTracks', () => {
    expect(derivedOverlayFlags({ showPopulations: true }).showPoints).toBe(true)
    expect(derivedOverlayFlags({ showTracks: true }).showPoints).toBe(true)
    expect(derivedOverlayFlags({}).showPoints).toBe(false)
    expect(derivedOverlayFlags({ showGatedTracks: true }).showPoints).toBe(false)
  })
})

describe('renderOverlayPreview — the three overlay-author branches', () => {
  const scene = buildOverlayScene()

  it('showPops + tracks + gated → coloured points (not grey) + ribbons for hasTracks pops', () => {
    const r = renderOverlayPreview(
      { showPopulations: true, showTracks: true, showGatedTracks: true }, scene)
    expect(r.points.length).toBeGreaterThan(0)
    // A pop-coloured render must NOT paint anything with the all-tracks grey — that would mean the
    // preview took the whole-seg branch when it shouldn't (the fXgbTl bug in PR #751).
    expect(r.points.every(p => p.colour !== ALL_TRACKS_GREY)).toBe(true)
    // Ribbons drew for hasTracks pops (first half of scene.pops). They must be coloured, not grey.
    expect(r.ribbons.length).toBeGreaterThan(0)
    expect(r.ribbons.every(rib => rib.colour !== ALL_TRACKS_GREY)).toBe(true)
  })

  it('showTracks alone → whole-seg grey, ribbons uniform grey', () => {
    const r = renderOverlayPreview({ showTracks: true }, scene)
    expect(r.points.length).toBeGreaterThan(0)
    expect(r.points.every(p => p.colour === ALL_TRACKS_GREY)).toBe(true)
    // includeTracks fires under showTracks (PR #751) → ribbons drawn AND uniformly grey
    expect(r.ribbons.length).toBeGreaterThan(0)
    expect(r.ribbons.every(rib => rib.colour === ALL_TRACKS_GREY)).toBe(true)
  })

  it('showPops alone → points, no ribbons', () => {
    const r = renderOverlayPreview({ showPopulations: true }, scene)
    expect(r.points.length).toBeGreaterThan(0)
    expect(r.ribbons.length).toBe(0)
  })

  it('nothing on → empty render with a caption', () => {
    const r = renderOverlayPreview({}, scene)
    expect(r.points).toEqual([])
    expect(r.ribbons).toEqual([])
    expect(r.caption).toBe('nothing on')
  })

  it('showPops with a filter that matches nothing → empty + explanatory caption', () => {
    // popsForConfig hashes paths to scene pop indices, so an empty filter is "all", not "none".
    // To get "matches nothing" we'd need a pre-filter of `scene.pops.length` slots and remove all
    // — instead assert the "no pops selected" caption path via `showPops` with no pops in scene.
    const empty: typeof scene = { cells: [], pops: [] }
    const r = renderOverlayPreview({ showPopulations: true }, empty)
    expect(r.points).toEqual([])
    expect(r.caption).toBe('no pops selected')
  })

  it('labelValueNames non-empty → every drawn point is ringed', () => {
    const r = renderOverlayPreview(
      { showPopulations: true, labelValueNames: ['flowTom'] }, scene)
    expect(r.points.length).toBeGreaterThan(0)
    expect(r.points.every(p => p.ringed === true)).toBe(true)
  })

  it('corner overlays follow the config flags 1:1', () => {
    const r = renderOverlayPreview({
      showPopulations: true, showTimestamp: true, showScaleBar: false,
      titleCard: { enabled: true },
    }, scene)
    expect(r.corners.showTimestamp).toBe(true)
    expect(r.corners.showScaleBar).toBe(false)
    expect(r.corners.showTitleChip).toBe(true)
  })
})

describe('popsForConfig — hashes real batch paths to scene indices', () => {
  const scene = buildOverlayScene()

  it('empty filter → every scene pop selected', () => {
    const s = popsForConfig({}, scene)
    expect(s.size).toBe(scene.pops.length)
  })

  it('same path always maps to the same scene index', () => {
    const a = popsForConfig({ popsFilter: ['/qc/CD169-'] }, scene)
    const b = popsForConfig({ popsFilter: ['/qc/CD169-'] }, scene)
    expect([...a]).toEqual([...b])
    expect(a.size).toBe(1)
  })

  it('different paths map to distinguishable indices (best-effort — small palette)', () => {
    // 6 pops → collisions possible, but two very different paths should NOT always collide.
    const a = popsForConfig({ popsFilter: ['/qc/CD169-'] }, scene)
    const b = popsForConfig({ popsFilter: ['/nuc+/highDp'] }, scene)
    expect([...a][0]).not.toBe([...b][0])
  })

  it('exports a palette (used by the schematic, kept OUT of the app palette)', () => {
    // Locked so a future edit that empties the palette fails a test — the schematic depends on it
    // having distinguishable entries.
    expect(PREVIEW_PALETTE.length).toBeGreaterThanOrEqual(4)
  })
})
