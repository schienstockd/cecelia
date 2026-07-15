import { describe, it, expect } from 'vitest'
import { parseOverlays, overlayPushConfig } from './overlayLayers'

describe('parseOverlays', () => {
  it('separates track ribbons from population points, ignores channels + labels', () => {
    const layers = {
      'gBT': {}, 'SHG': {},                                   // channels (plain names) — ignored
      '(A) Labels': {},                                       // label mask (one group) — ignored
      '(track) (A) Tracks /_tracked': {},
      '(track) (A) Tracks /directed': {},
      '(trackclust) (A) Tracks /meandering': {},
      '(flow) (A) /tcells': {},
      '(clust) (B) /0': {},
    }
    const ovs = parseOverlays(layers)
    expect(ovs).toContainEqual({ popType: 'track', valueName: 'A', path: '/_tracked', isTrack: true })
    expect(ovs).toContainEqual({ popType: 'track', valueName: 'A', path: '/directed', isTrack: true })
    expect(ovs).toContainEqual({ popType: 'trackclust', valueName: 'A', path: '/meandering', isTrack: true })
    expect(ovs).toContainEqual({ popType: 'flow', valueName: 'A', path: '/tcells', isTrack: false })
    expect(ovs).toContainEqual({ popType: 'clust', valueName: 'B', path: '/0', isTrack: false })
    expect(ovs).toHaveLength(5)                               // channels + labels excluded
  })
  it('is safe on empty / missing input', () => {
    expect(parseOverlays(null)).toEqual([])
    expect(parseOverlays({})).toEqual([])
  })
})

describe('overlayPushConfig', () => {
  it('derives the re-push config from parsed overlays', () => {
    const cfg = overlayPushConfig(parseOverlays({
      '(track) (A) Tracks /_tracked': {},
      '(track) (B) Tracks /_tracked': {},
      '(track) (A) Tracks /directed': {},        // gated track pop
      '(trackclust) (A) Tracks /meandering': {},
      '(flow) (A) /tcells': {},
    }))
    expect(cfg.trackValueNames.sort()).toEqual(['A', 'B'])   // whole-segmentation tracks
    expect(cfg.showGatedTracks).toBe(true)                   // a non-_tracked track pop present
    expect(cfg.showTrackclust).toBe(true)
    expect(cfg.popTypes).toEqual(['flow'])
  })
  it('is empty when nothing was overlaid', () => {
    const cfg = overlayPushConfig([])
    expect(cfg).toEqual({ trackValueNames: [], showGatedTracks: false, showTrackclust: false, popTypes: [] })
  })
})
