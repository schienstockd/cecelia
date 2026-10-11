import { describe, it, expect } from 'vitest'
import { parseOverlays, overlayPushConfig, captureOverlayLayers, trackPopSourceKey } from './overlayLayers'

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
  it("carries a track layer's captured colour — null too (by track / speed) — and only a track's", () => {
    const ovs = parseOverlays({
      '(track) (A) Tracks /_tracked': { visible: true, colour: '#ff8800' },
      '(track) (B) Tracks /_tracked': { visible: true, colour: null },
      '(flow) (A) /tcells': { visible: true, colour: '#000000' },
    })
    expect(ovs[0].colour).toBe('#ff8800')
    expect(ovs[1].colour).toBeNull()
    expect('colour' in ovs[2]).toBe(false)
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

describe('captureOverlayLayers', () => {
  type L = { vn: string; popType: string }; type P = { path: string }
  const layer = (vn: string, popType: string, ...paths: string[]) =>
    ({ layer: { vn, popType }, pops: paths.map(path => ({ path })) })
  const capture = (drawn: [string, string | null][]) => captureOverlayLayers<L, P>({
    popLayers: [layer('A', 'flow', '/tcells'), layer('A', 'clust', '/0')],
    pointDrawn: () => true,
    trackVns: ['A'],
    trackclust: [['A', { pops: [{ path: '/meandering' }, { path: '/off' }] }]],
    drawn: new Map(drawn), labelName: 'A',
  })

  it('gives every drawn ribbon layer — cell-track, whole-segmentation, trackclust — its tails\' colour', () => {
    const layers = capture([
      ['A::/tcells', '#00ff00'],          // gating pop: `vn::path` (trackPopSourceKey)
      ['A::clust::/0', null],             // other pop type: `vn::popType::path`; null = by track / speed
      ['A', '#ff8800'],
      ['A::trackclust::/meandering', '#123456'],
    ])
    expect(layers['(track) (A) Tracks /tcells']).toEqual({ visible: true, colour: '#00ff00' })
    expect(layers['(track) (A) Tracks /0']).toEqual({ visible: true, colour: null })
    expect(layers['(track) (A) Tracks /_tracked']).toEqual({ visible: true, colour: '#ff8800' })
    expect(layers['(trackclust) (A) Tracks /meandering']).toEqual({ visible: true, colour: '#123456' })
    expect(layers['(flow) (A) /tcells']).toEqual({ visible: true })     // points carry no colour
    expect(layers['(A) Labels']).toEqual({ visible: true })
    expect(layers['(trackclust) (A) Tracks /off']).toBeUndefined()
    // round-trips through the legend's parser with the tails' colour
    expect(parseOverlays(layers).find(o => o.isTrack && o.path === '/tcells')?.colour).toBe('#00ff00')
  })
  it('skips every ribbon that drew no tails — cell-track, whole-segmentation, trackclust', () => {
    // Shown but empty after the pop / highlight filters, or stood down for trackclust: no track source.
    const layers = capture([])
    expect(layers['(track) (A) Tracks /tcells']).toBeUndefined()
    expect(layers['(track) (A) Tracks /0']).toBeUndefined()
    expect(layers['(track) (A) Tracks /_tracked']).toBeUndefined()
    expect(layers['(trackclust) (A) Tracks /meandering']).toBeUndefined()
    expect(layers['(flow) (A) /tcells']).toEqual({ visible: true })     // points don't depend on tails
    expect(parseOverlays(layers).some(o => o.isTrack)).toBe(false)
  })
})

describe('trackPopSourceKey', () => {
  it('keys gating pops by vn::path and other pop types by vn::popType::path', () => {
    expect(trackPopSourceKey('A', 'flow', '/t')).toBe('A::/t')
    expect(trackPopSourceKey('A', 'trackclust', '/t')).toBe('A::trackclust::/t')
  })
})
