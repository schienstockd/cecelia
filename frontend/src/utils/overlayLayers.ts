// Parse the viewer overlay layers out of a captured view snapshot. The bridge names overlay layers
// "(popType) (valueName) {path}" for population POINTS and "(popType) (valueName) Tracks {path}" for
// TRACK ribbons (see viewer_bridge pop_layer_name / track_layer_name); image channels are plain-named
// and "(vn) Labels" is the label mask. From these names alone we can tell which overlays were on when
// the frame was captured — the durable source for restoring them (zoom-to-source) and for the strip
// legend. Pure → unit-tested.

// `colour` — only on a TRACK layer the viewer captured it for: the colour its tails are drawn in
// (null = coloured by track / speed). Absent on older snapshots, where the name is all there is.
export interface ParsedOverlay {
  popType: string; valueName: string; path: string; isTrack: boolean; colour?: string | null
}

// "(popType) (vn) rest" — two parenthesised groups then the remainder. "(vn) Labels" has ONE group so
// it doesn't match (labels aren't a pop/track overlay). Non-greedy groups so a path with spaces is kept.
const OVERLAY_RE = /^\(([^)]+)\) \(([^)]+)\) (.+)$/

export function parseOverlays(layers: Record<string, unknown> | null | undefined): ParsedOverlay[] {
  const out: ParsedOverlay[] = []
  for (const [name, layer] of Object.entries(layers ?? {})) {
    const m = OVERLAY_RE.exec(name)
    if (!m) continue
    const [, popType, valueName, rest] = m
    const isTrack = rest.startsWith('Tracks ')
    const ov: ParsedOverlay = { popType, valueName, path: isTrack ? rest.slice('Tracks '.length) : rest, isTrack }
    const c = (layer as { colour?: unknown } | null)?.colour
    if (isTrack && (c === null || typeof c === 'string')) ov.colour = c
    out.push(ov)
  }
  return out
}

/** The Tracks-legend key of a pop's cell-track ribbon — `vn::path` for gating pops (the key the
 *  colour overrides were always stored under), `vn::popType::path` for the other cell pop types; the
 *  trackclust form is `vn::trackclust::path`. The movie builds the same keys (`viewer_overlay_closure`). */
export const trackPopSourceKey = (vn: string, popType: string, path: string) =>
  popType === 'flow' ? `${vn}::${path}` : `${vn}::${popType}::${path}`

export type CapturedLayer = { visible: true; colour?: string | null }

/** The inverse of `parseOverlays`: name the overlay layers the viewer DREW, for the capture snapshot.
 *  Every ribbon layer — whole-segmentation, cell-track, trackclust — carries the colour its tails are
 *  drawn in (`drawn`, keyed by track source; null = by track / speed) when it drew, so the legend
 *  swatch is the tails' and not the pop's. */
export function captureOverlayLayers<L extends { vn: string; popType: string }, P extends { path: string }>(o: {
  popLayers: { layer: L; pops: P[] }[]
  pointDrawn: (l: L, p: P) => boolean
  ribbonDrawn: (l: L, p: P) => boolean
  trackVns: Iterable<string>
  trackclust: Iterable<[string, { pops?: { path: string; show: boolean }[] } | null | undefined]>
  drawn: Map<string, string | null>
  labelName: string
}): Record<string, CapturedLayer> {
  const layers: Record<string, CapturedLayer> = {}
  const withColour = (key: string): CapturedLayer =>
    o.drawn.has(key) ? { visible: true, colour: o.drawn.get(key) } : { visible: true }
  // Point pops: every population layer the viewer draws — each (segmentation, pop type) — keyed to
  // the segmentation its pops were authored on, which is what the server resolves the layer names
  // against. A cell-track ribbon layer alongside when the ribbon drew (gated chip, its eye on).
  for (const { layer: l, pops } of o.popLayers) {
    for (const p of pops) {
      if (o.pointDrawn(l, p)) layers[`(${l.popType}) (${l.vn}) ${p.path}`] = { visible: true }
      if (o.ribbonDrawn(l, p))
        layers[`(track) (${l.vn}) Tracks ${p.path}`] = withColour(trackPopSourceKey(l.vn, l.popType, p.path))
    }
  }
  // Whole-segmentation tracks: one per vn that DREW (its "directions" eye on, not stood down for
  // trackclust, some segments).
  for (const vn of o.trackVns) {
    if (o.drawn.has(vn)) layers[`(track) (${vn}) Tracks /_tracked`] = withColour(vn)
  }
  // Trackclust ribbons: whichever pops the trackclust payload publishes as `show`.
  for (const [vn, payload] of o.trackclust) {
    for (const p of payload?.pops ?? []) {
      if (p.show) layers[`(trackclust) (${vn}) Tracks ${p.path}`] = withColour(trackPopSourceKey(vn, 'trackclust', p.path))
    }
  }
  // Segmentation mask: one per visible label layer (currently ≤1).
  if (o.labelName) layers[`(${o.labelName}) Labels`] = { visible: true }
  return layers
}

export interface OverlayPushConfig {
  trackValueNames: string[]     // segmentations whose whole-segmentation tracks (_tracked) were shown
  showGatedTracks: boolean      // any gated `track` ribbon (a non-_tracked track pop)
  showTrackclust: boolean       // any `trackclust` ribbon
  popTypes: string[]            // population POINT layers by pop type (flow / clust)
}

/** Derive the overlay push config from the parsed layers — what to re-request so the same overlays
 *  reappear (zoom-to-source). Colour-by isn't encoded in layer names (it's captured separately). */
export function overlayPushConfig(ovs: ParsedOverlay[]): OverlayPushConfig {
  const trackValueNames = [...new Set(
    ovs.filter(o => o.isTrack && o.popType === 'track' && o.path === '/_tracked').map(o => o.valueName))]
  const showGatedTracks = ovs.some(o => o.isTrack && o.popType === 'track' && o.path !== '/_tracked')
  const showTrackclust  = ovs.some(o => o.isTrack && o.popType === 'trackclust')
  const popTypes        = [...new Set(ovs.filter(o => !o.isTrack).map(o => o.popType))]
  return { trackValueNames, showGatedTracks, showTrackclust, popTypes }
}
