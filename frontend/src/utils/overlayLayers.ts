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
