// Pure helpers for the Batch Movies page (F1.3). Kept out of the SFC so they're unit-testable:
//  - buildBatchMovieConfig: the persisted per-set config → the `config` object the backend's
//    _apply_movie_config! consumes (api/src/napari_api.jl). Tracks are shown for ALL segmentations
//    when `showTracks` is on (the backend skips ones without a track_id column).
//  - movieFilename: the output filename preview, mirroring the backend `_movie_basename`
//    (<attr1>_<attr2>_..._<uid>.mp4; blanks dropped, uid always terminates, unsafe chars → '_').

export interface BatchMovieCfg {
  valueName?: string
  channels?: Record<string, string>
  colourBy?: string
  showTracks?: boolean
  showGatedTracks?: boolean
  showTrackclust?: boolean
  showPopulations?: boolean
  popType?: string
  colourLabels?: boolean
  tailWidth?: number
  pointsSize?: number
}

export interface BatchMovieRequestConfig {
  valueName: string
  channels: Record<string, string>
  colourBy: string
  showTracks: boolean
  trackValueNames: string[]
  tailWidth: number
  showGatedTracks: boolean
  showTrackclust: boolean
  showPopulations: boolean
  popType: string
  pointsSize: number
  colourLabels: boolean
  colourOverrides: Record<string, string>
}

export function buildBatchMovieConfig(
  cfg: BatchMovieCfg,
  segNames: string[],
  colourOverrides: Record<string, string>,
): BatchMovieRequestConfig {
  return {
    valueName: cfg.valueName ?? '',
    channels: cfg.channels ?? {},
    colourBy: cfg.colourBy ?? '',
    showTracks: !!cfg.showTracks,
    trackValueNames: cfg.showTracks ? segNames : [],
    tailWidth: cfg.tailWidth ?? 4,
    showGatedTracks: !!cfg.showGatedTracks,
    showTrackclust: !!cfg.showTrackclust,
    showPopulations: !!cfg.showPopulations,
    popType: cfg.popType ?? 'flow',
    pointsSize: cfg.pointsSize ?? 6,
    colourLabels: !!cfg.colourLabels,
    colourOverrides: colourOverrides ?? {},
  }
}

/** Output filename for one image — mirrors the backend `_movie_basename`. */
export function movieFilename(fileAttrs: string[], attrValues: Record<string, string>, uid: string): string {
  const parts = fileAttrs.map(a => (attrValues[a] ?? '').trim()).filter(Boolean)
  parts.push(uid || 'uid')
  return parts.join('_').replace(/[^A-Za-z0-9._-]+/g, '_') + '.mp4'
}

// ── seeding (so the config isn't blank) ────────────────────────────────────────
// napari view snapshot shape we read (subset of capture_view_state): per-layer colormap + visibility.
interface ViewStateLike { layers?: Record<string, { colormap?: unknown; visible?: unknown }> }

/** Seed a config from the FIRST selected image's live napari view: which channels are shown + their
 *  colormap, and which overlays are present (tracks / track-clusters / population points). Channel layers
 *  are plain-named; overlays are parenthesised `(popType) …` (see the bridge naming). Colour-by isn't in
 *  the snapshot — the caller supplies it (the set's last colour-by). Pure → testable. */
export function seedConfigFromViewState(vs: ViewStateLike | null | undefined, channelNames: string[]): BatchMovieCfg {
  const layers = vs?.layers ?? {}
  const channels: Record<string, string> = {}
  for (const ch of channelNames) {
    const l = layers[ch]
    if (l && l.visible !== false && typeof l.colormap === 'string') channels[ch] = l.colormap
  }
  const names = Object.keys(layers)
  const out: BatchMovieCfg = { channels }
  if (names.some(n => n.startsWith('(track)')))      out.showTracks = true          // whole-seg + gated ribbons
  if (names.some(n => n.startsWith('(trackclust)'))) out.showTrackclust = true
  const flow = names.some(n => n.startsWith('(flow)'))
  const clust = names.some(n => n.startsWith('(clust)'))
  if (flow || clust) { out.showPopulations = true; out.popType = flow ? 'flow' : 'clust' }
  return out
}

/** Fallback seed when there's no live view to read: assign each channel a colour from `palette` in order,
 *  so the picker is populated (not all "hidden"). Assumes images generated together share the channel set. */
export function defaultChannelSeed(channelNames: string[], palette: string[]): Record<string, string> {
  const out: Record<string, string> = {}
  if (!palette.length) return out
  channelNames.forEach((n, i) => { out[n] = palette[i % palette.length] })
  return out
}
