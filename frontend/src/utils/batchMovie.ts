// Pure helpers for the Batch Movies page (F1.3). Kept out of the SFC so they're unit-testable:
//  - buildBatchMovieConfig: the persisted per-set config → the `config` object the backend's
//    _apply_movie_config! consumes (api/src/napari_api.jl). Tracks are shown for ALL segmentations
//    when `showTracks` is on (the backend skips ones without a track_id column).
//  - movieFilename: the output filename preview, mirroring the backend `_movie_basename`
//    (<attr1>_<attr2>_..._<uid>.mp4; blanks dropped, uid always terminates, unsafe chars → '_').

import { COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         type CompareLayout, type CompareContrast } from './movieCompare'

// Title-card options (Phase H) — a description slide prepended to each recorded movie.
export interface TitleCardCfg {
  enabled: boolean
  note: string
  durationSec: number
}

export interface BatchMovieCfg {
  // The image versions each movie shows, in COLUMN order (docs/todo/MOVIE_COMPARE_PLAN.md). Two or
  // more record a side-by-side comparison. `valueName` is what configs saved before that carried —
  // read it through `versionsFromConfig`, never directly, so an old config keeps its version.
  valueNames?: string[]
  // The segmentation masks each movie draws, in order. 2+ makes them the grid's ROWS (with the
  // versions as its columns); one draws it in every cell. Read via `segmentationsFromConfig`.
  labelValueNames?: string[]
  // Mask outline width in px — 0 draws them filled (napari's default), N draws an N-px contour so the
  // channel signal underneath stays readable. Clamped to 0..LABEL_CONTOUR_MAX.
  labelContour?: number
  // How much of the z stack a movie shows: the whole thing as a 3D render, or one slice in 2D.
  // `show3D` wins; `zSlice` undefined means "whatever is showing", which is what every recording did
  // before the setting existed. One switch for both layer kinds — see `set_z_view` in the bridge.
  show3D?: boolean
  zSlice?: number | null
  compareLayout?: CompareLayout
  compareContrast?: CompareContrast
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
  titleCard?: TitleCardCfg
}

export interface BatchMovieRequestConfig {
  valueName: string
  valueNames: string[]
  labelValueNames: string[]
  labelContour: number
  show3D: boolean
  zSlice: number | null
  compareLayout: CompareLayout
  compareContrast: CompareContrast
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
  titleCard: TitleCardCfg
}

// Title card is ON by default (Phase H decision 3); duration clamped to 1–10s.
export const TITLE_CARD_DEFAULT: TitleCardCfg = { enabled: true, note: '', durationSec: 3 }

/** Mask outline width, clamped. 0 = filled. Mirrors `LABEL_CONTOUR_MAX` / `_label_contour`
 *  (api/src/napari_api.jl) — both ends clamp rather than reject, since a bad value here is a display
 *  nicety and must not fail a whole batch. */
export const LABEL_CONTOUR_MAX = 10
export const clampContour = (v: number | undefined): number =>
  Math.min(LABEL_CONTOUR_MAX, Math.max(0, Math.round(v ?? 0) || 0))

export function buildBatchMovieConfig(
  cfg: BatchMovieCfg,
  segNames: string[],
  colourOverrides: Record<string, string>,
): BatchMovieRequestConfig {
  const tc = cfg.titleCard
  // The version list is authoritative; `valueName` stays in the payload as the FIRST column, which is
  // what an older backend (and the filename/channel lookups) read.
  const versions = cfg.valueNames ?? (cfg.valueName ? [cfg.valueName] : [])
  return {
    valueName: versions[0] ?? '',
    valueNames: versions,
    // Always sent, even empty: the backend treats an ABSENT list as "leave the masks alone" and an
    // empty one as "no masks", and an authored batch config always means the latter.
    labelValueNames: cfg.labelValueNames ?? [],
    labelContour: clampContour(cfg.labelContour),
    show3D: !!cfg.show3D,
    // a z index alongside show3D is a leftover from the last time 2D was picked — Julia ignores it
    // (`_z_slice`), and sending null rather than dropping the key keeps the two ends reading alike
    zSlice: cfg.show3D ? null : (cfg.zSlice ?? null),
    compareLayout: cfg.compareLayout ?? COMPARE_LAYOUT_DEFAULT,
    compareContrast: cfg.compareContrast ?? COMPARE_CONTRAST_DEFAULT,
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
    titleCard: {
      enabled: tc?.enabled ?? TITLE_CARD_DEFAULT.enabled,
      note: tc?.note ?? '',
      durationSec: Math.min(10, Math.max(1, tc?.durationSec ?? TITLE_CARD_DEFAULT.durationSec)),
    },
  }
}

// Sentinel token that can appear in the ordered `fileAttrs` list to mean "the displayed channel
// names, joined by '-'" — so channel names can be positioned in the filename like any attribute
// (drag-reorderable). Chosen to not collide with a real user attribute key. Mirrored in the backend
// `_movie_basename` (api/src/napari_api.jl) — keep the two in sync.
export const MOVIE_CHANNELS_TOKEN = '__channels__'

/** Output filename for one image — mirrors the backend `_movie_basename`. `fileAttrs` is the ordered
 *  list of attribute keys and/or the `MOVIE_CHANNELS_TOKEN`; `channelNames` are the channels shown in
 *  the movie (used only when the token is present, joined by '-'). Blanks drop, uid always terminates. */
export function movieFilename(
  fileAttrs: string[], attrValues: Record<string, string>, uid: string, channelNames: string[] = [],
): string {
  const parts: string[] = []
  for (const a of fileAttrs) {
    if (a === MOVIE_CHANNELS_TOKEN) {
      const chans = channelNames.map(c => c.trim()).filter(Boolean).join('-')
      if (chans) parts.push(chans)
    } else {
      const val = (attrValues[a] ?? '').trim()
      if (val) parts.push(val)
    }
  }
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
