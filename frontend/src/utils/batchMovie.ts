// Pure helpers for the Batch Movies page (F1.3). Kept out of the SFC so they're unit-testable:
//  - buildBatchMovieConfig: the persisted per-set config → the `config` object the backend's batch
//    renderer consumes (api/src/movie_rail.jl). Tracks are shown for ALL segmentations when
//    `showTracks` is on (the backend skips ones without a track_id column).
//  - movieFilename: the output filename preview, mirroring the backend `_movie_basename`
//    (<attr1>_<attr2>_..._<uid|image name>.mp4; blanks dropped, unsafe chars → '_').

import { COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         type CompareLayout, type CompareContrast } from './movieCompare'

// Title-card options (Phase H) — a description slide prepended to each recorded movie.
export interface TitleCardCfg {
  enabled: boolean
  note: string
  durationSec: number
}

/**
 * The AUTHORED movie config — what the Batch page's controls edit and what the settings store persists
 * per set (`MovieSetPrefs.batchMovie` is this type, not a copy of it). Distinct from
 * `BatchMovieRequestConfig` below, which is what `buildBatchMovieConfig` turns it into for the wire:
 * everything here is optional and means "not chosen", everything there is resolved and always present.
 *
 * The store used to RESTATE all twenty fields, from a file that already imports from this one — so the
 * two drifted, and the copy accumulated three fields nothing read (`trackValueNames`, which the builder
 * derives; `tStart`/`tEnd`, which had no control at all until the frame-range one was added).
 */
export interface BatchMovieCfg {
  // The image versions each movie shows, in COLUMN order (docs/todo/MOVIE_COMPARE_PLAN.md). Two or
  // more record a side-by-side comparison. `valueName` is what configs saved before that carried —
  // read it through `versionsFromConfig`, never directly, so an old config keeps its version.
  valueNames?: string[]
  // The segmentation masks each movie draws, in order. 2+ makes them the grid's ROWS (with the
  // versions as its columns); one draws it in every cell. Read via `segmentationsFromConfig`.
  labelValueNames?: string[]
  // Mask outline width in px — 0 draws them filled, N draws an N-px contour so the channel signal
  // underneath stays readable. Clamped to 0..LABEL_CONTOUR_MAX.
  labelContour?: number
  // How much of the z stack a movie shows: the whole thing as a 3D render, or one slice in 2D.
  // `show3D` wins; `zSlice` undefined means "whatever is showing", which is what every recording did
  // before the setting existed. One switch for both layer kinds — image and mask.
  show3D?: boolean
  zSlice?: number | null
  // 3D multiscale detail: level index (0 = full resolution, higher = coarser), null = renderer default
  detail3d?: number | null
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
  // Which SEGMENTATION the pops in `popsFilter` belong to (a labelProps key like `flowTom`). Pop trees
  // are per-segmentation, so a batch that draws mask `default` while filtering `/qc/CD169-` needs to
  // know the pop path was authored on `flowTom` — otherwise the renderer looks it up against `default`
  // and misses. Empty/absent → the batch's first `labelValueNames` entry (else the first segmentation
  // of the first selected image), matching the pre-picker behaviour.
  popValueName?: string
  // Which pop paths to draw when `showPopulations` is on. Empty (or absent) = ALL pops of `popType`
  // for the selected segmentation, which is the pre-picker behaviour every batch already had. The
  // backend `_overlays_raw_from_config` forwards this as `popPaths` (see `_resolve_movie_overlays_mask`).
  popsFilter?: string[]
  colourLabels?: boolean
  tailWidth?: number
  pointsSize?: number
  titleCard?: TitleCardCfg
  // Which stretch of the timelapse each movie sweeps, as FRAME INDICES; `tEnd` null/absent = the last
  // frame, which is what every recording did before the control existed. Applied across a batch of
  // unequal timelapses it CLAMPS per image (`_t_range`/`_t_sweep_frames` in api/src/movie_rail.jl), so
  // a range longer than a given image records to its end rather than failing.
  tStart?: number
  tEnd?: number | null
  // Authoring-only — the ordered attribute keys (and `MOVIE_CHANNELS_TOKEN`) composing the output
  // filename. Not part of the recorder's config: it is sent alongside it, and `movieFilename` below is
  // what reads it.
  fileAttrs?: string[]
  // Terminate the filename with the IMAGE NAME instead of its uid. Off by default — the uid is unique
  // by construction, while two images in a set can share a name and would overwrite each other. On, a
  // batch names its files exactly the way a single recording does, which is what makes a restored
  // config regenerate the SAME file rather than a uid-named twin.
  nameByImage?: boolean
}

export interface BatchMovieRequestConfig {
  valueName: string
  valueNames: string[]
  labelValueNames: string[]
  labelContour: number
  show3D: boolean
  zSlice: number | null
  detail3d: number | null
  compareLayout: CompareLayout
  compareContrast: CompareContrast
  tStart: number
  tEnd: number | null
  nameByImage: boolean
  channels: Record<string, string>
  colourBy: string
  showTracks: boolean
  trackValueNames: string[]
  tailWidth: number
  showGatedTracks: boolean
  showTrackclust: boolean
  showPopulations: boolean
  popType: string
  popValueName: string
  popsFilter: string[]
  pointsSize: number
  colourLabels: boolean
  colourOverrides: Record<string, string>
  titleCard: TitleCardCfg
}

// Title card is ON by default (Phase H decision 3); duration clamped to 1–10s.
export const TITLE_CARD_DEFAULT: TitleCardCfg = { enabled: true, note: '', durationSec: 3 }

/** Mask outline width, clamped. 0 = filled. Mirrors `LABEL_CONTOUR_MAX` / `_label_contour`
 *  (api/src/movie_rail.jl) — both ends clamp rather than reject, since a bad value here is a display
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
    // only meaningful in 3D; sent as 0 (full resolution) by default, because a coarser level erases a
    // strided label pyramid
    detail3d: cfg.show3D ? (cfg.detail3d ?? 0) : null,
    compareLayout: cfg.compareLayout ?? COMPARE_LAYOUT_DEFAULT,
    compareContrast: cfg.compareContrast ?? COMPARE_CONTRAST_DEFAULT,
    // The frame range, always sent — `null` for the end MEANS "to the last frame", and keeps meaning it
    // when one config runs across timelapses of different lengths (the recorder clamps per image).
    tStart: Math.max(0, Math.round(cfg.tStart ?? 0)),
    tEnd: cfg.tEnd === undefined || cfg.tEnd === null ? null : Math.max(0, Math.round(cfg.tEnd)),
    // read by `run_batch_movies` → `_movie_out_path`; the FILENAME is the only thing it changes
    nameByImage: !!cfg.nameByImage,
    channels: cfg.channels ?? {},
    colourBy: cfg.colourBy ?? '',
    showTracks: !!cfg.showTracks,
    trackValueNames: cfg.showTracks ? segNames : [],
    tailWidth: cfg.tailWidth ?? 4,
    showGatedTracks: !!cfg.showGatedTracks,
    showTrackclust: !!cfg.showTrackclust,
    showPopulations: !!cfg.showPopulations,
    popType: cfg.popType ?? 'flow',
    // Which segmentation the popsFilter paths belong to. Empty → the batch's first mask column,
    // else the first available segmentation (backend fallback matches).
    popValueName: cfg.popValueName ?? (cfg.labelValueNames?.[0] ?? segNames[0] ?? ''),
    // Empty list = ALL pops of the resolved popType (backend rule; matches the pre-picker default).
    popsFilter: cfg.showPopulations ? [...(cfg.popsFilter ?? [])] : [],
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
// `_movie_basename` (api/src/movie_rail.jl) — keep the two in sync.
export const MOVIE_CHANNELS_TOKEN = '__channels__'

/** Output filename for one image — mirrors the backend `_movie_basename`. `fileAttrs` is the ordered
 *  list of attribute keys and/or the `MOVIE_CHANNELS_TOKEN`; `channelNames` are the channels shown in
 *  the movie (used only when the token is present, joined by '-'). Blanks drop, uid always terminates. */
export function movieFilename(
  fileAttrs: string[], attrValues: Record<string, string>, uid: string, channelNames: string[] = [],
  imageName = '',
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
  // What TERMINATES the name: the image's, when asked for, else the uid. Mirrors `_movie_basename`'s
  // `name` keyword — including its fallback, since a name of pure punctuation sanitises to nothing and
  // a file still has to be written.
  parts.push(safeNamePart(imageName) || uid || 'uid')
  return safeNamePart(parts.join('_')) + '.mp4'
}

/** One filename-safe fragment — mirrors `_safe_name_part` (api/src/movie_rail.jl); keep the two in
 *  sync. Keeps [A-Za-z0-9._-], collapses every other run to `_`, and drops the separators that
 *  collapse leaves at the EDGES: an image called "… -res (cropped)" ends in `)`, so sanitising alone
 *  produced a name ending in `_`. */
export function safeNamePart(raw: string): string {
  return raw.trim().replace(/[^A-Za-z0-9._-]+/g, '_').replace(/^[_.]+|[_.]+$/g, '')
}

// ── the recording frame range ─────────────────────────────────────────────────
/**
 * A stored `(tStart, tEnd)` resolved against an actual timelapse length — the two thumb positions plus
 * whether that is the whole thing.
 *
 * The subtlety is `tEnd`: **null/absent means "the last frame", not a number**, and that is what a
 * full-range selection stores. Pinning the index instead would truncate the same config the moment it
 * ran on a longer image — which is exactly what a batch does. `resolveFrameRange` is where that
 * asymmetry lives (read null → last), and `storeFrameEnd` is its inverse (write last → null), so the
 * two surfaces that offer the control cannot disagree about it.
 */
export function resolveFrameRange(tStart: number | undefined, tEnd: number | null | undefined,
                                  frames: number): { lo: number; hi: number; full: boolean } {
  const last = Math.max(0, Math.round(frames) - 1)
  const lo = Math.max(0, Math.min(Math.round(tStart ?? 0), last))
  const hi = Math.max(lo, Math.min(Math.round(tEnd ?? last), last))
  return { lo, hi, full: lo === 0 && hi >= last }
}

/** What to STORE for a chosen end frame: `null` once it reaches the last one, so it keeps meaning
 *  "to the end" rather than freezing at this image's length. */
export const storeFrameEnd = (hi: number, frames: number): number | null =>
  hi >= Math.max(0, Math.round(frames) - 1) ? null : Math.max(0, Math.round(hi))

// ── seeding (so the config isn't blank) ────────────────────────────────────────
// The view-snapshot shape we read (subset of the captured view state): per-layer colormap + visibility.
/** The part of a view state this reads. Exported so a caller can TYPE its fetch instead of casting
 *  through `never` at the call site — two sites now read a live view into a config. */
export interface ViewStateLike { layers?: Record<string, { colormap?: unknown; visible?: unknown }> }

/** Seed a config from the FIRST selected image's live view: which channels are shown + their
 *  colormap, and which overlays are present (tracks / track-clusters / population points). Channel
 *  layers are plain-named; overlays are parenthesised `(popType) …`. Colour-by isn't in the
 *  snapshot — the caller supplies it (the set's last colour-by). Pure → testable. */
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
