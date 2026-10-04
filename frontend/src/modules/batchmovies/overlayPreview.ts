/**
 * The producer behind the Batch Movies overlay preview — a small schematic that mirrors
 * `_overlays_raw_from_config` (api/src/movie_rail.jl) + `_build_overlay_state`
 * (api/src/overlay_author.jl) so the picture can't drift from what the movie actually draws.
 *
 * **Why encode the branch rules here, not in the component.** The overlay author has three
 * exclusive branches:
 *   - `all_tracks && !showPops` → every tracked cell in default grey; ribbons uniform
 *   - showPops path → coloured points per pop; ribbons only for pops with `is_track || has_tracks`
 *   - (track poptype path — out of scope for this batch preview)
 * If the component said "toggle A shows dots, toggle B shows ribbons", it would show one thing when
 * the backend does another — the class of bug the visual-aid discipline exists to prevent. See
 * PR #751 for the rule locked here.
 *
 * **Same design as `tasks/smoothVis.ts`.** Everything a test needs to pin is in this producer;
 * `components/SceneAid.vue` (the sibling of `VisualAid`) draws whatever it hands over.
 */

import type { SceneAidRender, SceneAidPoint, SceneAidRibbon } from '../../lib/sceneAid'

/** A stable, tiny PRNG — same as `smoothVis.ts`. `Math.random` would make the scene
 *  non-reproducible and a test would pin nothing. */
function mulberry32(seed: number): () => number {
  let a = seed >>> 0
  return () => {
    a = (a + 0x6D2B79F5) >>> 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

/** Fake pop palette. Kept SEPARATE from the app palette — the schematic shows STRUCTURE (that
 *  four pops will draw in four colours), not identity (which pop got which hex). Matching the real
 *  palette here would make the preview a second legend to keep in step with Settings. */
export const PREVIEW_PALETTE = ['#ff6b6b', '#4ecdc4', '#ffd93d', '#a78bfa', '#5eead4', '#f472b6']

/** Grey stand-in for `all_tracks_colour` in the overlay author (`#9ca3af`) — a whole-seg preview
 *  reads the same neutral tone the movie would. */
export const ALL_TRACKS_GREY = '#9ca3af'

/** Stand-in heat ramp (cool → hot) for "speed" tails — structure, not the real `heatRamp`. */
const PREVIEW_HEAT = ['#1d4ed8', '#06b6d4', '#22c55e', '#f59e0b', '#ef4444']

/** A tail's colour under the track colour mode, as the overlay author picks it
 *  (`_build_overlay_state`): "track" cycles the palette by track, "speed" the heat ramp (the scene has
 *  no speeds, so by track), "solid" one colour for the source (`solid`), "pop" the cell's own (`own`
 *  — its pop, or its source in the whole-seg branch). */
function tailColour(mode: string | undefined, trackId: number, solid: string, own: string): string {
  if (mode === 'solid') return solid
  if (mode === 'pop') return own
  if (mode === 'speed') return PREVIEW_HEAT[trackId % PREVIEW_HEAT.length]!
  return PREVIEW_PALETTE[trackId % PREVIEW_PALETTE.length]!
}

const N_POPS = 6
const N_CELLS = 60
const RIBBON_STEPS = 4

/** One cell of the schematic — a centroid position with the fake pop it belongs to and the fake
 *  track it sits on. `trackId = null` means "untracked" — what makes `includeTracks` visibly
 *  different from `showPopulations` alone. `segIdx` is the pseudo-segmentation the cell lives in
 *  (0 or 1) — used only under multi-source track composition. */
export interface OverlayCell {
  x: number   // 0..1
  y: number
  popIdx: number
  trackId: number | null
  segIdx: number
}

/** One fake pop — index, colour, and whether its cells hold `track_id > 0` (`has_tracks`). Half
 *  of them are tracked so the difference between `showPops` alone and `showPops + includeTracks`
 *  is visible in one glance. */
export interface OverlayPop {
  idx: number
  colour: string
  hasTracks: boolean
}

/** The scene the preview draws against — deterministic, config-independent. The CONFIG changes
 *  what gets drawn from it, not the scene itself. */
export interface OverlayScene {
  cells: OverlayCell[]
  pops: OverlayPop[]
}

/** Build the schematic scene. */
export function buildOverlayScene(seed = 7): OverlayScene {
  const rnd = mulberry32(seed)
  const pops: OverlayPop[] = Array.from({ length: N_POPS }, (_, i) => ({
    idx: i,
    colour: PREVIEW_PALETTE[i % PREVIEW_PALETTE.length],
    // First half tracked, second half untracked — one visible axis of difference.
    hasTracks: i < N_POPS / 2,
  }))
  const cells: OverlayCell[] = []
  for (let i = 0; i < N_CELLS; i++) {
    const popIdx = Math.floor(rnd() * N_POPS)
    const x = 0.08 + rnd() * 0.84
    const y = 0.08 + rnd() * 0.84
    const trackId = pops[popIdx].hasTracks ? i + 1 : null
    // Split the scene across two pseudo-segmentations so a multi-source `showTracks && !showPops`
    // config shows two distinct colours in the frame. Real batches may have any number of tracked
    // segs; the preview always uses two — enough to see the composition without turning into a
    // legend.
    const segIdx = i % 2
    cells.push({ x, y, popIdx, trackId, segIdx })
  }
  return { cells, pops }
}

/** The config shape the preview reads — names match `BatchMovieCfg` 1:1 so the caller passes
 *  `cfg.value` straight through. */
export interface OverlayPreviewConfig {
  showPopulations?: boolean
  showTracks?: boolean
  showGatedTracks?: boolean
  showTrackclust?: boolean
  colourLabels?: boolean
  labelValueNames?: string[]
  showTimestamp?: boolean
  showScaleBar?: boolean
  titleCard?: { enabled?: boolean }
  /** Real batch-panel pop PATHS. Empty = all pops (matches the backend "empty popsFilter = every
   *  pop of popType" rule). Paths hash to a stable pop index; the preview can't interpret real
   *  paths without the tree, so this mapping just makes different picks look different. */
  popsFilter?: string[]
  /** Visible track sources — `{valueName, colour}` per seg the user ticked in the batch panel's
   *  Track sources list. Only meaningful under `showTracks && !showPops`. When present, the preview
   *  paints all-tracks cells split across two pseudo-segmentations, each in its picked colour
   *  (schematic — the scene is fixed at two pseudo-segs regardless of how many real ones exist).
   *  Absent → single-source grey (`ALL_TRACKS_GREY`); empty → every source hidden, no tails. */
  trackSources?: Array<{ valueName: string; colour: string }>
  /** How tails are coloured — `'track'` (default) | `'speed'` | `'solid'` | `'pop'`. */
  trackColourMode?: string
}

/** Whether the preview should ring each drawn point (mask-outline hint). Any picked mask counts. */
export function previewHasMask(cfg: OverlayPreviewConfig): boolean {
  return (cfg.labelValueNames?.length ?? 0) > 0
}

/** The two derived flags the overlay author actually acts on — locked so the preview and
 *  `_overlays_raw_from_config` cannot disagree.
 *  - `allTracks = showTracks && !showPops` — pops wins when both.
 *  - `includeTracks = showTracks || showGatedTracks` — either chip pushes ribbons.
 *  - `authorRuns = showPops || showTracks` — the pop / track author runs; it draws points only for
 *    pops (`include_points = showPopulations`), tracks alone are tails.
 *  See PR #751 + the API testset "movie rail — offline overlay-config translator". */
export function derivedOverlayFlags(cfg: OverlayPreviewConfig):
  { allTracks: boolean; includeTracks: boolean; authorRuns: boolean } {
  const showPops = !!cfg.showPopulations
  const showTracks = !!cfg.showTracks
  const showGated = !!cfg.showGatedTracks
  return {
    allTracks: showTracks && !showPops,
    includeTracks: showTracks || showGated,
    authorRuns: showPops || showTracks,
  }
}

/** Hash a pop path to a stable scene-pop index. Same path always maps to the same fake pop, so
 *  toggling one selection reliably adds/removes one colour. */
export function popsForConfig(cfg: OverlayPreviewConfig, scene: OverlayScene): Set<number> {
  const raw = cfg.popsFilter ?? []
  if (!raw.length) return new Set(scene.pops.map(p => p.idx))
  const out = new Set<number>()
  for (const entry of raw) {
    const asNum = Number(entry)
    if (Number.isFinite(asNum) && asNum >= 0 && asNum < scene.pops.length) {
      out.add(Math.floor(asNum))
      continue
    }
    let h = 0
    for (let i = 0; i < entry.length; i++) h = (h * 31 + entry.charCodeAt(i)) | 0
    out.add(Math.abs(h) % scene.pops.length)
  }
  return out
}

/** A tiny ribbon walk — a mostly-straight curve of `RIBBON_STEPS` points, deterministic from the
 *  cell's own coordinates. Not the actual track geometry; it just has to LOOK like a moving cell. */
function ribbonPath(c: OverlayCell): Array<{ x: number; y: number }> {
  const out: Array<{ x: number; y: number }> = []
  const seed = Math.floor((c.x * 1000 + c.y * 733) | 0)
  const rnd = mulberry32(seed)
  const dx = (rnd() - 0.5) * 0.12
  const dy = (rnd() - 0.5) * 0.12
  for (let s = 0; s < RIBBON_STEPS; s++) {
    const t = s / (RIBBON_STEPS - 1)
    const bendX = Math.sin(t * Math.PI) * 0.02
    const bendY = Math.cos(t * Math.PI) * 0.02
    out.push({
      x: Math.min(0.98, Math.max(0.02, c.x + dx * t + bendX)),
      y: Math.min(0.98, Math.max(0.02, c.y + dy * t + bendY)),
    })
  }
  return out
}

/** A second ribbon walk with a distinct tilt — draws trackclust ribbons offset from the cell
 *  ribbons so they don't overlap exactly. Track-cluster ribbons are the movie's second track family
 *  (trackclust pops) and read differently from cell-track ribbons in the app; the preview echoes
 *  that by giving them their own path shape. */
function trackclustRibbonPath(c: OverlayCell): Array<{ x: number; y: number }> {
  const out: Array<{ x: number; y: number }> = []
  const seed = Math.floor((c.x * 1500 + c.y * 913) | 0)
  const rnd = mulberry32(seed)
  const dx = (rnd() - 0.5) * 0.16
  const dy = (rnd() - 0.5) * 0.16
  for (let s = 0; s < RIBBON_STEPS; s++) {
    const t = s / (RIBBON_STEPS - 1)
    // Different bend axis so the two ribbon families don't stack.
    const bendX = Math.cos(t * Math.PI) * 0.03
    const bendY = Math.sin(t * Math.PI) * 0.03
    out.push({
      x: Math.min(0.98, Math.max(0.02, c.x + dx * t + bendX)),
      y: Math.min(0.98, Math.max(0.02, c.y + dy * t + bendY)),
    })
  }
  return out
}

/** Apply the current config to the scene and produce a `SceneAidRender`. Handles every chip the
 *  Overlays row surfaces (`tracks`, `trackclust`, `gated`, `pops`, `labels`) + the mask pickers,
 *  mirroring the backend `_config_overlay_pops` + `_build_overlay_state` branches. */
export function renderOverlayPreview(cfg: OverlayPreviewConfig, scene: OverlayScene): SceneAidRender {
  const { allTracks, includeTracks, authorRuns } = derivedOverlayFlags(cfg)
  const showTrackclust = !!cfg.showTrackclust
  const colourLabels = !!cfg.colourLabels
  const hasMask = previewHasMask(cfg)
  const points: SceneAidPoint[] = []
  const ribbons: SceneAidRibbon[] = []
  const wantedPops = popsForConfig(cfg, scene)

  const corners = {
    showTimestamp: !!cfg.showTimestamp,
    showScaleBar: !!cfg.showScaleBar,
    showTitleChip: !!cfg.titleCard?.enabled,
  }

  // Trackclust ribbons draw with the pops (`trackclust_requested`), and where they draw the pops' own
  // cell-track ribbons stand down — trackclust colours the SAME tracks by cluster.
  const trackclustOn = showTrackclust && !allTracks && !!cfg.showPopulations

  // ── Pop points + cell-track ribbons ────────────────────────────────────────
  if (authorRuns) {
    if (allTracks) {
      // Whole-seg branch — every tracked cell. `trackSources` splits the cells across two
      // pseudo-segmentations and paints each in its picked colour, as the backend's multi-source
      // composition does. Absent → uniform grey (single source); empty → every source hidden, no
      // tails (the translator's rule). Untracked cells never drew here either (the overlay author's
      // all_tracks branch reads `track_id`), so mirror that.
      const srcs = cfg.trackSources
      const multiColours = srcs?.length
        ? [srcs[0]!.colour, srcs[Math.min(1, srcs.length - 1)]!.colour]
        : null
      for (const c of scene.cells) {
        if (c.trackId === null || (srcs && !srcs.length)) continue
        const source = multiColours ? multiColours[c.segIdx % multiColours.length]! : null
        // one source with no colour picked: "solid" is the palette's first colour, "pop" the grey
        const colour = tailColour(cfg.trackColourMode, c.trackId, source ?? PREVIEW_PALETTE[0]!,
                                  source ?? ALL_TRACKS_GREY)
        // tails only — the viewer's (and the movie's) points are its populations
        if (includeTracks) ribbons.push({ points: ribbonPath(c), colour })
      }
    } else {
      // Pops branch — per-pop colour, ribbons only for pops with `hasTracks` under includeTracks.
      for (const c of scene.cells) {
        if (!wantedPops.has(c.popIdx)) continue
        const pop = scene.pops[c.popIdx]
        points.push({ x: c.x, y: c.y, colour: pop.colour, ringed: hasMask })
        if (includeTracks && !trackclustOn && pop.hasTracks && c.trackId !== null) {
          ribbons.push({ points: ribbonPath(c),
                         colour: tailColour(cfg.trackColourMode, c.trackId, PREVIEW_PALETTE[0]!, pop.colour) })
        }
      }
    }
  }

  // ── Trackclust ribbons — their own author call, drawn with the pops ───────
  // The movie draws them by a second `build_overlays3d_for(pop_type = "trackclust")` merged with the
  // pops' (`trackclust_requested`: the chip AND showPops, not whole-seg tracks). Alone the chip draws
  // nothing — `_overlays_raw_from_config` needs one of showPops/showTracks/showGatedTracks/has_mask.
  if (trackclustOn) {
    for (const c of scene.cells) {
      if (!wantedPops.has(c.popIdx)) continue
      const pop = scene.pops[c.popIdx]
      if (!pop.hasTracks || c.trackId === null) continue
      ribbons.push({ points: trackclustRibbonPath(c), colour: pop.colour })
    }
  }

  // ── Mask outlines when no points carry them ─────────────────────────────────
  // The movie's mask branch draws label CONTOURS around every labelled cell when a mask column is
  // picked. With no points to ring (nothing else on, or tracks alone — tails only), add ring-only
  // points to reflect it. Colour tinted by pop when `colourLabels`, else neutral grey.
  if (hasMask && points.length === 0) {
    for (const c of scene.cells) {
      const colour = colourLabels ? scene.pops[c.popIdx].colour : ALL_TRACKS_GREY
      points.push({ x: c.x, y: c.y, colour, mode: 'ring-only' })
    }
  }

  // ── Caption for the empty / near-empty cases ────────────────────────────────
  let caption: string | undefined
  if (points.length === 0 && ribbons.length === 0) {
    if (cfg.showGatedTracks && !cfg.showPopulations && !cfg.showTracks) {
      caption = 'cell-track ribbons need populations on'
    } else if (showTrackclust && !cfg.showPopulations && !cfg.showTracks) {
      caption = 'track-cluster ribbons need populations on'
    } else if (colourLabels && !hasMask) {
      caption = 'colour-labels needs a mask picked'
    } else if (cfg.showPopulations && !wantedPops.size) {
      caption = 'no pops selected'
    } else {
      caption = 'nothing on'
    }
  }

  return { points, ribbons, corners, caption }
}
