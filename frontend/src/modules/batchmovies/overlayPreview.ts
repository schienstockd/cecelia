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

const N_POPS = 6
const N_CELLS = 60
const RIBBON_STEPS = 4

/** One cell of the schematic — a centroid position with the fake pop it belongs to and the fake
 *  track it sits on. `trackId = null` means "untracked" — what makes `includeTracks` visibly
 *  different from `showPopulations` alone. */
export interface OverlayCell {
  x: number   // 0..1
  y: number
  popIdx: number
  trackId: number | null
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
    cells.push({ x, y, popIdx, trackId })
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
  labelValueNames?: string[]
  showTimestamp?: boolean
  showScaleBar?: boolean
  titleCard?: { enabled?: boolean }
  /** Real batch-panel pop PATHS. Empty = all pops (matches the backend "empty popsFilter = every
   *  pop of popType" rule). Paths hash to a stable pop index; the preview can't interpret real
   *  paths without the tree, so this mapping just makes different picks look different. */
  popsFilter?: string[]
}

/** Whether the preview should ring each drawn point (mask-outline hint). Any picked mask counts. */
export function previewHasMask(cfg: OverlayPreviewConfig): boolean {
  return (cfg.labelValueNames?.length ?? 0) > 0
}

/** The two derived flags the overlay author actually acts on — locked so the preview and
 *  `_overlays_raw_from_config` cannot disagree.
 *  - `allTracks = showTracks && !showPops` — pops wins when both.
 *  - `includeTracks = showTracks || showGatedTracks` — either chip pushes ribbons.
 *  See PR #751 + the API testset "movie rail — offline overlay-config translator". */
export function derivedOverlayFlags(cfg: OverlayPreviewConfig):
  { allTracks: boolean; includeTracks: boolean; showPoints: boolean } {
  const showPops = !!cfg.showPopulations
  const showTracks = !!cfg.showTracks
  const showGated = !!cfg.showGatedTracks
  return {
    allTracks: showTracks && !showPops,
    includeTracks: showTracks || showGated,
    showPoints: showPops || showTracks,
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

/** Apply the current config to the scene and produce a `SceneAidRender`. Three branches match
 *  `_build_overlay_state`. */
export function renderOverlayPreview(cfg: OverlayPreviewConfig, scene: OverlayScene): SceneAidRender {
  const { allTracks, includeTracks, showPoints } = derivedOverlayFlags(cfg)
  const points: SceneAidPoint[] = []
  const ribbons: SceneAidRibbon[] = []
  const ringed = previewHasMask(cfg)

  const corners = {
    showTimestamp: !!cfg.showTimestamp,
    showScaleBar: !!cfg.showScaleBar,
    showTitleChip: !!cfg.titleCard?.enabled,
  }

  if (!showPoints) {
    return { points, ribbons, corners, caption: 'nothing on' }
  }

  const wantedPops = popsForConfig(cfg, scene)

  if (allTracks) {
    // Whole-seg branch — every tracked cell, uniform grey. Untracked cells never drew here either
    // (the overlay author's all_tracks branch reads `track_id`), so mirror that.
    for (const c of scene.cells) {
      if (c.trackId === null) continue
      points.push({ x: c.x, y: c.y, colour: ALL_TRACKS_GREY, ringed })
      if (includeTracks) {
        ribbons.push({ points: ribbonPath(c), colour: ALL_TRACKS_GREY })
      }
    }
  } else {
    // Pops branch — per-pop colour, ribbons only for pops with `hasTracks` under includeTracks.
    for (const c of scene.cells) {
      if (!wantedPops.has(c.popIdx)) continue
      const pop = scene.pops[c.popIdx]
      points.push({ x: c.x, y: c.y, colour: pop.colour, ringed })
      if (includeTracks && pop.hasTracks && c.trackId !== null) {
        ribbons.push({ points: ribbonPath(c), colour: pop.colour })
      }
    }
  }

  const caption = points.length === 0
    ? (cfg.showPopulations ? 'no pops selected' : 'no tracked cells')
    : undefined

  return { points, ribbons, corners, caption }
}
