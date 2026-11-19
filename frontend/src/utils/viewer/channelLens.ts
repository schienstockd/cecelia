// The channel lens, the pure half: a circle on the pointer that draws its OWN channel set over the
// 2D view, so a 60-channel slide can keep a quiet context (DAPI, a structure marker) while the lens
// shows the panel being read. The idea is Scope2Screen's focus+context lens (Jessup, Krueger et al.,
// IEEE TVCG 2022, doi:10.1109/TVCG.2021.3114786); the drawing is ours (`shaders/lens.wgsl`).
//
// The lens lives in SCREEN space: it is where the pointer is, not a place on the tissue, so a pan
// slides the image under it. A recording pins it to the TISSUE instead (`lensForRecording`: centre
// and radius in level-0 image px) — a movie crops to the image and picks its own size, so a screen
// position would land elsewhere; an image position cannot.
//
// Three silent failures this module exists to prevent:
//   1. A lens channel that is not fetched: the tile atlas holds only the channels it was asked for,
//      so a lens-only channel would draw as black inside the lens. `lensFetchChannels` is the fetch
//      set — the visible channels plus, while the lens is on, the lens ones.
//   2. CSS px vs framebuffer px: the shader reads `@builtin(position)`, which is device px. A lens
//      placed in CSS px sits up-left of the pointer on a HiDPI screen by (dpr - 1) × the offset.
//   3. Readout and lens both on: two pointer tools fighting over one pointer. `pointerToolChips`
//      keeps them exclusive — the one just switched on wins.

import type { OrbitCamera, ViewerChannel, ViewerLens, ViewerMeta } from '../volumeViewer'
import { cameraViewL0 } from '../viewerPick'
import SHADER_CONSTANTS from '../../lib/webgpu/shaders/constants.json'

/** Lens radius bounds and default, CSS px. */
export const LENS_RADIUS_MIN = 40
export const LENS_RADIUS_MAX = 400
export const LENS_RADIUS_DEFAULT = 120
/** Rim width, CSS px — a hairline that still reads on a bright image. Shared with the movie host. */
export const LENS_RIM_PX: number = SHADER_CONSTANTS.LENS_RIM_PX

export function clampLensRadius(r: number): number {
  if (!Number.isFinite(r)) return LENS_RADIUS_DEFAULT
  return Math.round(Math.min(LENS_RADIUS_MAX, Math.max(LENS_RADIUS_MIN, r)))
}

/** The pointer tools — exclusive, so at most one of these is in the overlay chips. */
export const POINTER_TOOLS = ['readout', 'lens'] as const
export type PointerTool = typeof POINTER_TOOLS[number]

/**
 * The overlay chips after a click, with the pointer tools kept exclusive: when the click switched
 * one on while the other was on, the one just switched on wins. Other chips pass through.
 */
export function pointerToolChips(prev: string[], next: string[]): string[] {
  const added = POINTER_TOOLS.filter(t => next.includes(t) && !prev.includes(t))
  if (added.length !== 1) return next
  return next.filter(v => !(POINTER_TOOLS as readonly string[]).includes(v) || v === added[0])
}

/**
 * The lens for the renderer, from the pointer in CSS px over the canvas. `null` = no lens (off, or
 * the pointer never reached the canvas). Device px, top-left origin — what the shader reads.
 */
export function lensUniform(
  at: { cx: number; cy: number } | null, radiusCss: number, dpr: number,
): ViewerLens | null {
  if (!at) return null
  const k = dpr > 0 && Number.isFinite(dpr) ? dpr : 1
  return { x: at.cx * k, y: at.cy * k, r: clampLensRadius(radiusCss) * k, rimPx: LENS_RIM_PX * k }
}

/** The channels a tile fetch brings: the visible ones, plus the lens ones while the lens is on —
 *  in channel order, each once. `n` caps it (the shader's channel count). */
export function lensFetchChannels(
  channels: Pick<ViewerChannel, 'visible' | 'lens'>[], lensOn: boolean, n = channels.length,
): number[] {
  const out: number[] = []
  for (let c = 0; c < Math.min(n, channels.length); c++) {
    const ch = channels[c]
    if (ch.visible || (lensOn && ch.lens)) out.push(c)
  }
  return out
}

/** How the lens travels in a view state — the recording's copy. Level-0 image px (centre and
 *  radius), and the lens channels by name. */
export interface RecordedLens { x: number; y: number; r: number; channels: string[] }

/**
 * The lens as a recording should draw it, or `null` when the pointer never reached the canvas. No
 * channel picked = `channels: []`, the spotlight lens (`shaders/lens.wgsl`). `at` and the radius are
 * CSS px over a canvas of `canvasW` × `canvasH` showing `cam`.
 */
export function lensForRecording(
  at: { cx: number; cy: number } | null, radiusCss: number, canvasW: number, canvasH: number,
  cam: OrbitCamera, meta: Pick<ViewerMeta, 'nX' | 'nY' | 'voxelUm'>,
  channels: Pick<ViewerChannel, 'name' | 'lens'>[],
): RecordedLens | null {
  const c = lensCircleL0(at, radiusCss, canvasW, canvasH, cam, meta)
  if (!c) return null
  const round = (n: number) => Math.round(n * 100) / 100
  return { x: round(c.x), y: round(c.y), r: round(c.r), channels: channels.filter(ch => ch.lens).map(ch => ch.name) }
}

/** The lens on the tissue: centre and radius in float level-0 image px, or `null` before the pointer
 *  reached a canvas. `at` and the radius are CSS px over a canvas of `canvasW` × `canvasH` showing `cam`. */
export function lensCircleL0(
  at: { cx: number; cy: number } | null, radiusCss: number, canvasW: number, canvasH: number,
  cam: OrbitCamera, meta: Pick<ViewerMeta, 'nX' | 'nY' | 'voxelUm'>,
): { x: number; y: number; r: number } | null {
  if (!at || canvasW <= 0 || canvasH <= 0) return null
  // The canvas's rectangle of the image, in float L0 px — the pointer is a fraction across it.
  const v = cameraViewL0(cam, meta as ViewerMeta, canvasW, canvasH)
  return { x: v.x0 + (at.cx / canvasW) * (v.x1 - v.x0),
           y: v.y0 + (at.cy / canvasH) * (v.y1 - v.y0),
           r: clampLensRadius(radiusCss) * (v.y1 - v.y0) / canvasH }
}

/** A pinned lens's centre on the canvas (CSS px) — `lensCircleL0` the other way: the pin is a place
 *  on the tissue, so a pan or zoom carries the lens with it. */
export function lensScreenAt(
  pin: { x: number; y: number }, cam: OrbitCamera, meta: Pick<ViewerMeta, 'nX' | 'nY' | 'voxelUm'>,
  canvasW: number, canvasH: number,
): { cx: number; cy: number } {
  const v = cameraViewL0(cam, meta as ViewerMeta, Math.max(canvasW, 1), Math.max(canvasH, 1))
  return { cx: ((pin.x - v.x0) / Math.max(v.x1 - v.x0, 1e-9)) * canvasW,
           cy: ((pin.y - v.y0) / Math.max(v.y1 - v.y0, 1e-9)) * canvasH }
}

/** A radius in level-0 px as the canvas shows it now, CSS px, held in the lens's bounds — for a lens
 *  placed by name from outside (`set_viewer_view`). */
export function lensRadiusCss(
  rL0: number, cam: OrbitCamera, meta: Pick<ViewerMeta, 'nX' | 'nY' | 'voxelUm'>, canvasW: number, canvasH: number,
): number {
  const v = cameraViewL0(cam, meta as ViewerMeta, Math.max(canvasW, 1), Math.max(canvasH, 1))
  return clampLensRadius((rL0 * canvasH) / Math.max(v.y1 - v.y0, 1e-9))
}

/** One population's count inside the lens. */
export interface LensPopCount { key: string; name: string; colour: string; n: number }

/** A population layer as the count reads it — `OverlayPayload`'s cells and pops, the hidden paths. */
export interface LensCountLayer {
  vn: string
  payload: {
    cells: { label?: number[]; x?: number[]; y?: number[]; z?: number[]; t?: number[] }
    pops: { path: string; name: string; colour: string; labels: number[] }[]
  }
  hidden: ReadonlySet<string>
}

/**
 * What the lens holds: every drawn population's cells whose centroid is inside the circle, on the
 * frame and planes the markers are drawn on — the same cells the points show there, so the number and
 * the picture agree. `cells` = the segmented cells inside (each segmentation once), whatever their
 * population. Circle in level-0 px; centroids are image µm (`voxelUm`). `planes` is the marker window
 * (`overlayPlaneWindow`); a negative low end counts every plane.
 */
export function lensPopCounts(
  layers: readonly LensCountLayer[], circle: { x: number; y: number; r: number },
  voxelUm: readonly number[], t: number, planes: readonly [number, number],
): { cells: number; pops: LensPopCount[] } {
  const [vx, vy, vz] = [voxelUm[0] || 1, voxelUm[1] || 1, voxelUm[2] || 1]
  const cx = circle.x * vx, cy = circle.y * vy, r2 = (circle.r * vx) ** 2
  const [lo, hi] = planes
  const pops: LensPopCount[] = []
  const seenVn = new Set<string>()
  let cells = 0
  for (const l of layers) {
    const { label, x, y, z, t: tt } = l.payload.cells
    if (!label || !x || !y) continue
    // inside = on this frame, on the drawn planes, within the circle — by label
    const inside = new Set<number>()
    for (let i = 0; i < label.length; i++) {
      if (tt && tt.length && Math.round(tt[i]) !== t) continue
      if (lo >= 0 && z && z.length) {
        const pl = Math.floor(z[i] / vz)
        if (pl < lo || pl > hi) continue
      }
      if ((x[i] - cx) ** 2 + (y[i] - cy) ** 2 <= r2) inside.add(label[i])
    }
    if (!seenVn.has(l.vn)) { seenVn.add(l.vn); cells += inside.size }
    for (const p of l.payload.pops) {
      if (l.hidden.has(p.path)) continue
      let n = 0
      for (const lab of p.labels) if (inside.has(lab)) n++
      pops.push({ key: `${l.vn}|${p.path}`, name: p.name, colour: p.colour, n })
    }
  }
  return { cells, pops }
}

/** `n` of `of` as a whole percent — "<1%" for a few cells of many, so a present population never reads 0%. */
export function lensShare(n: number, of: number): string {
  if (of <= 0 || n <= 0) return '0%'
  const p = (100 * n) / of
  return p < 1 ? '<1%' : `${Math.round(p)}%`
}

/** The populations the lens chip lists: the fullest first, at most `max`, each with its share of the
 *  `cells` inside; `more` = how many are left off. Empty ones are left off too — the chip names what
 *  is there. Shares are of every cell inside, so nested populations sum past 100%. */
export function lensChipPops(
  pops: readonly LensPopCount[], cells: number, max = 8,
): { shown: (LensPopCount & { share: string })[]; more: number } {
  const full = pops.filter(p => p.n > 0).sort((a, b) => b.n - a.n)
  // a name two shown populations share (two branches' "CD4") takes its parent: "T/CD4", "NK/CD4"
  const seen = new Map<string, number>()
  for (const p of full) seen.set(p.name, (seen.get(p.name) ?? 0) + 1)
  const label = (p: LensPopCount) => {
    if ((seen.get(p.name) ?? 0) < 2) return p.name
    const parts = p.key.split('|').pop()!.split('/').filter(Boolean)
    return parts.length >= 2 ? `${parts[parts.length - 2]}/${p.name}` : p.name
  }
  return { shown: full.slice(0, max).map(p => ({ ...p, name: label(p), share: lensShare(p.n, cells) })),
           more: Math.max(0, full.length - max) }
}
