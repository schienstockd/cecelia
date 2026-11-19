// The viewer's two pointer read-outs, the pure half: the opt-in cursor readout (what is under the
// pointer — position, every channel's intensity, the cell and its populations) and the Measure mode's
// ruler (a line's length in physical units). 2D plane view only. The window does the pointer events
// and the `/api/viewer/peek` round trip; everything that could be wrong silently lives here.
//
// Two silent failures this module exists to prevent:
//   1. Reading the intensity at the DISPLAYED level: a zoomed-out view draws level N, and a value
//      read there is a downsampled sample, not what is stored. Intensities are read at the L0 pixel;
//      the cell id at the displayed level's pixel (the click pick's discipline), so the id named is the
//      cell drawn under the pointer.
//   2. Measuring in pixels on an anisotropic image: x and y are scaled by their own voxel size
//      BEFORE the hypotenuse, never after.

import { screenToImagePx } from '../viewerPick'
import type { OrbitCamera, ViewerMeta, ViewerChannel } from '../volumeViewer'
import { shortUnit } from '../physicalSize'
import { channelHexFrom } from '../viewerProps'
import type { OverlayPop } from '../viewerOverlays'
import { isInSubtree, popRowLabels } from '../popName'

/** The pixel under a canvas point, at both resolutions the peek reads. */
export interface PeekPoint {
  /** Level-0 pixel — where the intensities are read. */
  x: number
  y: number
  /** The displayed level's pixel — where the cell id is read. */
  lx: number
  ly: number
  /** False on the margin around a zoomed-out image: nothing to read. */
  in: boolean
}

/** Canvas point (CSS px from the canvas top-left) → the pixel at L0 and at the displayed level
 *  (`levelNX`/`levelNY` are that level's dims). */
export function peekPoint(
  cx: number, cy: number, canvasW: number, canvasH: number,
  cam: OrbitCamera, meta: ViewerMeta, levelNX: number, levelNY: number,
): PeekPoint {
  const l0 = screenToImagePx(cx, cy, canvasW, canvasH, cam, meta)
  const lv = screenToImagePx(cx, cy, canvasW, canvasH, cam, meta, levelNX, levelNY)
  return { x: l0.x, y: l0.y, lx: lv.x, ly: lv.y, in: l0.in && lv.in }
}

/** Spatial unit the readouts print: the image's own unit as a symbol (OME spells `micrometer` out) when
 *  calibrated, else pixels. */
function xyUnit(meta: Pick<ViewerMeta, 'calibrated' | 'spaceUnit'>): string {
  return meta.calibrated.xy ? shortUnit(meta.spaceUnit) : 'px'
}

/** A length or coordinate, to a precision that suits its size. */
export function fmtMeasure(v: number): string {
  const a = Math.abs(v)
  return a >= 100 ? v.toFixed(0) : a >= 10 ? v.toFixed(1) : v.toFixed(2)
}

/** "x 12.3 · y 45.6 µm · z 3 · t 5" — the pixel's centre in physical units (pixels when uncalibrated);
 *  z and t are the plane and frame indices the viewer's own controls show. */
export function fmtPosition(
  p: { x: number; y: number }, z: number, t: number,
  meta: Pick<ViewerMeta, 'calibrated' | 'spaceUnit' | 'voxelUm' | 'nZ' | 'nT'>,
): string {
  const cal = meta.calibrated.xy
  const ux = cal ? meta.voxelUm[0] || 1 : 1
  const uy = cal ? meta.voxelUm[1] || 1 : 1
  const xy = `x ${fmtMeasure((p.x + 0.5) * ux)} · y ${fmtMeasure((p.y + 0.5) * uy)} ${xyUnit(meta)}`
  const zt = [meta.nZ > 1 ? `z ${z}` : '', meta.nT > 1 ? `t ${t}` : ''].filter(Boolean)
  return [xy, ...zt].join(' · ')
}

/** An intensity as stored: integers whole, floats to 3 significant digits. */
export function fmtIntensity(v: number): string {
  if (!Number.isFinite(v)) return String(v)
  return Number.isInteger(v) ? String(v) : String(Number(v.toPrecision(3)))
}

/** A ruler's length between two L0 points (floats). x and y scale by their own voxel size first. */
export function rulerLength(
  a: readonly [number, number], b: readonly [number, number],
  meta: Pick<ViewerMeta, 'calibrated' | 'spaceUnit' | 'voxelUm'>,
): { value: number; unit: string } {
  const cal = meta.calibrated.xy
  const dx = (b[0] - a[0]) * (cal ? meta.voxelUm[0] || 1 : 1)
  const dy = (b[1] - a[1]) * (cal ? meta.voxelUm[1] || 1 : 1)
  return { value: Math.hypot(dx, dy), unit: xyUnit(meta) }
}

/** A population the hovered cell is in, as the readout lists it. */
export interface ReadoutPop { name: string; colour: string }

/**
 * Cell id → the populations it is in, from the overlay payloads the viewer already draws (so the answer
 * is the one on screen, at no server cost). Only the DEEPEST pops: a cell in `/T/CD8/Activated` lists
 * `Activated`, not its two parents too. Built once per payload change — a hover is then a map lookup.
 */
export function labelPopIndex(pops: readonly Pick<OverlayPop, 'path' | 'name' | 'colour' | 'labels'>[]):
  Map<number, ReadoutPop[]> {
  const byLabel = new Map<number, typeof pops[number][]>()
  for (const p of pops) {
    for (const l of p.labels ?? []) {
      const list = byLabel.get(l)
      if (list) list.push(p)
      else byLabel.set(l, [p])
    }
  }
  // named as the Populations section lists them: two pops sharing a name (every tracked pop is
  // `_tracked`) by as much of their path as tells them apart
  const names = popRowLabels(pops)
  const nameOf = new Map(pops.map((p, i) => [p.path, names[i]!]))
  const out = new Map<number, ReadoutPop[]>()
  for (const [label, list] of byLabel) {
    const deepest = list.filter(p => !list.some(q => q.path !== p.path && isInSubtree(q.path, p.path)))
    out.set(label, deepest.map(p => ({ name: nameOf.get(p.path) ?? p.name, colour: p.colour })))
  }
  return out
}

/** Rows per column in the readout's channel list — past this a multiplex panel wraps into columns
 *  rather than running down the image. */
export const READOUT_CHANNEL_ROWS = 6

/**
 * The channels the readout lists: only those drawn (a hidden channel's value is noise at a glance), each
 * with its display colour, plus how many were left out. `values` is one per channel, in channel order.
 */
export function readoutChannels(
  values: readonly number[], channels: readonly Pick<ViewerChannel, 'name' | 'visible' | 'lut'>[],
): { shown: { name: string; value: string; colour: string }[]; hidden: number } {
  const shown: { name: string; value: string; colour: string }[] = []
  let hidden = 0
  values.forEach((v, i) => {
    const ch = channels[i]
    if (ch && !ch.visible) { hidden++; return }
    shown.push({ name: ch?.name ?? `Channel ${i + 1}`, value: fmtIntensity(v), colour: ch ? channelHexFrom(ch.lut) : '' })
  })
  return { shown, hidden }
}
