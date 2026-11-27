// Image-aware slider ranges for µm task params.
//
// A spec's `min`/`max`/`step` for a length were picked on intravital 2P data (~0.3–1 µm/px, cells
// ~10 µm). On finer data — SIM at 0.04 µm/px, objects of 0.2–0.7 µm — the whole useful span sits in
// the slider's first step and its floor (`windowSize` ≥ 5 µm, `rMax` ≥ 10 µm) is above it. One rule,
// by role, for every param whose spec declares `unit: "um"` (a length) or `"um2"` (an area): the range
// is scaled by a factor derived from the selected images' pixel size, or one the user picks. Nothing
// task-specific lives here. The spec range is the reference range at ×1; the server checks a unit
// param only for sign (`_validate_leaf`), so a value typed outside the slider still runs.
//
// See docs/MODULES.md → *Length params (`unit`)*.
import type { ParamDef } from './types'
import type { CciaImage } from '../stores/project'
import { pixelSizeUm } from '../utils/physicalSize'
import { niceStep } from '../utils/niceStep'

/** µm/px the µm defaults and ranges were tuned at (the low end of the intravital 2P data: zolIMa at
 *  0.33 µm/px). An image at or above it gets ×1; finer images scale down. */
export const UM_REFERENCE_PX = 0.3

/** The factors on offer, coarse → fine (1-2-5 steps so a scaled range reads as round numbers). */
export const UM_SCALES = [5, 2, 1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01] as const

export type UmUnit = 'um' | 'um2'

export function isUmParam(p: Pick<ParamDef, 'unit' | 'type'>): boolean {
  return (p.unit === 'um' || p.unit === 'um2') && (p.type === 'int' || p.type === 'float')
}

/** Is `v` a value a length param refuses? Only the sign is a bound: < 0, or ≤ 0 when the spec's floor
 *  is above 0 (there 0 means nothing, not "off"). The one frontend copy of `_validate_range`'s rule. */
export function badLength(p: Pick<ParamDef, 'min'>, v: number): boolean {
  return (p.min ?? 0) > 0 ? v <= 0 : v < 0
}

/** The finest calibrated pixel size across the images (µm/px), and how many images carry one.
 *  Finest, not first: the finest image is the one the ×1 range is useless for, and a coarser image in
 *  the same batch can still type its value. Uncalibrated images are skipped. */
export function finestPixelUm(images: Pick<CciaImage, 'physicalSizeX' | 'physicalSizeUnit'>[] | undefined):
    { um: number; calibrated: number; distinct: boolean } | null {
  const sizes = (images ?? [])
    .map(i => pixelSizeUm(i.physicalSizeX, i.physicalSizeUnit))
    .filter((v): v is number => typeof v === 'number' && v > 0)
  if (!sizes.length) return null
  const um = Math.min(...sizes)
  return { um, calibrated: sizes.length, distinct: sizes.some(v => Math.abs(v - um) > um * 1e-3) }
}

/** The factor a pixel size earns: 1 at or above the reference, else the nearest `UM_SCALES` entry
 *  (log distance) to `px / UM_REFERENCE_PX`. Never above 1 — coarse data keeps the spec range,
 *  because cells do not get bigger when the pixels do. */
export function autoUmScale(pxUm: number | null | undefined): number {
  if (pxUm == null || !(pxUm > 0) || pxUm >= UM_REFERENCE_PX) return 1
  const want = Math.log10(pxUm / UM_REFERENCE_PX)
  let best = 1
  for (const s of UM_SCALES) {
    if (s > 1) continue
    if (Math.abs(Math.log10(s) - want) < Math.abs(Math.log10(best) - want)) best = s
  }
  return best
}

/** Snap to a multiple of `step`, without float residue (0.1 × 3 = 0.30000000000000004). */
function onStep(x: number, step: number): number {
  const decimals = Math.max(0, -Math.floor(Math.log10(step)) + 1)
  return +(Math.round(x / step) * step).toFixed(decimals)
}

export interface SliderRange { min: number; max: number; step: number }

/** The slider range for a param at `scale`: the spec's own range for a param without a unit (or at
 *  ×1); otherwise min/max × scale (× scale² for an area) and the step rounded to a 1-2-5 value. An int
 *  param keeps whole-number positions. */
export function umSliderRange(p: Pick<ParamDef, 'unit' | 'type' | 'min' | 'max' | 'step'>,
                              scale: number): SliderRange {
  const base: SliderRange = {
    min: p.min ?? 0,
    max: p.max ?? 100,
    step: p.step ?? (p.type === 'int' ? 1 : 0.01),
  }
  if (!isUmParam(p) || scale === 1 || !(scale > 0)) return base
  const k = p.unit === 'um2' ? scale * scale : scale
  // the 1-2-5 at or above; the input trimmed first, or 0.1² = 0.010000000000000002 rounds up to 0.02
  let step = +niceStep(+(base.step * k).toPrecision(9)).toPrecision(6)
  if (p.type === 'int') step = Math.max(1, Math.round(step))
  const min = p.type === 'int' ? Math.floor(base.min * k) : onStep(base.min * k, step)
  let max = p.type === 'int' ? Math.ceil(base.max * k) : onStep(base.max * k, step)
  if (max <= min) max = min + step
  return { min, max, step }
}

/** "×0.1" — the factor as the form shows it. */
export function umScaleLabel(scale: number): string {
  return `×${+scale.toPrecision(3)}`
}

/** Does the form hold a `unit` param anywhere (sections and groups included)? */
export function hasUmParam(params: ParamDef[] | undefined): boolean {
  return (params ?? []).some(p => isUmParam(p) || hasUmParam(p.params))
}

/** The scale-control tooltip: what auto picked and from what. */
export function umAutoReason(images: Parameters<typeof finestPixelUm>[0]): string {
  const f = finestPixelUm(images)
  if (!f) return 'No pixel size on the selected images'
  const px = `${+f.um.toPrecision(3)} µm pixels`
  return f.distinct ? `From the finest selected image: ${px}` : `From ${px}`
}

/** Per-project storage key for a picked factor (absent = auto). */
export function umScaleKey(projectUid: string): string {
  return `cc.umScale.${projectUid}`
}

/** A stored factor, or null (auto) for anything that is not one on offer. */
export function parseUmScale(raw: string | null | undefined): number | null {
  const n = Number(raw)
  return raw != null && raw !== '' && (UM_SCALES as readonly number[]).includes(n) ? n : null
}
