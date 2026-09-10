/**
 * What Z-plane coupling DOES — the figure offered beside `driftZSmoothness`.
 *
 * **Why this exists.** `driftPerPlane` fits one shift per (t, Z), which is exactly what a movie
 * with breathing-shear across depth needs — but every plane's fit is now independent, so a plane
 * with weak signal (a low-content Z, a temporary blur, an out-of-focus optical slice) contributes
 * a noisy or nonsense shift and the corrected movie shows that plane jumping alone against its
 * neighbours. `driftZSmoothness` is the post-solve gaussian across Z that pulls each plane's
 * shift toward its neighbours'; σ = 0 is fully independent, σ high is fully coupled (all planes
 * take the mean shift, which is whole-volume drift correction). The number is a knob without a
 * mental picture — this figure gives you the picture at the current σ.
 *
 * **The three scenarios.** Chosen to bracket the two failure modes:
 *
 *   1. `shear`    — a linear ramp of shifts across Z. Shallow planes move one way, deep planes
 *      move the other — real breathing shear, the case `driftPerPlane` was built for. Coupling
 *      must LEAVE THIS ALONE (edge effects dampen the extremes slightly, per the plan; the shear
 *      direction is preserved).
 *   2. `outlier`  — the same ramp with one plane (Z = middle) fit to a wildly wrong shift. What
 *      a blank / low-content plane looks like in the sidecar. Coupling should pull that plane
 *      back to its neighbours — THE win.
 *   3. `noise`    — independent random shifts around zero. What per-plane fitting produces on a
 *      well-behaved movie: the estimator faithfully reports the phase-correlation noise floor
 *      per plane. Coupling collapses the wobble toward zero, matching what the whole-volume
 *      estimator would give.
 *
 * **What the picture shows.** Each column is a small side-view of a Z stack: `ZP` planes stacked
 * vertically (Z = 0 shallow at top, Z = ZP-1 deep at bottom), with a vertical axis line at
 * shift = 0. Two markers per plane: the raw per-plane shift (faint), and the smoothed shift the
 * writer would apply at the CURRENT σ (opaque). As σ climbs, watch:
 *   - column 1's smoothed dots stay near the ramp (shear preserved)
 *   - column 2's outlier plane snaps toward the ramp line (bad plane fixed)
 *   - column 3's dots collapse toward the centre axis (independent noise dampened)
 * σ = 0 is the identity (raw == smoothed everywhere); σ = 5 (the JSON max) is heavy coupling
 * that eats the shear at the extremes. The default JSON tip suggests 0.3 then 0.5 — this figure
 * is where you SEE the difference between those settings.
 *
 * **The smoother is real, not a hand-wave.** `gaussianSmoothZ` is the same 1D gaussian
 * convolution with `mode='nearest'` that `driftSmoothVis` uses (matching the Python-side
 * `_estimate_drift_per_plane` post-solve step). Same edge policy, same σ semantics — the picture
 * and the writer apply the same math.
 */
import type { VisColumns, VisRow, VisCell, VisFrame } from './paramVis'

/** Number of Z-planes drawn. 10 is enough that a ramp has room to slope AND an outlier at the
 *  middle has neighbours on both sides; small enough to fit each plane's band in ~3 rows on a
 *  panel sized around the other vis-aids. */
export const ZP = 10

/** Display width. A shift of ±(W/2 - 4) has to be legible — 48 gives each unit of shift ~2 px
 *  after the axis margin, which reads as movement rather than a jitter. */
export const W = 48
/** Rows per plane band. 3 gives each plane a legible thickness without stacking beyond the
 *  panel's ~ H = 30 budget. */
const BAND = 3
/** Display height. */
export const H = ZP * BAND

/** Half-range for shifts, in display units. Chosen so the ramp (±SHIFT_AMP) and the outlier
 *  (±(SHIFT_AMP + OUTLIER_EXTRA)) both fit inside the frame with a small margin. */
const AMP = W / 2 - 6

/** Deterministic PRNG — same construction and reason as `driftSmoothVis`. */
function mulberry32(seed: number): () => number {
  let a = seed >>> 0
  return () => {
    a = (a + 0x6D2B79F5) >>> 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

/** A shift-per-plane profile: one value per Z, in display units (px on the schematic). */
export type PlaneShifts = number[]

/**
 * Shear scenario — a linear ramp of shifts across Z, from +AMP*0.7 at the shallow plane to
 * −AMP*0.7 at the deep plane, with a bath of sub-pixel noise on top. What breathing shear looks
 * like in the pilot dataset (`zolIMa/x4E5HU` frames 40–41: shallow +4 px Y, deep −9 px Y). The
 * smoother should leave the interior planes on the ramp; the extremes dampen slightly, which is
 * the documented edge behaviour of a `mode='nearest'` gaussian and the reason column 1 exists.
 */
export function shearScenario(seed = 51): PlaneShifts {
  const rnd = mulberry32(seed)
  const out: PlaneShifts = []
  for (let z = 0; z < ZP; z++) {
    // Linear ramp from +A at z=0 (shallow) to −A at z=ZP-1 (deep). A=0.7*AMP so the ends have
    // margin before the outlier scenario's extra offset would clip the frame.
    const base = 0.7 * AMP * (1 - (2 * z) / (ZP - 1))
    out.push(base + (rnd() - 0.5) * 1.2)
  }
  return out
}

/** How far the outlier plane sits off the ramp, in display units. Sized so it lands clearly
 *  outside the [−AMP, +AMP] band the ramp lives in — a nonsense fit, not a hint. */
const OUTLIER_EXTRA = 10

/**
 * Outlier scenario — the same shear ramp, but plane Z = middle got a wildly wrong shift (a
 * blanked plane, a low-content optical slice). What the sidecar records when phase correlation
 * on that plane finds no peak. The smoother has to pull the outlier back toward the ramp; the
 * other planes should barely move. THE win of Z-coupling.
 */
export const OUTLIER_Z = Math.floor(ZP / 2)
export function outlierScenario(seed = 51): PlaneShifts {
  // Reuse the shear ramp as-is so `outlierScenario() == shearScenario() everywhere except at
  // OUTLIER_Z` — the figure reads clearer when column 2 IS column 1 plus one bad plane.
  const out = shearScenario(seed)
  // Push the outlier well off the ramp, on the OPPOSITE side of zero from where the ramp put
  // it. Copying the ramp value would let a reader mistake the outlier for the raw signal;
  // flipping the sign and adding an extra 10 px makes the misfit unambiguous at any σ.
  const rampAtOutlier = out[OUTLIER_Z]
  out[OUTLIER_Z] = -Math.sign(rampAtOutlier || 1) * (Math.abs(rampAtOutlier) + OUTLIER_EXTRA)
  return out
}

/**
 * Noise scenario — independent per-plane shifts, no shear, no bulk drift. What per-plane fitting
 * gives on a well-behaved movie: sub-pixel phase-correlation noise, one realisation per plane,
 * uncorrelated. Coupling collapses the wobble toward zero, which is what the whole-volume
 * estimator would have delivered in the first place — the trade being that you gave up per-Z
 * detail to get there.
 */
export function noiseScenario(seed = 53): PlaneShifts {
  const rnd = mulberry32(seed)
  const out: PlaneShifts = []
  for (let z = 0; z < ZP; z++) {
    // ±6 display-px, uncorrelated. Big enough that σ ≥ 1 visibly collapses it; small enough
    // that σ = 0 doesn't look like a shear (which would confuse the reader about column 1).
    out.push((rnd() - 0.5) * 12)
  }
  return out
}

/**
 * 1D gaussian convolution over the Z axis, with `mode='nearest'` at the edges — the same kernel
 * `driftSmoothVis.gaussianSmooth` uses, and the same policy the Python-side
 * `_estimate_drift_per_plane` post-solve step applies (via `scipy.ndimage.gaussian_filter1d`).
 * σ = 0 (or negative) is the identity, matching the Python shortcut when `z_smoothness <= 0`.
 *
 * The kernel is truncated at 4σ each side (the scipy default), which puts the tails below
 * 3e-4 of the peak — safe to drop.
 */
export function gaussianSmoothZ(shifts: PlaneShifts, sigma: number): PlaneShifts {
  if (sigma <= 0) return shifts.slice()
  const radius = Math.max(1, Math.ceil(4 * sigma))
  const kernel: number[] = []
  let ksum = 0
  for (let k = -radius; k <= radius; k++) {
    const w = Math.exp(-(k * k) / (2 * sigma * sigma))
    kernel.push(w); ksum += w
  }
  for (let i = 0; i < kernel.length; i++) kernel[i] /= ksum

  const out: PlaneShifts = []
  const n = shifts.length
  for (let i = 0; i < n; i++) {
    let s = 0
    for (let k = -radius; k <= radius; k++) {
      const j = Math.min(n - 1, Math.max(0, i + k))
      s += shifts[j] * kernel[k + radius]
    }
    out.push(s)
  }
  return out
}

/**
 * Draw one Z-stack side-view into a VisFrame: the vertical axis at shift = 0, one horizontal
 * band per plane (from Z = 0 at top to Z = ZP-1 at bottom), and per plane a faint marker at the
 * RAW shift plus an opaque marker at the SMOOTHED shift. A light connector line traces the
 * smoothed profile through the band centres, so the reader can see the whole shape at once.
 *
 * X convention: shift = 0 sits at column W/2, positive shifts to the right, negative to the
 * left — matches the on-screen "the sample moved right" mental model.
 *
 * Y convention: display grows downward (row 0 at top). Shallow plane at top, deep at bottom,
 * matching the physical orientation of the stack when the coverslip is up.
 */
export function renderPlaneShifts(raw: PlaneShifts, smoothed: PlaneShifts): VisFrame {
  const f: VisFrame = Array.from({ length: H }, () => new Array<number>(W).fill(0))
  const axisX = Math.floor(W / 2)
  const clampX = (x: number) => Math.min(W - 1, Math.max(0, x))

  // Zero-shift axis — a full-height dashed line at the centre column, dark enough to read as
  // the reference the shifts are measured against but not so dark it competes with the markers.
  for (let y = 0; y < H; y++) {
    if (y % 3 !== 2) f[y][axisX] = Math.max(f[y][axisX], 0.28)
  }
  // A short scale tick on the top edge: 5 "px" left + 5 right, so the amplitude has an anchor.
  for (const off of [-5, 5]) {
    const x = clampX(axisX + off)
    f[0][x] = Math.max(f[0][x], 0.35)
    f[1][x] = Math.max(f[1][x], 0.35)
  }

  // Smoothed connector — a faint line through the smoothed markers, so the whole Z profile is
  // legible as a shape (a ramp, a corrected ramp, a flat line). Drawn under the markers so it
  // does not overpaint them.
  const bandY = (z: number) => z * BAND + Math.floor(BAND / 2)
  for (let z = 1; z < ZP; z++) {
    const y0 = bandY(z - 1), y1 = bandY(z)
    const x0 = clampX(Math.round(axisX + smoothed[z - 1]))
    const x1 = clampX(Math.round(axisX + smoothed[z]))
    const dy = y1 - y0
    // step per row along the connector — integer approximation, cheap and legible
    for (let dy2 = 0; dy2 <= dy; dy2++) {
      const t = dy > 0 ? dy2 / dy : 0
      const x = clampX(Math.round(x0 + t * (x1 - x0)))
      const y = y0 + dy2
      f[y][x] = Math.max(f[y][x], 0.35)
    }
  }

  for (let z = 0; z < ZP; z++) {
    const yc = bandY(z)
    // Raw marker — small faint dot (2 rows tall, single column at the raw shift)
    const xr = clampX(Math.round(axisX + raw[z]))
    for (let y = Math.max(0, yc - 1); y <= Math.min(H - 1, yc + 1); y++) {
      f[y][xr] = Math.max(f[y][xr], 0.45)
    }
    // Smoothed marker — bright dot (2 rows tall, 3 columns wide) at the smoothed shift
    const xs = clampX(Math.round(axisX + smoothed[z]))
    for (let dy2 = -1; dy2 <= 1; dy2++) {
      const y = yc + dy2
      if (y < 0 || y >= H) continue
      for (let dx = -1; dx <= 1; dx++) {
        const x = clampX(xs + dx)
        const w = dx === 0 ? 1.0 : 0.75
        f[y][x] = Math.max(f[y][x], w)
      }
    }
  }
  return f
}

export const Z_SMOOTH_VIS_COLUMNS = ['shear', 'outlier', 'noise'] as const

/** Per-column case labels for the "Case" row. Short — the whole figure teaches the same trade
 *  three ways, so the label names the case, not the verdict. */
const CASE_LABEL: Record<typeof Z_SMOOTH_VIS_COLUMNS[number], string> = {
  shear:   'breathing shear (real signal)',
  outlier: 'one bad plane',
  noise:   'independent noise',
}

function cell(text: string, frames?: VisFrame[]): VisCell {
  return { value: 0, px: null, r: 0, at: 0, text, pxText: '', frames }
}

export interface DriftZSmoothVisInput {
  /** `driftZSmoothness` from the form — 0 (independent) up to 5 (fully coupled). Reads live so
   *  the picture redraws as the slider moves. */
  sigma: number
}

/**
 * Build the figure's columns. One `grid` row plus two text rows underneath: the case name and
 * a short read of what the CURRENT σ did to that column's smoothed shifts. Same three-row shape
 * `driftSmoothVis` and the other drift figures use.
 */
export function driftZSmoothVisColumns(inp: DriftZSmoothVisInput): VisColumns {
  const scenarios: Record<typeof Z_SMOOTH_VIS_COLUMNS[number], PlaneShifts> = {
    shear:   shearScenario(),
    outlier: outlierScenario(),
    noise:   noiseScenario(),
  }
  const gridCells: VisCell[] = []
  const verdictCells: VisCell[] = []

  for (const col of Z_SMOOTH_VIS_COLUMNS) {
    const raw = scenarios[col]
    const smoothed = gaussianSmoothZ(raw, inp.sigma)
    gridCells.push(cell('', [renderPlaneShifts(raw, smoothed)]))
    verdictCells.push(cell(verdictFor(col, raw, smoothed)))
  }

  const caseCells: VisCell[] = [...Z_SMOOTH_VIS_COLUMNS].map(c => cell(CASE_LABEL[c]))

  const rows: VisRow[] = [
    { key: 'stack',   label: 'Z stack',  role: 'grid', uniform: false, cells: gridCells },
    { key: 'case',    label: 'Case',     role: 'text', uniform: false, cells: caseCells },
    { key: 'verdict', label: 'Effect',   role: 'text', uniform: false, cells: verdictCells },
  ]
  return { columns: [...Z_SMOOTH_VIS_COLUMNS], rows, pxSize: null, uniformKeys: [] }
}

function range(vs: PlaneShifts): number {
  if (!vs.length) return 0
  let lo = vs[0], hi = vs[0]
  for (const v of vs) { if (v < lo) lo = v; if (v > hi) hi = v }
  return hi - lo
}

function absMax(vs: PlaneShifts): number {
  let m = 0
  for (const v of vs) if (Math.abs(v) > m) m = Math.abs(v)
  return m
}

/**
 * Per-column read of what σ did. Three cases distinguished by shape:
 *   - `shear`   — smoothed range still ≥ 60% of raw ⇒ shear preserved; below that ⇒ eaten
 *   - `outlier` — smoothed value at the outlier plane snapped back inside the ramp ⇒ fixed
 *   - `noise`   — smoothed max magnitude collapsed vs raw ⇒ dampened
 * Words are kept short (`docs/ui/COPY.md` — tips ≤ 90 chars, one clause).
 */
function verdictFor(col: typeof Z_SMOOTH_VIS_COLUMNS[number],
                    raw: PlaneShifts, smoothed: PlaneShifts): string {
  if (col === 'shear') {
    const rawR = range(raw), outR = range(smoothed)
    if (outR >= rawR * 0.85) return 'shear preserved'
    if (outR >= rawR * 0.5)  return `shear ${Math.round(100 * outR / rawR)}% preserved`
    return 'shear eaten'
  }
  if (col === 'outlier') {
    // "Fixed" ⇔ the outlier's smoothed shift landed inside the ramp band that its neighbours
    // occupy. Compare the outlier plane's smoothed value against its neighbours' RAW ramp
    // values (which the smoother should have pulled it toward).
    const neighboursRaw = [raw[OUTLIER_Z - 1], raw[OUTLIER_Z + 1]].filter(v => Number.isFinite(v))
    const target = neighboursRaw.reduce((s, v) => s + v, 0) / neighboursRaw.length
    const gap = Math.abs(smoothed[OUTLIER_Z] - target)
    const rawGap = Math.abs(raw[OUTLIER_Z] - target)
    if (rawGap === 0) return 'outlier fixed'
    const fixedFrac = 1 - gap / rawGap
    if (fixedFrac >= 0.75) return 'outlier fixed'
    if (fixedFrac >= 0.35) return `outlier ${Math.round(100 * fixedFrac)}% pulled in`
    return 'outlier still off'
  }
  // noise
  const rawM = absMax(raw), outM = absMax(smoothed)
  if (rawM === 0) return 'flat'
  if (outM <= rawM * 0.3)  return 'noise collapsed'
  if (outM <= rawM * 0.75) return `noise ${Math.round(100 * (1 - outM / rawM))}% dampened`
  return 'noise mostly kept'
}

/** The line under the figure. Names the trade-off σ carries at both ends — small σ leaves bad
 *  planes alone, large σ eats the shear that per-plane was turned on to correct. Same discipline
 *  as `driftSmoothVerdict`.
 *
 *  Thresholds are for THIS figure's ZP=10 planes; on real stacks the σ:planes ratio is what
 *  matters, and the note names planes as the unit so a reader with 40 planes can scale up. */
export function driftZSmoothVerdict(sigma: number): string {
  if (sigma <= 0)   return 'σ = 0 — no coupling. Each plane keeps its own shift.'
  if (sigma >= 3)   return `σ = ${sigma} planes — shear erased. Same as whole-volume.`
  if (sigma >= 1.5) return `σ = ${sigma} planes — shear softening. Lower σ if that matters.`
  return `σ = ${sigma} planes — bad planes fixed, shear kept.`
}

/** The figure and the line under it — what a consumer mounts. */
export function driftZSmoothFigure(inp: DriftZSmoothVisInput): { vis: VisColumns; note: string } {
  return { vis: driftZSmoothVisColumns(inp), note: driftZSmoothVerdict(inp.sigma) }
}
