/**
 * What within-stack XY alignment DOES — the figure offered beside
 * `cleanupImages.stackAlign`'s `referenceMode` param.
 *
 * **Why this exists.** The task's story is three simultaneous things:
 *   1. per-plane XY shifts are ESTIMATED against a reference plane,
 *   2. two gates (confidence + max-shift) REFUSE to force shifts on
 *      planes that look structurally different rather than moved,
 *   3. what survives the gates gets APPLIED as a subpixel warp.
 * A slider (min-conf, max-shift) without a picture leaves the user
 * guessing which planes will pass and which won't. Three schematic
 * columns show the trade-off with the current form values in play, the
 * same construction `smoothVis` and `driftVis` use.
 *
 * **The three scenarios**, one per column:
 *   1. `stackMovement` — the tissue is offset lateraly plane-to-plane
 *      (breathing DURING acquisition). The aligned column is the target
 *      state: all planes back to the reference's position.
 *   2. `structural` — planes 0 and 4 are structurally different from
 *      the middle (e.g. different depths in a real 3-D volume). The
 *      confidence gate correctly REFUSES to shift them, so the aligned
 *      column preserves the depth structure rather than mangling it.
 *   3. `mixedRef` — the middle plane (default anchor) is itself
 *      motion-blurred; anchoring on it drags the smear laterally, while
 *      the "sharpest" reference picks a clean plane and produces the
 *      right alignment.
 *
 * **The picture reads the current form values.** `minConfidence`,
 * `maxShiftPx` and `referenceMode` feed the gate simulator so the
 * "kept / skipped" state matches what the runner will do. Same live
 * behaviour as `driftVis`'s `maxLag` / `maxAngleDeg` cells.
 */
import type { VisColumns, VisRow, VisCell, VisFrame } from './paramVis'

/** The schematic's resolution. Bigger than driftVis's 24 because a Z stack has 5-7 columns of
 *  content (one per plane) and the eye reads them as thumbnails; 40x40 keeps each plane
 *  legible when the whole figure is drawn at defaultW ~ 600. */
export const N = 40

/** Planes in the schematic stack. Enough that the "structurally different" scenario has an
 *  edge plane not adjacent to the ref, few enough that the whole row of thumbnails still
 *  fits at defaultW. */
export const Z = 5

/** Deterministic PRNG — same construction and reason as `driftVis`. */
function mulberry32(seed: number): () => number {
  let a = seed >>> 0
  return () => {
    a = (a + 0x6D2B79F5) >>> 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

const zeros = (): VisFrame => Array.from({ length: N }, () => new Array<number>(N).fill(0))

const BLOBS: Array<[number, number]> = [[10, 12], [16, 30], [28, 10], [26, 28]]
const BLOB_SIGMA = 2.6

function baseFrame(seed = 7): VisFrame {
  const rnd = mulberry32(seed)
  const f = zeros()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    f[y][x] = 0.04 + 0.06 * rnd()                                  // low-level shot noise
  }
  for (const [cy, cx] of BLOBS) {
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
      const d2 = (y - cy) ** 2 + (x - cx) ** 2
      f[y][x] += 0.9 * Math.exp(-d2 / (2 * BLOB_SIGMA * BLOB_SIGMA))
    }
  }
  return f
}

/** Bilinear sample of `src` at fractional (y, x). Clamped at the edges. */
function sample(src: VisFrame, y: number, x: number): number {
  const y0 = Math.floor(y), x0 = Math.floor(x)
  const y1 = y0 + 1, x1 = x0 + 1
  const cy = Math.min(N - 1, Math.max(0, y0)), fy = Math.min(N - 1, Math.max(0, y1))
  const cx = Math.min(N - 1, Math.max(0, x0)), fx = Math.min(N - 1, Math.max(0, x1))
  const dy = y - y0, dx = x - x0
  return (1 - dy) * ((1 - dx) * src[cy][cx] + dx * src[cy][fx])
       +      dy  * ((1 - dx) * src[fy][cx] + dx * src[fy][fx])
}

/** Translate `src` by (ty, tx). Positive ty is "content down". */
export function translate(src: VisFrame, ty: number, tx: number): VisFrame {
  const out = zeros()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    out[y][x] = sample(src, y - ty, x - tx)
  }
  return out
}

/** Box-blur — used to model the smeared reference plane in the `mixedRef` case. Keeps the four
 *  blobs identifiable but reduces PC's confidence peak, which is what the aligner sees. */
export function blur(src: VisFrame, r = 2): VisFrame {
  const out = zeros()
  const w = 2 * r + 1
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    let s = 0
    for (let dy = -r; dy <= r; dy++) for (let dx = -r; dx <= r; dx++) {
      s += sample(src, y + dy, x + dx)
    }
    out[y][x] = s / (w * w)
  }
  return out
}

/** Structural-difference plane: a fresh set of blobs at NEW positions, not `base` translated.
 *  No rigid XY shift can bring this plane onto the reference — the confidence gate has to
 *  reject it. The eye reads this as "different depth in a real 3D volume". */
export function structural(_base: VisFrame, seed: number): VisFrame {
  const rnd = mulberry32(seed)
  const f = zeros()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    f[y][x] = 0.04 + 0.06 * rnd()
  }
  // Four blobs at different positions than BLOBS — the "different depth" look. Same count,
  // similar amplitude, so the two planes have COMPARABLE energy (the gate must decide on
  // structural mismatch, not on brightness alone).
  const other: Array<[number, number]> = [[7, 22], [22, 8], [30, 20], [15, 34]]
  for (const [cy, cx] of other) {
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
      const d2 = (y - cy) ** 2 + (x - cx) ** 2
      f[y][x] += 0.9 * Math.exp(-d2 / (2 * BLOB_SIGMA * BLOB_SIGMA))
    }
  }
  return f
}

/**
 * The three scenarios' inputs — each is a stack of Z planes.
 *   - `stackMovement`: base translated by increasing amounts per plane.
 *   - `structural`: middle planes ≈ base; z=0 and z=Z-1 are structurally
 *      different (different tissue at depth).
 *   - `mixedRef`: like `stackMovement` but the MIDDLE plane is blurred
 *      (breath during that plane's raster), so anchoring on it is bad.
 */
export function buildScenarios(seed = 7): Record<'stackMovement' | 'structural' | 'mixedRef', VisFrame[]> {
  const base = baseFrame(seed)
  const stackMovement: VisFrame[] = []
  for (let z = 0; z < Z; z++) {
    // Increasing shift with z, symmetric around the middle plane so the mid-anchor makes sense.
    const t = z - Math.floor(Z / 2)
    stackMovement.push(translate(base, 1.4 * t, 0.9 * t))
  }
  const structural: VisFrame[] = []
  for (let z = 0; z < Z; z++) {
    if (z === 0 || z === Z - 1) structural.push(structuralPlane(base, z))
    else structural.push(base)
  }
  const mixedRef: VisFrame[] = []
  for (let z = 0; z < Z; z++) {
    const t = z - Math.floor(Z / 2)
    const plane = translate(base, 1.4 * t, 0.9 * t)
    // Blur only the middle plane — that IS the "middle is smeared" case.
    mixedRef.push(z === Math.floor(Z / 2) ? blur(plane, 2) : plane)
  }
  return { stackMovement, structural, mixedRef }
}

function structuralPlane(base: VisFrame, seed: number): VisFrame {
  return structural(base, seed + 101)
}

/**
 * Cross-correlation of `moving` on `fixed` — same construction as `driftVis.phaseAlign`, but
 * returns both the (ty, tx) fit AND a normalised peak strength that we use as the confidence
 * signal. Peak strength: peak / (mean absolute correlation) — bounded ~[0, ~N^2 for a bright
 * unique match]; rescaled to [0, 1] via a saturating map for the gate simulator.
 */
export function align(fixed: VisFrame, moving: VisFrame): { ty: number; tx: number; conf: number } {
  const meanOf = (f: VisFrame) => f.flat().reduce((s, v) => s + v, 0) / (N * N)
  const mf = meanOf(fixed), mm = meanOf(moving)

  let bestY = 0, bestX = 0, bestC = -Infinity
  const corr: number[][] = Array.from({ length: 2 * N - 1 }, () => new Array<number>(2 * N - 1).fill(0))
  let sumAbs = 0
  for (let dy = -(N - 1); dy < N; dy++) {
    for (let dx = -(N - 1); dx < N; dx++) {
      let s = 0
      for (let y = 0; y < N; y++) {
        const yy = y + dy
        if (yy < 0 || yy >= N) continue
        for (let x = 0; x < N; x++) {
          const xx = x + dx
          if (xx < 0 || xx >= N) continue
          s += (fixed[y][x] - mf) * (moving[yy][xx] - mm)
        }
      }
      corr[dy + N - 1][dx + N - 1] = s
      sumAbs += Math.abs(s)
      if (s > bestC) { bestC = s; bestY = dy; bestX = dx }
    }
  }
  const iy = bestY + N - 1, ix = bestX + N - 1
  const refine = (a: number, b: number, c: number) => {
    const denom = a - 2 * b + c
    return Math.abs(denom) < 1e-9 ? 0 : 0.5 * (a - c) / denom
  }
  const dy = (iy > 0 && iy < 2 * N - 2) ? refine(corr[iy - 1][ix], corr[iy][ix], corr[iy + 1][ix]) : 0
  const dx = (ix > 0 && ix < 2 * N - 2) ? refine(corr[iy][ix - 1], corr[iy][ix], corr[iy][ix + 1]) : 0

  const meanAbs = sumAbs / ((2 * N - 1) * (2 * N - 1))
  // Peak-to-mean ratio, then squashed to [0, 1] via 1 - 1/(1+r/K). K chosen so a clean match
  // (ratio ~40 in this scene) lands at ~0.85 and a noise-dominated match (ratio ~3) at ~0.35.
  const ratio = meanAbs > 1e-9 ? bestC / meanAbs : 0
  const conf = Math.max(0, 1 - 1 / (1 + Math.max(0, ratio) / 6))
  return { ty: bestY + dy, tx: bestX + dx, conf }
}

/** Sharpness proxy — same shape as the Python-side `_plane_sharpness`. Used by `pickRef` to
 *  choose an anchor when `referenceMode === 'sharpest'`. */
export function sharpness(f: VisFrame): number {
  let s = 0
  for (let y = 1; y < N; y++) for (let x = 1; x < N; x++) {
    s += Math.abs(f[y][x] - f[y - 1][x]) + Math.abs(f[y][x] - f[y][x - 1])
  }
  return s / (N * N)
}

export function pickRef(stack: VisFrame[], mode: 'middle' | 'sharpest'): number {
  if (mode === 'sharpest') {
    let best = 0, bestS = -Infinity
    for (let z = 0; z < stack.length; z++) {
      const s = sharpness(stack[z])
      if (s > bestS) { bestS = s; best = z }
    }
    return best
  }
  return Math.floor(stack.length / 2)
}

export interface StackAlignVisInput {
  referenceMode: 'middle' | 'sharpest'
  minConfidence: number
  maxShiftPx: number
}

interface AlignedStack {
  input: VisFrame[]
  aligned: VisFrame[]
  refIdx: number
  applied: boolean[]
  conf: number[]
  shifts: Array<[number, number]>
}

/**
 * Apply the aligner (as it will run on the user's data) to one scenario's stack. Returns the
 * aligned stack + which planes passed the gate. Determinism matters — the same input +
 * settings always gives the same picture.
 */
export function alignStack(stack: VisFrame[], inp: StackAlignVisInput): AlignedStack {
  const refIdx = pickRef(stack, inp.referenceMode)
  const ref = stack[refIdx]
  const aligned: VisFrame[] = stack.map((_, i) => (i === refIdx ? stack[i] : stack[i]))
  const applied: boolean[] = stack.map((_, i) => i === refIdx)
  const conf: number[]     = stack.map((_, i) => (i === refIdx ? 1 : 0))
  const shifts: Array<[number, number]> = stack.map(() => [0, 0])
  for (let z = 0; z < stack.length; z++) {
    if (z === refIdx) continue
    const { ty, tx, conf: c } = align(ref, stack[z])
    conf[z] = c
    const mag = Math.hypot(ty, tx)
    if (c >= inp.minConfidence && mag <= inp.maxShiftPx) {
      aligned[z] = translate(stack[z], -ty, -tx)
      applied[z] = true
      shifts[z] = [ty, tx]
    }
  }
  return { input: stack, aligned, refIdx, applied, conf, shifts }
}

function cell(text: string, frames?: VisFrame[]): VisCell {
  return { value: 0, px: null, r: 0, at: 0, text, pxText: '', frames }
}

/** Normalise stacks together so the display's opacity range is comparable across columns. */
function normaliseAll(...seqs: VisFrame[][]): VisFrame[][] {
  let peak = 0
  for (const s of seqs) for (const f of s) for (const row of f) for (const v of row) {
    if (v > peak) peak = v
  }
  const k = peak > 0 ? 1 / peak : 0
  return seqs.map(s => s.map(f => f.map(row => row.map(v => Math.min(1, Math.max(0, v * k))))))
}

/** A one-line summary of what happened to a scenario, matching the tone of `driftVerdict`. */
function verdictFor(kind: 'stackMovement' | 'structural' | 'mixedRef', a: AlignedStack): string {
  const nApplied = a.applied.filter(x => x).length
  const total = a.applied.length
  if (kind === 'stackMovement') {
    return nApplied === total ? 'all planes aligned' : `${nApplied}/${total} aligned`
  }
  if (kind === 'structural') {
    const nSkipped = total - nApplied
    return nSkipped >= 2 ? 'edges skipped (depth preserved)' : `${nApplied}/${total} aligned`
  }
  // mixedRef
  return `ref: z=${a.refIdx}`
}

/**
 * Build the figure. Two `grid` rows per column — INPUT stack and ALIGNED stack, so the user
 * sees what the aligner did — plus a case label and a per-column verdict. Same three-row shape
 * as the other vis-aids.
 */
export function stackAlignVisColumns(inp: StackAlignVisInput): VisColumns {
  const s = buildScenarios()
  const rMove = alignStack(s.stackMovement, inp)
  const rStr  = alignStack(s.structural,   inp)
  const rMix  = alignStack(s.mixedRef,     inp)

  const [i1, a1, i2, a2, i3, a3] = normaliseAll(
    rMove.input, rMove.aligned,
    rStr.input,  rStr.aligned,
    rMix.input,  rMix.aligned,
  )
  const gridInput: VisCell[]   = [cell('', i1), cell('', i2), cell('', i3)]
  const gridAligned: VisCell[] = [cell('', a1), cell('', a2), cell('', a3)]
  const caseCells: VisCell[] = [
    cell('offset planes'),
    cell('different depths'),
    cell('middle plane smeared'),
  ]
  const verdictCells: VisCell[] = [
    cell(verdictFor('stackMovement', rMove)),
    cell(verdictFor('structural',   rStr)),
    cell(verdictFor('mixedRef',     rMix)),
  ]

  const rows: VisRow[] = [
    { key: 'input',   label: 'Input',   role: 'grid', uniform: false, cells: gridInput },
    { key: 'aligned', label: 'Aligned', role: 'grid', uniform: false, cells: gridAligned },
    { key: 'case',    label: 'Case',    role: 'text', uniform: false, cells: caseCells },
    { key: 'verdict', label: 'Output',  role: 'text', uniform: false, cells: verdictCells },
  ]
  return { columns: [...SCENARIOS], rows, pxSize: null, uniformKeys: [] }
}

export const SCENARIOS = ['stackMovement', 'structural', 'mixedRef'] as const

/** The line under the figure — what the columns are saying. Not a verdict about the user's
 *  data (the schematic is synthetic); matches driftVerdict's discipline of naming what the
 *  figure shows rather than restating the form values. */
export function stackAlignVerdict(inp: StackAlignVisInput): string {
  return `ref: ${inp.referenceMode} · gates: conf ≥ ${inp.minConfidence.toFixed(2)}, |shift| ≤ ${inp.maxShiftPx} px`
       + '. Aligner shifts planes back to the anchor; gates leave structurally different planes untouched.'
}

/** What a consumer mounts. */
export function stackAlignFigure(inp: StackAlignVisInput): { vis: VisColumns; note: string } {
  return { vis: stackAlignVisColumns(inp), note: stackAlignVerdict(inp) }
}
