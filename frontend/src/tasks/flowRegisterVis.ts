/**
 * What flow-based per-pixel registration DOES — the figure offered beside
 * `cleanupImages.flowRegister`'s `referenceMode` param.
 *
 * **Why this exists.** flowRegister is the third rigidness tier in the cleanup
 * pipeline (after per-frame rigid `driftCorrect` and per-plane rigid
 * `stackAlign`), and the leap from "one translation per frame" to "one
 * translation per pixel" is invisible in a slider. Three schematic columns
 * show the three regimes that separate a per-pixel warp from any rigid one.
 *
 * **The three scenarios**, one per column:
 *   1. `nonrigidFlex` — 4 quadrants translate INDEPENDENTLY per frame (a
 *      moving sample captured row-by-row during a resonant scan produces
 *      exactly this signature). Rigid frame alignment cannot correct it;
 *      dense per-pixel flow CAN. The aligned column is the target state:
 *      adjacent-frame differences collapse to the reference geometry.
 *   2. `bulkDrift` — the whole scene translates uniformly per frame. Both
 *      driftCorrect and flowRegister handle it, but with a different signature
 *      depending on `referenceMode`: `previous` preserves the cumulative drift
 *      trajectory (each frame aligned to the last), `first` pins every frame
 *      to t=0 (fully collapses the drift).
 *   3. `staticScene` — no motion. flowRegister should be a NEAR-IDENTITY: the
 *      clamp guards against wild flow at low-signal regions from accidentally
 *      warping the scene. Silent no-op on a quiet movie is the safe baseline.
 *
 * **The picture reads the current form values.** `referenceMode`,
 * `aggressiveness` (mapped to a search radius) and `maxShiftPx` feed the
 * simulator so the warped column matches what the runner will do. Same live
 * behaviour as `stackAlignVis`'s gate cells.
 */
import type { VisColumns, VisRow, VisCell, VisFrame } from './paramVis'

/** Schematic resolution. Same 40 as stackAlignVis so quadrants read at 20×20. */
export const N = 40
/** Timepoints per scenario. Enough to show a drift trajectory building up. */
export const T = 5

/** Deterministic PRNG — same as stackAlignVis / driftVis. */
function mulberry32(seed: number): () => number {
  let a = seed >>> 0
  return () => {
    a = (a + 0x6D2B79F5) >>> 0
    let t = Math.imul(a ^ (a >>> 15), 1 | a)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

const zeros = (): VisFrame =>
  Array.from({ length: N }, () => new Array<number>(N).fill(0))

/** Four blobs, one per quadrant, so per-quadrant warps have distinct signal. */
const BLOBS: Array<[number, number]> = [[10, 10], [10, 30], [30, 10], [30, 30]]
const BLOB_SIGMA = 2.6

function baseFrame(seed = 11): VisFrame {
  const rnd = mulberry32(seed)
  const f = zeros()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    f[y][x] = 0.04 + 0.06 * rnd()
  }
  for (const [cy, cx] of BLOBS) {
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
      const d2 = (y - cy) ** 2 + (x - cx) ** 2
      f[y][x] += 0.9 * Math.exp(-d2 / (2 * BLOB_SIGMA * BLOB_SIGMA))
    }
  }
  return f
}

/** Bilinear sample at fractional (y, x), clamped at edges. */
function sample(src: VisFrame, y: number, x: number): number {
  const y0 = Math.floor(y), x0 = Math.floor(x)
  const y1 = y0 + 1, x1 = x0 + 1
  const cy = Math.min(N - 1, Math.max(0, y0)), fy = Math.min(N - 1, Math.max(0, y1))
  const cx = Math.min(N - 1, Math.max(0, x0)), fx = Math.min(N - 1, Math.max(0, x1))
  const dy = y - y0, dx = x - x0
  return (1 - dy) * ((1 - dx) * src[cy][cx] + dx * src[cy][fx])
       +      dy  * ((1 - dx) * src[fy][cx] + dx * src[fy][fx])
}

export function translate(src: VisFrame, ty: number, tx: number): VisFrame {
  const out = zeros()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    out[y][x] = sample(src, y - ty, x - tx)
  }
  return out
}

/**
 * Per-quadrant warp — the schematic's stand-in for a dense per-pixel flow field.
 * A real Farneback estimates flow at every pixel; here 2×2 quadrants read as
 * "not one number per frame" which is the point.
 */
function warpQuadrants(
  src: VisFrame,
  shifts: Array<[number, number]>,   // one (ty, tx) per quadrant, TL/TR/BL/BR order
): VisFrame {
  const out = zeros()
  const half = N / 2
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    const q = (y < half ? 0 : 2) + (x < half ? 0 : 1)
    const [ty, tx] = shifts[q]
    out[y][x] = sample(src, y - ty, x - tx)
  }
  return out
}

/**
 * Cross-correlation search within ±maxShift over one QUADRANT of the frame.
 * O(quadrant² × search²), fine at 20×20 × 11×11 ≈ 48k ops per quadrant.
 */
function alignQuadrant(
  fixed: VisFrame, moving: VisFrame,
  y0: number, x0: number, size: number,
  maxShift: number,
): { ty: number; tx: number } {
  let best = { ty: 0, tx: 0, s: -Infinity }
  for (let dy = -maxShift; dy <= maxShift; dy++) {
    for (let dx = -maxShift; dx <= maxShift; dx++) {
      let s = 0
      for (let y = 0; y < size; y++) for (let x = 0; x < size; x++) {
        const fy = y0 + y, fx = x0 + x
        const my = fy + dy, mx = fx + dx
        if (my < 0 || my >= N || mx < 0 || mx >= N) continue
        s += fixed[fy][fx] * moving[my][mx]
      }
      if (s > best.s) best = { ty: dy, tx: dx, s }
    }
  }
  return { ty: best.ty, tx: best.tx }
}

/** Aggressiveness → search radius (mirrors the Julia handler's winsize map, halved: the
 *  schematic operates in a smaller coordinate system). */
export function searchRadiusFor(aggressiveness: 'gentle' | 'balanced' | 'strong'): number {
  return aggressiveness === 'strong' ? 6 : aggressiveness === 'gentle' ? 2 : 4
}

/**
 * The three scenarios' input T-series.
 *   - `nonrigidFlex`: each quadrant drifts by its OWN direction/speed per frame.
 *   - `bulkDrift`: whole scene translates by (0.9, 0.9) per frame.
 *   - `staticScene`: base frame repeated. Small per-frame noise, no motion.
 */
export function buildScenarios(seed = 11):
    Record<'nonrigidFlex' | 'bulkDrift' | 'staticScene', VisFrame[]> {
  const base = baseFrame(seed)
  const nonrigidFlex: VisFrame[] = []
  const perQuadrant: Array<[number, number]> = [   // per-t incremental shift per quadrant
    [ 0.7,  0.2], [-0.4,  0.6], [ 0.2, -0.5], [-0.5, -0.3],
  ]
  for (let t = 0; t < T; t++) {
    if (t === 0) { nonrigidFlex.push(base); continue }
    nonrigidFlex.push(warpQuadrants(nonrigidFlex[t - 1], perQuadrant))
  }
  const bulkDrift: VisFrame[] = []
  for (let t = 0; t < T; t++) {
    bulkDrift.push(translate(base, 0.9 * t, 0.9 * t))
  }
  const staticScene: VisFrame[] = []
  const rnd = mulberry32(seed + 21)
  for (let t = 0; t < T; t++) {
    const f = zeros()
    for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
      f[y][x] = base[y][x] + (rnd() - 0.5) * 0.02     // tiny noise, no motion
    }
    staticScene.push(f)
  }
  return { nonrigidFlex, bulkDrift, staticScene }
}

export interface FlowRegisterVisInput {
  referenceMode: 'previous' | 'first'
  aggressiveness: 'gentle' | 'balanced' | 'strong'
  maxShiftPx: number
}

interface RegisteredStack {
  input: VisFrame[]
  aligned: VisFrame[]
  perFrameShiftMag: number[]   // per-frame max quadrant-shift magnitude (for verdict)
}

/**
 * Run the aligner on one scenario. Warps every frame (except t=0) to its reference
 * per `mode`, applying the max-shift clamp per quadrant.
 */
export function registerStack(stack: VisFrame[], inp: FlowRegisterVisInput): RegisteredStack {
  const search = searchRadiusFor(inp.aggressiveness)
  const clamp  = Math.max(0, inp.maxShiftPx)
  const half = N / 2
  const aligned: VisFrame[] = [stack[0]]
  const perFrameShiftMag: number[] = [0]
  for (let t = 1; t < stack.length; t++) {
    const ref = inp.referenceMode === 'first' ? stack[0] : aligned[t - 1]
    const mov = stack[t]
    const shifts: Array<[number, number]> = [[0, 0], [0, 0], [0, 0], [0, 0]]
    let maxMag = 0
    // TL, TR, BL, BR quadrants → indices 0, 1, 2, 3
    const quads: Array<[number, number]> = [[0, 0], [0, half], [half, 0], [half, half]]
    for (let q = 0; q < 4; q++) {
      const [y0, x0] = quads[q]
      const { ty, tx } = alignQuadrant(ref, mov, y0, x0, half, search)
      // Warp direction: shift moving BACK by (ty, tx) → apply (-ty, -tx).
      const mag = Math.hypot(ty, tx)
      if (mag > clamp) {
        shifts[q] = [0, 0]    // clamp: fall back to identity for this quadrant
      } else {
        shifts[q] = [-ty, -tx]
        if (mag > maxMag) maxMag = mag
      }
    }
    aligned.push(warpQuadrants(mov, shifts))
    perFrameShiftMag.push(maxMag)
  }
  return { input: stack, aligned, perFrameShiftMag }
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

/** One-line summary for a scenario. */
function verdictFor(
  kind: 'nonrigidFlex' | 'bulkDrift' | 'staticScene',
  r: RegisteredStack,
  mode: 'previous' | 'first',
): string {
  const maxShift = Math.max(...r.perFrameShiftMag).toFixed(1)
  if (kind === 'nonrigidFlex') {
    return `per-quadrant warp (max ${maxShift}px)`
  }
  if (kind === 'bulkDrift') {
    return mode === 'first'
      ? `pinned to t=0 (max ${maxShift}px)`
      : `each rolled to prev (max ${maxShift}px)`
  }
  // staticScene
  return `near-identity (max ${maxShift}px)`
}

/**
 * Build the figure. Two `grid` rows per column — INPUT T-series and ALIGNED T-series,
 * so the user sees what the aligner did — plus a case label and a per-column verdict.
 * Same shape as `stackAlignVis`.
 */
export function flowRegisterVisColumns(inp: FlowRegisterVisInput): VisColumns {
  const s = buildScenarios()
  const rFlex   = registerStack(s.nonrigidFlex, inp)
  const rDrift  = registerStack(s.bulkDrift,    inp)
  const rStatic = registerStack(s.staticScene,  inp)

  const [i1, a1, i2, a2, i3, a3] = normaliseAll(
    rFlex.input,   rFlex.aligned,
    rDrift.input,  rDrift.aligned,
    rStatic.input, rStatic.aligned,
  )
  const gridInput: VisCell[]     = [cell('', i1), cell('', i2), cell('', i3)]
  const gridAligned: VisCell[]   = [cell('', a1), cell('', a2), cell('', a3)]
  const caseCells: VisCell[] = [
    cell('regions drift differently'),
    cell('whole scene drifts'),
    cell('nothing moves'),
  ]
  const verdictCells: VisCell[] = [
    cell(verdictFor('nonrigidFlex', rFlex,   inp.referenceMode)),
    cell(verdictFor('bulkDrift',    rDrift,  inp.referenceMode)),
    cell(verdictFor('staticScene',  rStatic, inp.referenceMode)),
  ]

  // Live parameter rows — the values the sliders currently hold, per column. Same
  // "params visible in the figure, not just in the note" shape driftVis's 'Per-frame cap'
  // uses. All three knobs are GLOBAL (not per-column), so the value repeats — but the
  // user sees the numbers move as they change the form, which is the whole point.
  const searchPx = searchRadiusFor(inp.aggressiveness)
  const refText  = inp.referenceMode === 'first' ? 'first (t=0)' : 'previous (t-1)'
  const searchText = `±${searchPx} px (${inp.aggressiveness})`
  const clampText  = `≤ ${inp.maxShiftPx} px`
  const referenceCells: VisCell[] = [cell(refText),    cell(refText),    cell(refText)]
  const searchCells:    VisCell[] = [cell(searchText), cell(searchText), cell(searchText)]
  const clampCells:     VisCell[] = [cell(clampText),  cell(clampText),  cell(clampText)]

  const rows: VisRow[] = [
    { key: 'input',     label: 'Input',      role: 'grid', uniform: false, cells: gridInput },
    { key: 'aligned',   label: 'Registered', role: 'grid', uniform: false, cells: gridAligned },
    { key: 'case',      label: 'Case',       role: 'text', uniform: false, cells: caseCells },
    { key: 'verdict',   label: 'Output',     role: 'text', uniform: false, cells: verdictCells },
    { key: 'reference', label: 'Reference',  role: 'text', uniform: true,  cells: referenceCells },
    { key: 'search',    label: 'Search',     role: 'text', uniform: true,  cells: searchCells },
    { key: 'clamp',     label: 'Clamp',      role: 'text', uniform: true,  cells: clampCells },
  ]
  return { columns: [...SCENARIOS], rows, pxSize: null, uniformKeys: [] }
}

export const SCENARIOS = ['nonrigidFlex', 'bulkDrift', 'staticScene'] as const

/** Line under the figure — names what the columns show. Matches stackAlignVerdict's discipline. */
export function flowRegisterVerdict(inp: FlowRegisterVisInput): string {
  const searchPx = searchRadiusFor(inp.aggressiveness)
  return `ref: ${inp.referenceMode} · search: ${searchPx}px (${inp.aggressiveness}) · clamp: `
       + `|shift| ≤ ${inp.maxShiftPx}px`
       + '. Aligner warps each frame back to the reference geometry per region; '
       + 'clamp reverts high-flow pixels to the source.'
}

export function flowRegisterFigure(inp: FlowRegisterVisInput): { vis: VisColumns; note: string } {
  return { vis: flowRegisterVisColumns(inp), note: flowRegisterVerdict(inp) }
}
