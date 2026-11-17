/**
 * What the trajectory smoothing σ DOES — the figure offered beside `driftSmoothSigma`.
 *
 * **Why this exists.** The knob is a single number in frame units and it is easy to confuse two
 * distinct things it does — "kill sub-pixel jitter" is exactly what a movie with no real motion
 * needs, "eat real motion" is what happens when the kernel is wider than the motion's timescale.
 * A slider without a picture leaves the user guessing which regime they're in. Three schematic
 * columns for the three cases, all reading the CURRENT σ, so the trade-off is on screen the same
 * way `smoothVis` puts window count on screen.
 *
 * **The four scenarios.** Chosen to bracket the two failure modes, plus the case the smoother is
 * built not to touch:
 *
 *   1. `noise`  — a jittery random-walk trajectory that averages to zero. What the estimator
 *      returns on `zolIMa/2h06xA`: sub-pixel wobble around a still sample. Smoothing here has
 *      to collapse the rounded staircase toward zero.
 *   2. `ramp`   — a slow linear drift with a bath of sub-pixel noise. What the estimator returns
 *      on any well-behaved stage drift. Smoothing has to leave the rounded staircase alone: it
 *      is the SIGNAL, not the noise.
 *   3. `jerk`   — a sharp step (a fast displacement over a few frames), the kind of transition
 *      the `d5vw7z/ttRMjQ` movie shows around frame 30. Smoothing at a σ larger than the
 *      transition width rounds off the corners and delays the step — the "eating motion" regime.
 *   4. `bump`   — a stage bump: the sample jumps between two frames and stays. The smoother cuts
 *      the trajectory at a jump that stands out against the movie's own moves and smooths each
 *      side separately, so the bump stays one frame wide at any σ.
 *
 * **What the picture shows.** Each column is a small time-vs-position plot: the raw trajectory
 * (faint) and the INTEGER-ROUNDED trajectory the writer would apply (opaque). Rounded is the
 * one that matters — that is what shows up as visible integer-pixel jumps in the corrected
 * zarr. As σ climbs, watch the rounded staircase:
 *   - column 1's staircase flattens (jitter killed — the win)
 *   - column 2's staircase tracks the underlying slope (real drift preserved)
 *   - column 3's staircase softens at the corners and delays across the step (motion eaten)
 * σ=6 (the shipped default) sits at the largest kernel that still preserves column 3's step
 * shape at 25-frame width, chosen from the 2h06xA / ttRMjQ audit. Bigger σ trades column-3
 * fidelity for column-1 quiet.
 *
 * **All four trajectories and the smoother are real, not hand-waves.** `trajectorySmooth` is the
 * Python-side `_smooth_positions` ported line for line: cut at steps (`_trajectory_steps`), then a
 * Savitzky–Golay fit (order 2, window 4σ+1 and at least 5, `mode='interp'`) per segment. Rounded output is
 * `Math.round` per frame, matching `drift_frame_slices`' `round(slice.start)`.
 */
import type { VisColumns, VisRow, VisCell, VisFrame } from './paramVis'

/** Frames along the time axis. Wider than driftVis's 24 because the story is about temporal
 *  shape — 40 frames fits both a slow ramp and a fast jerk on the same axis at readable widths.
 *  Same order-of-magnitude as the panel width in cells so each frame gets a legible column when
 *  the figure renders at defaultW=460. */
export const W = 40
/** Vertical resolution of the display grid. Taller than the frame count is on purpose:
 *  a wobbling trajectory that spans only three rows reads as noise, not as a shift.
 *  Trajectory values are clamped into ±(H/2 - 2) so the endpoints and the top of a rounded
 *  staircase never crop against the border. */
export const H = 28

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

/**
 * A trajectory: one displacement per frame, in the same "px" units the display grid uses.
 * Kept as a plain array because the smoother is 1D and the caller may want to inspect it.
 */
export type Trajectory = number[]

/**
 * Noise scenario — a random walk around zero, ~1.5 px std, no drift. What 2h06xA looks like:
 * the estimator faithfully reports the phase-correlation noise floor and the writer rounds it
 * into visible integer transitions. Any σ over ~4 collapses the rounded staircase to a flat
 * zero line, which IS the whole point of shipping the smoother.
 */
export function noiseScenario(seed = 41): Trajectory {
  const rnd = mulberry32(seed)
  const out: Trajectory = []
  let x = 0
  for (let t = 0; t < W; t++) {
    // Random walk with light pull to zero. Amplitude sized so the raw wobble spans ~4 rows on
    // the display — small enough that the rounded staircase visibly wobbles across integers
    // (that IS the jitter), big enough that the wobble reads as motion rather than noise.
    x = 0.82 * x + (rnd() - 0.5) * 4.0
    out.push(x)
  }
  return out
}

/**
 * Ramp scenario — a slow linear drift (~5 px total) with sub-pixel noise on top. A well-behaved
 * stage drift; the rounded staircase should keep stepping upward as σ climbs, because the SLOPE
 * is a low-frequency component the gaussian passes through. Amplitude chosen so the ramp spans
 * roughly ±H/4 across the display — visibly a signal, not a wobble.
 */
export function rampScenario(seed = 42): Trajectory {
  const rnd = mulberry32(seed)
  const total = 16.0                                             // px displacement across W
  const out: Trajectory = []
  for (let t = 0; t < W; t++) {
    // Centre on zero: start negative, end positive, so the ramp visibly crosses the axis
    // (a plain shift would look like the whole trace was offset — this reads as movement).
    const base = -total / 2 + (total * t) / (W - 1)
    out.push(base + (rnd() - 0.5) * 1.5)
  }
  return out
}

/**
 * Jerk scenario — a sharp step over ~JERK_WIDTH frames. Represents a fast displacement event,
 * the shape ttRMjQ shows around frame 30 (S-curve settling in ~25 frames). Small enough that a
 * σ wider than JERK_WIDTH will visibly round the corners; large enough that σ=6 (the shipped
 * default) still passes the step through almost intact.
 */
export const JERK_WIDTH = 6
export function jerkScenario(seed = 43): Trajectory {
  const rnd = mulberry32(seed)
  const amp = 16.0                                               // px, plateau-to-plateau
  const centre = Math.floor(W / 2)
  const out: Trajectory = []
  for (let t = 0; t < W; t++) {
    // tanh over JERK_WIDTH gives a compact S-curve centred at `centre`.
    const s = Math.tanh((t - centre) / (JERK_WIDTH / 2))
    out.push((amp / 2) * s + (rnd() - 0.5) * 0.8)
  }
  return out
}

export const BUMP_FRAME = 24
/**
 * Bump scenario — slow drift with sub-pixel noise, and a 10 px stage bump between frames
 * BUMP_FRAME-1 and BUMP_FRAME. The jump is far outside the movie's normal frame-to-frame moves, so
 * the smoother cuts there and the rounded output keeps it as a single-frame jump.
 */
export function bumpScenario(seed = 44): Trajectory {
  const rnd = mulberry32(seed)
  const out: Trajectory = []
  for (let t = 0; t < W; t++) {
    const base = -8 + (6 * t) / (W - 1) + (t >= BUMP_FRAME ? 10 : 0)
    out.push(base + (rnd() - 0.5) * 0.8)
  }
  return out
}

function median(a: number[]): number {
  if (!a.length) return 0
  const s = [...a].sort((x, y) => x - y), m = s.length >> 1
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2
}

/**
 * Segment bounds `[0, …, n]`, cut after every frame whose move is a step — the Python-side
 * `_trajectory_steps`: a move beyond median + k·MAD of the trajectory's own moves, never below
 * `floorPx`.
 */
export function trajectorySteps(traj: Trajectory, k = 6, floorPx = 3): number[] {
  const d: number[] = []
  for (let i = 1; i < traj.length; i++) d.push(Math.abs(traj[i] - traj[i - 1]))
  if (!d.length) return [0, traj.length]
  const med = median(d)
  const mad = median(d.map(v => Math.abs(v - med)))
  const thr = Math.max(med + k * 1.4826 * mad, floorPx)
  const cuts = [0]
  d.forEach((v, i) => { if (v > thr) cuts.push(i + 1) })
  cuts.push(traj.length)
  return cuts
}

/** Least-squares quadratic through `ys` at x = 0..n-1, evaluated at `at`. */
function quadFitAt(ys: number[], at: number): number {
  // Normal equations for y = a + b·x + c·x², x centred on the window for conditioning.
  const n = ys.length, xc = (n - 1) / 2
  const S = [0, 0, 0, 0, 0], T = [0, 0, 0]
  for (let i = 0; i < n; i++) {
    const x = i - xc
    let p = 1
    for (let e = 0; e <= 4; e++) { S[e] += p; if (e <= 2) T[e] += p * ys[i]; p *= x }
  }
  const M = [[S[0], S[1], S[2]], [S[1], S[2], S[3]], [S[2], S[3], S[4]]]
  const det = (m: number[][]) =>
    m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1])
    - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
    + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0])
  const D = det(M)
  const coef = [0, 1, 2].map(c => det(M.map((row, r) => row.map((v, j) => (j === c ? T[r] : v)))) / D)
  const x = at - xc
  return coef[0] + coef[1] * x + coef[2] * x * x
}

/**
 * The trajectory smoother the drift task applies — Python's `_smooth_positions`, per axis:
 * segments split at steps, each smoothed by a Savitzky–Golay fit of order 2 over a 4σ+1 window (at least 5).
 * Near a segment's ends the window stays inside the segment and the fit is evaluated off-centre —
 * scipy's `mode='interp'`. σ ≤ 0 is the identity, as on the Python side.
 */
export function trajectorySmooth(traj: Trajectory, sigma: number): Trajectory {
  if (sigma <= 0 || traj.length < 3) return traj.slice()
  const out = traj.slice()
  const win = Math.max(5, 2 * Math.round(2 * sigma) + 1)  // 5 = shortest window a quadratic smooths
  const cuts = trajectorySteps(traj)
  for (let s = 0; s + 1 < cuts.length; s++) {
    const a = cuts[s], z = cuts[s + 1], len = z - a
    const w = Math.min(win, len % 2 ? len : len - 1)
    if (w < 5) continue
    const h = (w - 1) / 2
    for (let i = a; i < z; i++) {
      const start = Math.min(Math.max(i - h, a), z - w)
      out[i] = quadFitAt(traj.slice(start, start + w), i - start)
    }
  }
  return out
}

/** Integer round per frame — exactly what `drift_frame_slices` does when it places pixels. */
export function roundTrajectory(traj: Trajectory): Trajectory {
  return traj.map(v => Math.round(v))
}

/**
 * Draw a trajectory into a VisFrame. Two passes in one grid: the raw (faint) and the rounded
 * (opaque). The zero baseline is a thin line — it anchors the reader's eye without competing
 * with the two curves.
 *
 * The rounded curve is drawn as a staircase (each frame's pixel filled at ITS rounded row,
 * plus the vertical connector between adjacent frames), because that is literally the shape
 * the writer produces: content sits at one integer row for a stretch of frames, then jumps.
 *
 * Y convention: the display grows downward (row 0 at the top), and positive trajectory values
 * plot BELOW the centre line (so a positive shift reads as "down" the way a matplotlib plot
 * with reversed y-axis would — matches the on-screen "the sample moves down" mental model).
 */
export function renderTrajectory(raw: Trajectory, rounded: Trajectory): VisFrame {
  const f: VisFrame = Array.from({ length: H }, () => new Array<number>(W).fill(0))
  const midY = Math.floor(H / 2)
  const clampY = (y: number) => Math.min(H - 1, Math.max(0, y))

  // Zero baseline — a full-width dashed line at the midY, dark enough to read as an axis.
  // Dashed so the raw and rounded curves cross it visibly rather than getting swallowed.
  for (let x = 0; x < W; x++) {
    if (x % 3 !== 2) f[midY][x] = Math.max(f[midY][x], 0.28)
  }
  // A short scale tick at the left edge: 5 "px" up + 5 down, so the amplitude has an anchor.
  for (const off of [-5, 5]) {
    const y = clampY(midY + off)
    f[y][0] = Math.max(f[y][0], 0.35)
    f[y][1] = Math.max(f[y][1], 0.35)
  }

  // Raw: draw a 2-row-thick line at the sub-pixel row so the wobble is legible at small cell size.
  for (let x = 0; x < W; x++) {
    const yf = midY + raw[x]
    const y0 = Math.floor(yf), y1 = y0 + 1
    const a1 = yf - y0, a0 = 1 - a1
    // Extra half-row of thickness above and below so the raw trace is never one thin pixel.
    if (y0 - 1 >= 0)                f[y0 - 1][x] = Math.max(f[y0 - 1][x], 0.25 * a0)
    if (y0 >= 0 && y0 < H)          f[y0][x]     = Math.max(f[y0][x],     0.55 * a0)
    if (y1 >= 0 && y1 < H)          f[y1][x]     = Math.max(f[y1][x],     0.55 * a1)
    if (y1 + 1 < H)                 f[y1 + 1][x] = Math.max(f[y1 + 1][x], 0.25 * a1)
  }

  // Rounded staircase: full-width bright line at the integer row, plus vertical connectors on
  // integer transitions. Draw a 2-row-thick band so at small cell sizes the stair still reads as
  // a line and not a series of dots.
  for (let x = 0; x < W; x++) {
    const y = clampY(midY + rounded[x])
    f[y][x] = 1.0
    if (y + 1 < H) f[y + 1][x] = Math.max(f[y + 1][x], 0.85)
    if (x > 0) {
      const yPrev = clampY(midY + rounded[x - 1])
      if (yPrev !== y) {
        const lo = Math.min(yPrev, y), hi = Math.max(yPrev, y)
        for (let yy = lo; yy <= hi; yy++) {
          f[yy][x] = 1.0
          if (yy + 1 < H) f[yy + 1][x] = Math.max(f[yy + 1][x], 0.85)
        }
      }
    }
  }
  return f
}

export const SMOOTH_VIS_COLUMNS = ['noise', 'ramp', 'jerk', 'bump'] as const

/** Static per-column labels for the "What it shows" row. Kept short — the whole figure teaches
 *  the same trade-off three ways, so the label names the case, not the verdict. */
const SCENARIO_LABEL: Record<typeof SMOOTH_VIS_COLUMNS[number], string> = {
  noise: 'sub-pixel jitter (still sample)',
  ramp:  'slow drift (real motion)',
  jerk:  'fast step (motion in a few frames)',
  bump:  'stage bump (jumps between two frames)',
}

function cell(text: string, frames?: VisFrame[]): VisCell {
  return { value: 0, px: null, r: 0, at: 0, text, pxText: '', frames }
}

export interface DriftSmoothVisInput {
  /** `driftSmoothSigma` from the form — 0 (off) up to 30 (max). Reads live so the picture
   *  redraws as the slider moves, same convention as `driftVis`'s `maxLag` cell. */
  sigma: number
}

/**
 * Build the figure's columns. One `grid` row plus two text rows underneath: the scenario name
 * and a short read of what the CURRENT σ did to the rounded output (kept — collapsed — eaten).
 * Same three-row shape `smoothVis` and `driftVis` use.
 */
export function driftSmoothVisColumns(inp: DriftSmoothVisInput): VisColumns {
  const scenarios: Record<typeof SMOOTH_VIS_COLUMNS[number], Trajectory> = {
    noise: noiseScenario(),
    ramp:  rampScenario(),
    jerk:  jerkScenario(),
    bump:  bumpScenario(),
  }
  const gridCells: VisCell[] = []
  const verdictCells: VisCell[] = []

  for (const col of SMOOTH_VIS_COLUMNS) {
    const raw = scenarios[col]
    const smoothed = trajectorySmooth(raw, inp.sigma)
    const rounded = roundTrajectory(smoothed)
    gridCells.push(cell('', [renderTrajectory(raw, rounded)]))
    verdictCells.push(cell(verdictFor(col, raw, rounded)))
  }

  const scenarioCells: VisCell[] = [...SMOOTH_VIS_COLUMNS].map(c => cell(SCENARIO_LABEL[c]))

  const rows: VisRow[] = [
    { key: 'trajectory', label: 'Simulated', role: 'grid', uniform: false, cells: gridCells },
    { key: 'scenario',   label: 'Case',       role: 'text', uniform: false, cells: scenarioCells },
    { key: 'verdict',    label: 'Output',     role: 'text', uniform: false, cells: verdictCells },
  ]
  return { columns: [...SMOOTH_VIS_COLUMNS], rows, pxSize: null, uniformKeys: [] }
}

/**
 * The per-column read of what σ did to the rounded output. Three cases distinguished by counting
 * integer transitions in the rounded staircase relative to the raw baseline:
 *   - `noise` and the rounded is flat → jitter was killed
 *   - `ramp`  and the rounded still climbs → real motion preserved
 *   - `jerk`  and the rounded's step got wider/rounded → motion eaten
 * Words are kept short (`docs/ui/COPY.md` — tips ≤ 90 chars, one clause).
 */
function verdictFor(col: typeof SMOOTH_VIS_COLUMNS[number],
                    raw: Trajectory, rounded: Trajectory): string {
  const rawInt = roundTrajectory(raw)
  const rawTrans = countTransitions(rawInt)
  const outTrans = countTransitions(rounded)
  const rawRange = range(rawInt), outRange = range(rounded)

  if (col === 'noise') {
    if (outTrans === 0) return 'no jumps'
    if (outTrans < rawTrans / 2) return `${outTrans} jumps (was ${rawTrans})`
    return `${outTrans} jumps`
  }
  if (col === 'ramp') {
    // The signal is a monotonic climb; "preserved" means the rounded end-to-end span is close
    // to the raw's. Below half → we've eaten most of the drift.
    if (outRange >= rawRange - 1) return 'drift preserved'
    if (outRange >= rawRange / 2)  return `drift ${outRange}/${rawRange} px preserved`
    return `drift eaten (${outRange}/${rawRange} px)`
  }
  if (col === 'bump') {
    // Kept = the rounded output still makes the whole jump in the bump's one frame.
    const jump = Math.abs(rounded[BUMP_FRAME] - rounded[BUMP_FRAME - 1])
    const rawJump = Math.abs(rawInt[BUMP_FRAME] - rawInt[BUMP_FRAME - 1])
    return jump >= rawJump - 1 ? 'bump kept in one frame' : `bump spread (${jump}/${rawJump} px in one frame)`
  }
  // jerk: raw step is compact (~JERK_WIDTH). Wider rounded transition ⇒ motion eaten.
  const transWidth = transitionWidth(rounded)
  if (transWidth <= JERK_WIDTH + 2) return 'step preserved'
  return `step widened to ${transWidth} frames`
}

/** Count integer-pixel transitions along a trajectory — this is the writer's per-frame jump count. */
export function countTransitions(int: Trajectory): number {
  let n = 0
  for (let i = 1; i < int.length; i++) if (int[i] !== int[i - 1]) n++
  return n
}

function range(int: Trajectory): number {
  if (!int.length) return 0
  let lo = int[0], hi = int[0]
  for (const v of int) { if (v < lo) lo = v; if (v > hi) hi = v }
  return hi - lo
}

/** How many consecutive frames the trajectory takes to cross from its early plateau to its late
 *  plateau. Used only for the jerk verdict — where "wider than the raw step" means the smoother
 *  softened the corners. Works from the first and last rounded values as the plateaus. */
export function transitionWidth(int: Trajectory): number {
  if (int.length < 2) return 0
  const start = int[0], end = int[int.length - 1]
  if (start === end) return 0
  const lo = Math.min(start, end), hi = Math.max(start, end)
  let first = -1, last = -1
  for (let i = 0; i < int.length; i++) {
    if (int[i] !== start && int[i] !== end && int[i] > lo && int[i] < hi) {
      if (first === -1) first = i
      last = i
    }
  }
  if (first === -1) {
    // No intermediate steps — width is one frame per unit change, capped at 1 (a clean single jump).
    return 1
  }
  return last - first + 2                                        // includes the two plateau-touch frames
}

/** The line under the figure. `smoothVis` and `driftVis` name what the columns are saying; this
 *  one names the trade-off, because that IS what σ does. */
export function driftSmoothVerdict(sigma: number): string {
  if (sigma <= 0) {
    return 'σ = 0 — no smoothing. Sub-pixel noise reaches the writer as per-frame jumps.'
  }
  return `σ = ${sigma} — smoothing kills jitter (col 1). Motion faster than ~${Math.round(2 * sigma)} frames`
       + ' gets rounded off (col 3); a stage bump stays sharp (col 4).'
}

/** The figure and the line under it — what a consumer mounts. */
export function driftSmoothFigure(inp: DriftSmoothVisInput): { vis: VisColumns; note: string } {
  return { vis: driftSmoothVisColumns(inp), note: driftSmoothVerdict(inp.sigma) }
}
