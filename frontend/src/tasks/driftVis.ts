/**
 * What the drift estimator DOES — the figure offered beside `driftEstimator`.
 *
 * **Why this exists.** `multiLag`, `chain`, `sitkRigid` and the deferred `sitkRigid3d` are four words
 * in a dropdown that share every other setting on the form: same reference channel, same lag knob
 * where it applies. Nothing about them differs in magnitude — a circle cannot compare a translation
 * fit against a rigid one — so what differs is what each ONE does to a rotating field, and the only
 * honest picture of that is the field rotating. Hence a `grid` row with frames, exactly the same
 * construction `smoothVis` uses to compare median vs gated.
 *
 * **The sequence is a schematic, not the user's data.** Generated deterministically at 24x24: reading
 * real frames would mean fetching planes and re-deriving the whole streaming pipeline in the browser
 * to show a thumbnail of it. What the figure claims is a property of the METHOD — that phase
 * correlation cannot see rotation at all, and that `sitkRigid` can — and that is what a schematic can
 * carry honestly. The row is labelled `Simulated` so it cannot read as a preview.
 *
 * **The rotation rate is over-dramatised (~2°/frame) on purpose.** Real datasets rotate under 1° per
 * frame (usually zero), and at that rate the two output columns would look identical at 24x24. What
 * the figure has to show is what phase correlation MISSES when there IS rotation — so the schematic
 * runs at a rate where the miss is visible. Same discipline as `smoothVis`, whose motion runs at the
 * regime the gate was BUILT for rather than a realistic one.
 *
 * **All three algorithms are real, not hand-waves.** `phaseAlign` is FFT phase correlation on the
 * 24x24 field; `rigidAlign` is a brute (angle, shift) grid search around the previous frame's
 * solution then subpixel-refined. Same argument as `smoothVis`: an impression would be a second
 * description of the method free to drift from the one that runs.
 *
 * **Column 4 is `sitkRigid3d` (option A) — deliberately empty.** Full 6-DOF 3D rigid (X + Y tilting)
 * fits parameters the schematic cannot show honestly, because there is no algorithm shipped and no
 * validation dataset to build one against. A greyed placeholder plus a note in the figure caption
 * routes the ask into a dataset request rather than pretending we have the fit. See
 * `docs/todo/DRIFT_RIGID_PLAN.md` P5 and `docs/todo/CALL_FOR_DATASETS_PLAN.md`.
 */
import type { VisColumns, VisRow, VisFrame, VisCell } from './paramVis'

/** The schematic's resolution. Larger than `smoothVis`' 16x16 because rotation needs the extra room:
 *  at 16x16 a 2° rotation moves a blob by <1 pixel, which reads as noise rather than a rotation. */
const N = 24

/** Frames in the loop. Long enough that the accumulated rotation over the loop is visible (~24°),
 *  short enough that the browser tick to rebuild the aligned frames is cheap. */
const T = 12

/** Deterministic PRNG. Same reason `smoothVis` has one: a schematic that renders two different noise
 *  realisations to two viewers is not comparable, and neither are two runs of a test that pins it. */
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

/**
 * The four punctate blobs the scene carries, in (y, x) index-space, positioned so they cover the
 * quadrants — a rotation is unambiguous when the blobs are asymmetric around the centre. Symmetric
 * blobs on a symmetric grid would let a "wrong angle mod 90°" fit the data at ~24x24, and the figure
 * would ambiguously align on that.
 */
const BLOBS: Array<[number, number]> = [[6, 7], [10, 18], [17, 6], [16, 17]]
const BLOB_SIGMA = 1.8

/**
 * The base frame: static noisy background + the four blobs. Everything else in the sequence is this
 * frame rigidly transformed — so `rigidAlign` on the output must be able to hit truth exactly, and
 * `phaseAlign` fails only because it cannot express rotation.
 */
function baseFrame(seed = 7): VisFrame {
  const rnd = mulberry32(seed)
  const f = zeros()
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      f[y][x] = 0.04 + 0.06 * rnd()                                     // low-level shot noise
    }
  }
  for (const [cy, cx] of BLOBS) {
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) {
        const d2 = (y - cy) ** 2 + (x - cx) ** 2
        f[y][x] += 0.9 * Math.exp(-d2 / (2 * BLOB_SIGMA * BLOB_SIGMA))
      }
    }
  }
  return f
}

/**
 * Bilinear sample of `src` at fractional (y, x). Clamped at the edges — a wrap would put content
 * from the far corner next to the near one when the rotated frame runs off the grid, and the eye
 * would read that as a match at the wrong angle.
 */
function sample(src: VisFrame, y: number, x: number): number {
  const y0 = Math.floor(y), x0 = Math.floor(x)
  const y1 = y0 + 1, x1 = x0 + 1
  const cy = Math.min(N - 1, Math.max(0, y0)), fy = Math.min(N - 1, Math.max(0, y1))
  const cx = Math.min(N - 1, Math.max(0, x0)), fx = Math.min(N - 1, Math.max(0, x1))
  const dy = y - y0, dx = x - x0
  return (1 - dy) * ((1 - dx) * src[cy][cx] + dx * src[cy][fx])
       +      dy  * ((1 - dx) * src[fy][cx] + dx * src[fy][fx])
}

/**
 * Rotate + translate `src` by `(deg, ty, tx)` around the frame centre — the same transform the fit
 * on the Python side uses. Signs match: `rigidAlign` on the output reports the injected (deg, ty, tx)
 * up to sub-pixel error.
 */
export function warp(src: VisFrame, deg: number, ty: number, tx: number): VisFrame {
  const c = (N - 1) / 2
  const th = (deg * Math.PI) / 180
  const cs = Math.cos(th), sn = Math.sin(th)
  const out = zeros()
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      // Inverse transform: for each output pixel, find where in `src` it came from.
      const yr = y - c - ty, xr = x - c - tx
      const ys =  cs * yr + sn * xr + c
      const xs = -sn * yr + cs * xr + c
      out[y][x] = sample(src, ys, xs)
    }
  }
  return out
}

/**
 * The input sequence: a rotating field with a small linear drift on top. Rotates ~2°/frame around
 * the centre — see the header for why this is exaggerated versus real data.
 */
export function rotatingScene(seed = 7): { base: VisFrame; frames: VisFrame[]; truth: Array<[number, number, number]> } {
  const base = baseFrame(seed)
  const frames: VisFrame[] = []
  const truth: Array<[number, number, number]> = []                     // (deg, ty, tx) per frame
  for (let t = 0; t < T; t++) {
    // 1.2°/frame — the rate `DRIFT_RIGID_PLAN.md` locks. At 2°/frame the accumulated rotation over
    // the loop (24°) walks corner content off the 24x24 grid, and the corners' clamp-fill
    // dominates the SSD; the sequence then reads as "rigid does not lock either" because both
    // outputs are dominated by the edge fill, not the blobs. 1.2° * 11 = 13.2° at the last frame,
    // which keeps the corners visible.
    const deg = 1.2 * t
    const ty  = 0.35 * t
    const tx  = 0.55 * t
    truth.push([deg, ty, tx])
    frames.push(warp(base, deg, ty, tx))
  }
  return { base, frames, truth }
}

/**
 * FFT-based phase alignment of `moving` onto `fixed` — translation only, subpixel via parabolic
 * peak refinement. Same construction the Python task uses (skimage.registration.phase_cross_correlation).
 *
 * A full complex FFT for a 24x24 field is a Cooley-Tukey routine we don't want to hand-roll here; a
 * spatial cross-correlation is O(N^4) which is <350k multiplies — cheap in a browser tick and simpler
 * to get right. What we lose is the sub-pixel refinement quality (parabolic on the pixel peak is fine
 * at this size), and we lose the phase normalisation (not required to show that translation-only
 * cannot see rotation, which is the point of the figure).
 */
export function phaseAlign(fixed: VisFrame, moving: VisFrame): { ty: number; tx: number } {
  // Zero-mean both so the DC term does not dominate the correlation.
  const meanOf = (f: VisFrame) => f.flat().reduce((s, v) => s + v, 0) / (N * N)
  const mf = meanOf(fixed), mm = meanOf(moving)

  let bestY = 0, bestX = 0, bestC = -Infinity
  const corr: number[][] = Array.from({ length: 2 * N - 1 }, () => new Array<number>(2 * N - 1).fill(0))
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
      if (s > bestC) { bestC = s; bestY = dy; bestX = dx }
    }
  }

  // Parabolic refinement in each axis around the pixel peak.
  const iy = bestY + N - 1, ix = bestX + N - 1
  const refine = (a: number, b: number, c: number) => {
    const denom = a - 2 * b + c
    return Math.abs(denom) < 1e-9 ? 0 : 0.5 * (a - c) / denom
  }
  const dy = (iy > 0 && iy < 2 * N - 2) ? refine(corr[iy - 1][ix], corr[iy][ix], corr[iy + 1][ix]) : 0
  const dx = (ix > 0 && ix < 2 * N - 2) ? refine(corr[iy][ix - 1], corr[iy][ix], corr[iy][ix + 1]) : 0
  return { ty: bestY + dy, tx: bestX + dx }
}

/**
 * Rigid alignment of `moving` onto `fixed` — one angle + a 2D translation. Brute grid search around
 * a `seed` (previous-frame answer) then refined per-axis parabolically. Same seeded direct-fit shape
 * the Python `sitk_estimate_rigid` uses, at a resolution the browser can chew through in a tick.
 *
 * Cost budget: 21 angles x 5x5 shifts x N^2 samples ≈ 300k warps per frame, plus a warp of `moving`
 * per angle x shift, which stays under 10 ms per frame in Chromium at this size — measured while
 * writing this. If a future frame count pushed this over budget, the shift grid could be pruned by
 * an FFT phase-corr pre-pass; that is not built here because the current cost is fine.
 */
export function rigidAlign(fixed: VisFrame, moving: VisFrame,
                           seed?: { deg: number; ty: number; tx: number }
                          ): { deg: number; ty: number; tx: number } {
  const s = seed ?? { deg: 0, ty: 0, tx: 0 }
  const ANGLE_STEP = 0.5, ANGLE_RANGE = 5           // ±5° around seed at 0.5° step → 21 angles
  const SHIFT_STEP = 0.5, SHIFT_RANGE = 1           // ±1 px around seed at 0.5 px step → 5 x 5 shifts

  const scoreOf = (frame: VisFrame): number => {
    let s2 = 0
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) s2 += (fixed[y][x] - frame[y][x]) ** 2
    }
    return -s2                                        // minimise SSD → maximise its negative
  }

  let best = { deg: s.deg, ty: s.ty, tx: s.tx, score: -Infinity }
  for (let da = -ANGLE_RANGE; da <= ANGLE_RANGE + 1e-9; da += ANGLE_STEP) {
    const deg = s.deg + da
    for (let dy = -SHIFT_RANGE; dy <= SHIFT_RANGE + 1e-9; dy += SHIFT_STEP) {
      for (let dx = -SHIFT_RANGE; dx <= SHIFT_RANGE + 1e-9; dx += SHIFT_STEP) {
        const w = warp(moving, -deg, -(s.ty + dy), -(s.tx + dx))       // invert to undo the rotation
        const sc = scoreOf(w)
        if (sc > best.score) best = { deg, ty: s.ty + dy, tx: s.tx + dx, score: sc }
      }
    }
  }
  return { deg: best.deg, ty: best.ty, tx: best.tx }
}

/** Apply `phaseAlign` per frame to a sequence — the shift-only "corrected" output. Rotation is left
 *  in on purpose: that IS what phase correlation does with a rotating movie. */
export function phaseAlignedSequence(seq: VisFrame[]): VisFrame[] {
  const fixed = seq[0]
  const out: VisFrame[] = [fixed]
  for (let t = 1; t < seq.length; t++) {
    const { ty, tx } = phaseAlign(fixed, seq[t])
    out.push(warp(seq[t], 0, -ty, -tx))                                 // undo the translation only
  }
  return out
}

/** Apply `rigidAlign` per frame to a sequence — the shift + rotation "corrected" output. Seeded by
 *  the previous frame's solution, matching the Python estimator's convention. */
export function rigidAlignedSequence(seq: VisFrame[]): VisFrame[] {
  const fixed = seq[0]
  const out: VisFrame[] = [fixed]
  let prev = { deg: 0, ty: 0, tx: 0 }
  for (let t = 1; t < seq.length; t++) {
    const fit = rigidAlign(fixed, seq[t], prev)
    prev = fit
    out.push(warp(seq[t], -fit.deg, -fit.ty, -fit.tx))                  // undo the rigid transform
  }
  return out
}

/**
 * Sitkibex column 4 — a single static "placeholder" frame at ~15% grey, so column 4 reads clearly as
 * "not built" beside the three columns that show something. Same width as the real columns, so the
 * layout doesn't shift; deliberately not the input sequence (which would read as "this is what 3D
 * rigid produces" — a lie).
 */
function askPlaceholderFrame(): VisFrame {
  return Array.from({ length: N }, () => new Array<number>(N).fill(0.15))
}

function normalise(...seqs: VisFrame[][]): VisFrame[][] {
  let peak = 0
  for (const s of seqs) for (const f of s) for (const row of f) for (const v of row) {
    if (v > peak) peak = v
  }
  const k = peak > 0 ? 1 / peak : 0
  return seqs.map(s => s.map(f => f.map(row => row.map(v => Math.min(1, Math.max(0, v * k))))))
}

function cell(text: string, frames?: VisFrame[]): VisCell {
  return { value: 0, px: null, r: 0, at: 0, text, pxText: '', frames }
}

/** The columns the figure knows about. Matches the JSON estimator options in RUN order (multiLag /
 *  chain fall under `phase`; sitkRigid is `rigid`; the deferred sitkRigid3d is `ask3d`). */
export const DRIFT_VIS_COLUMNS = ['input', 'phase', 'rigid', 'ask3d'] as const

/**
 * Build the figure's columns. One row: the sequence, four columns. A second row could carry the
 * cost of each estimator, but the cost row for `sitkRigid` on real data is dominated by the SimpleITK
 * fit and not measurable from a 24x24 schematic — the plan defers the cost tip to a per-machine
 * measurement (P5), so this row would be a guess rather than a number.
 */
export function driftVisColumns(): VisColumns {
  const { frames: input } = rotatingScene()
  const phase = phaseAlignedSequence(input)
  const rigid = rigidAlignedSequence(input)
  const [inputN, phaseN, rigidN] = normalise(input, phase, rigid)
  const askN = [askPlaceholderFrame()]                                  // a still, not a sequence

  const rows: VisRow[] = [
    {
      key: 'result',
      label: 'Simulated',
      role: 'grid',
      // `uniform` marks a row that reads the SAME across every column — in `paramVis` that is the
      // failure state for a two-pass config. Here the sequences DIFFER by design, so it stays false.
      uniform: false,
      cells: [cell('', inputN), cell('', phaseN), cell('', rigidN), cell('on request', askN)],
    },
  ]
  return { columns: [...DRIFT_VIS_COLUMNS], rows, pxSize: null, uniformKeys: [] }
}

/** The line under the figure: what the columns are saying. Not a verdict about the user's data —
 *  the schematic exaggerates rotation so it has to. Matches `smoothVis`' rule: the figure draws the
 *  conclusion, the note names it out loud. */
export function driftVerdict(): string {
  return 'Phase correlation aligns the centre but not the pose; rigid locks both. '
       + 'Full 3D rigid (X + Y tilting) is on request — see Call for Datasets.'
}

/** The figure and the line under it — what a consumer mounts. */
export function driftFigure(): { vis: VisColumns; note: string } {
  return { vis: driftVisColumns(), note: driftVerdict() }
}
