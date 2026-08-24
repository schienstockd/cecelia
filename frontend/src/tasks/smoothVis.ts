/**
 * What the temporal statistic DOES — the figure offered beside smoothing's `temporalStat`.
 *
 * **Why this exists.** `median`, `mean` and `gated` are three words in a dropdown that share every
 * other setting on the form: same window, same sigma, same channels. Nothing about them differs in
 * magnitude, so the shapes `paramVis` draws — a circle, a span, a disc — have nothing to say about
 * the choice. What differs is what each one does to a spot that MOVED, and the only honest picture of
 * that is the spot moving. Hence a `grid` row with frames (see `VisRole`), and hence this second
 * producer: `paramVisColumns` builds its columns from a repeatable group's entries, and there is no
 * group here — the two columns are two possible values of ONE select.
 *
 * **The sequence is a schematic, not the user's data.** It is generated here, deterministically, at
 * 16x16. That is a deliberate limit: reading real frames would mean fetching planes and re-deriving
 * the whole streaming pipeline in the browser to show a thumbnail of it. What the figure claims is a
 * property of the METHOD — a median mixes whatever sits at the same pixel, a gate follows the patch
 * and falls back to the current frame when it cannot — and that is exactly what a schematic can
 * carry honestly. The row is labelled `Simulated` so it cannot be read as a preview.
 *
 * **Both columns get the SAME input.** One sequence, computed once, drawn once as a spanning row
 * above them. Two noise realisations would make the comparison a comparison of two datasets.
 *
 * **The gate is the real algorithm, not an impression of it.** Block-match a +-1 px window, weight
 * each neighbour by `exp(-d2/scale)` with `scale = 2*(k*sigma)^2` and sigma from the MAD of the
 * temporal difference — the same construction as `coastal.smooth._match`/`noise_sigma`, at 16x16
 * instead of a plane. A hand-waved approximation ("gated keeps the spot brighter") would make the
 * figure a second description of the method, free to drift from the one that runs — the same class of
 * bug as a preview that ignores the order chips.
 *
 * **Spatial comes first here too**, because it does in the task: the streaming loop caches spatially
 * smoothed frames and takes the temporal statistic across those. A figure that skipped it would show
 * a temporal term operating on data the engine never gives it.
 */
import { MAX_R, type VisCell, type VisColumns, type VisFrame, type VisRow } from './paramVis'

/** The schematic's resolution. Big enough for a spot to visibly move, small enough to stay a glyph. */
const N = 16

/** Frames in the loop. Must exceed the largest window (9) so a centred window is never all clamped. */
const T = 12

/** Seconds per z-plane per channel for `gated`, measured through the task (#554: 180t x 19z ~ 7 min). */
export const GATED_SEC_PER_PLANE = 0.12

/** Block-match search radius and patch width — coastal's defaults, which the task does not expose. */
const SEARCH = 1
const PATCH = 3

/** `k` in the agreement scale. 1.0 is coastal's default and the task never varies it. */
const K = 1.0

/**
 * A deterministic PRNG. NOT `Math.random`: the figure has to render the same way twice, or a test
 * pins nothing and two people comparing screens are comparing different noise.
 */
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
 * The input: a bright spot crossing a dim, noisy field.
 *
 * The spot moves ~1 cell a frame — the regime `gated` was built for, where the median inter-frame
 * displacement over signal is ~1 px with a tail to ~6 (#554). Slower and the median would look fine,
 * which would be a figure arguing the opposite of what was measured.
 */
export function motionSequence(seed = 7): VisFrame[] {
  const rnd = mulberry32(seed)
  // A static background, so what changes between frames is the spot and the noise — not the scene.
  const bg = zeros().map(row => row.map(() => 0.06 + 0.05 * rnd()))
  const out: VisFrame[] = []
  for (let t = 0; t < T; t++) {
    const f = bg.map(row => row.slice())
    // Sized so the spot is still fully inside the grid at the LAST frame: leaving early makes the
    // loop pop back to the start over an empty field, and an empty field is what "this setting does
    // nothing" looks like.
    const cy = 2.8 + 0.62 * t
    const cx = 2.8 + 1.00 * t
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) {
        const d2 = (y - cy) ** 2 + (x - cx) ** 2
        f[y][x] += 0.95 * Math.exp(-d2 / 2.2)          // the punctum
        f[y][x] += 0.16 * (rnd() - 0.5)                // shot noise, fresh every frame
      }
    }
    out.push(f)
  }
  return out
}

/** Separable box blur, `passes` of it — a Gaussian's cheap stand-in at this size. */
function blur(f: VisFrame, sigma: number): VisFrame {
  if (sigma <= 0) return f.map(r => r.slice())
  const passes = Math.max(1, Math.round(sigma * 2))
  let cur = f.map(r => r.slice())
  for (let p = 0; p < passes; p++) {
    const next = zeros()
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) {
        let s = 0, n = 0
        for (let dy = -1; dy <= 1; dy++) {
          for (let dx = -1; dx <= 1; dx++) {
            const yy = y + dy, xx = x + dx
            if (yy < 0 || yy >= N || xx < 0 || xx >= N) continue
            s += cur[yy][xx]; n++
          }
        }
        next[y][x] = s / n
      }
    }
    cur = next
  }
  return cur
}

/** Window indices around `t`, clamped at the edges — the same convention the task streams with. */
function windowAt(t: number, frames: number): number[] {
  const half = Math.max(0, (frames - 1) >> 1)
  const idx: number[] = []
  for (let d = -half; d <= half; d++) idx.push(Math.min(T - 1, Math.max(0, t + d)))
  return idx
}

function medianOf(vals: number[]): number {
  const s = [...vals].sort((a, b) => a - b)
  const m = s.length >> 1
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2
}

/** Per-pixel median across the window. */
export function medianSequence(seq: VisFrame[], frames: number): VisFrame[] {
  return seq.map((_, t) => {
    const win = windowAt(t, frames).map(i => seq[i])
    const out = zeros()
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) out[y][x] = medianOf(win.map(w => w[y][x]))
    }
    return out
  })
}

/**
 * The noise scale: MAD of the temporal difference, `1.4826 * MAD / sqrt(2)` — `d = I_t+1 - I_t` has
 * variance `2*sigma^2` wherever motion is absent, and a MAD rather than a std so the moving minority
 * does not set the scale. Estimated once over the whole sequence, as the task estimates it once over
 * a slab, so the gate's strictness cannot drift between frames.
 */
export function noiseSigma(seq: VisFrame[]): number {
  const d: number[] = []
  for (let t = 1; t < seq.length; t++) {
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) d.push(seq[t][y][x] - seq[t - 1][y][x])
    }
  }
  if (!d.length) return 0
  const med = medianOf(d)
  return 1.4826 * medianOf(d.map(v => Math.abs(v - med))) / Math.SQRT2
}

/** Box mean over a `patch`-wide window — coastal matches on a patch, not a bare pixel difference. */
function patchMean(f: VisFrame, patch: number): VisFrame {
  const r = patch >> 1
  const out = zeros()
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      let s = 0, n = 0
      for (let dy = -r; dy <= r; dy++) {
        for (let dx = -r; dx <= r; dx++) {
          const yy = y + dy, xx = x + dx
          if (yy < 0 || yy >= N || xx < 0 || xx >= N) continue
          s += f[yy][xx]; n++
        }
      }
      out[y][x] = s / n
    }
  }
  return out
}

/**
 * The gate for one neighbour: for every pixel, WHERE its patch went within +-`SEARCH`, and how much
 * that match is worth. Returns the matched values already gathered, plus the weight.
 */
function match(target: VisFrame, nb: VisFrame, scale: number): { w: VisFrame; take: VisFrame } {
  const bestD = zeros().map(r => r.map(() => Infinity))
  const take = zeros()
  for (let dy = -SEARCH; dy <= SEARCH; dy++) {
    for (let dx = -SEARCH; dx <= SEARCH; dx++) {
      const shifted = zeros()
      for (let y = 0; y < N; y++) {
        for (let x = 0; x < N; x++) {
          // clamped rather than wrapped: at 16x16 a wrap puts the far edge next to the near one,
          // which invents a match across the whole field
          shifted[y][x] = nb[Math.min(N - 1, Math.max(0, y - dy))][Math.min(N - 1, Math.max(0, x - dx))]
        }
      }
      const d2 = patchMean(shifted.map((row, y) => row.map((v, x) => (v - target[y][x]) ** 2)), PATCH)
      for (let y = 0; y < N; y++) {
        for (let x = 0; x < N; x++) {
          if (d2[y][x] < bestD[y][x]) { bestD[y][x] = d2[y][x]; take[y][x] = shifted[y][x] }
        }
      }
    }
  }
  return { w: bestD.map(row => row.map(v => Math.exp(-v / scale))), take }
}

/**
 * Agreement-gated average. The current frame always carries weight 1, so the worst case — nothing
 * matches anywhere — is the identity, never a blur. That property is the whole argument for `gated`,
 * so the figure has to be built the way that makes it true rather than merely look true.
 */
export function gatedSequence(seq: VisFrame[], frames: number): VisFrame[] {
  const sigma = noiseSigma(seq)
  const scale = Math.max(2 * (K * sigma) ** 2, 1e-12)
  return seq.map((target, t) => {
    const nbs = windowAt(t, frames).filter(i => i !== t).map(i => seq[i])
    const acc = target.map(r => r.slice())
    const wsum = zeros().map(r => r.map(() => 1))
    for (const nb of nbs) {
      const { w, take } = match(target, nb, scale)
      for (let y = 0; y < N; y++) {
        for (let x = 0; x < N; x++) { acc[y][x] += w[y][x] * take[y][x]; wsum[y][x] += w[y][x] }
      }
    }
    return acc.map((row, y) => row.map((v, x) => v / wsum[y][x]))
  })
}

/**
 * ONE scale across every grid in the figure. Normalising each row to its own maximum would hide the
 * only thing the `result` rows have to show — that the median dims a spot the gate keeps — by
 * rescaling the dimming away.
 */
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

/** The two methods the figure compares. `mean` is deliberately absent — see `smoothVisColumns`. */
export const SMOOTH_VIS_METHODS = ['median', 'gated'] as const

export interface SmoothVisInput {
  /** `temporalFrames` — the same window for both columns; the comparison is the method */
  frames: number
  /** `spatialSigma`, applied before the temporal term because the task applies it before */
  sigma: number
  /** z-planes x timepoints across the selected images, or null when nothing is selected yet */
  planes: number | null
  /** how many channels are selected — `gated`'s cost is per channel */
  channels: number
}

/**
 * The cost row, in the units a person schedules by. NOT #554's headline "~7 min/channel": that was a
 * 180t x 19z movie, so it is a property of that acquisition, not of the method — the same reason the
 * amplitude numbers stay out of this figure. The rate is the measured constant; the size is the
 * user's own.
 */
export function gatedCost(planes: number | null, channels: number): string {
  // No image selected yet — `TaskRunner` builds the context from the ticked rows, so this is the
  // ordinary state of someone opening the figure to decide BEFORE picking anything. The rate is
  // known regardless, and a rate is a number you can act on; the previous fallback said "minutes",
  // which is the shape of an answer without being one.
  if (!planes || planes <= 0 || channels <= 0) return `${GATED_SEC_PER_PLANE} s / plane`
  const sec = planes * channels * GATED_SEC_PER_PLANE
  if (sec < 90) return `~${Math.max(1, Math.round(sec))} s`
  return `~${Math.round(sec / 60)} min`
}

/**
 * How much brighter the gate leaves the moving spot than the median does, over the whole loop —
 * 0 when they agree, 0.5 when the median has lost half of it.
 *
 * Read back off the FRAMES the figure is about to draw, not recomputed from the settings. The verdict
 * and the picture then cannot disagree: whatever the grids show is what the line under them says, and
 * a change to the schematic moves both at once. Same reason `uniformWarning` lives beside the producer
 * that builds the strip rather than in the component.
 */
export function amplitudeGap(med: VisFrame[], gat: VisFrame[]): number {
  const peak = (f: VisFrame) => Math.max(...f.map(r => Math.max(...r)))
  const gaps: number[] = []
  for (let t = 0; t < Math.min(med.length, gat.length); t++) {
    const g = peak(gat[t])
    if (g > 0) gaps.push(1 - peak(med[t]) / g)
  }
  // The MEDIAN across frames, not the worst and not the mean. The first and last frames of the loop
  // sit against a clamped window — both methods see the same frame repeated, so both are the identity
  // and the gap there is exactly 0 by construction. A mean is dragged down by those two, and a worst
  // is set by whichever single frame the spot happened to be brightest in. The median is what the
  // loop mostly shows, which is what the person watching it is judging.
  return gaps.length ? medianOf(gaps) : 0
}

/**
 * Where "the median is fine here" turns into "this is what you are paying for".
 *
 * Placed to match the crossing MEASURED through the real task on `WIaUjL/p6t4mC` (#554), not chosen
 * off this schematic: at window 3 the median removes MORE noise than the gate (32% against 25%) and
 * keeps 92% of the punctum, so it is the better pick and the free one; from window 5 up the two match
 * on noise (44/45%, 53/54%) while the median's amplitude falls away (85%, then 69%). So the line
 * belongs between 3 and 5. On the frames this figure draws, at the default sigma of 1, the gaps are
 * 0.07 at window 3 and 0.18 at 5 (then 0.29 at 7, 0.31 at 9) — so 0.12 sits roughly midway with ~45%
 * margin either side rather than balanced on a knife edge.
 *
 * A threshold tuned to make the schematic come out right would be circular. This one is placed by the
 * measurement and then checked against the schematic, which is the other direction.
 *
 * It moves with `spatialSigma`, and correctly: a heavier Gaussian masks the median's temporal
 * smearing (the same gaps at sigma 2 are 0.03 / 0.11), so the gate buys less and the line says so.
 */
export const GAP_WORTH_PAYING_FOR = 0.12

/**
 * The line under the figure: which one to pick, in the user's current settings.
 *
 * Deliberately silent about the cost — the cost row states it, and how many minutes are worth a
 * sharper cell is not a judgement this figure can make for someone. It says what the pictures show;
 * the price is beside it.
 */
export function smoothVerdict(gap: number): string {
  return gap < GAP_WORTH_PAYING_FOR
    ? 'Median is enough at this window'
    : 'Gated keeps what the median smears at this window'
}

/** The figure and the line under it — what a consumer mounts. */
export function smoothFigure(inp: SmoothVisInput): { vis: VisColumns; note: string } {
  const vis = smoothVisColumns(inp)
  const result = vis.rows.find(r => r.key === 'result')
  const [med, gat] = [result?.cells[0].frames ?? [], result?.cells[1].frames ?? []]
  return { vis, note: smoothVerdict(amplitudeGap(med, gat)) }
}

/**
 * Build the figure. Two columns, and `mean` is not one of them: it is the option nobody should pick
 * (it averages the whole window regardless of motion), and a third column would spend a third of the
 * width arguing against a straw man instead of showing the choice that is actually being made.
 */
export function smoothVisColumns(inp: SmoothVisInput): VisColumns {
  const frames = Math.max(1, inp.frames)
  const raw = motionSequence()
  const spatial = raw.map(f => blur(f, inp.sigma))
  const [input, med, gat] = normalise(spatial, medianSequence(spatial, frames),
                                      gatedSequence(spatial, frames))

  // `uniform` stays false throughout. In `paramVis` it marks the failure state — two segmentation
  // passes configured alike — and colours the label as a warning. Here an identical window across the
  // columns is the POINT, not a mistake, so the flag would be a red mark on the one row that must be
  // the same.
  const rows: VisRow[] = [
    { key: 'motion', label: 'Simulated', role: 'grid', span: true, uniform: false,
      cells: [cell('', input)] },
    { key: 'method', label: 'Statistic', role: 'text', uniform: false,
      cells: [cell('Median'), cell('Gated')] },
    { key: 'result', label: 'Output', role: 'grid', uniform: false,
      cells: [cell('', med), cell('', gat)] },
    { key: 'window', label: 'Window', role: 'distance', uniform: false,
      cells: [
        // Both at full radius: `paramVis` scales a row relative to its own columns, and these two
        // are equal by construction, so "full" is what that rule yields — spelled out rather than
        // recomputed, since there is no peak to divide by.
        { value: frames, px: null, r: MAX_R, at: 0, text: `${frames} frames`, pxText: '' },
        { value: frames, px: null, r: MAX_R, at: 0, text: `${frames} frames`, pxText: '' },
      ] },
    { key: 'cost', label: 'Extra time', role: 'text', uniform: false,
      cells: [cell('~free'), cell(gatedCost(inp.planes, inp.channels))] },
  ]

  return { columns: [...SMOOTH_VIS_METHODS], rows, pxSize: null, uniformKeys: [] }
}
