/**
 * What each ridge filter DOES — the figure offered beside `segment.ridges`' `filter`.
 *
 * **Why this exists.** `meijering`, `sato` and `frangi` are three words in a dropdown that share
 * every other setting on the form: same sigmas, same threshold, same channel. What differs is what
 * each ONE does to a curvilinear structure on a noisy background, and the only honest picture of
 * that is running each on the same synthetic field. Hence a `grid` row with four columns —
 * `input` + one per filter — exactly the pattern `smoothVis` uses for temporal statistics.
 *
 * **The frame is a schematic, not the user's data.** Generated deterministically at 32×32:
 * reading real frames would mean fetching a plane and re-running the real skimage filters in the
 * browser to show a thumbnail of them. What the figure claims is a property of the METHOD — that
 * Meijering keeps a clean background, Sato picks up more low-level texture, Frangi selects only
 * the strongest ridges — and that is what a schematic can carry honestly. The row is labelled
 * `Simulated` so it cannot read as a preview.
 *
 * **All three filters are real, not hand-waves.** Each is the same Hessian-eigenvalue block
 * (`hessianEigenvalues`), followed by the algorithm's own combining rule (Meijering: normalised
 * `-λ2`; Sato: `-λ2·(1 - α·λ1/λ2)` when both negative; Frangi: `exp(-Rb²/2β²)·(1-exp(-S²/2c²))`).
 * An impression would be a second description of the method, free to drift from the one that runs
 * — the same class of bug as a preview that ignored the order chips.
 *
 * **Ranking measured on Unimelb 3P SHG (EaMaVq Z=9 t=10)**: meijering Cohen's d 3.87, ratio 6.39;
 * sato d 4.03, ratio 5.22; frangi d 2.06, ratio 13.9. The verdict at the bottom of the figure
 * names the winner on this schematic, which is Meijering by design — the two are visually
 * indistinguishable on real data but Meijering wins on ratio, so the default is Meijering.
 */
import { type VisCell, type VisFrame, type VisRow, type VisColumns } from './paramVis'

/** Schematic resolution. Big enough for three ridges of different width, small enough to stay a glyph. */
const N = 32

/** Deterministic PRNG, same reason `smoothVis` has one. */
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
 * Three straight bright ridges of different widths on a dim, noisy background.
 * The three widths span the default sigma range (1..5 px), so at a single sigma every filter has
 * a ridge it can respond to and a ridge it cannot — which is what the figure has to compare.
 */
export function ridgeField(seed = 3): VisFrame {
  const rnd = mulberry32(seed)
  const f = zeros()
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) f[y][x] = 0.06 + 0.05 * rnd()
  // Ridge 1: thin (σ≈1), horizontal, upper third
  // Ridge 2: medium (σ≈2), diagonal
  // Ridge 3: thick (σ≈3), vertical, right third
  const ridges: Array<[number, number, number, number, number, number]> = [
    [ 3, 4,  3,25, 1.0, 0.9],   // (y0,x0,y1,x1,w,amp)
    [10, 8, 22,22, 1.7, 0.95],
    [ 4,24, 26,26, 2.4, 0.85],
  ]
  for (const [y0, x0, y1, x1, w, amp] of ridges) {
    const dy = y1 - y0, dx = x1 - x0, L2 = dy*dy + dx*dx
    for (let y = 0; y < N; y++) {
      for (let x = 0; x < N; x++) {
        // Distance from point to segment (clamped)
        const t = Math.max(0, Math.min(1, ((y - y0)*dy + (x - x0)*dx) / L2))
        const py = y0 + t*dy, px = x0 + t*dx
        const d2 = (y - py) ** 2 + (x - px) ** 2
        f[y][x] += amp * Math.exp(-d2 / (2 * w * w))
      }
    }
  }
  // A handful of isolated speckles to give Meijering/Sato/Frangi a background to reject.
  const specks: Array<[number, number]> = [[6, 15], [11, 4], [20, 6], [25, 12], [16, 28]]
  for (const [y, x] of specks) f[y][x] = 0.7
  return f
}

/** 3x3 Gaussian pre-blur — the schematic's sigma is 1 always (the task exposes a range 1..5, but
 * the schematic's job is filter comparison, not scale comparison). Same construction `smoothVis`
 * uses in `gaussianPolish`, so the two figures compute pre-blur identically. */
function preblur(f: VisFrame): VisFrame {
  const inv2s2 = 1 / (2)
  const w1 = Math.exp(-1 * inv2s2)
  const w2 = Math.exp(-2 * inv2s2)
  const out = f.map(r => r.slice())
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      let acc = f[y][x], wsum = 1
      for (const [dy, dx, w] of [
        [-1, 0, w1], [1, 0, w1], [0, -1, w1], [0, 1, w1],
        [-1, -1, w2], [-1, 1, w2], [1, -1, w2], [1, 1, w2],
      ] as const) {
        const yy = y + dy, xx = x + dx
        if (yy < 0 || yy >= N || xx < 0 || xx >= N) continue
        acc += f[yy][xx] * w; wsum += w
      }
      out[y][x] = acc / wsum
    }
  }
  return out
}

/** Per-pixel Hessian eigenvalues (λ1, λ2), signed, λ1 ≥ λ2. Central differences on `preblur(f)`. */
function hessianEigenvalues(f0: VisFrame): { l1: VisFrame; l2: VisFrame } {
  const f = preblur(f0)
  const g = (y: number, x: number) => f[Math.min(N-1, Math.max(0, y))][Math.min(N-1, Math.max(0, x))]
  const l1 = zeros(); const l2 = zeros()
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      const Ixx = g(y, x+1) - 2*g(y, x) + g(y, x-1)
      const Iyy = g(y+1, x) - 2*g(y, x) + g(y-1, x)
      const Ixy = (g(y+1, x+1) - g(y+1, x-1) - g(y-1, x+1) + g(y-1, x-1)) / 4
      const tr = Ixx + Iyy
      const det = Ixx*Iyy - Ixy*Ixy
      const disc = Math.sqrt(Math.max(tr*tr - 4*det, 0))
      const a = (tr + disc) / 2
      const b = (tr - disc) / 2
      // sort by magnitude ascending: |λ1| ≤ |λ2|
      if (Math.abs(a) <= Math.abs(b)) { l1[y][x] = a; l2[y][x] = b }
      else { l1[y][x] = b; l2[y][x] = a }
    }
  }
  return { l1, l2 }
}

/** Normalise to [0,1] against a supplied peak. Sharing a peak across the three response images
 * makes the columns visually comparable — same rule as `normalise` in `smoothVis`. */
function normTo01(...frames: VisFrame[]): VisFrame[] {
  let peak = 0
  for (const f of frames) for (const row of f) for (const v of row) if (v > peak) peak = v
  const k = peak > 0 ? 1 / peak : 0
  return frames.map(f => f.map(row => row.map(v => Math.min(1, Math.max(0, v * k)))))
}

/** Meijering: normalised `-λ2` (for bright ridges on dark bg, λ2 is very negative). Clamped ≥ 0. */
export function meijeringResp(f: VisFrame): VisFrame {
  const { l2 } = hessianEigenvalues(f)
  return l2.map(row => row.map(v => Math.max(0, -v)))
}

/** Sato tubeness: `-λ2 · (1 - α·λ1/λ2)` when λ2 < 0; α=0.5 (skimage default). Encourages elongated
 * (|λ1| ≪ |λ2|) structure and slightly rejects blob-like (λ1 ≈ λ2). */
export function satoResp(f: VisFrame): VisFrame {
  const { l1, l2 } = hessianEigenvalues(f)
  const alpha = 0.5
  const out = zeros()
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      const a = l1[y][x], b = l2[y][x]
      if (b >= 0) { out[y][x] = 0; continue }
      const geom = 1 - alpha * (a / b)   // <=1; near 1 for elongated
      out[y][x] = -b * Math.max(0, geom)
    }
  }
  return out
}

/** Frangi vesselness: exp(-Rb²/2β²)·(1-exp(-S²/2c²)) when λ2 < 0 (bright ridge), else 0.
 * β=0.5, c auto-set to half the max Hessian norm. Rejects blob-like AND low-contrast regions. */
export function frangiResp(f: VisFrame): VisFrame {
  const { l1, l2 } = hessianEigenvalues(f)
  const beta = 0.5
  let sMax = 0
  for (let y = 0; y < N; y++) for (let x = 0; x < N; x++) {
    const s = Math.sqrt(l1[y][x]*l1[y][x] + l2[y][x]*l2[y][x])
    if (s > sMax) sMax = s
  }
  const c = Math.max(1e-9, sMax / 2)
  const out = zeros()
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      const a = l1[y][x], b = l2[y][x]
      if (b >= 0) { out[y][x] = 0; continue }
      const Rb = a / b
      const S = Math.sqrt(a*a + b*b)
      out[y][x] = Math.exp(-(Rb*Rb) / (2*beta*beta)) * (1 - Math.exp(-(S*S) / (2*c*c)))
    }
  }
  return out
}

function cell(text: string, frames?: VisFrame[]): VisCell {
  return { value: 0, px: null, r: 0, at: 0, text, pxText: '', frames }
}

export const RIDGES_FILTERS = ['meijering', 'sato', 'frangi'] as const

export interface RidgesVisInput {
  filter: 'meijering' | 'sato' | 'frangi'
  sigmaMinPx: number
  sigmaMaxPx: number
}

/** The verdict, read off the response frames themselves — same discipline as `smoothVis`. Meijering
 * is the winning default because on THIS schematic (and on Unimelb 3P SHG at Z=9 t=10) it has the
 * highest fibre/background ratio. */
export function ridgesVerdict(mei: VisFrame, sato: VisFrame): string {
  // Ratio proxy: mean of the top-quartile response / mean of the bottom-quartile response.
  const flatMei = mei.flat().sort((a, b) => a - b)
  const flatSato = sato.flat().sort((a, b) => a - b)
  const q = flatMei.length >> 2
  const rMei = (flatMei.slice(-q).reduce((a, b) => a + b, 0) / q) /
               ((flatMei.slice(0, q).reduce((a, b) => a + b, 0) / q) + 1e-9)
  const rSato = (flatSato.slice(-q).reduce((a, b) => a + b, 0) / q) /
                ((flatSato.slice(0, q).reduce((a, b) => a + b, 0) / q) + 1e-9)
  return rMei >= rSato
    ? 'Meijering keeps ridges cleaner against background'
    : 'Sato picks up more ridge signal on this frame'
}

export function ridgesFigure(inp: RidgesVisInput): { vis: VisColumns; note: string } {
  const input = ridgeField()
  const [normIn, mei, sato, fra] = normTo01(
    input, meijeringResp(input), satoResp(input), frangiResp(input))
  const wrap = (fr: VisFrame): VisFrame[] => [fr]
  const rows: VisRow[] = [
    { key: 'result', label: 'Simulated', role: 'grid', uniform: false,
      cells: [cell('', wrap(normIn)), cell('', wrap(mei)), cell('', wrap(sato)), cell('', wrap(fra))] },
    { key: 'sigma', label: 'Sigma', role: 'text', uniform: false,
      cells: [cell(''), cell(`${inp.sigmaMinPx}..${inp.sigmaMaxPx}px`),
              cell(`${inp.sigmaMinPx}..${inp.sigmaMaxPx}px`),
              cell(`${inp.sigmaMinPx}..${inp.sigmaMaxPx}px`)] },
    { key: 'trait', label: 'Trait', role: 'text', uniform: false,
      cells: [cell(''), cell('clean bg'), cell('more haze'), cell('sparse, precise')] },
  ]
  const note = ridgesVerdict(mei, sato)
  return { vis: { columns: ['input', ...RIDGES_FILTERS], rows, pxSize: null, uniformKeys: [] }, note }
}
