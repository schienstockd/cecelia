/**
 * What each ACQUISITION CARD looks like — the figure drawn on the card face in the
 * `CorrectionCardPicker`. The card IS the vis: an immunologist recognizes their data by its
 * appearance ("mine looks like this") rather than by the scanner name ("was this resonance or
 * galvo?"). The recognition surface is the point of the card picker; a labelled pill row shipped
 * that up-front decision to a phrase most users cannot answer without asking.
 *
 * **Pixel-grid intensity frames, same shape as `smoothVis.ts`.** Every card produces one or more
 * `N x N` grids of intensities in `[0, 1]`, rendered by the picker as SVG rects with
 * `fill-opacity` — the same primitive `VisualAid` uses for its `grid` role. That is deliberate:
 * the reader is meant to recognise MICROSCOPY, not a labelled diagram. Line-art dots on a black
 * background looked like a legend key, not the thing itself.
 *
 * **What the four figures show, and why an animation cycles some of them:**
 *   - `resonance`   — sparse bright single-pixel strikes on a mostly-dark field; strikes REFRESH
 *                     every frame because single-photon events do exactly that at a fast dwell.
 *   - `galvo`       — a clean gaussian blob on a low-noise background; STATIC because the
 *                     signature of high-SNR is that nothing between frames tells you anything the
 *                     first frame did not.
 *   - `spinning_disk` — multiple small puncta that drift a cell or two between frames; the
 *                     frame-to-frame displacement IS the "fast timelapse" the card names.
 *   - `deep_3d`     — a side-view of a z-stack, each plane offset laterally from the one below;
 *                     STATIC — the shear reads as sample motion between planes on inspection, not
 *                     as a movie.
 *
 * **Deterministic schematics, not real crops.** Same rule as `smoothVis`: the figure claims a
 * property of the REGIME, not a property of any one movie. A real crop would mislead every user
 * whose data does not look exactly like it. Mulberry32 with a fixed seed per figure so re-renders
 * match and a test can pin the layout.
 *
 * **Pure module** so the shape of each figure is testable in Vitest without mounting anything.
 * The renderer in `CorrectionCardPicker.vue` walks these plain arrays; frame cycling (at 220 ms,
 * reduced-motion honoured) lives in the picker, not here.
 */

/** Grid resolution. Big enough to read as pixel data, small enough to draw cheaply — same order
 *  of magnitude as `smoothVis.N = 16`, doubled because the card face is ~150 px vs a 48 px cell. */
export const N = 24

/** One card's figure: a sequence of `N x N` intensity grids. Length-1 = a still image. */
export interface CardFigure {
  frames: number[][][]
}

/**
 * Card ids in the picker's display order. Matches `CARD_ORDER` in `CorrectionPlanPanel.vue`
 * exactly — this list is the one authority so a re-order in either place would fail the shared
 * test.
 */
export const CARD_IDS = ['resonance', 'galvo', 'spinning_disk', 'deep_3d', 'custom'] as const
export type CardId = typeof CARD_IDS[number]

/** True when the card cycles between frames (implies motion, implies a timer needed). */
export function isAnimated(id: string): boolean {
  return id === 'resonance' || id === 'spinning_disk' || id === 'deep_3d'
}

/**
 * Look up the figure for a card id. Unknown ids and `custom` return a black grid — the picker
 * renders Custom as a dashed-rect + "?" escape row rather than a card face, so its figure is
 * never displayed, but returning a valid `frames` array keeps callers total.
 */
export function cardFigure(id: string): CardFigure {
  switch (id) {
    case 'resonance':     return resonanceFigure()
    case 'galvo':         return galvoFigure()
    case 'spinning_disk': return spinningDiskFigure()
    case 'deep_3d':       return deep3dFigure()
    default:              return { frames: [zeros()] }
  }
}

// ── PRNG + helpers ────────────────────────────────────────────────────────────────────────────

function mulberry32(seed: number): () => number {
  let s = seed >>> 0
  return () => {
    s = (s + 0x6d2b79f5) >>> 0
    let t = s
    t = Math.imul(t ^ (t >>> 15), t | 1)
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61)
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

const zeros = (): number[][] =>
  Array.from({ length: N }, () => new Array<number>(N).fill(0))

/** Faint textured background — the "dark microscopy field", not pure black. Same treatment as
 *  `smoothVis.motionSequence`'s bg (0.06 + rnd * 0.05). */
function textureBg(rnd: () => number, base = 0.03, amp = 0.03): number[][] {
  const out = zeros()
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) out[y][x] = base + rnd() * amp
  }
  return out
}

/** Additive gaussian splat at (cx, cy) with amplitude `amp` and radius sigma — clamped to [0, 1]. */
function splat(g: number[][], cx: number, cy: number, sigma: number, amp: number): void {
  const s2 = 2 * sigma * sigma
  for (let y = 0; y < N; y++) {
    for (let x = 0; x < N; x++) {
      const d2 = (x - cx) ** 2 + (y - cy) ** 2
      g[y][x] = Math.min(1, g[y][x] + amp * Math.exp(-d2 / s2))
    }
  }
}

// ── per-card figures ──────────────────────────────────────────────────────────────────────────

/**
 * The scene the "resonance" and "galvo" cards share — same cellular content, drawn at two
 * different sampling regimes. Same objects, so the visible difference between the two cards is the
 * sampling (photon strikes vs continuous signal), not the scene itself. That is the honest
 * comparison: galvo and resonance are two ways of imaging the same specimen.
 */
function scannedScene(): { cx: number; cy: number; sigma: number; amp: number }[] {
  return [
    { cx: 6.5,  cy: 7.0,  sigma: 2.2, amp: 0.90 },
    { cx: 13.0, cy: 5.5,  sigma: 3.4, amp: 0.95 },
    { cx: 18.0, cy: 10.0, sigma: 1.8, amp: 0.85 },
    { cx: 8.5,  cy: 14.0, sigma: 2.8, amp: 0.92 },
    { cx: 15.5, cy: 16.5, sigma: 4.0, amp: 0.98 },
    { cx: 4.0,  cy: 18.5, sigma: 2.0, amp: 0.85 },
    { cx: 19.5, cy: 20.0, sigma: 1.6, amp: 0.80 },
  ]
}

/** The smooth intensity field the scene would produce at unlimited photons — used as the weighting
 *  field for `resonanceFigure`'s strike positions AND as the render target for `galvoFigure`. */
function sceneField(): number[][] {
  const g = zeros()
  for (const o of scannedScene()) splat(g, o.cx, o.cy, o.sigma, o.amp)
  return g
}

/**
 * Resonance — the same cellular scene as galvo, but photon-limited. Strikes are single-photon
 * events, so they REFRESH every frame; but they are NOT uniform noise — they concentrate where
 * the underlying signal is (rejection-sampled against `sceneField`), so the eye still reads the
 * scene's structure emerging from sparse dots. A uniformly-scattered field would misrepresent the
 * regime as "random noise across the image" rather than "the same cells, undersampled".
 */
function resonanceFigure(): CardFigure {
  const field = sceneField()
  const N_STRIKES = 40
  const N_FRAMES = 4
  const frames: number[][][] = []
  for (let f = 0; f < N_FRAMES; f++) {
    const rnd = mulberry32(7 + f * 137)
    const g = textureBg(rnd, 0.03, 0.02)
    let strikes = 0, tries = 0
    // Rejection sampling: propose a cell, keep it with probability = field intensity. Bounded
    // attempt count so a low-photon frame is not an infinite loop if the field is nearly empty.
    while (strikes < N_STRIKES && tries < 1500) {
      tries++
      const x = Math.floor(rnd() * N)
      const y = Math.floor(rnd() * N)
      if (rnd() < field[y][x]) {
        splat(g, x, y, 0.6, 0.80 + rnd() * 0.20)
        strikes++
      }
    }
    frames.push(g)
  }
  return { frames }
}

/**
 * Galvo — a scattered field of clean, varied-size cell-like objects on a low-noise background.
 * The regime's signature is *clean structure across the field*: you see cells, not photon strikes.
 * A single central blob was the first version and read as an abstract diagram, not microscopy —
 * a real galvo scan shows several objects at once. Static, single frame: at this scale the
 * "clean signal" regime has no visible time-variation.
 */
function galvoFigure(): CardFigure {
  // Continuous rendering of the same scene resonance samples sparsely. Sharing the scene is the
  // point: the visible difference between the two cards is the SAMPLING (dense/smooth vs
  // sparse/discrete), not the content.
  const rnd = mulberry32(11)
  const g = textureBg(rnd, 0.02, 0.02)      // quieter bg than the others — the "high-SNR" claim
  for (const o of scannedScene()) splat(g, o.cx, o.cy, o.sigma, o.amp)
  return { frames: [g] }
}

/**
 * Spinning-disk — the same scene as galvo, but with each object wobbling on a small smooth orbit
 * between frames. The regime's signature is *coherent motion between fast grabs*: cells wobble,
 * they do not teleport, so per-frame deltas follow one continuous per-object trajectory (cos/sin
 * with a phase). A random-per-frame scatter made this card read as noise, which is the opposite
 * of "live cells being tracked".
 */
function spinningDiskFigure(): CardFigure {
  const scene = scannedScene()
  const N_FRAMES = 4
  const ORBIT_R = 1.4                // cells — small enough to read as "wobble", not "flee"
  const frames: number[][][] = []
  for (let f = 0; f < N_FRAMES; f++) {
    const rnd = mulberry32(101 + f * 53)
    const g = textureBg(rnd, 0.03, 0.02)
    const theta = (f / N_FRAMES) * 2 * Math.PI
    // A stable per-object phase so each object orbits a different point on its own circle —
    // otherwise every object translates in lock-step, which reads as "the whole scene shifted"
    // (a drift) rather than "each cell moved on its own" (a live timelapse).
    for (let i = 0; i < scene.length; i++) {
      const o = scene[i]
      const phase = i * (Math.PI * 2 / scene.length) + 0.4
      const dx = Math.cos(theta + phase) * ORBIT_R
      const dy = Math.sin(theta + phase) * ORBIT_R
      splat(g, o.cx + dx, o.cy + dy, o.sigma, o.amp)
    }
    frames.push(g)
  }
  return { frames }
}

/**
 * Deep 3D / breathing — the same scene as galvo, but scrolled through the z-stack. Each frame
 * is one z-plane; between planes the sample has moved (breathing shear), so the objects DRIFT
 * monotonically in one direction across frames rather than wobbling in place (spinning-disk).
 * Objects also fade at the edge planes (out-of-focus depth cue) so the eye reads "z-stack",
 * not "the scene is moving".
 *
 * The visible difference from spinning-disk is the trajectory: linear drift here (constant shear
 * per plane), oscillating orbits there (each cell wobbles on its own). A viewer scrolling
 * through a real deep-3D stack with breathing sees exactly this — the same specimen sliding
 * across the frame as z advances.
 */
function deep3dFigure(): CardFigure {
  const scene = scannedScene()
  const N_PLANES = 5
  const SHEAR_X = 1.4       // cells per plane — the intra-stack lateral drift
  const SHEAR_Y = 0.5
  const frames: number[][][] = []
  for (let p = 0; p < N_PLANES; p++) {
    const rnd = mulberry32(41 + p * 71)
    const g = textureBg(rnd, 0.03, 0.02)
    const offset = p - (N_PLANES - 1) / 2                 // -2, -1, 0, 1, 2
    const dx = offset * SHEAR_X
    const dy = offset * SHEAR_Y
    // Depth-focus fade: middle plane at full amplitude, edges dimmer — the classic out-of-focus
    // cue that says "we are scrolling through the Z axis of a stack", not "the objects are
    // moving through the field".
    const focus = 1 - (Math.abs(offset) / ((N_PLANES - 1) / 2)) * 0.4
    for (const o of scene) {
      splat(g, o.cx + dx, o.cy + dy, o.sigma, o.amp * focus)
    }
    frames.push(g)
  }
  return { frames }
}
