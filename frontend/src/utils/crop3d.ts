// Pure helpers for the 3D-crop control (Viewer panel). Kept out of the .vue so the range maths is
// unit-testable (docs/DEV.md → Tests). The crop itself lives in napari — the user draws the XY box over
// a Z/T max-projection there; here we only normalise the z- and t-ranges picked with two 0–100 % sliders
// each into the [0,1] fractions the bridge expects (it converts fractions → pixels, so the frontend
// needn't know the slice/frame count). See docs/NAPARI.md → "3D crop".

export interface Range { lo: number; hi: number }   // fractions in [0,1], lo <= hi

/**
 * Clamp two 0–100 (%) slider values into a valid {lo,hi} fraction pair: clamps each to [0,1], and
 * swaps them if the user dragged the "to" thumb below the "from" thumb. A zero-width range (lo === hi)
 * is allowed. Used for both the z-range and the t-range.
 */
export function normalizeRange(loPct: number, hiPct: number): Range {
  let lo = Math.max(0, Math.min(100, Number.isFinite(loPct) ? loPct : 0)) / 100
  let hi = Math.max(0, Math.min(100, Number.isFinite(hiPct) ? hiPct : 100)) / 100
  if (hi < lo) [lo, hi] = [hi, lo]
  return { lo, hi }
}

/**
 * Whether a range actually trims (i.e. it's not the full 0–100 %). Tolerant of float noise. Used to
 * decide whether the z/t part of the crop is doing anything.
 */
export function rangeCrops({ lo, hi }: Range): boolean {
  return lo > 1e-4 || hi < 1 - 1e-4
}

export interface IndexRange { i0: number; i1: number }   // 0-based, half-open [i0, i1)

/**
 * Which slice/frame indices a 0–100 % range keeps for an axis of length `n`. A faithful mirror of the
 * bridge's `_frac_bounds` (napari_bridge.crop_box): `floor(lo·n)` → `ceil(hi·n)`, clamped, half-open,
 * always ≥1 wide — so a readout built from this matches EXACTLY what Save crops. Assumes `n >= 1`.
 */
export function fracToIndexRange(loPct: number, hiPct: number, n: number): IndexRange {
  const { lo, hi } = normalizeRange(loPct, hiPct)
  let i0 = Math.floor(lo * n)
  let i1 = Math.ceil(hi * n)
  i0 = Math.max(0, Math.min(i0, n - 1))
  i1 = Math.max(i0 + 1, Math.min(i1, n))
  return { i0, i1 }
}

/**
 * Human label for the kept slices/frames next to the % slider: 1-based inclusive, e.g. `"5–16/20"`.
 * Empty when `n` is unknown or ≤1 (a single-slice/frame axis can't be trimmed, so there's nothing to
 * show). This is the concrete count the % never made obvious.
 */
export function fracRangeLabel(loPct: number, hiPct: number, n: number | null | undefined): string {
  if (!n || n <= 1) return ''
  const { i0, i1 } = fracToIndexRange(loPct, hiPct, n)
  return `${i0 + 1}–${i1}/${n}`
}
