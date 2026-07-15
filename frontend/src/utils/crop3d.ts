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
