/**
 * What a segmentation parameter LOOKS like, per pass — the geometry behind the column strip beside a
 * repeatable model group.
 *
 * **Why this exists.** Coastal's model group carries ten-odd numbers per pass, most of them lengths
 * in microns, and reading two columns of digits does not tell you the thing that actually matters:
 * whether the two passes are looking for different objects. They must be — entries are applied in
 * order and each labels only what an earlier one left, so a second pass configured like the first
 * grows to the same regions, gets clipped along the first's boundaries and leaves slivers. Two
 * circles of visibly different size, and two threshold markers at opposite ends of their track, say
 * that in one glance. Two identical columns say the opposite, just as loudly.
 *
 * **Roles come from the spec, not from key names.** A param declares `vis` and this module knows how
 * to draw that role. Matching on `seedSize` here would make the picture a second, silently divergent
 * description of the form — the same class of bug as the preview that ignored the order chips.
 *
 * **Scale is RELATIVE across the columns**, with the largest value in a row setting the full radius.
 * Absolute scale would make a 32 px seed window and a 1.5 px merge distance unplottable together, and
 * the question being answered is "do these two passes differ", which is a comparison. Pixel counts
 * appear in the caption when the image's pixel size is known, because that is the number the engine
 * actually receives and the one to check against a reference in pixels.
 */
import type { ParamDef, ParamValues } from './types'

/** How a parameter is drawn. Anything else in the spec's `vis` is ignored rather than guessed at. */
export type VisRole = 'diameter' | 'blur' | 'distance' | 'area' | 'fraction'

const SPATIAL: VisRole[] = ['diameter', 'blur', 'distance', 'area']

/** Largest radius, in the SVG's own units. The component scales the whole thing with CSS. */
export const MAX_R = 22

/** A `fraction` row's track length, same units. */
export const TRACK = 56

export interface VisCell {
  /** the raw parameter value, in the form's units */
  value: number
  /** `value` in image pixels — null when the image has no pixel size, or the role is unitless */
  px: number | null
  /** drawn radius for a spatial role, 0 for `fraction` */
  r: number
  /** marker position along the track, 0–1, for `fraction`; 0 otherwise */
  at: number
  /** what to print under the shape, in the form's own units — the same as the row label */
  text: string
  /** the same value in image pixels, or '' — the engine-facing number, shown small underneath */
  pxText: string
}

export interface VisRow {
  key: string
  label: string
  role: VisRole
  /** one per column, in the same order as `columns` */
  cells: VisCell[]
  /** true when every column holds the same value — the state a two-pass config must not be in */
  uniform: boolean
}

export interface VisColumns {
  /** entry keys, in RUN order */
  columns: string[]
  rows: VisRow[]
  /** µm per pixel, when known */
  pxSize: number | null
  /** rows whose values are identical across every column, when there is more than one */
  uniformKeys: string[]
}

/** The spec's declared role, or null. Unknown strings are ignored — a typo must not draw a guess. */
function roleOf(p: ParamDef): VisRole | null {
  const v = (p as { vis?: string }).vis
  return v === 'diameter' || v === 'blur' || v === 'distance' || v === 'area'
    || v === 'fraction' ? v : null
}

/** Group sub-params, flattened — sections are a visual box, the values are stored flat. */
function leaves(param: ParamDef): ParamDef[] {
  const out: ParamDef[] = []
  for (const p of param.params ?? []) {
    if (p.type === 'section') out.push(...(p.params ?? []))
    else out.push(p)
  }
  return out
}

function numeric(v: unknown): number | null {
  const n = typeof v === 'number' ? v : typeof v === 'string' ? Number(v) : NaN
  return Number.isFinite(n) ? n : null
}

/**
 * `sqrt` for `area`, linear otherwise — an area drawn with its value as the radius exaggerates by the
 * square, so a 10 µm² floor beside a 2 µm² one would look 5x bigger instead of 2.2x. The radius is
 * what the eye compares, so the radius is what has to carry the ratio.
 */
function magnitude(role: VisRole, value: number): number {
  return role === 'area' ? Math.sqrt(Math.max(0, value)) : Math.max(0, value)
}

/**
 * What a role can be COMPARED with. Everything in microns shares one scale; an area in µm² is a
 * different quantity and gets its own; a threshold has no scale at all, it has a track.
 *
 * THE bug this fixes: scaling each row against only its own columns means a single-column group draws
 * every shape at full radius, because each row's one value is trivially its own maximum. A 4 µm seed
 * window and a 2 µm² size floor then render as identical circles, which is not a small imprecision —
 * it is the picture answering "these are the same" about two unrelated numbers. Sharing the scale
 * across every row of the same dimension makes one column as readable as two.
 */
function dimension(role: VisRole): 'length' | 'area' | 'none' {
  return role === 'area' ? 'area' : role === 'fraction' ? 'none' : 'length'
}

/**
 * Build the strip. `order` is the entry keys in RUN order (from `groupOrderKeys`) — not the object's
 * own key order, because which pass is "first" is the whole meaning of the picture.
 *
 * `pxSize` is µm per pixel, or null. A row with no numeric value in any column is dropped rather than
 * drawn at zero: a blank shape reads as "this is off", which is a claim about the parameter.
 */
export function paramVisColumns(param: ParamDef, values: Record<string, ParamValues>,
                                order: string[], pxSize?: number | null): VisColumns {
  const columns = order.filter(k => k in values)
  const rows: VisRow[] = []
  const px = pxSize && pxSize > 0 ? pxSize : null

  // First pass: what each row holds, and the peak magnitude per DIMENSION across every row and
  // column. The scale cannot be decided row by row — see `dimension`.
  const found: Array<{ p: ParamDef; role: VisRole; raw: Array<number | null>; mags: number[] }> = []
  const peaks: Record<string, number> = { length: 0, area: 0, none: 0 }
  for (const p of leaves(param)) {
    const role = roleOf(p)
    if (!role) continue
    const raw = columns.map(k => numeric(values[k]?.[p.key]))
    if (!raw.some(v => v !== null)) continue
    const mags = raw.map(v => (v === null ? 0 : magnitude(role, v)))
    const dim = dimension(role)
    peaks[dim] = Math.max(peaks[dim], ...mags)
    found.push({ p, role, raw, mags })
  }

  for (const { p, role, raw, mags } of found) {
    const peak = peaks[dimension(role)]
    const cells: VisCell[] = raw.map((v, i) => {
      const value = v ?? 0
      // Every shape of one dimension being zero is a real state — "blur off on both passes" — and
      // must not divide by zero into NaN radii. A non-zero value never draws as nothing.
      const r = SPATIAL.includes(role)
        ? (peak > 0 ? Math.max(value > 0 ? 2 : 0, (mags[i] / peak) * MAX_R) : 0)
        : 0
      const at = role === 'fraction' ? Math.min(1, Math.max(0, value)) : 0
      return { value, px: px && role !== 'fraction' ? pxOf(role, value, px) : null, r, at,
               text: caption(role, value), pxText: pxCaption(role, value, px) }
    })
    const uniform = columns.length > 1 && raw.every(v => v === raw[0])
    rows.push({ key: p.key, label: p.label ?? p.key, role, cells, uniform })
  }

  // Spatial rows first, then the thresholds: "what size is it looking for" before "how readily does
  // it grow". Stable within each block, so the spec's own order still decides ties.
  rows.sort((a, b) => Number(SPATIAL.includes(b.role)) - Number(SPATIAL.includes(a.role)))

  return { columns, rows, pxSize: px,
           uniformKeys: rows.filter(r => r.uniform).map(r => r.key) }
}

/** A length in µm → pixels; an AREA in µm² → pixels², which needs both axes. */
function pxOf(role: VisRole, value: number, pxSize: number): number | null {
  if (value <= 0) return 0
  return role === 'area' ? value / (pxSize * pxSize) : value / pxSize
}

/**
 * The caption. Pixels are what the engine receives, so they lead when known — a reference tuned in
 * pixels (coastal's own is) can only be checked against this number.
 */
export function caption(role: VisRole, value: number): string {
  if (role === 'fraction') return trim(value)
  return value <= 0 ? 'off' : trim(value)
}

/**
 * The SECOND line — the same quantity in image pixels, or ''. Secondary, not primary: the row label
 * says "Seed window (µm)" and the control the user is dragging is in µm, so a caption reading "32 px"
 * contradicts both. But pixels are what the engine receives and the only thing a reference tuned in
 * pixels can be checked against, so they belong here rather than nowhere.
 */
export function pxCaption(role: VisRole, value: number, pxSize: number | null): string {
  if (role === 'fraction' || !pxSize || value <= 0) return ''
  const p = pxOf(role, value, pxSize) ?? 0
  return role === 'area' ? `${Math.round(p)} px²` : `${Math.round(p)} px`
}

function trim(n: number): string {
  return String(Number(n.toFixed(2)))
}

/**
 * The warning under the strip, or ''. Named here rather than in the component so the wording and the
 * condition cannot drift apart, and so a test can pin both.
 *
 * Only for rows that decide how far a pass GROWS. Two passes sharing a merge threshold or a
 * normalisation percentile is ordinary; two passes sharing their seed size and growing threshold is
 * the failure this whole picture exists to make visible.
 */
export const DEFINING_ROLES: VisRole[] = ['diameter', 'fraction']

export function uniformWarning(vis: VisColumns): string {
  if (vis.columns.length < 2) return ''
  const same = vis.rows.filter(r => DEFINING_ROLES.includes(r.role) && r.uniform)
  if (!same.length) return ''
  return same.length === 1
    ? `Both passes share the same ${same[0].label.toLowerCase()} — the second will mostly repeat the first`
    : `${same.length} settings are identical on both passes — the second will mostly repeat the first`
}
