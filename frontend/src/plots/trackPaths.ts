/**
 * Track paths in the browser — the arithmetic, kept out of the SFC so it is testable.
 *
 * THERE WAS NO TRACK-PATH PLOTTING IN THE FRONTEND BEFORE THIS. Tracks are viewed as polylines over
 * the raw image in napari, and `lib/tips.ts` states that as the project's position. That stays true:
 * napari is where you look at tracks. What napari cannot do is answer one question quickly, 31 times
 * in a row — "do these two pieces of track look like the same cell?" — because each answer costs a
 * seek and a frame change. So the correction worklist (docs/todo/CORRECTION_PLAN.md → P4) draws a
 * small path thumbnail per candidate, purely to let the user REJECT the obvious non-issues without
 * leaving the list, and fly to napari for the ones that need the image.
 *
 * ONE module for this, used by every track-path drawing that follows — not a private helper inside
 * the correction view. Observable Plot (the charting decision, see docs/PLOTS.md), `Plot.line` with
 * `z` for one polyline per track; no new dependency.
 *
 * Coordinates arrive in µm from `GET /api/tracking/issues` (converted server-side by the shared
 * `scale_centroids!`, so distances mean the same thing here as in `track_measures`). Never scale
 * pixels here — that is how two "µm" that disagree get created.
 */

/** One track's path, as the issues endpoint sends it. `y` is empty for a 1-D/degenerate case. */
export interface TrackPath {
  t: number[]
  x: number[]
  y: number[]
  label: number[]
}

export type TrackPathMap = Record<string, TrackPath>

/** A point on a path, flattened for Observable Plot (which wants row objects, not arrays). */
export interface PathPoint {
  track: string
  t: number
  x: number
  y: number
  label: number
  /** Index within the track, 0-based — for marking the first/last point. */
  i: number
}

/**
 * Flatten the selected tracks into plot rows, in time order.
 *
 * Tracks named in `trackIds` but absent from `paths` are skipped rather than throwing: the worklist
 * and its geometry are one response, but a candidate can reference a track the geometry cap left out,
 * and a missing thumbnail is better than a broken page.
 */
export function pathPoints(paths: TrackPathMap, trackIds: readonly (number | string)[]): PathPoint[] {
  const out: PathPoint[] = []
  for (const id of trackIds) {
    const key = String(id)
    const p = paths[key]
    if (!p || !p.x?.length) continue
    const n = Math.min(p.t.length, p.x.length)
    for (let i = 0; i < n; i++) {
      out.push({
        track: key,
        t: p.t[i],
        x: p.x[i],
        y: p.y?.length ? p.y[i] : 0,
        label: p.label?.[i] ?? -1,
        i,
      })
    }
  }
  return out
}

/**
 * A square-ish data window around the points, padded, with EQUAL x/y extents.
 *
 * Equal extents on purpose: this is a picture of a path in space, so an auto-fitted box would
 * stretch one axis and turn a straight run into a diagonal or a right-angled turn into a shallow one
 * — which is exactly the shape judgement the thumbnail exists to support. `pad` is a fraction of the
 * larger extent. A degenerate window (one point, or no motion) gets `minSpan` so the scale never
 * collapses to zero width.
 */
export function pathDomain(
  pts: readonly PathPoint[],
  opts: { pad?: number; minSpan?: number } = {},
): { x: [number, number]; y: [number, number] } | null {
  if (!pts.length) return null
  const pad = opts.pad ?? 0.15
  const minSpan = opts.minSpan ?? 1
  let x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity
  for (const p of pts) {
    if (p.x < x0) x0 = p.x
    if (p.x > x1) x1 = p.x
    if (p.y < y0) y0 = p.y
    if (p.y > y1) y1 = p.y
  }
  const cx = (x0 + x1) / 2
  const cy = (y0 + y1) / 2
  const span = Math.max(x1 - x0, y1 - y0, minSpan) * (1 + pad * 2)
  const h = span / 2
  return { x: [cx - h, cx + h], y: [cy - h, cy + h] }
}

/**
 * The point a candidate is ABOUT — where the viewer should look.
 *
 * For a gap that is the end of the first track; for a jump/split it is the cell at `atT`. Returned
 * separately from the paths so the thumbnail can mark it and the napari fly-to can use the same
 * coordinate, rather than each deriving its own idea of "the spot".
 */
export function focusPoint(
  pts: readonly PathPoint[],
  atT: number,
  trackId?: number | string,
): PathPoint | null {
  const key = trackId === undefined ? null : String(trackId)
  let best: PathPoint | null = null
  let bestD = Infinity
  for (const p of pts) {
    if (key !== null && p.track !== key) continue
    const d = Math.abs(p.t - atT)
    if (d < bestD) { bestD = d; best = p }
  }
  return best
}

/**
 * Gap geometry: the two ends being considered for a join, and how the two tracks are HEADING.
 *
 * The heading is the actual discriminator and the number in the worklist row cannot express it. Two
 * track ends 2 µm apart are one cell if the first was travelling toward where the second starts, and
 * are two different cells if it was travelling away — same distance, opposite answer. `cosine` is
 * between track A's final direction and the A-end → B-start vector: ~1 means "carried straight on",
 * ~-1 means "the new track is behind it".
 *
 * `null` when either track has too few points to have a direction (a single-point track has none).
 */
export function gapGeometry(
  paths: TrackPathMap,
  aId: number | string,
  bId: number | string,
): { from: PathPoint; to: PathPoint; distance: number; cosine: number | null } | null {
  const a = pathPoints(paths, [aId])
  const b = pathPoints(paths, [bId])
  if (!a.length || !b.length) return null
  const from = a[a.length - 1]
  const to = b[0]
  const dx = to.x - from.x
  const dy = to.y - from.y
  const distance = Math.hypot(dx, dy)

  let cosine: number | null = null
  if (a.length >= 2 && distance > 0) {
    const prev = a[a.length - 2]
    const hx = from.x - prev.x
    const hy = from.y - prev.y
    const hlen = Math.hypot(hx, hy)
    if (hlen > 0) cosine = (hx * dx + hy * dy) / (hlen * distance)
  }
  return { from, to, distance, cosine }
}

/**
 * One short sentence for what the geometry adds to the row's text — the instruction, not a readout.
 *
 * Deliberately only three outcomes. A cosine printed to two decimals is a number the user then has to
 * interpret, which is the thing this whole surface is trying to remove.
 */
export function gapHint(cosine: number | null): string {
  if (cosine === null) return 'Too short to tell which way it was going.'
  if (cosine >= 0.5) return 'Carries straight on — likely the same cell.'
  if (cosine <= -0.5) return 'Doubles back — likely a different cell.'
  return 'Turns sharply — check the image.'
}

/**
 * Star plot (rose plot) — every track translated to start at a common origin.
 *
 * Port of celltrackR's `normalizeTracks()` (Wortel et al. 2021, Cell Reports Methods,
 * doi:10.1016/j.crmeth.2021.100006; vignette `ana-methods` §1.2 "Star plots (Rose plots)"). Overlaying
 * the starting points is the standard way to SEE directionality: with unbiased migration the paths
 * fan out evenly in all directions, and a population that is actually drifting — or following a
 * gradient — shows as a fan pointing one way.
 *
 * It is a pure translation, so speeds, turning angles and path lengths are all unchanged; only the
 * absolute position is discarded. That is the point — position is exactly what makes directionality
 * impossible to see in the raw plot, because each cell starts somewhere different.
 *
 * Note this is a VIEW transform. It never touches stored coordinates.
 */
export function normalizeTracks(pts: readonly PathPoint[]): PathPoint[] {
  const origin = new Map<string, { x: number; y: number }>()
  for (const p of pts) {
    // the track's FIRST point by time — `pathPoints` emits in time order, so the first seen wins
    if (!origin.has(p.track)) origin.set(p.track, { x: p.x, y: p.y })
  }
  return pts.map(p => {
    const o = origin.get(p.track)!
    return { ...p, x: p.x - o.x, y: p.y - o.y }
  })
}

/**
 * Net displacement per track, from a star-plot point set — the arrow a rose plot draws.
 *
 * Returned separately from the paths because the two answer different questions: the paths show HOW
 * each cell wandered, the displacement vectors show WHERE the population ended up. A rose plot with
 * hundreds of tracks becomes unreadable as paths and stays readable as vectors.
 */
export function displacementVectors(
  pts: readonly PathPoint[],
): { track: string; x: number; y: number; angle: number; distance: number }[] {
  const first = new Map<string, PathPoint>()
  const last = new Map<string, PathPoint>()
  for (const p of pts) {
    if (!first.has(p.track)) first.set(p.track, p)
    last.set(p.track, p)
  }
  const out: { track: string; x: number; y: number; angle: number; distance: number }[] = []
  for (const [track, f] of first) {
    const l = last.get(track)!
    const x = l.x - f.x
    const y = l.y - f.y
    out.push({
      track, x, y,
      // degrees CCW from +x, in [0, 360) — a compass bearing for the fan, not a signed angle
      angle: ((Math.atan2(y, x) * 180) / Math.PI + 360) % 360,
      distance: Math.hypot(x, y),
    })
  }
  return out
}

/**
 * One CSV row per plotted point — the numbers behind the picture, in the same order it drew them.
 *
 * `values` is the per-track colour value (whatever the plot is coloured by), repeated on every row of
 * that track. That is a denormalised column, deliberately: the alternative is a second file to join,
 * and the reason to export this at all is to take one track's coordinates into another tool.
 */
export function pathCsvRows(
  pts: readonly PathPoint[],
  values: Record<string, number | string | null> = {},
  valueLabel = 'value',
): Record<string, unknown>[] {
  const hasValues = Object.keys(values).length > 0
  return pts.map(p => ({
    track: p.track, t: p.t, x: p.x, y: p.y, label: p.label,
    ...(hasValues ? { [valueLabel]: values[p.track] ?? '' } : {}),
  }))
}

/**
 * What the plot is NOT showing, as a phrase — empty when it is showing everything.
 *
 * A capped plot that says nothing is a plot that lies: a hairball of 500 tracks looks exactly like a
 * hairball of 5000, and the reader has no way to tell which they are looking at.
 */
export function trackCountNote(shown: number, total: number): string {
  return total > shown ? `${shown} of ${total} tracks — longest first` : ''
}
