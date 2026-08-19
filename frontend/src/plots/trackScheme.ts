/**
 * The track scheme — tracks as LANES OVER FRAMES. The arithmetic, kept out of the SFC so it is
 * testable (docs/todo/TRACK_SCHEME_PLAN.md).
 *
 * WHY A SECOND TRACK GEOMETRY MODULE. `plots/trackPaths.ts` answers "what shape did this cell trace",
 * in µm, on two spatial axes. This one answers "WHEN did this track exist", in frames, on one
 * temporal axis. They read the same wire format (`GET /api/tracking/paths`) and share nothing else:
 * every function here is about the `t` array and none of them touch `x`/`y`. Merging them would mean
 * one module with two coordinate systems in it, which is how a µm gets drawn on a frame axis.
 *
 * The reason it exists at all is recorded in the plan: with the tracks on a pair of spatial axes,
 * "I selected two tracks that look almost identical. join and split are both greyed out … are they
 * both from the same timepoints?" — every part of that is a time question asked of a space-only
 * picture. Here it is the x axis.
 *
 * ONE IDEA CARRIES THE WHOLE DESIGN (Decision 3): **a lane is one rect per contiguous run of
 * frames**, so a gap is the ABSENCE of a rect rather than a decoration drawn on top of one. That is
 * what makes the picture load-bearing instead of illustrative — a joinable pair reads as two runs
 * that do not overlap in x, and a pair that cannot be joined reads as two bars side by side over the
 * same frames. The blocked button stops needing a tooltip to explain itself.
 *
 * FRAMES, NOT SECONDS. `t` arrives as `centroid_t` — the raw frame index, unscaled (only the spatial
 * columns go through `scale_centroids!`). Seconds are `t * timeStep` and that conversion belongs at
 * the axis label, not in here: runs are contiguous in FRAMES, and a float second would make
 * "consecutive" a tolerance question.
 */

import type { TrackPathMap } from './trackPaths'
import type { TrackIssue } from '../lib/trackCorrection'

/**
 * One unbroken stretch of frames a track was detected in.
 *
 * `t0`/`t1` are inclusive frames; `i0`/`i1` index the path arrays, so a caller that needs the
 * coordinates of the run's ends (the XY companion marking where a join would happen) can reach them
 * without searching the `t` array again.
 */
export interface TrackRun {
  t0: number
  t1: number
  i0: number
  i1: number
}

/** A track as the scheme draws it: its runs, its extent, and the shape of what is missing. */
export interface Lane {
  track: string
  runs: TrackRun[]
  /** first and last frame the track exists in, inclusive */
  t0: number
  t1: number
  /** frames actually occupied — NOT `t1 - t0 + 1`, which counts the gaps in */
  nFrames: number
  /** number of holes; `runs.length - 1`, named because it is what the eye is looking for */
  nGaps: number
}

/**
 * The step between consecutive frames. 1 for every segmentation Cecelia produces (`centroid_t` is an
 * index, not a time), exposed only so the contiguity rule is a stated assumption rather than a bare
 * `=== 1` buried in a loop.
 */
export const FRAME_STEP = 1

/**
 * Contiguous runs of frames in one track's `t` array.
 *
 * Tolerance is half a step, which is doing real work: `t` arrives as `Float64` from Julia, so two
 * consecutive integer frames can differ by 1 ± 1e-15, and an exact `=== step` test would shatter a
 * perfectly continuous track into one run per frame — a lane that looks like nothing but gaps. It is
 * deliberately NOT a "close enough" fudge for genuinely irregular sampling: a diff of 2 is two
 * frames and must read as a hole.
 *
 * Assumes `t` is ascending, which `track_path_dicts` guarantees (it sorts). A duplicate frame — the
 * same track twice in one timepoint, which is exactly what the `duplicate` detector looks for —
 * extends the current run rather than starting a new one, so a doubled track still draws as one bar.
 */
export function trackRuns(t: readonly number[], step: number = FRAME_STEP): TrackRun[] {
  if (!t?.length) return []
  const tol = step / 2
  const out: TrackRun[] = []
  let i0 = 0
  for (let i = 1; i < t.length; i++) {
    if (t[i] - t[i - 1] > step + tol) {
      out.push({ t0: t[i0], t1: t[i - 1], i0, i1: i - 1 })
      i0 = i
    }
  }
  out.push({ t0: t[i0], t1: t[t.length - 1], i0, i1: t.length - 1 })
  return out
}

/**
 * Every track in the response as a lane.
 *
 * Tracks with no points are dropped rather than drawn as an empty row: the endpoint can only send one
 * if a track_id exists with no timepoints, and a blank lane in the middle of the scheme reads as "this
 * cell vanished", which is a claim about the data rather than about the response.
 */
export function buildLanes(paths: TrackPathMap, step: number = FRAME_STEP): Lane[] {
  const out: Lane[] = []
  for (const [track, p] of Object.entries(paths ?? {})) {
    const t = p?.t
    if (!t?.length) continue
    const runs = trackRuns(t, step)
    out.push({
      track,
      runs,
      t0: runs[0].t0,
      t1: runs[runs.length - 1].t1,
      nFrames: t.length,
      nGaps: runs.length - 1,
    })
  }
  return out
}

/** The frame range the scheme's x axis has to cover. `null` when there is nothing to draw. */
export function frameDomain(lanes: readonly Lane[]): [number, number] | null {
  if (!lanes.length) return null
  let t0 = Infinity
  let t1 = -Infinity
  for (const l of lanes) {
    if (l.t0 < t0) t0 = l.t0
    if (l.t1 > t1) t1 = l.t1
  }
  return [t0, t1]
}

// ── ordering ────────────────────────────────────────────────────────────────

export type LaneOrder = 'pair' | 'start' | 'length' | 'severity' | 'track'

/**
 * The sort choices, and why there is more than one (Open question 1 in the plan).
 *
 * `start` reads like a musical score — lanes enter left to right — and is the right default for
 * BROWSING, where the user is looking for a shape that is wrong. `severity` is the right default when
 * the candidate filter is on, because then the list is a queue. Offering both is the answer to "can't
 * we let the user decide"; picking one and hard-coding it is the mistake the shipped surface made.
 */
export const ORDER_LABEL: Record<LaneOrder, string> = {
  pair: 'Join candidates',
  start: 'First frame',
  length: 'Longest first',
  severity: 'Most suspicious',
  track: 'Track ID',
}

/**
 * Sort lanes for display. Pure — returns a new array.
 *
 * Every comparator ends in the track id, so the order is TOTAL. Without that tiebreak the lane order
 * of, say, 40 equally-severe tracks depends on `Object.entries` iteration order, and a lane that
 * moves between two renders of identical data is the kind of flicker nobody can reproduce on purpose.
 */
export function orderLanes(
  lanes: readonly Lane[], order: LaneOrder, severity: Record<string, number> = {},
): Lane[] {
  const id = (l: Lane) => Number(l.track) || 0
  const cmp: Record<LaneOrder, (a: Lane, b: Lane) => number> = {
    // 'pair' is not a comparator — it is a GROUPING, handled by `orderLanesByPair`. Falling back to
    // `start` here keeps `orderLanes` total for every value of the type.
    pair: (a, b) => a.t0 - b.t0 || id(a) - id(b),
    start: (a, b) => a.t0 - b.t0 || id(a) - id(b),
    length: (a, b) => b.nFrames - a.nFrames || id(a) - id(b),
    severity: (a, b) => (severity[b.track] ?? 0) - (severity[a.track] ?? 0) || id(a) - id(b),
    track: (a, b) => id(a) - id(b),
  }
  return [...lanes].sort(cmp[order] ?? cmp.start)
}

// ── filtering ───────────────────────────────────────────────────────────────

export interface LaneFilter {
  /** keep only these tracks — the candidate filter passes the detector's track ids here */
  tracks?: ReadonlySet<string> | null
  /** keep only lanes with at least one hole — "show me the broken ones" without running the detector */
  gapsOnly?: boolean
  /** drop lanes shorter than this many occupied frames */
  minFrames?: number
}

/**
 * Narrow the lane set. This is Decision 2 in one function: "work the ranked candidates" and "browse
 * and spot it yourself" are the SAME screen with a different filter, not two modes. There is
 * therefore no branch anywhere that asks which mode is on.
 */
export function filterLanes(lanes: readonly Lane[], f: LaneFilter = {}): Lane[] {
  return lanes.filter(l => {
    if (f.tracks && !f.tracks.has(l.track)) return false
    if (f.gapsOnly && l.nGaps === 0) return false
    if (f.minFrames != null && l.nFrames < f.minFrames) return false
    return true
  })
}

// ── the window ──────────────────────────────────────────────────────────────

export interface LaneWindow {
  lanes: Lane[]
  offset: number
  /** how many lanes matched the filter, before windowing */
  total: number
}

/**
 * The visible slice (Decision 7: lanes are windowed, never all of them).
 *
 * 374 tracks is the reference image and is not the ceiling; at one legible lane per ~14 px that is
 * 5000 px of scheme, which is not a plot but a scroll. `offset` is clamped rather than validated so
 * that shrinking the filter under a scrolled window lands on the last page instead of on nothing —
 * an empty plot the user has to scroll BACK from reads as "the filter matched nothing".
 */
export function laneWindow(lanes: readonly Lane[], offset: number, count: number): LaneWindow {
  const total = lanes.length
  const n = Math.max(1, Math.floor(count))
  const off = Math.max(0, Math.min(Math.floor(offset) || 0, Math.max(0, total - n)))
  return { lanes: lanes.slice(off, off + n), offset: off, total }
}

/**
 * What the window is NOT showing, as a phrase — empty when it is showing everything.
 *
 * Same contract as `trackCountNote`: a capped plot that says nothing is a plot that lies, because 40
 * lanes of 374 look exactly like 40 lanes of 40.
 */
export function windowNote(w: LaneWindow, order: LaneOrder): string {
  if (w.total <= w.lanes.length) return ''
  const first = w.offset + 1
  const last = w.offset + w.lanes.length
  return `Tracks ${first}–${last} of ${w.total} — ${ORDER_LABEL[order].toLowerCase()}`
}

// ── candidates ──────────────────────────────────────────────────────────────

/** A detector candidate placed on a lane. `key` is `issueKey`, so ticking survives a refetch. */
export interface SchemeMarker {
  track: string
  /** the frame to draw it at, clamped into the lane's own runs — see `markerFrame` */
  t: number
  kind: string
  severity: number
  key: string
  /** the other tracks the same candidate names — a gap marks both ends, and both know it */
  partners: string[]
}

/**
 * Where a candidate's tick belongs on ONE track's lane.
 *
 * A gap's `atT` is the end of track A, so putting it verbatim on track B's lane draws the tick in
 * empty space BEFORE B starts — a mark on a frame where the cell does not exist, which is precisely
 * the thing the run rects exist to make impossible. Clamping to the nearest occupied frame puts it on
 * B's first bar, so the pair reads as "this end, and that start".
 */
export function markerFrame(lane: Lane, t: number): number {
  let best = lane.t0
  let bestD = Infinity
  for (const r of lane.runs) {
    // inside a run the frame is already right
    if (t >= r.t0 && t <= r.t1) return t
    for (const edge of [r.t0, r.t1]) {
      const d = Math.abs(edge - t)
      if (d < bestD) { bestD = d; best = edge }
    }
  }
  return best
}

/**
 * One marker per (candidate × track it names), for the lanes currently shown.
 *
 * Both ends of a gap get a tick at their own end of the hole, rather than one tick on the "first"
 * track — the candidate is about the relationship, and marking only one side makes the other lane
 * look innocent.
 */
export function issueMarkers(
  issues: readonly TrackIssue[], lanes: readonly Lane[], keyOf: (i: TrackIssue) => string,
): SchemeMarker[] {
  const byTrack = new Map(lanes.map(l => [l.track, l]))
  const out: SchemeMarker[] = []
  for (const issue of issues ?? []) {
    const ids = (issue.trackIds ?? []).map(String)
    for (const id of ids) {
      const lane = byTrack.get(id)
      if (!lane) continue
      out.push({
        track: id,
        t: markerFrame(lane, issue.atT),
        kind: issue.kind,
        severity: issue.severity,
        key: keyOf(issue),
        partners: ids.filter(o => o !== id),
      })
    }
  }
  return out
}

/** Per-track worst severity — the sort key for `orderLanes(…, 'severity')`. */
export function laneSeverity(issues: readonly TrackIssue[]): Record<string, number> {
  const out: Record<string, number> = {}
  for (const i of issues ?? []) {
    for (const id of i.trackIds ?? []) {
      const k = String(id)
      if (!(k in out) || i.severity > out[k]) out[k] = i.severity
    }
  }
  return out
}

/** Every track any candidate names — the set the candidate filter keeps. */
export function candidateTracks(issues: readonly TrackIssue[]): Set<string> {
  const out = new Set<string>()
  for (const i of issues ?? []) for (const id of i.trackIds ?? []) out.add(String(id))
  return out
}

// ── overlap: why a join is refused ──────────────────────────────────────────

/** A stretch of frames where two tracks BOTH have detections. */
export interface FrameOverlap { t0: number; t1: number }

/**
 * The frame spans where two lanes are simultaneously occupied.
 *
 * This is Decision 4 made computable. The engine refuses `track.join` when the two tracks share
 * frames — correctly, because one cell cannot be in two places at once — and the shipped surface
 * expressed that as a greyed button with a tooltip. Here the caller draws these spans, so the reason
 * is on screen BEFORE the button is pressed.
 *
 * Compares runs rather than the lanes' outer extents: two tracks can span the same 40 frames and
 * still interleave perfectly (A on 0–9 and 20–29, B on 10–19), which is a joinable pair whose extents
 * overlap completely. Testing extents would refuse it.
 */
export function laneOverlap(a: Lane, b: Lane): FrameOverlap[] {
  const out: FrameOverlap[] = []
  for (const ra of a.runs) {
    for (const rb of b.runs) {
      const t0 = Math.max(ra.t0, rb.t0)
      const t1 = Math.min(ra.t1, rb.t1)
      if (t0 <= t1) out.push({ t0, t1 })
    }
  }
  return out
}

/** Do these tracks share any frame — i.e. would the engine refuse to join them? */
export function lanesOverlap(a: Lane, b: Lane): boolean {
  return laneOverlap(a, b).length > 0
}

/**
 * The overlaps within a SELECTION, pairwise — what the scheme highlights when a join is impossible.
 *
 * Returns the pairs, not a boolean, because "these two of your four overlap" is the useful statement
 * and "your selection overlaps" is not.
 */
export function selectionOverlaps(
  lanes: readonly Lane[], selected: ReadonlySet<string>,
): { a: string; b: string; spans: FrameOverlap[] }[] {
  const picked = lanes.filter(l => selected.has(l.track))
  const out: { a: string; b: string; spans: FrameOverlap[] }[] = []
  for (let i = 0; i < picked.length; i++) {
    for (let j = i + 1; j < picked.length; j++) {
      const spans = laneOverlap(picked[i], picked[j])
      if (spans.length) out.push({ a: picked[i].track, b: picked[j].track, spans })
    }
  }
  return out
}

// ── geometry ────────────────────────────────────────────────────────────────

export interface SchemeGeom {
  /** plot area, excluding the axis gutters the caller reserves */
  x0: number
  x1: number
  y0: number
  /** lane pitch: bar height plus the gap under it */
  laneH: number
  /** height of the drawn bar within its lane — `laneH - laneGap` */
  barH: number
  /** frame domain, inclusive */
  t0: number
  t1: number
}

export const DEFAULT_LANE_H = 14
export const DEFAULT_BAR_H = 10

/**
 * Frame → pixel.
 *
 * A frame is a BOX of width one, not a point: `t0` maps to the left edge of the first frame and
 * `t1 + 1` to the right edge of the last, so a one-frame run is one frame wide instead of zero wide.
 * Getting this wrong is invisible on a 300-frame track and makes every single-frame detection — the
 * `short` candidates, which are the ones being judged — vanish.
 */
export function frameToX(g: SchemeGeom, t: number): number {
  const span = g.t1 + 1 - g.t0
  if (span <= 0) return g.x0
  return g.x0 + ((t - g.t0) / span) * (g.x1 - g.x0)
}

/** Pixel → frame, floored to a whole frame. The inverse of `frameToX`, for click and hover. */
export function xToFrame(g: SchemeGeom, px: number): number {
  const span = g.t1 + 1 - g.t0
  const w = g.x1 - g.x0
  if (span <= 0 || w <= 0) return g.t0
  const t = g.t0 + ((px - g.x0) / w) * span
  return Math.max(g.t0, Math.min(g.t1, Math.floor(t)))
}

/** Top edge of lane `i`. */
export function laneY(g: SchemeGeom, i: number): number {
  return g.y0 + i * g.laneH
}

/** Which lane index a y pixel lands in, or `null` outside the drawn lanes. */
export function laneAtY(g: SchemeGeom, py: number, n: number): number | null {
  if (n <= 0 || g.laneH <= 0) return null
  const i = Math.floor((py - g.y0) / g.laneH)
  return i >= 0 && i < n ? i : null
}

/** A run, positioned. `t1x` is the RIGHT edge in frames (`t1 + 1`) — see `frameToX`. */
export interface RunRect {
  track: string
  lane: number
  t0: number
  t1: number
  x: number
  y: number
  w: number
  h: number
}

/**
 * The rects to draw, in lane order.
 *
 * `minW` is not cosmetic. On a 400-frame image in a 700 px panel a single-frame run is 1.75 px wide,
 * and the `short` candidates — tracks of one or two frames, the single most common thing the detector
 * flags — would be sub-pixel slivers the user cannot click. Widening them slightly is an honest lie:
 * it distorts duration for the shortest runs only, and the alternative is that they are invisible.
 */
export function runRects(
  lanes: readonly Lane[], g: SchemeGeom, minW = 2,
): RunRect[] {
  const out: RunRect[] = []
  lanes.forEach((lane, i) => {
    const y = laneY(g, i)
    for (const r of lane.runs) {
      const x = frameToX(g, r.t0)
      const w = Math.max(minW, frameToX(g, r.t1 + 1) - x)
      out.push({ track: lane.track, lane: i, t0: r.t0, t1: r.t1, x, y, w, h: g.barH })
    }
  })
  return out
}

export interface SchemeHit {
  lane: number
  track: string
  frame: number
  /** true when the click landed ON a bar, false when it landed in a hole */
  occupied: boolean
}

/**
 * A click, resolved to (track, frame) — the one gesture the whole surface is built on.
 *
 * `occupied` is the part that matters downstream: a click on a bar is "this cell, at this frame"
 * (select, or split here); a click in a hole between two runs of the SAME lane is "this is the gap",
 * which is what a join-across-a-gap gesture needs. P1 only reports it; P2 acts on it.
 */
export function hitTest(
  lanes: readonly Lane[], g: SchemeGeom, px: number, py: number,
): SchemeHit | null {
  const i = laneAtY(g, py, lanes.length)
  if (i === null) return null
  if (px < g.x0 || px > g.x1) return null
  const lane = lanes[i]
  const frame = xToFrame(g, px)
  const occupied = lane.runs.some(r => frame >= r.t0 && frame <= r.t1)
  return { lane: i, track: lane.track, frame, occupied }
}

// ── readouts ────────────────────────────────────────────────────────────────

/**
 * One line describing a lane, for the row label's tooltip.
 *
 * Terse on purpose — the row itself is a bar, and the lesson from the shipped surface was that prose
 * in a list does not get read ("too long. nobody will read this"). This is the hover, not the row.
 */
export function laneSummary(lane: Lane): string {
  const span = `frames ${lane.t0}–${lane.t1}`
  if (!lane.nGaps) return `Track ${lane.track}: ${span}, ${lane.nFrames} detections`
  return `Track ${lane.track}: ${span}, ${lane.nFrames} detections, ` +
    `${lane.nGaps} gap${lane.nGaps > 1 ? 's' : ''}`
}

/** CSV rows behind the picture: one per run, in draw order. */
export function schemeCsvRows(lanes: readonly Lane[]): Record<string, unknown>[] {
  const out: Record<string, unknown>[] = []
  lanes.forEach((lane, i) => {
    lane.runs.forEach((r, j) => {
      out.push({
        lane: i, track: lane.track, run: j,
        firstFrame: r.t0, lastFrame: r.t1, frames: r.t1 - r.t0 + 1,
        trackFirstFrame: lane.t0, trackLastFrame: lane.t1,
        trackDetections: lane.nFrames, trackGaps: lane.nGaps,
      })
    })
  })
  return out
}

/**
 * Frame ticks for the x axis, at a round interval — 1/2/5 × a power of ten.
 *
 * Here rather than in the SFC because it is arithmetic, which is this module's whole charter, and
 * because "how many ticks fit" is the one axis decision that changes with the panel width and so is
 * the one most likely to be got wrong silently. Always includes `t0`; includes `t1` only when it
 * lands on the interval, so the last label is never crowded against the axis end.
 */
export function frameTicks(t0: number, t1: number, target = 8): number[] {
  const span = t1 - t0
  if (!(span > 0) || target < 1) return [t0]
  const raw = span / target
  const mag = Math.pow(10, Math.floor(Math.log10(raw)))
  const step = [1, 2, 5, 10].find(m => m * mag >= raw)! * mag
  const out: number[] = []
  for (let t = Math.ceil(t0 / step) * step; t <= t1; t += step) out.push(t)
  if (!out.length || out[0] !== t0) out.unshift(t0)
  return out
}

// ── join candidates: the pairs, and the order that makes them readable ───────

/** Two tracks the detector believes are one cell, worst first. */
export interface JoinPair {
  a: string
  b: string
  severity: number
  /** the frame track A ends on — where the proposed join starts */
  atT: number
  key: string
}

/**
 * The candidates that are actually a JOIN, in the order they should be worked.
 *
 * Only `track.join` ops: a `jump` or a `short` names one track and has no partner, so including them
 * would produce "pairs" whose second half is missing. Deduplicated on the unordered pair, because the
 * detector can emit the same two tracks from two signatures (a gap and a duplicate) and the user
 * should see one row, not two identical ones.
 */
export function joinPairs(
  issues: readonly TrackIssue[], keyOf: (i: TrackIssue) => string,
): JoinPair[] {
  const seen = new Set<string>()
  const out: JoinPair[] = []
  for (const i of issues ?? []) {
    if (i.op?.op !== 'track.join') continue
    const ids = (i.op.trackIds ?? i.trackIds ?? []).map(String)
    if (ids.length !== 2) continue
    const dedup = [...ids].sort().join('|')
    if (seen.has(dedup)) continue
    seen.add(dedup)
    out.push({ a: ids[0], b: ids[1], severity: i.severity, atT: i.atT, key: keyOf(i) })
  }
  return out.sort((x, y) => y.severity - x.severity || Number(x.a) - Number(y.a))
}

/**
 * Lanes ordered so the two halves of each join candidate are ADJACENT — worst pair at the top.
 *
 * This is the answer to "is there any order of candidates that could be joined?" Every other order
 * scatters a pair across the panel, so the one comparison the surface exists to support — do these
 * two overlap in time, or is there a clean hole between them? — needs scrolling to make. Here the pair
 * is two neighbouring rows, and the answer is the shape.
 *
 * Lanes named by no pair follow, by first frame, rather than being dropped: the order is a RANKING,
 * not a filter, and the two are kept separate so "candidates only" stays one checkbox (Decision 2).
 */
export function orderLanesByPair(lanes: readonly Lane[], pairs: readonly JoinPair[]): Lane[] {
  const byTrack = new Map(lanes.map(l => [l.track, l]))
  const used = new Set<string>()
  const out: Lane[] = []
  for (const p of pairs) {
    for (const t of [p.a, p.b]) {
      const lane = byTrack.get(t)
      if (!lane || used.has(t)) continue
      used.add(t)
      out.push(lane)
    }
  }
  for (const l of orderLanes(lanes, 'start')) if (!used.has(l.track)) out.push(l)
  return out
}

/**
 * The proposed joins that are drawable right now: both halves on screen, with their lane rows.
 *
 * `fromT`/`toT` are the frames the connector spans — A's last detection and B's first — rather than
 * the candidate's `atT`, so the line lands on the bars' actual ends even when the detector's frame
 * sits elsewhere.
 */
export interface JoinLink {
  key: string
  a: string
  b: string
  laneA: number
  laneB: number
  fromT: number
  toT: number
  /** the two tracks share frames, so the engine would REFUSE this join */
  blocked: boolean
}

export function joinLinks(lanes: readonly Lane[], pairs: readonly JoinPair[]): JoinLink[] {
  const index = new Map(lanes.map((l, i) => [l.track, i]))
  const out: JoinLink[] = []
  for (const p of pairs) {
    const ia = index.get(p.a)
    const ib = index.get(p.b)
    if (ia === undefined || ib === undefined) continue
    const la = lanes[ia]
    const lb = lanes[ib]
    out.push({
      key: p.key, a: p.a, b: p.b, laneA: ia, laneB: ib,
      fromT: la.t1, toT: lb.t0, blocked: lanesOverlap(la, lb),
    })
  }
  return out
}

/**
 * The exact frames two lanes share — the engine's own join rule, answerable from the runs.
 *
 * `_op_join` refuses a join when the set of timepoints track B shares with track A is non-empty, so
 * this is that set. Passed to `manualActions` as its `sharedFrames`, it makes the button's refusal and
 * the red band on the timeline the SAME fact rather than two rules that agree most of the time.
 */
export function sharedFrames(a: Lane, b: Lane): number[] {
  const out: number[] = []
  for (const s of laneOverlap(a, b)) for (let t = s.t0; t <= s.t1; t++) out.push(t)
  return [...new Set(out)].sort((x, y) => x - y)
}
