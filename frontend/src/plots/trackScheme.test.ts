import { describe, it, expect } from 'vitest'
import {
  trackRuns, buildLanes, frameDomain, orderLanes, filterLanes, laneWindow, windowNote,
  markerFrame, issueMarkers, laneSeverity, candidateTracks,
  laneOverlap, lanesOverlap, selectionOverlaps,
  frameToX, xToFrame, laneY, laneAtY, runRects, hitTest, laneSummary, schemeCsvRows, frameTicks,
  joinPairs, orderLanesByPair, joinLinks, sharedFrames,
  type SchemeGeom,
} from './trackScheme'
import type { TrackIssue } from '../lib/trackCorrection'

const path = (t: number[]) => ({ t, x: t.map(() => 0), y: t.map(() => 0), label: t.map((_, i) => i) })

const geom = (over: Partial<SchemeGeom> = {}): SchemeGeom =>
  ({ x0: 0, x1: 100, y0: 0, laneH: 10, barH: 8, t0: 0, t1: 9, ...over })

describe('trackRuns', () => {
  it('a continuous track is ONE run', () => {
    expect(trackRuns([0, 1, 2, 3])).toEqual([{ t0: 0, t1: 3, i0: 0, i1: 3 }])
  })

  it('splits at a hole — the gap is the absence of a run', () => {
    expect(trackRuns([0, 1, 2, 5, 6])).toEqual([
      { t0: 0, t1: 2, i0: 0, i1: 2 },
      { t0: 5, t1: 6, i0: 3, i1: 4 },
    ])
  })

  it('a one-frame track is a run, not nothing', () => {
    expect(trackRuns([7])).toEqual([{ t0: 7, t1: 7, i0: 0, i1: 0 }])
  })

  it('empty in, empty out', () => {
    expect(trackRuns([])).toEqual([])
  })

  // `t` arrives as Float64 from Julia. An exact `=== 1` test would shatter a continuous track into
  // one run per frame — a lane that is nothing but gaps, on data with no gaps at all.
  it('float noise does not shatter a continuous track', () => {
    expect(trackRuns([0, 1 + 1e-12, 2 - 1e-12, 3])).toHaveLength(1)
  })

  it('a diff of two is a hole, not noise', () => {
    expect(trackRuns([0, 2])).toHaveLength(2)
  })

  // the `duplicate` detector's signature: the same track twice in one frame. It must still draw as
  // one bar — a doubled track is a track, not a track with a hole in it.
  it('a duplicated frame extends the run', () => {
    expect(trackRuns([0, 1, 1, 2])).toEqual([{ t0: 0, t1: 2, i0: 0, i1: 3 }])
  })

  it('honours a non-unit step', () => {
    expect(trackRuns([0, 2, 4, 10], 2)).toEqual([
      { t0: 0, t1: 4, i0: 0, i1: 2 },
      { t0: 10, t1: 10, i0: 3, i1: 3 },
    ])
  })
})

describe('buildLanes', () => {
  it('counts occupied frames, not the span', () => {
    const [lane] = buildLanes({ '1': path([0, 1, 2, 8, 9]) })
    expect(lane).toMatchObject({ track: '1', t0: 0, t1: 9, nFrames: 5, nGaps: 1 })
    expect(lane.runs).toHaveLength(2)
  })

  it('drops a track with no points rather than drawing a blank lane', () => {
    expect(buildLanes({ '1': path([]), '2': path([0, 1]) }).map(l => l.track)).toEqual(['2'])
  })

  it('survives a missing paths object', () => {
    expect(buildLanes(undefined as never)).toEqual([])
  })
})

describe('frameDomain', () => {
  it('spans every lane', () => {
    expect(frameDomain(buildLanes({ '1': path([4, 5]), '2': path([0, 1]) }))).toEqual([0, 5])
  })
  it('is null when there is nothing to draw', () => {
    expect(frameDomain([])).toBeNull()
  })
})

describe('orderLanes', () => {
  const lanes = buildLanes({
    '3': path([5, 6, 7, 8]),
    '1': path([2, 3]),
    '2': path([2, 3, 4]),
  })

  it('start reads like a score — earliest entry first', () => {
    expect(orderLanes(lanes, 'start').map(l => l.track)).toEqual(['1', '2', '3'])
  })

  it('length puts the longest first', () => {
    expect(orderLanes(lanes, 'length').map(l => l.track)).toEqual(['3', '2', '1'])
  })

  it('severity uses the detector ranking', () => {
    expect(orderLanes(lanes, 'severity', { '1': 0.9, '3': 0.5 }).map(l => l.track))
      .toEqual(['1', '3', '2'])
  })

  // without a total order, equally-ranked lanes swap between renders of identical data — flicker
  // nobody can reproduce on purpose
  it('ties break on track id, so the order is total', () => {
    const tied = buildLanes({ '9': path([0, 1]), '2': path([0, 1]), '5': path([0, 1]) })
    expect(orderLanes(tied, 'start').map(l => l.track)).toEqual(['2', '5', '9'])
    expect(orderLanes(tied, 'severity').map(l => l.track)).toEqual(['2', '5', '9'])
  })

  it('does not mutate its input', () => {
    const before = lanes.map(l => l.track)
    orderLanes(lanes, 'length')
    expect(lanes.map(l => l.track)).toEqual(before)
  })
})

describe('filterLanes', () => {
  const lanes = buildLanes({ '1': path([0, 1, 2]), '2': path([0, 5]), '3': path([4]) })

  it('keeps the named tracks — this is the candidate filter', () => {
    expect(filterLanes(lanes, { tracks: new Set(['2']) }).map(l => l.track)).toEqual(['2'])
  })
  it('gapsOnly finds the broken ones without the detector', () => {
    expect(filterLanes(lanes, { gapsOnly: true }).map(l => l.track)).toEqual(['2'])
  })
  it('minFrames drops the stubs', () => {
    expect(filterLanes(lanes, { minFrames: 2 }).map(l => l.track)).toEqual(['1', '2'])
  })
  it('no filter keeps everything', () => {
    expect(filterLanes(lanes)).toHaveLength(3)
  })
})

describe('laneWindow', () => {
  const lanes = buildLanes(Object.fromEntries(
    Array.from({ length: 10 }, (_, i) => [String(i + 1), path([0, 1])]),
  ))

  it('slices', () => {
    expect(laneWindow(lanes, 2, 3)).toMatchObject({ offset: 2, total: 10 })
    expect(laneWindow(lanes, 2, 3).lanes).toHaveLength(3)
  })

  // shrinking the filter under a scrolled window must land on the last page, not on nothing: an
  // empty plot the user has to scroll BACK from reads as "the filter matched nothing"
  it('clamps an offset past the end onto the last page', () => {
    expect(laneWindow(lanes, 99, 4).offset).toBe(6)
  })
  it('clamps a negative offset', () => {
    expect(laneWindow(lanes, -5, 4).offset).toBe(0)
  })
  it('a window bigger than the set shows all of it at offset 0', () => {
    expect(laneWindow(lanes, 3, 50)).toMatchObject({ offset: 0, total: 10 })
  })
})

describe('windowNote', () => {
  it('says what is hidden', () => {
    const lanes = buildLanes(Object.fromEntries(
      Array.from({ length: 10 }, (_, i) => [String(i + 1), path([0, 1])]),
    ))
    expect(windowNote(laneWindow(lanes, 2, 3), 'length')).toBe('Tracks 3–5 of 10 — longest first')
  })
  it('is silent when nothing is hidden', () => {
    const lanes = buildLanes({ '1': path([0, 1]) })
    expect(windowNote(laneWindow(lanes, 0, 10), 'start')).toBe('')
  })
})

describe('markerFrame', () => {
  const [lane] = buildLanes({ '1': path([10, 11, 12, 20, 21]) })

  it('leaves a frame that is inside a run alone', () => {
    expect(markerFrame(lane, 11)).toBe(11)
  })

  // a gap's atT is the END OF TRACK A. On track B's lane that is empty space before B starts — a
  // mark on a frame where the cell does not exist, which the run rects exist to make impossible.
  it('clamps a frame in the hole onto the nearer run edge', () => {
    expect(markerFrame(lane, 13)).toBe(12)
    expect(markerFrame(lane, 19)).toBe(20)
  })
  it('clamps a frame outside the lane entirely', () => {
    expect(markerFrame(lane, 0)).toBe(10)
    expect(markerFrame(lane, 99)).toBe(21)
  })
})

const issue = (over: Partial<TrackIssue>): TrackIssue => ({
  kind: 'gap', op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2],
  atT: 12, centroid: [0, 0], severity: 0.8, reason: 'r', ...over,
})

describe('issueMarkers', () => {
  const lanes = buildLanes({ '1': path([10, 11, 12]), '2': path([20, 21]) })
  const key = (i: TrackIssue) => `${i.kind}:${i.trackIds.join('+')}`

  // marking only the "first" track makes the other lane look innocent — the candidate is about the
  // relationship
  it('marks BOTH ends of a gap, each at its own end of the hole', () => {
    const m = issueMarkers([issue({})], lanes, key)
    expect(m).toHaveLength(2)
    expect(m.map(x => [x.track, x.t])).toEqual([['1', 12], ['2', 20]])
    expect(m[0].partners).toEqual(['2'])
  })

  it('skips a track that is not on screen', () => {
    const m = issueMarkers([issue({ trackIds: [1, 77] })], lanes, key)
    expect(m.map(x => x.track)).toEqual(['1'])
  })

  it('carries the key so ticking survives a refetch', () => {
    expect(issueMarkers([issue({})], lanes, key)[0].key).toBe('gap:1+2')
  })

  it('handles no issues', () => {
    expect(issueMarkers([], lanes, key)).toEqual([])
  })
})

describe('laneSeverity / candidateTracks', () => {
  it('keeps the worst severity per track', () => {
    expect(laneSeverity([
      issue({ trackIds: [1], severity: 0.3 }),
      issue({ trackIds: [1], severity: 0.9 }),
      issue({ trackIds: [1], severity: 0.5 }),
    ])).toEqual({ '1': 0.9 })
  })
  it('collects every named track', () => {
    expect([...candidateTracks([issue({ trackIds: [3, 4] }), issue({ trackIds: [4] })])])
      .toEqual(['3', '4'])
  })
})

describe('laneOverlap', () => {
  it('finds the shared frames', () => {
    const [a, b] = buildLanes({ '1': path([0, 1, 2, 3]), '2': path([2, 3, 4]) })
    expect(laneOverlap(a, b)).toEqual([{ t0: 2, t1: 3 }])
    expect(lanesOverlap(a, b)).toBe(true)
  })

  it('a joinable pair shares nothing', () => {
    const [a, b] = buildLanes({ '1': path([0, 1, 2]), '2': path([5, 6]) })
    expect(laneOverlap(a, b)).toEqual([])
    expect(lanesOverlap(a, b)).toBe(false)
  })

  // two tracks can span the same 30 frames and interleave perfectly — a joinable pair whose OUTER
  // EXTENTS overlap completely. Comparing extents instead of runs would refuse it.
  it('interleaved runs over the same extent do NOT overlap', () => {
    const [a, b] = buildLanes({
      '1': path([0, 1, 2, 3, 4, 20, 21, 22, 23, 24]),
      '2': path([10, 11, 12, 13, 14]),
    })
    expect(a.t0).toBeLessThan(b.t0)
    expect(a.t1).toBeGreaterThan(b.t1)
    expect(lanesOverlap(a, b)).toBe(false)
  })

  it('a single shared frame is an overlap', () => {
    const [a, b] = buildLanes({ '1': path([0, 1, 2]), '2': path([2, 3]) })
    expect(laneOverlap(a, b)).toEqual([{ t0: 2, t1: 2 }])
  })
})

describe('selectionOverlaps', () => {
  const lanes = buildLanes({ '1': path([0, 1, 2]), '2': path([2, 3]), '3': path([9, 10]) })

  it('names the offending pair, not just "the selection overlaps"', () => {
    const o = selectionOverlaps(lanes, new Set(['1', '2', '3']))
    expect(o).toHaveLength(1)
    expect(o[0]).toMatchObject({ a: '1', b: '2' })
  })
  it('is empty for a joinable selection', () => {
    expect(selectionOverlaps(lanes, new Set(['1', '3']))).toEqual([])
  })
  it('is empty for a single track', () => {
    expect(selectionOverlaps(lanes, new Set(['1']))).toEqual([])
  })
})

describe('frameToX / xToFrame', () => {
  // a frame is a BOX of width one: getting this wrong is invisible on a 300-frame track and makes
  // every single-frame detection vanish
  it('t1 + 1 is the right edge, so one frame has width', () => {
    const g = geom({ t0: 0, t1: 9, x0: 0, x1: 100 })
    expect(frameToX(g, 0)).toBe(0)
    expect(frameToX(g, 10)).toBe(100)
    expect(frameToX(g, 1) - frameToX(g, 0)).toBe(10)
  })

  it('round-trips through the frame it came from', () => {
    const g = geom({ t0: 0, t1: 9, x0: 0, x1: 100 })
    for (const t of [0, 3, 9]) expect(xToFrame(g, frameToX(g, t) + 0.5)).toBe(t)
  })

  it('clamps outside the axis', () => {
    const g = geom({ t0: 5, t1: 9 })
    expect(xToFrame(g, -50)).toBe(5)
    expect(xToFrame(g, 5000)).toBe(9)
  })

  it('survives a degenerate domain', () => {
    const g = geom({ t0: 4, t1: 4, x0: 0, x1: 100 })
    expect(frameToX(g, 4)).toBe(0)
    expect(frameToX(g, 5)).toBe(100)
    expect(xToFrame(g, 50)).toBe(4)
  })

  it('survives a zero-width plot area', () => {
    const g = geom({ x0: 10, x1: 10 })
    expect(xToFrame(g, 10)).toBe(g.t0)
  })
})

describe('lane geometry', () => {
  it('stacks lanes at the pitch', () => {
    const g = geom({ y0: 5, laneH: 12 })
    expect(laneY(g, 0)).toBe(5)
    expect(laneY(g, 3)).toBe(41)
  })
  it('maps y back to a lane', () => {
    const g = geom({ y0: 0, laneH: 10 })
    expect(laneAtY(g, 0, 3)).toBe(0)
    expect(laneAtY(g, 25, 3)).toBe(2)
  })
  it('is null outside the drawn lanes', () => {
    const g = geom({ y0: 0, laneH: 10 })
    expect(laneAtY(g, -1, 3)).toBeNull()
    expect(laneAtY(g, 30, 3)).toBeNull()
    expect(laneAtY(g, 5, 0)).toBeNull()
  })
})

describe('runRects', () => {
  it('one rect per run, in lane order', () => {
    const lanes = buildLanes({ '1': path([0, 1, 2, 8, 9]) })
    const r = runRects(lanes, geom({ t0: 0, t1: 9, x0: 0, x1: 100 }))
    expect(r).toHaveLength(2)
    expect(r[0]).toMatchObject({ track: '1', lane: 0, t0: 0, t1: 2, x: 0, w: 30 })
    expect(r[1]).toMatchObject({ t0: 8, t1: 9, x: 80, w: 20 })
  })

  // a single-frame run on a 400-frame image in a 700 px panel is 1.75 px wide — the `short`
  // candidates are exactly these, and sub-pixel slivers cannot be clicked
  it('widens a sliver to minW so it can be seen and clicked', () => {
    const lanes = buildLanes({ '1': path([200]) })
    const [r] = runRects(lanes, geom({ t0: 0, t1: 399, x0: 0, x1: 700 }), 3)
    expect(r.w).toBe(3)
  })

  it('positions each lane on its own row', () => {
    const lanes = buildLanes({ '1': path([0, 1]), '2': path([0, 1]) })
    const r = runRects(orderLanes(lanes, 'track'), geom({ laneH: 10, barH: 8 }))
    expect(r.map(x => x.y)).toEqual([0, 10])
    expect(r.every(x => x.h === 8)).toBe(true)
  })
})

describe('hitTest', () => {
  const lanes = orderLanes(buildLanes({ '1': path([0, 1, 2, 8, 9]), '2': path([0, 1]) }), 'track')
  const g = geom({ t0: 0, t1: 9, x0: 0, x1: 100, y0: 0, laneH: 10 })

  it('resolves a click to (track, frame)', () => {
    expect(hitTest(lanes, g, 15, 5)).toEqual({ lane: 0, track: '1', frame: 1, occupied: true })
  })

  // a click in a hole between two runs of the SAME lane is "this is the gap" — the gesture a
  // join-across-a-gap needs in P2
  it('reports a click in a hole as unoccupied', () => {
    expect(hitTest(lanes, g, 55, 5)).toMatchObject({ track: '1', frame: 5, occupied: false })
  })

  it('is null off the lanes and off the axis', () => {
    expect(hitTest(lanes, g, 15, 500)).toBeNull()
    expect(hitTest(lanes, g, -5, 5)).toBeNull()
  })
})

describe('readouts', () => {
  it('names the gaps when there are any', () => {
    const [lane] = buildLanes({ '7': path([0, 1, 5]) })
    expect(laneSummary(lane)).toBe('Track 7: frames 0–5, 3 detections, 1 gap')
  })
  it('stays quiet about gaps when there are none', () => {
    const [lane] = buildLanes({ '7': path([0, 1, 2]) })
    expect(laneSummary(lane)).toBe('Track 7: frames 0–2, 3 detections')
  })
  it('pluralises', () => {
    const [lane] = buildLanes({ '7': path([0, 5, 9]) })
    expect(laneSummary(lane)).toContain('2 gaps')
  })

  it('exports one CSV row per run', () => {
    const lanes = buildLanes({ '7': path([0, 1, 5]) })
    const rows = schemeCsvRows(lanes)
    expect(rows).toHaveLength(2)
    expect(rows[0]).toMatchObject({ track: '7', run: 0, firstFrame: 0, lastFrame: 1, frames: 2 })
    expect(rows[1]).toMatchObject({ run: 1, firstFrame: 5, lastFrame: 5, frames: 1, trackGaps: 1 })
  })
})

describe('frameTicks', () => {
  it('picks a round interval', () => {
    expect(frameTicks(0, 100, 5)).toEqual([0, 20, 40, 60, 80, 100])
  })
  it('always starts at the domain start', () => {
    expect(frameTicks(3, 47, 4)[0]).toBe(3)
  })
  it('never runs past the domain end', () => {
    expect(Math.max(...frameTicks(0, 37, 8))).toBeLessThanOrEqual(37)
  })
  it('survives a degenerate span', () => {
    expect(frameTicks(5, 5)).toEqual([5])
    expect(frameTicks(9, 2)).toEqual([9])
  })
  it('scales down to a short movie', () => {
    expect(frameTicks(0, 6, 8)).toEqual([0, 1, 2, 3, 4, 5, 6])
  })
})

const key = (i: TrackIssue) => `${i.kind}:${i.trackIds.join('+')}`

describe('joinPairs', () => {
  it('keeps only the ops that are actually a join', () => {
    const p = joinPairs([
      issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2], severity: 0.4 }),
      issue({ kind: 'jump', op: { op: 'track.split', trackId: 5, atT: 3 }, trackIds: [5] }),
      issue({ kind: 'short', op: { op: 'track.remove', trackIds: [9] }, trackIds: [9] }),
    ], key)
    expect(p.map(x => [x.a, x.b])).toEqual([['1', '2']])
  })

  it('ranks worst first', () => {
    const p = joinPairs([
      issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2], severity: 0.2 }),
      issue({ op: { op: 'track.join', trackIds: [3, 4] }, trackIds: [3, 4], severity: 0.9 }),
    ], key)
    expect(p.map(x => x.a)).toEqual(['3', '1'])
  })

  // the detector can emit the same two tracks from two signatures (a gap AND a duplicate); the user
  // should see one row, not two identical ones
  it('deduplicates a pair regardless of which way round it is named', () => {
    const p = joinPairs([
      issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2] }),
      issue({ kind: 'duplicate', op: { op: 'track.join', trackIds: [2, 1] }, trackIds: [2, 1] }),
    ], key)
    expect(p).toHaveLength(1)
  })

  it('ignores a join naming anything other than two tracks', () => {
    expect(joinPairs([issue({ op: { op: 'track.join', trackIds: [1] }, trackIds: [1] })], key)).toEqual([])
  })
})

describe('orderLanesByPair', () => {
  const lanes = buildLanes({
    '1': path([0, 1, 2]), '2': path([6, 7]), '3': path([0, 1]), '4': path([9]), '5': path([3, 4]),
  })

  // every other order scatters a pair across the panel, so the one comparison this surface exists for
  // needs scrolling to make
  it('puts the two halves of a pair on neighbouring rows, worst pair first', () => {
    const pairs = joinPairs([
      issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2], severity: 0.3 }),
      issue({ op: { op: 'track.join', trackIds: [3, 4] }, trackIds: [3, 4], severity: 0.8 }),
    ], key)
    expect(orderLanesByPair(lanes, pairs).map(l => l.track)).toEqual(['3', '4', '1', '2', '5'])
  })

  it('is a ranking, not a filter — unpaired lanes follow', () => {
    expect(orderLanesByPair(lanes, []).map(l => l.track))
      .toEqual(orderLanes(lanes, 'start').map(l => l.track))
  })

  it('never repeats a lane that appears in two pairs', () => {
    const pairs = joinPairs([
      issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2], severity: 0.9 }),
      issue({ op: { op: 'track.join', trackIds: [2, 5] }, trackIds: [2, 5], severity: 0.5 }),
    ], key)
    const out = orderLanesByPair(lanes, pairs).map(l => l.track)
    expect(out).toHaveLength(5)
    expect(new Set(out).size).toBe(5)
  })

  it('skips a pair whose tracks are not on screen', () => {
    const pairs = joinPairs([issue({ op: { op: 'track.join', trackIds: [77, 88] }, trackIds: [77, 88] })], key)
    expect(orderLanesByPair(lanes, pairs)).toHaveLength(5)
  })
})

describe('joinLinks', () => {
  it('spans A last frame → B first frame, on their two rows', () => {
    const lanes = orderLanes(buildLanes({ '1': path([0, 1, 2]), '2': path([6, 7]) }), 'track')
    const pairs = joinPairs([issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2] })], key)
    expect(joinLinks(lanes, pairs)).toEqual([
      { key: 'gap:1+2', a: '1', b: '2', laneA: 0, laneB: 1, fromT: 2, toT: 6, blocked: false },
    ])
  })

  // the engine refuses a join for tracks that share a frame — the link says so before the button does
  it('flags a pair the engine would refuse', () => {
    const lanes = orderLanes(buildLanes({ '1': path([0, 1, 2]), '2': path([2, 3]) }), 'track')
    const pairs = joinPairs([issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2] })], key)
    expect(joinLinks(lanes, pairs)[0].blocked).toBe(true)
  })

  it('drops a link whose other half is off screen', () => {
    const lanes = buildLanes({ '1': path([0, 1]) })
    const pairs = joinPairs([issue({ op: { op: 'track.join', trackIds: [1, 2] }, trackIds: [1, 2] })], key)
    expect(joinLinks(lanes, pairs)).toEqual([])
  })
})

describe('sharedFrames', () => {
  it('lists the frames the engine would refuse on', () => {
    const [a, b] = buildLanes({ '1': path([0, 1, 2, 3]), '2': path([2, 3, 4]) })
    expect(sharedFrames(a, b)).toEqual([2, 3])
  })

  // the exact rule and the range rule differ here, and the engine takes the exact one: 395 pairs on
  // the reference image are joinable despite fully overlapping ranges
  it('is empty for interleaved runs, however far their ranges overlap', () => {
    const [a, b] = buildLanes({ '1': path([0, 1, 2, 3, 4, 20, 21]), '2': path([10, 11, 12]) })
    expect(sharedFrames(a, b)).toEqual([])
  })

  it('is empty for a clean gap', () => {
    const [a, b] = buildLanes({ '1': path([0, 1]), '2': path([5, 6]) })
    expect(sharedFrames(a, b)).toEqual([])
  })
})
