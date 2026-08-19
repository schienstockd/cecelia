import { describe, it, expect } from 'vitest'
import {
  opLabel, opDescription, issueKey, visibleIssues, worklistSummary, undoLast, worklistCsvRows,
  KIND_LABEL, trackRows, tracksOverlap, joinOrder, manualActions,
  buildRemoveOp, buildJoinOp, buildSplitOp, buildUntrackOp,
  thresholdQuery, thresholdsChanged, THRESHOLD_FIELDS,
  selectionSummary, selectedTracks, parseTrackIds, suggestedOps,
  type TrackIssue, type TrackOp, type IssuesResponse, type TrackRow,
} from './trackCorrection'

const issue = (over: Partial<TrackIssue> = {}): TrackIssue => ({
  kind: 'gap', op: { op: 'track.join', trackIds: [1, 2] },
  trackIds: [1, 2], atT: 5, centroid: [0, 0], severity: 1, reason: 'because', ...over,
})

describe('opLabel', () => {
  it('is a WORD for each op — join/split are deliberately not glyphs', () => {
    expect(opLabel({ op: 'track.join', trackIds: [1, 2] })).toBe('Join')
    expect(opLabel({ op: 'track.split', trackId: 1, atT: 2 })).toBe('Split')
    expect(opLabel({ op: 'track.remove', trackIds: [3] })).toBe('Remove')
    expect(opLabel({ op: 'points.remove', labels: [1] })).toBe('Untrack')
    expect(opLabel({ op: 'points.add', labels: [1] })).toBe('Add')
  })
  it('never returns an empty label', () => {
    expect(opLabel({ op: 'nonsense' } as unknown as TrackOp)).toBeTruthy()
  })
})

describe('opDescription', () => {
  it('names the tracks the op will touch, in the direction it will touch them', () => {
    // a join CONSUMES the second — the description has to say which one disappears
    expect(opDescription({ op: 'track.join', trackIds: [7, 9] })).toBe('Join track 9 into 7')
    expect(opDescription({ op: 'track.split', trackId: 4, atT: 12 })).toBe('Split track 4 at t=12')
    expect(opDescription({ op: 'track.remove', trackIds: [5] })).toBe('Remove track 5')
    expect(opDescription({ op: 'track.remove', trackIds: [5, 6] })).toBe('Remove tracks 5, 6')
  })
  it('degrades to a readable string when a field is missing', () => {
    for (const o of [{ op: 'track.join' }, { op: 'track.split' }, { op: 'track.remove' }] as TrackOp[]) {
      expect(opDescription(o)).not.toMatch(/undefined|null|NaN/)
    }
  })
})

describe('issueKey', () => {
  it('is stable across a re-rank — keyed on the OP, not severity or reason', () => {
    const a = issue({ severity: 9, reason: 'one wording' })
    const b = issue({ severity: 1, reason: 'quite another' })
    expect(issueKey(a)).toBe(issueKey(b))
  })
  it('separates different ops on the same tracks', () => {
    expect(issueKey(issue({ op: { op: 'track.join', trackIds: [1, 2] } })))
      .not.toBe(issueKey(issue({ op: { op: 'track.remove', trackIds: [1, 2] } })))
  })
  it('separates the same op at different timepoints', () => {
    const at = (t: number) => issueKey(issue({ op: { op: 'track.split', trackId: 1, atT: t } }))
    expect(at(3)).not.toBe(at(9))
  })
})

describe('visibleIssues', () => {
  const gap = issue({ kind: 'gap', op: { op: 'track.join', trackIds: [1, 2] } })
  const jump = issue({ kind: 'jump', op: { op: 'track.split', trackId: 3, atT: 4 } })
  const dup = issue({ kind: 'duplicate', op: { op: 'track.remove', trackIds: [9] } })
  const all = [gap, jump, dup]

  it('shows everything with no filter', () => {
    expect(visibleIssues(all)).toHaveLength(3)
  })
  it('filters by kind', () => {
    expect(visibleIssues(all, { kinds: ['gap'] }).map(i => i.kind)).toEqual(['gap'])
    expect(visibleIssues(all, { kinds: ['gap', 'duplicate'] })).toHaveLength(2)
  })
  it('treats an empty kind list as no filter, not as "nothing"', () => {
    // a chip picker with everything deselected should not look like a clean bill of health
    expect(visibleIssues(all, { kinds: [] })).toHaveLength(3)
  })
  it('removes a row whose op is already QUEUED — applying it twice would fail the run', () => {
    const left = visibleIssues(all, { pending: [gap.op] })
    expect(left.map(i => i.kind)).toEqual(['jump', 'duplicate'])
  })
  it('removes a row the user skipped', () => {
    expect(visibleIssues(all, { skipped: [issueKey(jump)] }).map(i => i.kind))
      .toEqual(['gap', 'duplicate'])
  })
  it('combines all three', () => {
    expect(visibleIssues(all, { kinds: ['gap', 'jump'], pending: [gap.op], skipped: [issueKey(jump)] }))
      .toEqual([])
  })
})

describe('worklistSummary', () => {
  const base = (over: Partial<IssuesResponse> = {}): IssuesResponse =>
    ({ valueName: 'v', tracked: true, nTracks: 374, total: 31,
       issues: Array(31).fill(issue()), paths: {}, ...over })

  it('reports the DETECTOR total and the track count', () => {
    expect(worklistSummary(base(), 0)).toBe('31 of 374 tracks need review')
  })
  it('says when the response was capped — the page size is not the finding', () => {
    expect(worklistSummary(base({ total: 900, issues: Array(100).fill(issue()) }), 0))
      .toContain('showing 100')
  })
  it('mentions the queued count only when there is one', () => {
    expect(worklistSummary(base(), 0)).not.toContain('queued')
    expect(worklistSummary(base(), 3)).toContain('3 queued')
  })
  it('is a clean bill of health at zero, not an empty string', () => {
    expect(worklistSummary(base({ total: 0, issues: [] }), 0)).toMatch(/Nothing to review/)
  })
  it('says so when the image is not tracked', () => {
    expect(worklistSummary(base({ tracked: false }), 0)).toMatch(/run tracking first/i)
  })
  it('is empty before anything has loaded', () => {
    expect(worklistSummary(null, 0)).toBe('')
  })
})

describe('undoLast', () => {
  const ops: TrackOp[] = [
    { op: 'track.split', trackId: 1, atT: 2 },
    { op: 'track.join', trackIds: [3, 4] },
  ]
  it('drops only the LAST op — ops apply in order and each sees the previous result', () => {
    expect(undoLast(ops)).toEqual([ops[0]])
  })
  it('is a no-op on an empty stack', () => {
    expect(undoLast([])).toEqual([])
  })
  it('does not mutate the input', () => {
    const copy = [...ops]
    undoLast(ops)
    expect(ops).toEqual(copy)
  })
})

describe('KIND_LABEL', () => {
  it('covers every signature the detector emits', () => {
    for (const k of ['gap', 'jump', 'short', 'duplicate']) expect(KIND_LABEL[k]).toBeTruthy()
  })
})

describe('worklistCsvRows', () => {
  it('records the decision, not just the scan', () => {
    const open = issue()
    expect(worklistCsvRows([open], [], [])[0].decision).toBe('open')
    expect(worklistCsvRows([open], [open.op], [])[0].decision).toBe('queued')
    expect(worklistCsvRows([open], [], [issueKey(open)])[0].decision).toBe('dismissed')
  })

  it('flattens the candidate into readable columns', () => {
    expect(worklistCsvRows([issue({ centroid: [10, 20, 0], severity: 2.5, atT: 4 })], [], [])[0])
      .toMatchObject({ kind: 'gap', tracks: '1 2', atT: 4, x: 10, y: 20, severity: 2.5 })
  })

  it('leaves a missing z blank rather than undefined', () => {
    expect(worklistCsvRows([issue({ centroid: [1, 2] })], [], [])[0].z).toBe('')
  })

  it('is empty for an empty worklist', () => {
    expect(worklistCsvRows([], [], [])).toEqual([])
  })
})

// ── Authoring an op the detector did not suggest (P4d) ─────────────────────────
const row = (track: number, t0: number, t1: number): TrackRow =>
  ({ track, nFrames: t1 - t0 + 1, t0, t1, netDistance: 10 })

describe('trackRows', () => {
  it('summarises each track, longest first', () => {
    const rows = trackRows({
      '7': { t: [0, 1], x: [0, 3], y: [0, 4] },
      '2': { t: [5, 6, 7], x: [0, 1, 2], y: [0, 0, 0] },
    })
    expect(rows.map(r => r.track)).toEqual([2, 7])
    expect(rows[0]).toMatchObject({ nFrames: 3, t0: 5, t1: 7 })
    expect(rows[1].netDistance).toBeCloseTo(5, 10)      // 3-4-5 triangle
  })

  it('skips a track with no timepoints and handles an empty map', () => {
    expect(trackRows({ '1': { t: [], x: [], y: [] } })).toEqual([])
    expect(trackRows({})).toEqual([])
  })
})

describe('tracksOverlap / joinOrder', () => {
  it('detects a shared frame range', () => {
    expect(tracksOverlap(row(1, 0, 5), row(2, 5, 9))).toBe(true)     // touching at 5
    expect(tracksOverlap(row(1, 0, 4), row(2, 5, 9))).toBe(false)
  })

  it('orders a join so the EARLIER track is A', () => {
    // the engine folds B into A, so A must be the one that comes first in time or the joined track
    // reads backwards
    expect(joinOrder(row(9, 10, 20), row(3, 0, 5))).toEqual([3, 9])
    expect(joinOrder(row(3, 0, 5), row(9, 10, 20))).toEqual([3, 9])
  })
})

describe('manualActions', () => {
  const rows = [row(1, 0, 5), row(2, 6, 9), row(3, 4, 8)]
  const act = (picked: number[], splitAt: number | null = null) =>
    Object.fromEntries(manualActions(picked, rows, splitAt).map(a => [a.key, a]))

  it('offers Join for two non-overlapping tracks, ordered by time', () => {
    const j = act([2, 1]).join
    expect(j.blocked).toBeNull()
    expect(j.op).toEqual({ op: 'track.join', trackIds: [1, 2] })
  })

  it('BLOCKS a join whose tracks share frames, and says which', () => {
    // the engine refuses this; learning it only when the task fails after Apply is the bad trade
    const j = act([1, 3]).join
    expect(j.op).toBeNull()
    expect(j.blocked).toMatch(/frame 4, 5/)
  })

  // The engine's rule is a SET INTERSECTION of frames (`_op_join`), not a range test. Without an
  // exact answer this falls back to ranges, which is conservative — and measurably so: on
  // zolIMa/fXgbTl it refuses 395 pairs the engine would accept. A caller holding the frames passes
  // `sharedFrames` and gets the engine's own verdict.
  it('takes an exact shared-frame answer over the conservative range test', () => {
    const rows = [
      { track: 1, nFrames: 7, t0: 0, t1: 21, netDistance: 0 },
      { track: 2, nFrames: 3, t0: 10, t1: 12, netDistance: 0 },
    ]
    // ranges overlap completely, so the fallback refuses
    expect(manualActions([1, 2], rows, null)[0].blocked).toMatch(/Both have a cell/)
    // …but the runs interleave and share no frame, so the engine would allow it
    const exact = manualActions([1, 2], rows, null, () => [])[0]
    expect(exact.blocked).toBeNull()
    expect(exact.op).toEqual({ op: 'track.join', trackIds: [1, 2] })
  })

  it('names the shared frames the exact answer reports, truncating a long list', () => {
    const rows = [
      { track: 1, nFrames: 9, t0: 0, t1: 8, netDistance: 0 },
      { track: 2, nFrames: 9, t0: 0, t1: 8, netDistance: 0 },
    ]
    const j = manualActions([1, 2], rows, null, () => [1, 2, 3, 4, 5, 6])[0]
    expect(j.blocked).toBe('Both have a cell at frame 1, 2, 3, 4… — they are not one cell')
  })

  it('needs exactly two for a join — never silently uses the first two', () => {
    expect(act([1]).join.blocked).toMatch(/exactly two/)
    expect(act([1, 2, 3]).join.blocked).toMatch(/exactly two/)
  })

  it('splits one track at a frame strictly inside it', () => {
    expect(act([1], 3).split.op).toEqual({ op: 'track.split', trackId: 1, atT: 3 })
    // the first frame would leave an empty first half, which the engine rejects
    expect(act([1], 0).split.blocked).toMatch(/inside 0–5/)
    expect(act([1], 6).split.blocked).toMatch(/inside 0–5/)
    expect(act([1], null).split.blocked).toMatch(/Set the frame/)
    expect(act([1, 2], 3).split.blocked).toMatch(/one track/)
  })

  it('removes any number of picked tracks', () => {
    expect(act([1, 3]).remove.op).toEqual({ op: 'track.remove', trackIds: [1, 3] })
    expect(act([]).remove.blocked).toMatch(/at least one/)
  })

  it('always returns every action, with a reason when blocked', () => {
    // a button that vanishes teaches nothing — "why can't I join these" is the question the surface
    // exists to answer
    const all = manualActions([], rows, null)
    expect(all.map(a => a.key)).toEqual(['join', 'split', 'remove'])
    expect(all.every(a => a.op === null && a.blocked)).toBe(true)
  })

  it('ignores a picked id that is not in the rows', () => {
    expect(act([1, 999]).join.blocked).toMatch(/exactly two/)
  })
})

describe('hand-built ops are the same shape the engine accepts', () => {
  it('uses the keys apply_track_op! reads', () => {
    // `trackIds` for remove/join, `trackId`+`atT` for split, `labels` for points.* — mismatching any
    // of these fails only at Apply, inside the task
    expect(buildRemoveOp([4, 5])).toEqual({ op: 'track.remove', trackIds: [4, 5] })
    expect(buildJoinOp(1, 2)).toEqual({ op: 'track.join', trackIds: [1, 2] })
    expect(buildSplitOp(7, 12)).toEqual({ op: 'track.split', trackId: 7, atT: 12 })
    expect(buildUntrackOp([9])).toEqual({ op: 'points.remove', labels: [9] })
  })

  it('every built op carries a label and a description, like a suggested one', () => {
    for (const op of [buildRemoveOp([1]), buildJoinOp(1, 2), buildSplitOp(1, 2), buildUntrackOp([1])]) {
      expect(opLabel(op)).toBeTruthy()
      expect(opDescription(op).length).toBeGreaterThan(5)
    }
  })
})

// ── Detector thresholds (P4e) ─────────────────────────────────────────────────
describe('thresholdQuery', () => {
  const defaults = { gapFrames: 3, gapSteps: 3, jumpFactor: 4, jumpQuantile: 0.99, minLen: 5 }

  it('sends only what the user moved', () => {
    // an untouched panel must take the SERVER's defaults — duplicating them in TS is how the two
    // drift apart
    expect(thresholdQuery({ ...defaults }, defaults)).toBe('')
    expect(thresholdQuery({}, defaults)).toBe('')
    expect(thresholdQuery({ gapSteps: 5 }, defaults)).toBe('&gapSteps=5')
  })

  it('sends several, in a stable order', () => {
    expect(thresholdQuery({ minLen: 8, gapFrames: 1 }, defaults)).toBe('&gapFrames=1&minLen=8')
  })

  it('knows whether anything changed', () => {
    expect(thresholdsChanged({}, defaults)).toBe(false)
    expect(thresholdsChanged({ jumpFactor: 2 }, defaults)).toBe(true)
  })

  it('every field is settable and explained', () => {
    for (const f of THRESHOLD_FIELDS) {
      expect(f.label).toBeTruthy()
      expect(f.tip.length).toBeGreaterThan(20)
      expect(f.step).toBeGreaterThan(0)
    }
  })
})

describe('the napari bridge', () => {
  const sel = { valueName: 'memTom', labels: [1, 2, 3, 4], nLabels: 4, nUntracked: 1,
                tracks: [{ track: 7, nCells: 2 }, { track: 9, nCells: 1 }] }

  it('says what was drawn and what it resolved to, untracked cells separately', () => {
    // "4 cells, 2 tracks" would hide that one of them belongs to no track — which is exactly the
    // cell points.add exists for
    expect(selectionSummary(sel)).toBe('4 cells · 2 tracks · 1 untracked')
    expect(selectionSummary({ ...sel, nUntracked: 0 })).toBe('4 cells · 2 tracks')
    expect(selectionSummary({ ...sel, nLabels: 1, labels: [1], tracks: [{ track: 7, nCells: 1 }],
                              nUntracked: 0 })).toBe('1 cell · 1 track')
  })

  it('is empty when nothing is drawn', () => {
    expect(selectionSummary(null)).toBe('')
    expect(selectionSummary({ ...sel, nLabels: 0, labels: [], tracks: [] })).toBe('')
  })

  it('keeps the server order — most cells inside the region first', () => {
    // so preselecting the top two makes "draw around the break, hit Join" do the obvious thing
    expect(selectedTracks(sel)).toEqual([7, 9])
    expect(selectedTracks(null)).toEqual([])
  })
})

describe('parseTrackIds', () => {
  it('accepts the separators someone actually types', () => {
    expect(parseTrackIds('12, 40 91;3')).toEqual([12, 40, 91, 3])
  })

  it('drops nonsense and duplicates rather than querying for them', () => {
    expect(parseTrackIds('12 12 abc -4 0 3.5')).toEqual([12])
    expect(parseTrackIds('')).toEqual([])
    expect(parseTrackIds('   ')).toEqual([])
  })
})

describe('suggestedOps', () => {
  const a = issue({ trackIds: [1, 2] })
  const b = issue({ kind: 'jump', op: { op: 'track.split', trackId: 7, atT: 5 }, trackIds: [7] })

  it('returns each ticked candidate\'s OWN op, in tick order', () => {
    // the one-click path: removing the per-row buttons left the pre-picked fix unreachable
    expect(suggestedOps([issueKey(b), issueKey(a)], [a, b])).toEqual([b.op, a.op])
  })

  it('ignores a key with no candidate behind it', () => {
    expect(suggestedOps(['gone'], [a])).toEqual([])
    expect(suggestedOps([], [a])).toEqual([])
  })
})
