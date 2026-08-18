import { describe, it, expect } from 'vitest'
import {
  opLabel, opDescription, issueKey, visibleIssues, worklistSummary, undoLast,
  KIND_LABEL, type TrackIssue, type TrackOp, type IssuesResponse,
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
