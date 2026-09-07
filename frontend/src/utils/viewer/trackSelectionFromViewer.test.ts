import { describe, it, expect } from 'vitest'
import { readSelectionMessage } from './trackSelectionFromViewer'
import type { TrackSelection } from '../../lib/trackCorrection'

const sel = (patch: Partial<TrackSelection>): TrackSelection => ({
  valueName: 'default', labels: [], tracks: [], nLabels: 0, nUntracked: 0, ...patch,
})

describe('readSelectionMessage', () => {
  it('empty picks: no tracks, no cells', () => {
    // The empty case is the common one — nothing drawn, or a background click. The rail should
    // say WHY nothing came back rather than looking like a silent no-op.
    expect(readSelectionMessage(sel({}))).toBe('Read no tracks — 0 cells picked.')
  })

  it('all cells untracked — points.add territory', () => {
    // The "untracked" figure is the actionable one for points.add and matters for the
    // interpretation of Show ("nothing to highlight because none of your cells is tracked").
    expect(readSelectionMessage(sel({ nLabels: 3, nUntracked: 3 })))
      .toBe('Read no tracks — 3 cells picked (+3 untracked).')
  })

  it('one track resolved from picks — id is in the message', () => {
    // Whole point of the diagnostic — the ids are what a repro pastes back.
    const s = sel({ nLabels: 5, tracks: [{ track: 17, nCells: 5 }] })
    expect(readSelectionMessage(s)).toBe('Read 1 track (17) from 5 cells.')
  })

  it('several tracks: shows every id when short, plural noun', () => {
    const s = sel({ nLabels: 8, tracks: [
      { track: 12, nCells: 3 }, { track: 40, nCells: 3 }, { track: 91, nCells: 2 },
    ] })
    expect(readSelectionMessage(s)).toBe('Read 3 tracks (12, 40, 91) from 8 cells.')
  })

  it('caps preview at six ids with an ellipsis — the rest live in the store', () => {
    // Keeps the rail tidy for a drawn rect that touches many tracks; the log is a summary, the
    // full list is what the cockpit store holds.
    const s = sel({ nLabels: 30, tracks: [1, 2, 3, 4, 5, 6, 7, 8].map(t => ({ track: t, nCells: 1 })) })
    expect(readSelectionMessage(s)).toBe('Read 8 tracks (1, 2, 3, 4, 5, 6, …) from 30 cells.')
  })

  it('mixed tracked + untracked cells surface both counts', () => {
    const s = sel({ nLabels: 10, nUntracked: 2,
                    tracks: [{ track: 4, nCells: 5 }, { track: 9, nCells: 3 }] })
    expect(readSelectionMessage(s)).toBe('Read 2 tracks (4, 9) from 10 cells (+2 untracked).')
  })
})
