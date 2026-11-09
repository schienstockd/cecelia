import { describe, it, expect } from 'vitest'
import { roiRecordSelection, roiRecordBlock, roiCheckedKey, roiPadFrames, ROI_PAD_MAX, roiFigureSpec, ROI_MAX_DEFAULT, ROI_PAD_FRAMES } from './roiMovie'
import type { RoiRow } from './rois'
import type { FigureSpec } from './figureSpec'

const row = (id: string, rank: number, status: RoiRow['status'] = 'pending'): RoiRow => ({
  id, rank, score: 1, t0: 10, t1: 20, status, review: null, nObjects: 3, objectLabels: [],
  box: { x0: 100, x1: 139, y0: 50, y1: 69, z0: 4, z1: 8 }, centroid: { t: 15, z: 6, y: 60, x: 120 },
})

describe('roiRecordSelection', () => {
  it('takes what the ROI list shows: its status filter, rank order, capped', () => {
    const rows = [row('c', 3), row('a', 1, 'accepted'), row('b', 2), row('d', 4)]
    expect(roiRecordSelection(rows, 'all', 2).map(r => r.id)).toEqual(['a', 'b'])
    expect(roiRecordSelection(rows, 'pending', 10).map(r => r.id)).toEqual(['b', 'c', 'd'])
  })
  it('a blank or nonsense cap falls back to the default', () => {
    const rows = Array.from({ length: 30 }, (_, i) => row(`r${i}`, i + 1))
    expect(roiRecordSelection(rows, 'all', 0)).toHaveLength(ROI_MAX_DEFAULT)
    expect(roiRecordSelection(rows, 'all', Number.NaN)).toHaveLength(ROI_MAX_DEFAULT)
  })
  it('ticked ROIs win over the cap, in list order; a tick the filter hides does not count', () => {
    const rows = Array.from({ length: 30 }, (_, i) => row(`r${i}`, i + 1, i === 3 ? 'rejected' : 'pending'))
    expect(roiRecordSelection(rows, 'all', 2, ['r20', 'r1', 'r15']).map(r => r.id)).toEqual(['r1', 'r15', 'r20'])
    expect(roiRecordSelection(rows, 'pending', 2, ['r3', 'r5']).map(r => r.id)).toEqual(['r5'])
    expect(roiRecordSelection(rows, 'all', 2, ['gone'])).toEqual([])
  })
})

describe('roiCheckedKey', () => {
  it('keeps one image\'s sets apart', () => {
    expect(roiCheckedKey('img', 'debris')).not.toBe(roiCheckedKey('img', 'cells'))
  })
})

describe('roiRecordBlock', () => {
  it('carries the ids in order; a sheet of one ROI is just its movie', () => {
    expect(roiRecordBlock('debris', [row('b', 2), row('a', 1)], 'both'))
      .toEqual({ suffix: 'debris', ids: ['b', 'a'], sheet: true, sheetOnly: false, padFrames: ROI_PAD_FRAMES })
    expect(roiRecordBlock('debris', [row('a', 1)], 'both').sheet).toBe(false)
    // sheet only, unless there is one ROI — then it is just its movie
    expect(roiRecordBlock('debris', [row('b', 2), row('a', 1)], 'sheet')).toMatchObject({ sheet: true, sheetOnly: true })
    expect(roiRecordBlock('debris', [row('a', 1)], 'sheet')).toMatchObject({ sheet: false, sheetOnly: false })
    expect(roiRecordBlock('debris', [row('b', 2), row('a', 1)], 'movies')).toMatchObject({ sheet: false, sheetOnly: false })
  })
  it('the pad is whole frames in 0…ROI_PAD_MAX; blank is the default', () => {
    expect(roiRecordBlock('d', [row('a', 1)], 'movies', 10).padFrames).toBe(10)
    expect(roiPadFrames(-3)).toBe(0)
    expect(roiPadFrames(1e6)).toBe(ROI_PAD_MAX)
    expect(roiPadFrames(null)).toBe(ROI_PAD_FRAMES)
    expect(roiPadFrames(Number.NaN)).toBe(ROI_PAD_FRAMES)
  })
})

describe('roiFigureSpec', () => {
  it('frames the box with context: centre, twice its side, its planes, frames from its window', () => {
    const base = { kind: 'sequence', center: [0, 0], sizePx: 512, z: 0, zHalf: 0, frames: [0],
                   rows: [], display: { ranges: {} }, layout: 'grid' } as FigureSpec
    const s = roiFigureSpec(base, row('a', 1), 5)
    expect(s.center).toEqual([60, 120])
    expect(s.sizePx).toBe(80)                        // context: twice the 40 px side
    expect([s.z, s.zHalf]).toEqual([6, 2])
    expect(s.frames).toEqual([10, 15, 20])
    expect(s.layout).toBe('grid')                    // the view's layout and display carry over
  })
  it('a pad widens the frames, inside the movie', () => {
    const base = { kind: 'sequence', center: [0, 0], sizePx: 512, z: 0, zHalf: 0, frames: [0],
                   rows: [], display: { ranges: {} } } as FigureSpec
    expect(roiFigureSpec(base, row('a', 1), 5, 5).frames).toEqual([5, 10, 15, 20, 25])
    expect(roiFigureSpec(base, row('a', 1), 5, 20, 24).frames[0]).toBe(0)
    expect(roiFigureSpec(base, row('a', 1), 5, 20, 24).frames.at(-1)).toBeLessThanOrEqual(23)
  })
})
