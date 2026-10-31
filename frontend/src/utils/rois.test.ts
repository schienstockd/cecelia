import { describe, expect, it } from 'vitest'
import { claudeLabelText, disagrees, filterRois, roiFocusTarget, type RoiRow } from './rois'

const row = (id: string, rank: number, status: RoiRow['status']): RoiRow => ({
  id, rank, status, score: 1, t0: 10, t1: 21,
  box: { x0: 100, x1: 160, y0: 40, y1: 60, z0: 2, z1: 9 },
  centroid: { t: 15, z: 5.4, y: 50, x: 130 }, nObjects: 3, objectLabels: [1, 2, 3], review: null,
})

describe('filterRois', () => {
  it('keeps rank order and filters by status', () => {
    const rows = [row('b', 2, 'accepted'), row('a', 1, 'pending'), row('c', 3, 'pending')]
    expect(filterRois(rows, 'all').map(r => r.id)).toEqual(['a', 'b', 'c'])
    expect(filterRois(rows, 'pending').map(r => r.id)).toEqual(['a', 'c'])
    expect(rows.map(r => r.id)).toEqual(['b', 'a', 'c'])   // input not mutated
  })
})

describe('roiFocusTarget', () => {
  it('centres on the box, mid-window t, centroid z, and fits the box', () => {
    expect(roiFocusTarget(row('a', 1, 'pending'))).toEqual({ t: 16, cx: 130, cy: 50, cz: 5, halfWpx: 30, halfHpx: 10 })
  })
  it('never hands a zero half-width to the fit', () => {
    const r = { ...row('a', 1, 'pending'), box: { x0: 5, x1: 5, y0: 7, y1: 7, z0: 0, z1: 0 } }
    const f = roiFocusTarget(r)
    expect(f.halfWpx).toBe(1)
    expect(f.halfHpx).toBe(1)
  })
})

describe('Claude label text + disagreement', () => {
  const cl = { label: 'free debris', confidence: 0.75, reason: 'r' }
  it('formats label and confidence', () => {
    expect(claudeLabelText({ claudeLabel: cl })).toBe('free debris 0.8')
    expect(claudeLabelText(null)).toBe('')
  })
  it('flags only when both labelled and different', () => {
    expect(disagrees({ claudeLabel: cl, userLabel: 'engulfment' })).toBe(true)
    expect(disagrees({ claudeLabel: cl, userLabel: 'free debris' })).toBe(false)
    expect(disagrees({ claudeLabel: cl })).toBe(false)
  })
})
