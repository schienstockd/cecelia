import { describe, expect, it } from 'vitest'
import { isTimeSeries } from './imageState'
import type { CciaImage } from '../stores/project'

const img = (extra: Partial<CciaImage> = {}): CciaImage => ({ uid: 'u', name: 'n', status: 'done', ...extra })

// One T test for the image table's columns, the guide prereq and the task gate (taskGating.imageAxes).
describe('isTimeSeries', () => {
  it('several frames', () => {
    expect(isTimeSeries(img({ sizeT: 12 }))).toBe(true)
    expect(isTimeSeries(img({ sizeT: 1 }))).toBe(false)
    expect(isTimeSeries(img())).toBe(false)
  })
  it('an older import with only a frame interval is still a timelapse', () => {
    expect(isTimeSeries(img({ sizeT: null, timeIncrement: 30 }))).toBe(true)
    expect(isTimeSeries(img({ sizeT: null, timeIncrement: 0 }))).toBe(false)
  })
})
