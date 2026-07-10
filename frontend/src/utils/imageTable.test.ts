import { describe, it, expect } from 'vitest'
import { timelapseDuration, imageTableCsvRows } from './imageTable'
import type { CciaImage } from '../stores/project'

const img = (o: Partial<CciaImage>): CciaImage => ({ uid: 'u', name: 'n', kind: 'live', status: 'done', ...o } as CciaImage)

describe('timelapseDuration', () => {
  it('is empty when not a timelapse or the interval is unknown', () => {
    expect(timelapseDuration(1, 30, 'second')).toBe('')     // single frame
    expect(timelapseDuration(60, null, 'second')).toBe('')  // no interval
    expect(timelapseDuration(null, 30, 'second')).toBe('')
  })
  it('spans first→last frame = (frames-1) × interval, formatted compactly', () => {
    expect(timelapseDuration(61, 30, 'second')).toBe('30m')     // 60×30s = 1800s
    expect(timelapseDuration(3, 1.5, 'minute')).toBe('3m')      // 2×1.5min = 3m
    expect(timelapseDuration(2, 1, 'hour')).toBe('1h')
    expect(timelapseDuration(121, 30, 's')).toBe('1h')          // 120×30s = 3600s
  })
  it('falls back to raw value + unit for an unrecognised time unit', () => {
    expect(timelapseDuration(2, 5, 'frames')).toBe('5 frames')
  })
})

describe('imageTableCsvRows', () => {
  it('includes excluded images flagged with their note; one aligned column per channel', () => {
    const rows = imageTableCsvRows([
      img({ uid: 'a', name: 'incl', sizeC: 2, channelNames: ['CD4', 'CD8'], sizeZ: 5, sizeT: 10,
            timeIncrement: 30, timeIncrementUnit: 'second', attr: { Treatment: 'X' } }),
      img({ uid: 'b', name: 'excl', sizeC: 1, channelNames: ['DAPI'], included: false,
            note: 'blurry' }),
    ], ['Treatment'])

    expect(rows).toHaveLength(2)
    expect(rows[0]).toMatchObject({
      Name: 'incl', Channels: 2, 'Channel 1': 'CD4', 'Channel 2': 'CD8', 'Z slices': 5, Frames: 10,
      Duration: '4m 30s', 'attr:Treatment': 'X', Excluded: 'no', 'Exclusion note': '',
    })
    // both rows carry the SAME channel columns (max across the set), so the CSV columns line up
    expect(rows[1]).toMatchObject({
      Name: 'excl', Channels: 1, 'Channel 1': 'DAPI', 'Channel 2': '',   // fewer channels → blank, column present
      Excluded: 'yes', 'Exclusion note': 'blurry', 'attr:Treatment': '',
    })
    expect(Object.keys(rows[0])).toEqual(Object.keys(rows[1]))          // identical key set → aligned
  })
})
