import { describe, it, expect } from 'vitest'
import { plotAxisSuffix, seriesAreGrouped } from './csvName'

describe('plotAxisSuffix', () => {
  it('uses the measure as the axis descriptor', () => {
    expect(plotAxisSuffix('live.cell.speed', '', true)).toBe('live.cell.speed')
  })

  it('appends a by_{groupBy} sub-axis when set', () => {
    expect(plotAxisSuffix('live.cell.speed', 'live.cell.hmm.state', true))
      .toBe('live.cell.speed_by_live.cell.hmm.state')
  })

  it('is empty for a measure-less population summary', () => {
    expect(plotAxisSuffix(undefined, '', false)).toBe('')
    expect(plotAxisSuffix('live.cell.speed', '', false)).toBe('')  // hasMeasure gates the measure
  })

  it('emits only the groupBy sub-axis when there is no measure', () => {
    expect(plotAxisSuffix(undefined, 'track_generation', false)).toBe('by_track_generation')
  })

  it('drops an empty measure even when hasMeasure is true', () => {
    expect(plotAxisSuffix('', 'track_state', true)).toBe('by_track_state')
    expect(plotAxisSuffix('', '', true)).toBe('')
  })
})

describe('seriesAreGrouped', () => {
  it('is true when any series carries a non-empty group level', () => {
    expect(seriesAreGrouped([{ group: 'A' }, { group: 'B' }])).toBe(true)
    expect(seriesAreGrouped([{ group: '' }, { group: 'A' }])).toBe(true)  // one real level is enough
  })

  it('is false when the groupBy was echoed but not applied (all group empty)', () => {
    // the track-measure case: groupBy set on the request but every series comes back group=''
    expect(seriesAreGrouped([{ group: '' }, { group: '' }])).toBe(false)
    expect(seriesAreGrouped([{}, {}])).toBe(false)                        // group omitted entirely
  })

  it('is false for no/empty series', () => {
    expect(seriesAreGrouped([])).toBe(false)
    expect(seriesAreGrouped(undefined)).toBe(false)
  })
})
