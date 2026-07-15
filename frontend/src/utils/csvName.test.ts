import { describe, it, expect } from 'vitest'
import { plotAxisSuffix } from './csvName'

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
