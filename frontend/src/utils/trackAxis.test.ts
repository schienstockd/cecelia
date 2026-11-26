import { describe, it, expect } from 'vitest'
import { splitTrackAxis, trackAxisColumn, measureColumnLabel, popFractionGroup, TRACK_AGG_DEFAULT } from './trackAxis'

// what `/api/gating/channels` returns for a track pop type (api/src/gating_api.jl)
const AGGS = ['mean', 'median', 'sum', 'qUp', 'qLow', 'sd']
const CELL = ['area', 'mean_intensity_1', 'centroid_x']

describe('splitTrackAxis', () => {
  it('splits a per-track summary of a cell measure into measure + aggregate', () => {
    expect(splitTrackAxis('mean_intensity_1.median', CELL, AGGS)).toEqual({ base: 'mean_intensity_1', agg: 'median' })
    expect(splitTrackAxis('area.qUp', CELL, AGGS)).toEqual({ base: 'area', agg: 'qUp' })
  })
  it('leaves a motility measure whole, even though it contains dots', () => {
    expect(splitTrackAxis('live.track.speed', CELL, AGGS)).toEqual({ base: 'live.track.speed', agg: null })
  })
  it('an unknown suffix or base is not an aggregate', () => {
    expect(splitTrackAxis('area.max', CELL, AGGS)).toEqual({ base: 'area.max', agg: null })
    expect(splitTrackAxis('volume.median', CELL, AGGS)).toEqual({ base: 'volume.median', agg: null })
  })
})

describe('trackAxisColumn', () => {
  it('a newly picked cell measure starts on the median', () => {
    expect(TRACK_AGG_DEFAULT).toBe('median')
    expect(trackAxisColumn('mean_intensity_1', null, CELL, AGGS)).toBe('mean_intensity_1.median')
  })
  it('keeps the chosen aggregate when offered', () => {
    expect(trackAxisColumn('area', 'sd', CELL, AGGS)).toBe('area.sd')
    expect(trackAxisColumn('area', 'max', CELL, AGGS)).toBe('area.median')
  })
  it('motility measures and cell plots (no aggregates) pass through', () => {
    expect(trackAxisColumn('live.track.speed', 'median', CELL, AGGS)).toBe('live.track.speed')
    expect(trackAxisColumn('area', 'median', CELL, [])).toBe('area')
    expect(trackAxisColumn('', null, CELL, AGGS)).toBe('')
  })
  it('round-trips through splitTrackAxis', () => {
    for (const a of AGGS) {
      const col = trackAxisColumn('mean_intensity_1', a, CELL, AGGS)
      expect(splitTrackAxis(col, CELL, AGGS)).toEqual({ base: 'mean_intensity_1', agg: a })
    }
  })
})

describe('measureColumnLabel', () => {
  const names = ['CD169-Kat', 'nuc-GFP', 'mem-TOM']
  it('a flow channel reads by its name; anything else is left to the caller', () => {
    const ctx = { channels: ['mean_intensity_1'], channelNames: names }
    expect(measureColumnLabel('mean_intensity_1', ctx)).toBe('nuc-GFP')
    expect(measureColumnLabel('area', ctx)).toBeNull()
  })
  it('a track summary of a channel reads "<name> <summary>"; of a shape measure, "<measure> <summary>"', () => {
    // the track branch of /api/gating/channels names no `channels`
    const ctx = { channels: [], channelNames: names, cellMeasures: CELL, trackAggregates: AGGS }
    expect(measureColumnLabel('mean_intensity_1.median', ctx)).toBe('nuc-GFP median')
    expect(measureColumnLabel('mean_intensity_1.qUp', ctx)).toBe('nuc-GFP 95th pct')
    expect(measureColumnLabel('area.sd', ctx)).toBe('area SD')
    expect(measureColumnLabel('live.track.speed', ctx)).toBeNull()
  })
})

describe('popFractionGroup — Cluster tracks "Populations" chips', () => {
  it('one chip per population token, named; parents only where two share a name', () => {
    expect(popFractionGroup(['popfrac:/qc/frags', 'popfrac:/qc/frags/TOM+', 'popfrac:/qc/cells/TOM+'])).toEqual([{
      title: 'Populations',
      opts: [{ label: 'frags', value: 'popfrac:/qc/frags' }, { label: 'frags/TOM+', value: 'popfrac:/qc/frags/TOM+' },
             { label: 'cells/TOM+', value: 'popfrac:/qc/cells/TOM+' }],
    }])
  })
  it('no group without tokens', () => {
    expect(popFractionGroup(undefined)).toEqual([])
    expect(popFractionGroup(['area'])).toEqual([])
  })
})
