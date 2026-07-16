import { describe, it, expect } from 'vitest'
import { plotDataToCsv } from './plot'
import type { PlotDataResponse } from './types'

describe('plotDataToCsv — raw datapoint export', () => {
  it('emits one tidy row per datapoint with identity + the measure column', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'live.cell.speed', granularity: 'cell', series: [],
      rows: [
        { uID: 'img1', label: '1', value_name: 'A', pop: '/p', value: 1.5 },
        { uID: 'img1', label: '2', value_name: 'A', pop: '/p', value: 2.5 },
      ],
    }
    const csv = plotDataToCsv(r)
    const lines = csv.split('\n')
    expect(lines[0]).toBe('uID,label,value_name,pop,live.cell.speed')
    expect(lines[1]).toBe('img1,1,A,/p,1.5')
    expect(lines[2]).toBe('img1,2,A,/p,2.5')
    expect(lines).toHaveLength(3)
  })

  it('adds the groupBy column (named after it) only when groupBy is set', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'm', granularity: 'cell', groupBy: 'live.cell.hmm.state', series: [],
      rows: [{ uID: 'x', label: '7', value_name: 'A', pop: '/p', group: '2', value: 3 }],
    }
    const csv = plotDataToCsv(r)
    const [header, row] = csv.split('\n')
    expect(header).toBe('uID,label,value_name,pop,live.cell.hmm.state,m')
    expect(row).toBe('x,7,A,/p,2,3')
  })

  it('adds a track_id column only when a row carries one', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'live.track.speed', granularity: 'track', series: [],
      rows: [{ uID: 'x', track_id: '4', value_name: 'A', pop: '/_tracked', value: 9 }],
    }
    const header = plotDataToCsv(r).split('\n')[0]
    expect(header).toBe('uID,track_id,value_name,pop,live.track.speed')
  })

  it('drops identity columns that are empty for every row (no dead columns)', () => {
    // single image (uID all empty), track table (no label), groupBy present but never applied (group empty)
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'live.track.speed', granularity: 'track', groupBy: 'live.cell.hmm.state', series: [],
      rows: [
        { uID: '', track_id: '4', value_name: 'A', pop: '/_tracked', group: '', value: 9 },
        { uID: '', track_id: '5', value_name: 'A', pop: '/_tracked', group: '', value: 8 },
      ],
    }
    const lines = plotDataToCsv(r).split('\n')
    expect(lines[0]).toBe('track_id,value_name,pop,live.track.speed')   // uID + group dropped, no label
    expect(lines[1]).toBe('4,A,/_tracked,9')
  })

  it('population summary: empty label is dropped, keeping only useful columns', () => {
    const r: PlotDataResponse = {
      chartType: 'raw', measure: 'count', granularity: 'cell', series: [],
      rows: [
        { uID: 'x', value_name: 'A', pop: '/p', value: 12 },
        { uID: 'y', value_name: 'A', pop: '/p', value: 8 },
      ],
    }
    expect(plotDataToCsv(r).split('\n')[0]).toBe('uID,value_name,pop,count')
  })

  it('falls back to just the value column when there are no rows', () => {
    const r: PlotDataResponse = { chartType: 'raw', measure: '', granularity: 'cell', series: [], rows: [] }
    expect(plotDataToCsv(r)).toBe('value')
  })
})
