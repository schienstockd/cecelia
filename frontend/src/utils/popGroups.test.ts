import { describe, it, expect } from 'vitest'
import { groupPopulations } from './popGroups'

describe('groupPopulations', () => {
  const groups = [{
    valueName: 'B',
    populations: [
      { path: '/', name: 'all', granularity: 'cell', category: 'gated' },
      { path: '/qc', name: 'qc', granularity: 'cell', category: 'gated' },
      { path: '/myeloid', name: 'myeloid', granularity: 'cell', category: 'clustered' },
      { path: '/r0', name: 'r0', granularity: 'cell', category: 'region' },
      { path: '/qc/_aggregated', name: '_aggregated', granularity: 'cell', category: 'aggregated' },
      { path: '/qc/_tracked', name: '_tracked', granularity: 'track', category: 'tracked' },
      { path: '/TEST', name: 'TEST', granularity: 'track', category: 'gated' },
      { path: '/clusterA', name: 'clusterA', granularity: 'track', category: 'clustered' },
    ],
  }]

  it('orders headers cells-before-tracks and by category', () => {
    const g = groupPopulations(groups)
    expect(g.map(x => x.title)).toEqual([
      'Cells · Gated', 'Cells · Clustered', 'Cells · Regions', 'Cells · Aggregated',
      'Tracks · Gated', 'Tracks · Tracked', 'Tracks · Clustered',
    ])
  })

  it('value_name-prefixes paths and labels the all-cells root', () => {
    const g = groupPopulations(groups)
    const cellsGated = g.find(x => x.title === 'Cells · Gated')!
    expect(cellsGated.opts.map(o => o.value)).toEqual(['B/', 'B/qc'])
    expect(cellsGated.opts[0].label).toBe('B · all')
    expect(cellsGated.opts[1].label).toBe('B/qc')
  })

  it('places the auto-created aggregated pop under Cells · Aggregated', () => {
    const agg = groupPopulations(groups).find(x => x.title === 'Cells · Aggregated')!
    expect(agg.opts.map(o => o.value)).toEqual(['B/qc/_aggregated'])
  })

  it('defaults untagged pops to Cells · Gated and never drops unknown combos', () => {
    const g = groupPopulations([{ valueName: 'C', populations: [
      { path: '/x', name: 'x' },                                   // untagged → cell:gated
      { path: '/y', name: 'y', granularity: 'track', category: 'weird' },
    ] }])
    expect(g.find(x => x.title === 'Cells · Gated')!.opts[0].value).toBe('C/x')
    expect(g.some(x => x.opts.some(o => o.value === 'C/y'))).toBe(true)   // appended, not lost
  })

  it('omits empty groups', () => {
    const g = groupPopulations([{ valueName: 'C', populations: [
      { path: '/x', name: 'x', granularity: 'cell', category: 'clustered' },
    ] }])
    expect(g).toHaveLength(1)
    expect(g[0].title).toBe('Cells · Clustered')
  })
})
