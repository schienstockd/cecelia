import { describe, it, expect } from 'vitest'
import { summaryLegend, viewLegend, measureWords, boardLegendsMarkdown, type SummaryLegendFacts } from './panelLegend'

// Board 1 of a real project: straightness of three track-cluster populations, a boxplot per track, no test
const facts = (o: Partial<SummaryLegendFacts> = {}): SummaryLegendFacts => ({
  plot: 'Track measures', measure: 'live.track.straightness', chart: 'boxplot',
  series: [
    { pop: 'Tcells/slow', uID: 'a', n: 40 }, { pop: 'Tcells/slow', uID: 'b', n: 2 },
    { pop: 'Tcells/directed', uID: 'a', n: 10 }, { pop: 'Tcells/long migrating', uID: 'b', n: 7 },
  ],
  pointKind: 'track', statUnit: 'individual', imageAgg: 'mean', unitNoun: { one: 'image', many: 'images' }, test: '',
  ...o,
})

describe('summaryLegend', () => {
  it('states the measure in words, the populations, the images pooled, the chart, n and the test', () => {
    expect(summaryLegend('B', facts())).toBe(
      '**B — Track measures.** Straightness of slow, directed and long migrating tracks, 2 images pooled. ' +
      'Boxes show the median and interquartile range, whiskers 1.5 × IQR. ' +
      'Each point is a track (n = slow 42, directed 10 and long migrating 7 tracks). No statistical test.')
  })
  it('names the panel by its own title and the test it drew', () => {
    const l = summaryLegend('A', facts({ title: 'Straightness', test: 'Kruskal-Wallis' }))
    expect(l.startsWith('**A — Straightness.**')).toBe(true)
    expect(l.endsWith('Kruskal-Wallis.')).toBe(true)
  })
  it('at the image level a point is an image mean, n is images', () => {
    const l = summaryLegend('A', facts({ statUnit: 'image', series: [{ pop: 'Tcells/slow', n: 16 }] }))
    expect(l).toContain('Each point is the mean of an image (n = slow 16 images).')
  })
})

describe('viewLegend', () => {
  it('names the view and its label set', () => {
    expect(viewLegend('A', { label: 'Gating strategy', valueName: 'Tcells' }))
      .toBe('**A — Gating strategy.** Gating strategy of the Tcells segmentation.')
  })
  it('a view with nothing to name is just its label', () => {
    expect(viewLegend('C', { label: 'Viewer' })).toBe('**C — Viewer.**')
  })
})

describe('helpers', () => {
  it('says a measure in words, as the Methods report does', () => {
    expect(measureWords('live.track.meanTurningAngle')).toBe('mean turning angle')
    expect(measureWords('live.cell.hmm.state.movement_3')).toBe('HMM states (movement_3)')
    expect(measureWords('surface_to_volume')).toBe('surface-to-volume ratio')
  })
  it('puts the legends under one heading', () => {
    expect(boardLegendsMarkdown('Board 1', ['x', 'y'])).toBe('## Board 1 — legends\n\nx\n\ny\n')
  })
})
