import { describe, it, expect } from 'vitest'
import {
  PLOT_CATEGORIES, PLOT_CATEGORY_IDS, PLOT_SKETCH_KINDS, groupPlotOptions, matchesPlotQuery,
  specOption, type PlotOption,
} from './plotPicker'
import { CLUSTER_PANELS } from '../modules/cluster/clusterPanels'

// the registry plot specs, exactly as the backend serves them (app/src/plotDefinitions)
type SpecJson = { id: string; label: string; answers?: string; category?: string; sketch?: string; chartTypes?: string[] }
const SPECS = Object.values(import.meta.glob<SpecJson>('../../../app/src/plotDefinitions/*.json',
                                                       { eager: true, import: 'default' }))

const opt = (value: string, category: PlotOption['category'], label = value, answers = ''): PlotOption =>
  ({ value, label, answers, category, sketch: 'generic' })

describe('plot picker categories', () => {
  it('every built-in spec is filed under a real category (not the plugin fallback)', () => {
    expect(SPECS.length).toBeGreaterThan(10)
    for (const s of SPECS) {
      expect(PLOT_CATEGORY_IDS, s.id).toContain(s.category)
      expect(s.category, s.id).not.toBe('other')
      // named, not left to the fallback: a built-in plot that loses its sketch shows the plain one
      expect(PLOT_SKETCH_KINDS, `${s.id}: sketch`).toContain(s.sketch)
      expect(s.sketch, s.id).not.toBe('generic')
    }
  })

  it('every cluster panel is filed under a real category with a known sketch', () => {
    for (const [k, d] of Object.entries(CLUSTER_PANELS)) {
      expect(PLOT_CATEGORY_IDS, k).toContain(d.category)
      expect(d.category, k).not.toBe('other')
      expect(PLOT_SKETCH_KINDS, k).toContain(d.sketch)
    }
  })

  // the report that started this: the plain comparison of a track measure between treatments must
  // be the first thing under the first group on the behaviour page, not item 9 of an A–Z list
  it('the plain between-group comparisons come first', () => {
    expect(PLOT_CATEGORIES[0]!.id).toBe('compare')
    const compare = SPECS.filter(s => s.category === 'compare').map(s => s.id)
    expect(compare).toEqual(expect.arrayContaining(['track_measures', 'cell_properties', 'population_summary']))
  })

  it('groups in the fixed category order, drops empty groups, keeps order within a group', () => {
    const g = groupPlotOptions([opt('a', 'checks'), opt('b', 'compare'), opt('c', 'checks'), opt('d', 'time')])
    expect(g.map(x => x.id)).toEqual(['compare', 'time', 'checks'])
    expect(g[2]!.options.map(o => o.value)).toEqual(['a', 'c'])
  })
})

describe('plot sketch', () => {
  it('a spec draws the sketch it names', () => {
    expect(specOption({ id: 'x', label: 'X', sketch: 'track' }).sketch).toBe('track')
  })

  it('a plugin spec with no or an unknown sketch draws the plain one, not a blank card', () => {
    expect(specOption({ id: 'x', label: 'X' }).sketch).toBe('generic')
    expect(specOption({ id: 'x', label: 'X', sketch: 'box' }).sketch).toBe('generic')
  })

  it('a plugin spec with no or an unknown category lands in Other', () => {
    expect(specOption({ id: 'x', label: 'X' }).category).toBe('other')
    expect(specOption({ id: 'x', label: 'X', category: 'nonsense' }).category).toBe('other')
    expect(specOption({ id: 'x', label: 'X', category: 'time' }, 'summary:x'))
      .toMatchObject({ value: 'summary:x', category: 'time' })
  })
})

describe('plot search', () => {
  const track = specOption(SPECS.find(s => s.id === 'track_measures')!)
  const hmm = specOption(SPECS.find(s => s.id === 'hmm_state_count')!)

  it('an empty query matches everything', () => {
    expect(matchesPlotQuery(hmm, '  ')).toBe(true)
  })

  it('matches the label and the question, case-insensitive', () => {
    expect(matchesPlotQuery(track, 'TRACK')).toBe(true)
    expect(matchesPlotQuery(track, 'straightness')).toBe(true)
    expect(matchesPlotQuery(hmm, 'straightness')).toBe(false)
  })

  // the user's words: they asked for "mean velocity between treatments"; the app says "speed"
  it('finds Track measures from "mean velocity between treatments"', () => {
    expect(matchesPlotQuery(track, 'mean velocity between treatments')).toBe(true)
    expect(matchesPlotQuery(hmm, 'mean velocity between treatments')).toBe(false)
  })

  it('every word must match', () => {
    expect(matchesPlotQuery(track, 'speed heatmap')).toBe(false)
  })
})
