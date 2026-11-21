import { describe, it, expect } from 'vitest'
import { guideCatalogue } from './guideText'
import { plotCatalogueEntry, specPages, type PlotSpecText } from './plotCatalogue'
import { GUIDES, RECIPES, isWanted } from './index'
import { CALL_FOR_DATASETS, datasetAskUrl } from '../callForDatasets'
import { FEATURE_REQUEST_URL } from '../links'

// the registry plot specs, as the backend serves them (app/src/plotDefinitions) — handed to the
// catalogue here so the app bundle never imports across the tree
const SPECS = Object.values(import.meta.glob<PlotSpecText>('../../../../app/src/plotDefinitions/*.json',
                                                           { eager: true, import: 'default' }))

describe('guide text for the MCP', () => {
  const cat = [...guideCatalogue(), plotCatalogueEntry(SPECS)]

  it('carries every guide, every written recipe and every dataset call, ids unique', () => {
    const written = RECIPES.filter(r => !isWanted(r)).length
    expect(cat).toHaveLength(GUIDES.length + written + CALL_FOR_DATASETS.length + 2)   // + request-a-feature, which-plot
    expect(new Set(cat.map(e => e.id)).size).toBe(cat.length)
  })

  it('names the function a task guide runs', () => {
    expect(cat.find(e => e.id === 'track-cells')!.text).toContain('`tracking.bayesian_track_measures`')
  })

  it('a dataset call carries the issue link the modal opens', () => {
    for (const a of CALL_FOR_DATASETS) {
      const e = cat.find(x => x.id === a.id)!
      expect(e.kind).toBe('dataset-call')
      expect(e.text).toContain(datasetAskUrl(a))
    }
  })

  it('everything else goes to the feature-request form', () => {
    expect(cat.find(e => e.id === 'request-a-feature')!.text).toContain(FEATURE_REQUEST_URL)
  })

  it('a recipe inlines its guides in order', () => {
    const t = cat.find(e => e.id === 'intravital-timelapse')!.text
    expect(t.indexOf('# Train a flow model')).toBeLessThan(t.indexOf('# Segment a movie by motion'))
  })

  // "How do I plot X?" — every spec found, on a page the catalogue can name, saying what it answers
  it('the plot catalogue lists every spec on a named page with its question', () => {
    expect(SPECS.length).toBeGreaterThan(10)
    const text = cat.find(e => e.id === 'which-plot')!.text
    for (const s of SPECS) {
      expect(s.answers, s.id).toMatch(/\?$/)
      expect(specPages(s).every(p => !p.startsWith('?')), `${s.id}: module missing from moduleRouteFor`).toBe(true)
      expect(text).toContain(`\`${s.id}\``)
    }
    // the composition plot of track clusters per gated population is findable by its question
    expect(text).toMatch(/## Behaviour[^#]*\*\*Track cluster frequency\*\* — [^\n]*behaviour make-up/)
    // an interactive view lists the summary pages its `modules` names, not only the board
    expect(text).toMatch(/## Behaviour[^#]*\*\*Motif cards\*\*/)
    expect(text).toMatch(/\*\*Population positions\*\*[^\n]*Also on:[^\n]*Spatial/)
  })

  // The committed copy the MCP serves. Stale ⇒ re-run with `-u` and commit mcp/cecelia_mcp/guides.json.
  it('matches mcp/cecelia_mcp/guides.json', async () => {
    await expect(JSON.stringify(cat, null, 1) + '\n')
      .toMatchFileSnapshot('../../../../mcp/cecelia_mcp/guides.json')
  })
})
