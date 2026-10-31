import { describe, it, expect } from 'vitest'
import { guideCatalogue } from './guideText'
import { GUIDES, RECIPES, isWanted } from './index'
import { CALL_FOR_DATASETS, datasetAskUrl } from '../callForDatasets'
import { FEATURE_REQUEST_URL } from '../links'

describe('guide text for the MCP', () => {
  const cat = guideCatalogue()

  it('carries every guide, every written recipe and every dataset call, ids unique', () => {
    const written = RECIPES.filter(r => !isWanted(r)).length
    expect(cat).toHaveLength(GUIDES.length + written + CALL_FOR_DATASETS.length + 1)   // + request-a-feature
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

  // The committed copy the MCP serves. Stale ⇒ re-run with `-u` and commit mcp/cecelia_mcp/guides.json.
  it('matches mcp/cecelia_mcp/guides.json', async () => {
    await expect(JSON.stringify(cat, null, 1) + '\n')
      .toMatchFileSnapshot('../../../../mcp/cecelia_mcp/guides.json')
  })
})
