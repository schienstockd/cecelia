import { describe, it, expect } from 'vitest'
import { guideCatalogue } from './guideText'
import { GUIDES, RECIPES, isWanted } from './index'

describe('guide text for the MCP', () => {
  const cat = guideCatalogue()

  it('carries every guide and every written recipe, ids unique', () => {
    const written = RECIPES.filter(r => !isWanted(r)).length
    expect(cat).toHaveLength(GUIDES.length + written)
    expect(new Set(cat.map(e => e.id)).size).toBe(cat.length)
  })

  it('names the function a task guide runs', () => {
    expect(cat.find(e => e.id === 'track-cells')!.text).toContain('`tracking.bayesian_track_measures`')
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
