import { describe, it, expect } from 'vitest'
import { firstStepOf, litRoutes, suggestRecipe, resolveSetRecipe, nextStepFor, nextSourceLine, writtenRecipe, WRITTEN_RECIPES } from './setRecipe'
import { RECIPES } from '../lib/guides/recipes'

const imgs = (n: number, sizeT: number, sizeC: number) => Array.from({ length: n }, () => ({ sizeT, sizeC }))

describe('suggestRecipe', () => {
  it('suggests the timelapse recipe when most images have a time axis', () => {
    expect(suggestRecipe({ images: [...imgs(2, 40, 3), ...imgs(1, 1, 3)] })?.recipe.id).toBe('intravital-timelapse')
  })
  it('picks tissue recipes by channel count, and nothing in between', () => {
    expect(suggestRecipe({ images: imgs(3, 1, 4) })?.recipe.id).toBe('few-marker-tissue')
    expect(suggestRecipe({ images: imgs(3, 1, 24) })?.recipe.id).toBe('multiplex-tissue')
    expect(suggestRecipe({ images: imgs(3, 1, 8) })).toBeNull()
  })
  it('suggests nothing for an empty set or one with no axis info', () => {
    expect(suggestRecipe({ images: [] })).toBeNull()
    expect(suggestRecipe({ images: [{}] })).toBeNull()
  })
})

describe('resolveSetRecipe', () => {
  it('prefers the set\'s own recipe over the suggestion', () => {
    const r = resolveSetRecipe({ recipe: 'in-vitro-migration', images: imgs(2, 40, 3) })
    expect(r?.recipe.id).toBe('in-vitro-migration')
    expect(r?.source).toBe('set')
  })
  it('falls back to the suggestion for an unknown or wanted id', () => {
    const wanted = RECIPES.find(r => r.wanted)
    for (const id of ['no-such-recipe', wanted?.id]) {
      expect(resolveSetRecipe({ recipe: id, images: imgs(2, 40, 3) })?.source).toBe('suggested')
    }
  })
})

describe('nextStepFor', () => {
  const tl = writtenRecipe('intravital-timelapse')!
  it('names the next required page after the current one', () => {
    expect(nextStepFor(tl, '/segment')?.route).toBe('/gate')
    expect(nextStepFor(tl, '/gate')?.route).toBe('/track')
  })
  it('gives the sidebar label and the recipe\'s reason', () => {
    const n = nextStepFor(tl, '/gate')!
    expect(n.label).toBe('Track')
    expect(n.why.length).toBeGreaterThan(0)
  })
  it('is null off the recipe', () => {
    expect(nextStepFor(tl, '/notebooks')).toBeNull()
  })
  it('never points at the page it is on, for every written recipe', () => {
    for (const r of WRITTEN_RECIPES) for (const s of r.steps) {
      const route = 'page' in s && s.page ? s.page : null
      if (route) expect(nextStepFor(r, route)?.route).not.toBe(route)
    }
  })
})

describe('nextSourceLine', () => {
  it('says where the step comes from', () => {
    const r = writtenRecipe('intravital-timelapse')!
    expect(nextSourceLine({ recipe: r, source: 'set', reason: null })).toBe('From recipe · Intravital timelapse')
    expect(nextSourceLine({ recipe: r, source: 'suggested', reason: 'has a time axis' }))
      .toBe('Suggested · Intravital timelapse (has a time axis)')
  })
})

describe('litRoutes', () => {
  it('lights the confirmed recipe\'s pages only', () => {
    const lit = litRoutes({ recipe: 'intravital-timelapse', images: [] })!
    expect(lit.has('/track')).toBe(true)
    expect(lit.has('/clust-cells')).toBe(false)
  })
  it('is null without a recipe of its own, even when one is suggested', () => {
    expect(litRoutes({ images: imgs(2, 40, 3) })).toBeNull()
    expect(litRoutes(null)).toBeNull()
  })
})

describe('firstStepOf', () => {
  it('starts every written recipe on a sidebar page', () => {
    for (const r of WRITTEN_RECIPES) expect(firstStepOf(r)?.route, r.id).toMatch(/^\//)
  })
  it('skips optional steps', () => {
    const r = writtenRecipe('few-marker-tissue')!
    expect(r.steps[0].optional).toBeFalsy()
    expect(firstStepOf(r)?.label.length).toBeGreaterThan(0)
  })
})

