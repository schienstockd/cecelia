import { describe, it, expect } from 'vitest'
import { resumeStepOf, ranPagesOf, litRoutes, suggestRecipe, resolveSetRecipe, nextStepFor, nextSourceLine, writtenRecipe, WRITTEN_RECIPES, recipeProgress, guideStartView, recipeChoiceRows } from './setRecipe'
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

describe('resumeStepOf with nothing run — the recipe\'s first step', () => {
  it('starts every written recipe on a sidebar page', () => {
    for (const r of WRITTEN_RECIPES) expect(resumeStepOf(r, new Set())?.route, r.id).toMatch(/^\//)
  })
  it('skips optional steps', () => {
    const r = writtenRecipe('few-marker-tissue')!
    expect(r.steps[0].optional).toBeFalsy()
    expect(resumeStepOf(r, new Set())?.label.length).toBeGreaterThan(0)
  })
})


describe('resumeStepOf — where Welcome starts a set', () => {
  const r = WRITTEN_RECIPES[0]
  const first = resumeStepOf(r, new Set())!
  it('starts at the first step when nothing has run', () => {
    expect(resumeStepOf(r, new Set())?.route).toBe(first.route)
  })
  it('skips a step whose page already ran on the set (imported images start past Import)', () => {
    const next = resumeStepOf(r, new Set([first.route]))
    expect(next?.route).not.toBe(first.route)
  })
  it('reads the pages from successful runs only', () => {
    const pages = ranPagesOf([{ runLog: [
      { fun: 'importImages.omezarr', at: '2026-11-11T09:00:00' },
      { fun: 'segment.cellpose', status: 'failed', at: '2026-11-11T10:00:00' },
    ] }])
    expect([...pages]).toEqual(['/manage-images'])
  })
})

describe('recipeProgress — the Guides dialog and Welcome read one rule', () => {
  it('numbers the required steps only, and counts them', () => {
    for (const r of WRITTEN_RECIPES) {
      const p = recipeProgress(r, new Set())
      const req = r.steps.filter(s => !s.optional).length
      expect(p.required, r.id).toBe(req)
      expect(p.rows.filter(x => x.n !== null).map(x => x.n), r.id).toEqual(Array.from({ length: req }, (_, i) => i + 1))
      expect(p.rows.filter(x => x.step.optional).every(x => x.n === null), r.id).toBe(true)
    }
  })
  it('ticks a step whose page ran on the set, whether or not its guide was used', () => {
    const r = writtenRecipe('few-marker-tissue')!
    const p = recipeProgress(r, new Set(['/manage-images']))
    expect(p.rows.filter(x => x.ran).every(x => x.route === '/manage-images')).toBe(true)
    expect(p.rows[0].ran).toBe(true)
  })
  it('never makes an optional step the next one', () => {
    for (const r of WRITTEN_RECIPES) {
      const all = new Set<string>()
      for (const row of recipeProgress(r, new Set()).rows) {
        expect(recipeProgress(r, all).next?.step.optional, r.id).toBeFalsy()
        if (row.route) all.add(row.route)
      }
    }
  })
  it('names the same page as resumeStepOf, for every recipe and every prefix of ran pages', () => {
    for (const r of WRITTEN_RECIPES) {
      const ran = new Set<string>()
      for (const row of recipeProgress(r, new Set()).rows) {
        expect(recipeProgress(r, ran).next?.route, r.id).toBe(resumeStepOf(r, ran)?.route)
        if (row.route) ran.add(row.route)
      }
    }
  })
  it('starts every written recipe on Import', () => {
    for (const r of WRITTEN_RECIPES) expect(recipeProgress(r, new Set()).next?.route, r.id).toBe('/manage-images')
  })
})

describe('guideStartView — what the Guides dialog opens on', () => {
  const few = { recipe: 'few-marker-tissue', images: imgs(2, 1, 4) }
  it('asks for images first when the project has none', () => {
    expect(guideStartView({ projectImages: 0, set: few, picked: null, changing: false }).kind).toBe('import')
    expect(guideStartView({ projectImages: 0, set: null, picked: null, changing: false }).kind).toBe('import')
  })
  it('walks the set\'s stored recipe', () => {
    const v = guideStartView({ projectImages: 2, set: few, picked: null, changing: false })
    expect(v).toMatchObject({ kind: 'recipe', saved: true })
    expect(v.kind === 'recipe' && v.recipe.id).toBe('few-marker-tissue')
  })
  it('asks, with the suggestion, when the set has no recipe — and saves the answer', () => {
    const v = guideStartView({ projectImages: 2, set: { images: imgs(2, 1, 40) }, picked: null, changing: false })
    expect(v).toMatchObject({ kind: 'choose', saves: true })
    expect(v.kind === 'choose' && v.suggestion?.recipe.id).toBe('multiplex-tissue')
  })
  it('asks again on Change, even with a stored recipe', () => {
    expect(guideStartView({ projectImages: 2, set: few, picked: null, changing: true }).kind).toBe('choose')
  })
  it('with no active set: no suggestion, nothing saved, the selection walked in place', () => {
    expect(guideStartView({ projectImages: 2, set: null, picked: null, changing: false }))
      .toEqual({ kind: 'choose', suggestion: null, saves: false })
    expect(guideStartView({ projectImages: 2, set: null, picked: 'in-vitro-migration', changing: false }))
      .toMatchObject({ kind: 'recipe', saved: false })
  })
})

describe('recipeChoiceRows — the recipe question, Welcome and Guides alike', () => {
  it('lists every written recipe with its recognition line', () => {
    const rows = recipeChoiceRows(null)
    expect(rows.map(r => r.id)).toEqual(WRITTEN_RECIPES.map(r => r.id))
    expect(rows.every(r => r.when.length > 0 && !r.title.includes('suggested'))).toBe(true)
  })
  it('names the suggestion and its reason on its own row only', () => {
    const rows = recipeChoiceRows(suggestRecipe({ images: imgs(2, 1, 40) }))
    expect(rows.filter(r => r.title.includes('suggested')).map(r => r.id)).toEqual(['multiplex-tissue'])
    expect(rows.find(r => r.id === 'multiplex-tissue')!.title).toBe('Multiplex tissue · suggested (40 channels)')
  })
})
