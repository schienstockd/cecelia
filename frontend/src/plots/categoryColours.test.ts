import { describe, it, expect } from 'vitest'
import { categoryColours, categoryKey, type Category } from './categoryColours'
import { POPULATION_PALETTE } from './plot'
import CASES_JSON from '../../../share/category_colour_cases.json?raw'

// the SAME cases app/test/suite/filter_pop.jl checks against Julia's `category_colours`, so the
// viewer and the UMAP colour a category exactly as a movie does
const { cases } = JSON.parse(CASES_JSON) as { cases: Array<{
  name: string; values: Category[]; claims: Record<string, string>
  expect: Array<[Category, { slot?: number; hex?: string }]>
}> }

describe('categoryColours — the shared cases', () => {
  it.each(cases.map(c => [c.name, c] as const))('%s', (_n, c) => {
    const got = categoryColours(c.values, c.claims)
    for (const [v, want] of c.expect)
      expect(got.get(categoryKey(v)), String(v)).toBe(want.hex ?? POPULATION_PALETTE[want.slot! % POPULATION_PALETTE.length])
  })
})

