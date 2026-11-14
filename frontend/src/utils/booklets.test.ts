// The user booklets' frame rules (user-docs/booklets/spec.ts → checkBooklets), run over the real
// registry so CI catches a broken booklet before anyone builds the PDFs. The build
// (scripts/booklets/build.mjs) runs the same check; this is the copy that runs on every PR.
import { describe, expect, it } from 'vitest'
import { BOOKLETS } from '../../../user-docs/booklets/index.ts'
import { bookletNumbers, checkBooklets, type Booklet } from '../../../user-docs/booklets/spec.ts'

const minimal = (over: Partial<Booklet> = {}): Booklet => ({
  id: 'x', series: 'C', title: 'X', audience: 'a', get: 'g', need: ['n'], endsAt: 'e',
  intro: [{ kind: 'para', text: 'p' }], steps: [], askAi: null, ...over,
})

describe('user booklets', () => {
  it('every registered booklet passes the frame rules', () => {
    expect(checkBooklets(BOOKLETS)).toEqual([])
  })

  it('numbers are generated per series in registry order', () => {
    const n = bookletNumbers([minimal({ id: 'a', series: 'B' }), minimal({ id: 'b' }), minimal({ id: 'c', series: 'B' })])
    expect([...n.values()]).toEqual(['B1', 'C1', 'B2'])
  })

  it('refuses an empty "If it goes wrong", a dangling Next and a B-series booklet without its frame', () => {
    const step = { title: 't', do: 'd', see: 's', ifWrong: ' ' }
    const problems = checkBooklets([
      minimal({ id: 'c', steps: [step], next: 'nope' }),
      minimal({ id: 'b', series: 'B', askAi: null }),
    ]).map(i => `${i.booklet}: ${i.problem}`)
    expect(problems).toEqual([
      'c: step 1: "ifWrong" is empty',
      'b: B-series booklet has no "Before you image" rows',
      'b: B-series booklet has no steps',
      'b: B-series booklet does not name its sample',
      'b: only C-series booklets may drop the Ask AI box',
      'c: "next" names unknown booklet "nope"',
    ])
  })
})
