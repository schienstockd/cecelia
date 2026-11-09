import { describe, it, expect } from 'vitest'
import { splitChips } from './chipOverflow'

describe('splitChips', () => {
  it('shows the first max, hides the rest', () => {
    expect(splitChips(['roi', 'uptake', 'sheet'], 1)).toEqual({ shown: ['roi'], hidden: ['uptake', 'sheet'] })
    expect(splitChips(['roi', 'uptake'], 2)).toEqual({ shown: ['roi', 'uptake'], hidden: [] })
    expect(splitChips([], 1)).toEqual({ shown: [], hidden: [] })
  })
  it('a nonsense max still shows one', () => {
    expect(splitChips(['a', 'b'], 0).shown).toEqual(['a'])
    expect(splitChips(['a', 'b'], Number.NaN).shown).toEqual(['a'])
  })
})
