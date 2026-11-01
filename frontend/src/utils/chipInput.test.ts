import { describe, it, expect } from 'vitest'
import { parseChips, joinChips, addChips, chipKeyAction } from './chipInput'

describe('chipInput', () => {
  it('parses a stored string the way the backend does: trimmed, no empties, no repeats, in order', () => {
    expect(parseChips('of interest, artefact,, unsure ,artefact')).toEqual(['of interest', 'artefact', 'unsure'])
    expect(parseChips('')).toEqual([])
    expect(parseChips(undefined)).toEqual([])
  })

  it('round-trips through the stored form', () => {
    const chips = ['ingested', 'contact only', 'unsure']
    expect(joinChips(chips)).toBe('ingested, contact only, unsure')
    expect(parseChips(joinChips(chips))).toEqual(chips)
  })

  it('adds a typed word, or every word of a paste, without repeats', () => {
    expect(addChips(['a'], ' b ')).toEqual(['a', 'b'])
    expect(addChips(['a'], 'b, a, c')).toEqual(['a', 'b', 'c'])
    expect(addChips(['a'], '  ')).toEqual(['a'])
  })

  it('stops at the cap', () => {
    expect(addChips(['a', 'b'], 'c, d', 3)).toEqual(['a', 'b', 'c'])
    expect(addChips(['a', 'b'], 'c', 2)).toEqual(['a', 'b'])
  })

  it('maps keys: Enter and comma commit, Backspace on an empty box takes the last chip back', () => {
    expect(chipKeyAction('Enter', 'x')).toBe('commit')
    expect(chipKeyAction(',', 'x')).toBe('commit')
    expect(chipKeyAction('Backspace', '')).toBe('pop')
    expect(chipKeyAction('Backspace', 'x')).toBeNull()
    expect(chipKeyAction('a', '')).toBeNull()
  })
})
