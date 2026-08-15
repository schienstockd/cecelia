import { describe, it, expect } from 'vitest'
import {
  filterSuggestions, moveHighlight, isExistingOption, activeToken, replaceActiveToken,
} from './suggestInput'

const NAMES = ['Neutrophil', 'Tcell', 'cellA', 'Bcell']

describe('filterSuggestions', () => {
  it('offers nothing for an empty query — the list opens on TYPING, not on focus', () => {
    expect(filterSuggestions(NAMES, '')).toEqual([])
    expect(filterSuggestions(NAMES, '   ')).toEqual([])
  })

  it('matches anywhere, not just the start', () => {
    expect(filterSuggestions(NAMES, 'cell')).toContain('Tcell')
  })

  it('ranks entries that START with the query above entries that merely contain it', () => {
    expect(filterSuggestions(NAMES, 'cell')).toEqual(['cellA', 'Tcell', 'Bcell'])
  })

  it('is case-insensitive both ways', () => {
    expect(filterSuggestions(NAMES, 'tc')).toEqual(['Tcell'])
    expect(filterSuggestions(['TCELL'], 'tcell')).toEqual(['TCELL'])
  })

  it('still offers an exact match — that is how you see you are REUSING an entry', () => {
    expect(filterSuggestions(NAMES, 'Tcell')).toEqual(['Tcell'])
  })

  it('offers nothing when nothing matches, so a NEW entry shows no popover', () => {
    expect(filterSuggestions(NAMES, 'Macrophage')).toEqual([])
  })

  it('keeps the caller order within a rank group', () => {
    expect(filterSuggestions(['b1', 'a1', 'c1'], '1')).toEqual(['b1', 'a1', 'c1'])
  })
})

describe('moveHighlight', () => {
  it('starts at the first item going down, and the LAST going up', () => {
    expect(moveHighlight(-1, 1, 3)).toBe(0)
    expect(moveHighlight(-1, -1, 3)).toBe(2)
  })

  it('wraps at both ends', () => {
    expect(moveHighlight(2, 1, 3)).toBe(0)
    expect(moveHighlight(0, -1, 3)).toBe(2)
  })

  it('steps normally in the middle', () => {
    expect(moveHighlight(1, 1, 3)).toBe(2)
    expect(moveHighlight(1, -1, 3)).toBe(0)
  })

  it('is "nothing highlighted" when there is nothing to highlight', () => {
    expect(moveHighlight(-1, 1, 0)).toBe(-1)
    expect(moveHighlight(0, 1, 0)).toBe(-1)
  })
})

describe('isExistingOption', () => {
  it('is case-insensitive — mem-TOM and mem-Tom were a real pair', () => {
    expect(isExistingOption(['mem-TOM'], 'mem-tom')).toBe(true)
  })

  it('is false for something new and for nothing typed', () => {
    expect(isExistingOption(NAMES, 'Macrophage')).toBe(false)
    expect(isExistingOption(NAMES, '')).toBe(false)
    expect(isExistingOption(NAMES, '  ')).toBe(false)
  })

  it('needs a WHOLE match, not a prefix', () => {
    expect(isExistingOption(NAMES, 'Tce')).toBe(false)
  })
})

describe('multi-value fields (tags)', () => {
  // Completing the whole field would replace every tag already typed.
  describe('activeToken', () => {
    it('is the whole value when the field holds ONE value', () => {
      expect(activeToken('Tcell')).toBe('Tcell')
      expect(activeToken('a, b')).toBe('a, b')       // no separator passed → not a tag field
    })

    it('is the text after the last separator', () => {
      expect(activeToken('live, q', ',')).toBe(' q')
      expect(activeToken('live,qc,re', ',')).toBe('re')
    })

    it('is the whole value before any separator is typed', () => {
      expect(activeToken('liv', ',')).toBe('liv')
    })

    it('is empty right after a separator — which offers nothing, by filterSuggestions', () => {
      expect(activeToken('live,', ',')).toBe('')
      expect(filterSuggestions(['qc'], activeToken('live,', ','))).toEqual([])
    })
  })

  describe('replaceActiveToken', () => {
    it('replaces the whole value for a single-value field', () => {
      expect(replaceActiveToken('Tce', 'Tcell')).toBe('Tcell')
    })

    it('replaces only the token being typed, keeping the earlier ones', () => {
      expect(replaceActiveToken('live, q', 'qc', ',')).toBe('live, qc, ')
    })

    it('keeps the user\'s own spacing rather than re-joining a parsed list', () => {
      expect(replaceActiveToken('live,q', 'qc', ',')).toBe('live, qc, ')
      expect(replaceActiveToken('a,b, c', 'cd', ',')).toBe('a,b, cd, ')
    })

    it('handles the first token', () => {
      expect(replaceActiveToken('li', 'live', ',')).toBe('live, ')
    })

    it('leaves a trailing separator so the next tag can be typed straight away', () => {
      expect(replaceActiveToken('li', 'live', ',').endsWith(', ')).toBe(true)
    })
  })
})
