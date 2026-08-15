import { describe, it, expect } from 'vitest'
import {
  filterSuggestions, moveHighlight, isExistingOption, activeToken, replaceActiveToken, withoutChosen,
} from './suggestInput'

const NAMES = ['Neutrophil', 'Tcell', 'cellA', 'Bcell']

describe('filterSuggestions', () => {
  // Clicking into the field asks "what did I call the other one?" — a question the list cannot answer
  // if it only appears once you can already spell the name. So an empty query filters nothing, and
  // clearing the box brings the whole list back instead of leaving it stuck empty.
  it('offers everything for an empty query — clicking in shows what is already in use', () => {
    expect(filterSuggestions(NAMES, '')).toEqual(NAMES)
    expect(filterSuggestions(NAMES, '   ')).toEqual(NAMES)
  })

  it('copies rather than aliasing the caller list, so a consumer cannot sort the source', () => {
    expect(filterSuggestions(NAMES, '')).not.toBe(NAMES)
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

    // …which then offers every tag, the same as clicking into an empty field: having just typed a
    // comma, "what else do I use?" is precisely the question.
    it('is empty right after a separator — which offers the full list', () => {
      expect(activeToken('live,', ',')).toBe('')
      expect(filterSuggestions(['qc'], activeToken('live,', ','))).toEqual(['qc'])
    })
  })

  describe('withoutChosen', () => {
    const TAGS = ['live', 'qc', 'redo']

    it('drops the tags already in the box — the same one cannot be added twice', () => {
      expect(withoutChosen(TAGS, 'live, ', ',')).toEqual(['qc', 'redo'])
      expect(withoutChosen(TAGS, 'live, qc, ', ',')).toEqual(['redo'])
    })

    it('keeps the token being TYPED on offer — it is not chosen yet', () => {
      // `li` is on its way to `live`; dropping it would empty the list exactly as it became useful
      expect(withoutChosen(TAGS, 'li', ',')).toEqual(TAGS)
      expect(withoutChosen(TAGS, 'live, q', ',')).toEqual(['qc', 'redo'])
    })

    it('ignores spacing and case, like isExistingOption', () => {
      expect(withoutChosen(TAGS, ' LIVE ,', ',')).toEqual(['qc', 'redo'])
    })

    it('ignores empty tokens, so a stray separator drops nothing', () => {
      expect(withoutChosen(TAGS, ',,', ',')).toEqual(TAGS)
    })

    it('is a no-op for a single-value field — there are no other tokens', () => {
      expect(withoutChosen(TAGS, 'live')).toEqual(TAGS)
      expect(withoutChosen(TAGS, 'live, qc')).toEqual(TAGS)   // no separator passed → one value
    })

    it('can empty the list, which is the correct answer once everything is added', () => {
      expect(withoutChosen(TAGS, 'live, qc, redo, ', ',')).toEqual([])
    })

    it('copies rather than aliasing the caller list', () => {
      expect(withoutChosen(TAGS, 'live')).not.toBe(TAGS)
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
