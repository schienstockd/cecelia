import { describe, it, expect } from 'vitest'
import { parsePane, nextPane, paneShows } from './paneExpand'

describe('parsePane', () => {
  it('accepts the three modes', () => {
    expect(parsePane('split')).toBe('split')
    expect(parsePane('top')).toBe('top')
    expect(parsePane('bottom')).toBe('bottom')
  })

  it('falls back to split for anything else', () => {
    // a stale or hand-edited localStorage value must not leave a panel in a mode that hides both halves
    for (const bad of [null, undefined, '', 'runner', 'BOTTOM', 0, {}]) {
      expect(parsePane(bad)).toBe('split')
    }
  })
})

describe('nextPane', () => {
  it('expands the clicked half', () => {
    expect(nextPane('split', 'top')).toBe('top')
    expect(nextPane('split', 'bottom')).toBe('bottom')
  })

  it('clicking the expanded half restores the split', () => {
    expect(nextPane('top', 'top')).toBe('split')
    expect(nextPane('bottom', 'bottom')).toBe('split')
  })

  it('switches straight from one half to the other', () => {
    expect(nextPane('top', 'bottom')).toBe('bottom')
    expect(nextPane('bottom', 'top')).toBe('top')
  })
})

describe('paneShows', () => {
  it('shows both halves when split', () => {
    expect(paneShows('split', 'top')).toBe(true)
    expect(paneShows('split', 'bottom')).toBe(true)
  })

  it('an expanded half hides only the other one', () => {
    // the expanded half must never hide ITSELF — that state would render an empty panel whose only
    // control is the bar, with nothing explaining where the content went
    expect(paneShows('top', 'top')).toBe(true)
    expect(paneShows('top', 'bottom')).toBe(false)
    expect(paneShows('bottom', 'bottom')).toBe(true)
    expect(paneShows('bottom', 'top')).toBe(false)
  })
})
