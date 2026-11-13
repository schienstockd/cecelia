import { describe, it, expect } from 'vitest'
import { parsePane, nextPane, nextPaneExclusive } from './paneExpand'

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

describe('a panel default', () => {
  it('is what an unset or unreadable value falls back to', () => {
    expect(parsePane(null, 'top')).toBe('top')
    expect(parsePane('nonsense', 'top')).toBe('top')
    expect(parsePane('bottom', 'top')).toBe('bottom')
  })
})

describe('an exclusive (either/or) panel', () => {
  it('never lands in split — clicking the showing half flips to the other', () => {
    expect(nextPaneExclusive('top', 'top')).toBe('bottom')
    expect(nextPaneExclusive('bottom', 'bottom')).toBe('top')
    expect(nextPaneExclusive('top', 'bottom')).toBe('bottom')
    expect(nextPaneExclusive('bottom', 'top')).toBe('top')
  })

  it('reads a split stored by the three-state version as the default half', () => {
    expect(parsePane('split', 'top', true)).toBe('top')
    expect(parsePane('split', 'bottom', true)).toBe('bottom')
    expect(parsePane('split', 'split', true)).toBe('top')
    expect(parsePane(null, 'split', true)).toBe('top')
    expect(parsePane('bottom', 'top', true)).toBe('bottom')
  })
})
