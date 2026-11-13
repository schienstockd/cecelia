import { describe, it, expect } from 'vitest'
import { parsePane, nextPane } from './paneExpand'

describe('parsePane', () => {
  it('accepts the two halves', () => {
    expect(parsePane('top')).toBe('top')
    expect(parsePane('bottom')).toBe('bottom')
  })

  it('falls back to the panel default for anything else', () => {
    // a stale or hand-edited localStorage value must not leave a panel in a mode that hides both halves
    for (const bad of [null, undefined, '', 'runner', 'BOTTOM', 0, {}]) {
      expect(parsePane(bad)).toBe('top')
      expect(parsePane(bad, 'bottom')).toBe('bottom')
    }
  })

  it('reads a split stored when panels could show both halves as the default', () => {
    expect(parsePane('split')).toBe('top')
    expect(parsePane('split', 'bottom')).toBe('bottom')
  })
})

describe('nextPane', () => {
  it('shows the clicked half', () => {
    expect(nextPane('top', 'bottom')).toBe('bottom')
    expect(nextPane('bottom', 'top')).toBe('top')
  })

  it('clicking the showing half flips to the other — never both', () => {
    expect(nextPane('top', 'top')).toBe('bottom')
    expect(nextPane('bottom', 'bottom')).toBe('top')
  })
})
