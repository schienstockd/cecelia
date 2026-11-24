import { describe, it, expect } from 'vitest'
import { isTypingTarget, escapeClearsSelection, nudgesSelection } from './typingTarget'

const ev = (target: unknown) => ({ target }) as unknown as Event

describe('isTypingTarget', () => {
  it('is true for the fields a shortcut must not steal keys from', () => {
    expect(isTypingTarget(ev({ tagName: 'INPUT', isContentEditable: false }))).toBe(true)
    expect(isTypingTarget(ev({ tagName: 'TEXTAREA', isContentEditable: false }))).toBe(true)
    expect(isTypingTarget(ev({ tagName: 'DIV', isContentEditable: true }))).toBe(true)
  })
  it('is false for ordinary targets, and for none at all', () => {
    expect(isTypingTarget(ev({ tagName: 'DIV', isContentEditable: false }))).toBe(false)
    expect(isTypingTarget(ev({ tagName: 'BUTTON', isContentEditable: false }))).toBe(false)
    expect(isTypingTarget(ev(null))).toBe(false)
  })
})

describe('escapeClearsSelection', () => {
  const key = (k: string, o: { target?: unknown; defaultPrevented?: boolean } = {}) =>
    ({ key: k, target: o.target ?? { tagName: 'DIV', isContentEditable: false },
       defaultPrevented: o.defaultPrevented ?? false }) as unknown as KeyboardEvent
  it('clears on Escape when there is a selection', () => {
    expect(escapeClearsSelection(key('Escape'), true)).toBe(true)
  })
  it('does nothing without a selection, or on another key', () => {
    expect(escapeClearsSelection(key('Escape'), false)).toBe(false)
    expect(escapeClearsSelection(key('x'), true)).toBe(false)
  })
  it('yields to a field being typed in and to a nearer handler that claimed the key', () => {
    expect(escapeClearsSelection(key('Escape', { target: { tagName: 'INPUT' } }), true)).toBe(false)
    expect(escapeClearsSelection(key('Escape', { defaultPrevented: true }), true)).toBe(false)
  })
})

describe('nudgesSelection', () => {
  // A target whose `closest` answers for the Vue Flow classes it sits inside.
  const el = (inside: string | null, tagName = 'DIV') =>
    ({ tagName, isContentEditable: false, closest: (sel: string) => (inside && sel.includes(inside) ? {} : null) })
  const key = (k: string, target: unknown) => ({ key: k, target }) as unknown as KeyboardEvent
  it('is true for an arrow key on a selected node or on the box-selection rectangle', () => {
    expect(nudgesSelection(key('ArrowLeft', el('.vue-flow__node')), true)).toBe(true)
    expect(nudgesSelection(key('ArrowDown', el('.vue-flow__nodesselection-rect')), true)).toBe(true)
  })
  it('is false with nothing selected, off a node, on another key, or while typing', () => {
    expect(nudgesSelection(key('ArrowLeft', el('.vue-flow__node')), false)).toBe(false)
    expect(nudgesSelection(key('ArrowLeft', el(null)), true)).toBe(false)
    expect(nudgesSelection(key('a', el('.vue-flow__node')), true)).toBe(false)
    expect(nudgesSelection(key('ArrowLeft', el('.vue-flow__node', 'INPUT')), true)).toBe(false)
  })
})
