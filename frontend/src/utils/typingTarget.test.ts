import { describe, it, expect } from 'vitest'
import { isTypingTarget, escapeClearsSelection } from './typingTarget'

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
