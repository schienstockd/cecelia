import { describe, it, expect } from 'vitest'
import { isTypingTarget } from './typingTarget'

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
