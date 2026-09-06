import { describe, it, expect } from 'vitest'
import { keyToAction, isTypingTarget, isActivatingButton, KEY_HINT } from './trackSchemeKeymap'

// Frontend tests run without jsdom (frontend/CLAUDE.md → pure logic in utils only). So `target`
// here is a plain object shaped like an element — `keyToAction` duck-types on `tagName` and
// `isContentEditable`, so this is faithful to what fires at runtime.
type FakeTarget = { tagName?: string; isContentEditable?: boolean }
function ev(init: {
  key: string; ctrlKey?: boolean; metaKey?: boolean; altKey?: boolean; shiftKey?: boolean
  target?: FakeTarget | null
}): KeyboardEvent {
  return { ...init } as unknown as KeyboardEvent
}
const el = (tagName: string, isContentEditable = false): FakeTarget => ({ tagName, isContentEditable })

describe('keyToAction', () => {
  it('maps the vizsla letters to our op actions', () => {
    expect(keyToAction(ev({ key: 'l' }))).toBe('join')   // l = link in vizsla, Join for us
    expect(keyToAction(ev({ key: 'b' }))).toBe('split')  // b = break in vizsla, Split for us
    expect(keyToAction(ev({ key: 'r' }))).toBe('remove') // r = ours, no vizsla equivalent
  })

  it('maps Backspace to undo, Enter to apply, Escape to clearSel', () => {
    expect(keyToAction(ev({ key: 'Backspace' }))).toBe('undo')
    expect(keyToAction(ev({ key: 'Enter' }))).toBe('apply')
    expect(keyToAction(ev({ key: 'Escape' }))).toBe('clearSel')
  })

  it('accepts Shift on a letter but refuses Ctrl/Meta/Alt', () => {
    expect(keyToAction(ev({ key: 'L', shiftKey: true }))).toBe('join')
    expect(keyToAction(ev({ key: 'l', ctrlKey: true }))).toBeNull()  // Ctrl+L = location bar
    expect(keyToAction(ev({ key: 'l', metaKey: true }))).toBeNull()
    expect(keyToAction(ev({ key: 'l', altKey: true }))).toBeNull()
  })

  it('returns null for keys we do not own', () => {
    expect(keyToAction(ev({ key: 'x' }))).toBeNull()
    expect(keyToAction(ev({ key: 'ArrowLeft' }))).toBeNull()
    expect(keyToAction(ev({ key: ' ' }))).toBeNull()
  })
})

describe('isTypingTarget', () => {
  it('is true for INPUT / TEXTAREA / contenteditable', () => {
    // The util duck-types on tagName/isContentEditable; cast through unknown so TS accepts the
    // structural shape without dragging in the full EventTarget interface.
    const t = (x: FakeTarget) => x as unknown as EventTarget
    expect(isTypingTarget(t(el('INPUT')))).toBe(true)
    expect(isTypingTarget(t(el('TEXTAREA')))).toBe(true)
    expect(isTypingTarget(t(el('DIV')))).toBe(false)
    expect(isTypingTarget(t(el('DIV', true)))).toBe(true)
  })

  it('is false for null and non-element targets', () => {
    expect(isTypingTarget(null)).toBe(false)
    expect(isTypingTarget({} as unknown as EventTarget)).toBe(false)
  })

  it('keyToAction refuses a keystroke from a typing target — the user is typing, not curating', () => {
    expect(keyToAction(ev({ key: 'l', target: el('INPUT') }))).toBeNull()
    expect(keyToAction(ev({ key: 'r', target: el('TEXTAREA') }))).toBeNull()
    expect(keyToAction(ev({ key: 'b', target: el('DIV', true) }))).toBeNull()
  })
})

describe('isActivatingButton', () => {
  it('is true for Enter/Space on a BUTTON so we do not shadow the browser click', () => {
    // A user tabs to Join and presses Enter — the browser fires click, not our `apply`.
    expect(isActivatingButton(ev({ key: 'Enter', target: el('BUTTON') }))).toBe(true)
    expect(isActivatingButton(ev({ key: ' ', target: el('BUTTON') }))).toBe(true)
    // A letter on a button is fine: `l` on the focused Join button still means "join".
    expect(isActivatingButton(ev({ key: 'l', target: el('BUTTON') }))).toBe(false)
    // On a non-button, Enter is ours.
    expect(isActivatingButton(ev({ key: 'Enter', target: el('DIV') }))).toBe(false)
  })

  it('keyToAction refuses Enter on a focused button — user tabbed to Join, pressed Enter to click it', () => {
    expect(keyToAction(ev({ key: 'Enter', target: el('BUTTON') }))).toBeNull()
    // Letter binding still works though — `r` while focused on some button still means Remove.
    expect(keyToAction(ev({ key: 'r', target: el('BUTTON') }))).toBe('remove')
  })
})

describe('KEY_HINT', () => {
  it('is complete — every action has a hint (so a new action cannot ship without a tooltip letter)', () => {
    const actions = ['join', 'split', 'remove', 'undo', 'apply', 'clearSel'] as const
    for (const a of actions) expect(KEY_HINT[a]).toBeTruthy()
  })
})
