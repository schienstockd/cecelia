// Pure DOM-order logic, on a minimal stand-in document (the browser behaviour: TeleportPopover.browser.test.ts).
import { afterEach, describe, expect, it, vi } from 'vitest'
import { anySurfaceOpen, isTopmost, SURFACES } from './topmostSurface'

function fakeDocument(found: Element[]) {
  vi.stubGlobal('document', {
    querySelectorAll: (sel: string) => (sel === SURFACES || sel === '.cc-popover' ? found : []),
    querySelector: (sel: string) => (sel === SURFACES ? found[0] ?? null : null),
  })
}
afterEach(() => vi.unstubAllGlobals())

describe('topmostSurface', () => {
  const a = {} as Element, b = {} as Element
  it('the last surface in document order is the topmost', () => {
    fakeDocument([a, b])
    expect(isTopmost(b)).toBe(true)
    expect(isTopmost(a)).toBe(false)
  })
  it('nothing open: anyone may answer', () => {
    fakeDocument([])
    expect(isTopmost(a)).toBe(true)
    expect(anySurfaceOpen()).toBe(false)
  })
  it('anySurfaceOpen sees an open surface', () => {
    fakeDocument([a])
    expect(anySurfaceOpen()).toBe(true)
  })
})
