import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { POPOUT_ROUTES, POPOUT_WINDOW_NAMES, popoutUrl, popoutRouteOfWindow, isPopoutWindow, nameMainWindow, mainWindowUrl, MAIN_WINDOW_NAME,
  rightHalf, toggleSideBySide, savedPlacement, isSideBySide, PLACE_MOVE_DELAY_MS, PLACE_SETTLE_MS } from './popout'

const BASE = { origin: 'http://localhost:8080', pathname: '/' }
// The suite runs in node, with no `window` — so every call passes the window name it is asking about.
const MAIN = ''

describe('popoutUrl', () => {
  it('keeps the document and appends the hash route', () => {
    expect(popoutUrl('/console', BASE)).toBe('http://localhost:8080/#/console')
  })

  it('carries a query through — the task window names its project there', () => {
    expect(popoutUrl('/tasks-window?project=abc', BASE))
      .toBe('http://localhost:8080/#/tasks-window?project=abc')
  })
})

describe('popoutRouteOfWindow', () => {
  it('maps each window name back to the route it owns', () => {
    for (const r of POPOUT_ROUTES) expect(popoutRouteOfWindow(POPOUT_WINDOW_NAMES[r])).toBe(r)
  })

  it('is null for the main window and for anything the app did not open', () => {
    expect(popoutRouteOfWindow(MAIN)).toBeNull()
    expect(popoutRouteOfWindow('some-other-window')).toBeNull()
  })
})

describe('isPopoutWindow', () => {
  it('recognises each popout route', () => {
    for (const p of POPOUT_ROUTES) expect(isPopoutWindow(`#${p}`, MAIN)).toBe(true)
  })

  it('ignores the query — the task window carries its project there', () => {
    expect(isPopoutWindow('#/tasks-window?project=abc', MAIN)).toBe(true)
  })

  it('is false for the shell routes', () => {
    for (const h of ['', '#/', '#/tasks', '#/segment', '#/settings'])
      expect(isPopoutWindow(h, MAIN)).toBe(false)
  })

  it('is false for /setup — bare, but it becomes the main window', () => {
    // The wizard navigates to `/` when it finishes; a window that started there must end up with the
    // whole shell running, background workers included.
    expect(isPopoutWindow('#/setup', MAIN)).toBe(false)
  })

  it('trusts the window NAME over the hash — a misrouted popout is still a popout', () => {
    // The state this exists for: the Task Manager window sitting on a shell route (a stale bundle, a
    // restored session, a hand-edited URL). It must not paint the app shell inside a 1100×700 popup —
    // `main.ts` sends it back to /tasks-window, and until it lands there App.vue keeps it bare.
    expect(isPopoutWindow('#/tasks', 'cecelia-tasks')).toBe(true)
    expect(isPopoutWindow('#/', 'cecelia-console')).toBe(true)
  })
})

describe('the way back to the main window', () => {
  it('names the main window once and never renames a popout', () => {
    const main = { name: '' }; nameMainWindow(main); expect(main.name).toBe(MAIN_WINDOW_NAME)
    const pop = { name: 'cecelia-viewer' }; nameMainWindow(pop); expect(pop.name).toBe('cecelia-viewer')
  })
  it('reopens the opener at its own URL, else the app root', () => {
    expect(mainWindowUrl({ closed: false, location: { href: 'http://h/#/segment' } }, 'http://h', '/')).toBe('http://h/#/segment')
    expect(mainWindowUrl({ closed: true, location: { href: 'http://h/#/segment' } }, 'http://h', '/')).toBe('http://h/#/')
    expect(mainWindowUrl(null, 'http://h', '/app/')).toBe('http://h/app/#/')
  })
})

describe('side by side', () => {
  const area = { availLeft: 38, availTop: 32, availWidth: 1882, availHeight: 1048 }   // a GNOME screen
  const SCREEN_RIGHT = 38 + 1882
  // A window manager in miniature: a move that would push the window past the right edge is clamped
  // (what GNOME did), and every call is logged.
  function fakeWin(x = 80, y = 80, w = 500, h = 432) {
    const calls: string[] = []
    const win = {
      screenX: x, screenY: y, outerWidth: w, outerHeight: h,
      moveTo(nx: number, ny: number) { calls.push(`move ${nx},${ny}`); win.screenX = Math.min(nx, SCREEN_RIGHT - win.outerWidth); win.screenY = ny },
      resizeTo(nw: number, nh: number) { calls.push(`resize ${nw}x${nh}`); win.outerWidth = nw; win.outerHeight = nh },
    }
    return { calls, win }
  }
  function memStore() {
    const m = new Map<string, string>()
    return { getItem: (k: string) => m.get(k) ?? null, setItem: (k: string, v: string) => { m.set(k, v) },
             removeItem: (k: string) => { m.delete(k) } }
  }
  const settle = () => vi.advanceTimersByTime(PLACE_SETTLE_MS + PLACE_MOVE_DELAY_MS)
  beforeEach(() => { vi.useFakeTimers() })
  afterEach(() => { vi.useRealTimers() })

  it('takes the right half of the available area, the odd pixel to the right', () => {
    expect(rightHalf(area)).toEqual({ x: 979, y: 32, w: 941, h: 1048 })
    expect(rightHalf({ availWidth: 1366, availHeight: 768 })).toEqual({ x: 683, y: 0, w: 683, h: 768 })
  })

  it('goes to the half sizing first, then moving', () => {
    const { calls, win } = fakeWin(); const store = memStore()
    expect(toggleSideBySide(win, area, store)).toBe(true)
    expect(calls).toEqual(['resize 941x1048'])
    settle()
    expect(calls).toEqual(['resize 941x1048', 'move 979,32'])           // landed: no settle retry
    expect(savedPlacement(store)).toEqual({ x: 80, y: 80, w: 500, h: 432 })
  })

  it('a wide window lands on the half in ONE click (moving first, the WM clamped it short)', () => {
    const { win } = fakeWin(38, 32, 1200, 1000); const store = memStore()
    toggleSideBySide(win, area, store); settle()
    expect([win.screenX, win.outerWidth]).toEqual([979, 941])
    expect(isSideBySide(win, area, store)).toBe(true)
  })

  it('comes back sizing first, then moving', () => {
    const { calls, win } = fakeWin(38, 32, 1200, 1000); const store = memStore()
    toggleSideBySide(win, area, store); settle()
    calls.length = 0
    expect(toggleSideBySide(win, area, store)).toBe(false)
    settle()
    expect(calls).toEqual(['resize 1200x1000', 'move 38,32'])
    expect([win.screenX, win.screenY, win.outerWidth]).toEqual([38, 32, 1200])
    expect(savedPlacement(store)).toBeNull()
  })

  it('places it again once if the window did not land', () => {
    const { calls, win } = fakeWin(); const store = memStore()
    const realMove = win.moveTo.bind(win); let ignored = false
    win.moveTo = (x: number, y: number) => { if (!ignored) { ignored = true; calls.push('move ignored'); return } realMove(x, y) }
    toggleSideBySide(win, area, store); settle()
    expect(calls).toEqual(['resize 941x1048', 'move ignored', 'resize 941x1048', 'move 979,32'])
    expect(isSideBySide(win, area, store)).toBe(true)
  })

  it('counts a window on the half as side by side, title-bar offset and all', () => {
    const { win } = fakeWin(); const store = memStore()
    toggleSideBySide(win, area, store); settle()
    Object.assign(win, { screenY: 69, outerHeight: 1011 })              // GNOME reports the content
    expect(isSideBySide(win, area, store)).toBe(true)
  })

  it('a window dragged away by hand is not side by side — the next click places it again', () => {
    const { calls, win } = fakeWin(); const store = memStore()
    toggleSideBySide(win, area, store); settle()
    Object.assign(win, { screenX: 300, screenY: 200 })                 // the user drags it
    expect(isSideBySide(win, area, store)).toBe(false)
    calls.length = 0
    expect(toggleSideBySide(win, area, store)).toBe(true)
    settle()
    expect(calls).toEqual(['resize 941x1048', 'move 979,32'])
    expect(savedPlacement(store)).toEqual({ x: 300, y: 200, w: 941, h: 1048 })   // the new way back
  })

  it('reads a damaged saved placement as not side by side', () => {
    const store = memStore(); store.setItem('cc.vw.sideBySide', '{"x":1}')
    expect(savedPlacement(store)).toBeNull()
    expect(savedPlacement(null)).toBeNull()
  })
})
