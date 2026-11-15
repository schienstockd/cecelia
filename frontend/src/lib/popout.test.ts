import { describe, it, expect, vi } from 'vitest'
import { POPOUT_ROUTES, POPOUT_WINDOW_NAMES, popoutUrl, popoutRouteOfWindow, isPopoutWindow, nameMainWindow, mainWindowUrl, MAIN_WINDOW_NAME,
  rightHalf, toggleSideBySide, savedPlacement, RESTORE_MOVE_DELAY_MS } from './popout'

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
  function fakeWin() {
    const calls: string[] = []
    return { calls, win: {
      screenX: 80, screenY: 80, outerWidth: 500, outerHeight: 432,
      moveTo: (x: number, y: number) => { calls.push(`move ${x},${y}`) },
      resizeTo: (w: number, h: number) => { calls.push(`resize ${w}x${h}`) },
    } }
  }
  function memStore() {
    const m = new Map<string, string>()
    return { getItem: (k: string) => m.get(k) ?? null, setItem: (k: string, v: string) => { m.set(k, v) },
             removeItem: (k: string) => { m.delete(k) } }
  }

  it('takes the right half of the available area, the odd pixel to the right', () => {
    expect(rightHalf(area)).toEqual({ x: 979, y: 32, w: 941, h: 1048 })
    expect(rightHalf({ availWidth: 1366, availHeight: 768 })).toEqual({ x: 683, y: 0, w: 683, h: 768 })
  })

  it('goes to the half moving first, then growing', () => {
    const { calls, win } = fakeWin(); const store = memStore()
    expect(toggleSideBySide(win, area, store)).toBe(true)
    expect(calls).toEqual(['move 979,32', 'resize 941x1048'])
    expect(savedPlacement(store)).toEqual({ x: 80, y: 80, w: 500, h: 432 })
  })

  it('comes back shrinking first, then moving — a big window moved first is clamped by the WM', () => {
    vi.useFakeTimers()
    try {
      const { calls, win } = fakeWin(); const store = memStore()
      toggleSideBySide(win, area, store)
      calls.length = 0
      expect(toggleSideBySide(win, area, store)).toBe(false)
      expect(calls).toEqual(['resize 500x432'])
      vi.advanceTimersByTime(RESTORE_MOVE_DELAY_MS)
      expect(calls).toEqual(['resize 500x432', 'move 80,80'])
      expect(savedPlacement(store)).toBeNull()
    } finally { vi.useRealTimers() }
  })

  it('reads a damaged saved placement as not side by side', () => {
    const store = memStore(); store.setItem('cc.vw.sideBySide', '{"x":1}')
    expect(savedPlacement(store)).toBeNull()
    expect(savedPlacement(null)).toBeNull()
  })
})
