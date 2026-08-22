import { describe, it, expect } from 'vitest'
import { POPOUT_ROUTES, POPOUT_WINDOW_NAMES, popoutUrl, popoutRouteOfWindow, isPopoutWindow } from './popout'

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
