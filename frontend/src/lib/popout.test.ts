import { describe, it, expect } from 'vitest'
import { POPOUT_ROUTES, popoutUrl, isPopoutWindow } from './popout'

const BASE = { origin: 'http://localhost:8080', pathname: '/' }

describe('popoutUrl', () => {
  it('keeps the document and appends the hash route', () => {
    expect(popoutUrl('/console', BASE)).toBe('http://localhost:8080/#/console')
  })

  it('carries a query through — the task window names its project there', () => {
    expect(popoutUrl('/tasks-window?project=abc', BASE))
      .toBe('http://localhost:8080/#/tasks-window?project=abc')
  })
})

describe('isPopoutWindow', () => {
  it('recognises each popout route', () => {
    for (const p of POPOUT_ROUTES) expect(isPopoutWindow(`#${p}`)).toBe(true)
  })

  it('ignores the query — the task window carries its project there', () => {
    expect(isPopoutWindow('#/tasks-window?project=abc')).toBe(true)
  })

  it('is false for the shell routes', () => {
    for (const h of ['', '#/', '#/tasks', '#/segment', '#/settings'])
      expect(isPopoutWindow(h)).toBe(false)
  })

  it('is false for /setup — bare, but it becomes the main window', () => {
    // The wizard navigates to `/` when it finishes; a window that started there must end up with the
    // whole shell running, background workers included.
    expect(isPopoutWindow('#/setup')).toBe(false)
  })
})
