// Mounting a component in the browser tests (`*.browser.test.ts`, vitest.browser.config.ts) — the
// app's own setup (`installAppPlugins`), a fresh pinia per mount, and nothing else. No
// `@vue/test-utils`: the tests measure the real DOM, so a real app on a real host element is all
// they need. Plan: docs/todo/GUI_TESTING_PLAN.md (Decision 2).
import { createApp, type App, type Component } from 'vue'
import { createPinia } from 'pinia'
import { installAppPlugins } from '../../appPlugins'

const mounted: Array<() => void> = []

/** Mount `root` into a fresh element on <body>. `root` is usually a small render function that builds
 *  the scenario around the component under test (a scroll box, a wrapper, a section). `extend` adds
 *  what the app's setup leaves out (a router, for a component with a RouterLink). */
export function mountInto(root: Component, extend?: (app: App) => void): HTMLElement {
  const host = document.createElement('div')
  document.body.appendChild(host)
  const app = createApp(root)
  installAppPlugins(app, createPinia())
  extend?.(app)
  app.mount(host)
  mounted.push(() => { app.unmount(); host.remove() })
  return host
}

/** Unmount everything `mountInto` mounted — the setup file calls it after every test. */
export function unmountAll(): void {
  while (mounted.length) mounted.pop()!()
}

/** Wait `n` animation frames. Placement here is `rafCoalesce`d, so "after a scroll" means "after the
 *  next frame" — waited for by frame, never by a sleep. */
export async function frames(n = 2): Promise<void> {
  for (let i = 0; i < n; i++) await new Promise(r => requestAnimationFrame(() => r(undefined)))
}

/** Scroll `el` and let a frame-coalesced listener catch up: set the position, dispatch nothing extra
 *  (the browser fires `scroll` itself), wait. */
export async function scrollTo(el: Element, top: number, left?: number): Promise<void> {
  el.scrollTop = top
  if (left !== undefined) el.scrollLeft = left
  await frames(3)
}
