// Every rename of a user-facing route needs a redirect at the old path — pinned tabs, bookmarks and
// the old guide catalogue's `route:` entries all still point there, and a broken URL yields a router
// warning + a blank page. `main.ts` cannot be imported (it boots the app), so the route table is
// read as SOURCE, the same trick navGroups.test.ts uses.
import { describe, it, expect } from 'vitest'

const MAIN = import.meta.glob('/src/main.ts', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

function routerSource(): string {
  const src = Object.values(MAIN)[0]
  if (!src) throw new Error('could not read src/main.ts — did the glob path change?')
  return src
}

/**
 * Router redirects declared as `{ path: '/from', redirect: '/to' }` (single-line, single-quoted).
 * The route table is authored line-per-route so a regex is enough.
 */
function redirects(): Record<string, string> {
  const out: Record<string, string> = {}
  const re = /\{\s*path:\s*'([^']+)'\s*,\s*redirect:\s*'([^']+)'\s*\}/g
  for (const m of routerSource().matchAll(re)) out[m[1]] = m[2]
  return out
}

describe('route redirects', () => {
  // Phase C of docs/todo/DENOISE_INTEGRATION_PLAN.md renamed the OpticalFlow module page to Model
  // Training. Old bookmarks + the guide catalogue's older `route: '/optical-flow'` still point at
  // the old path, and this is the redirect that catches them.
  it('/optical-flow → /model-training (Phase C rename)', () => {
    expect(redirects()['/optical-flow']).toBe('/model-training')
  })
})
