// Node guard for the browser tests' config: they must never reach the maintainer's backend or his dev
// server. `vite.config.ts`'s `server` block proxies /api and /ws to CECELIA_PORT ?? 8080 (live) and pins
// 5173 with `strictPort`; `vitest.browser.config.ts` takes the rest of that config and must drop it.
// Read as text — importing the config would launch-probe for Chromium, which a Node run on Windows or
// macOS CI doesn't have. docs/todo/GUI_TESTING_PLAN.md, Decision 3.
import { describe, expect, it } from 'vitest'
import cfg from '../../../vitest.browser.config.ts?raw'
import setup from './setup.ts?raw'

// the code only: the file's own comments name the things it must not contain
const code = cfg.replace(/^\s*\/\/.*$/gm, '')

describe('vitest.browser.config.ts', () => {
  it("drops the app's dev server block (proxy to the live backend, strict port 5173)", () => {
    expect(cfg).toMatch(/const \{ server: _\w+,[^}]*\.\.\.shared \} = appConfig/)
    expect(cfg).toMatch(/\.\.\.shared,/)
    expect(cfg).not.toMatch(/\.\.\.appConfig/)
    expect(code).not.toMatch(/proxy|5173|strictPort/)
    expect(code).not.toMatch(/^\s*server\s*:/m)
  })
  it('only runs *.browser.test.ts, with the no-network setup file', () => {
    expect(cfg).toMatch(/include: \['src\/\*\*\/\*\.browser\.test\.ts'\]/)
    expect(cfg).toMatch(/setupFiles: \['src\/test\/browser\/setup\.ts'\]/)
  })
})

describe('browser test setup', () => {
  it('replaces fetch and WebSocket for every test, and fails a test that used them', () => {
    expect(setup).toMatch(/window\.fetch = /)
    expect(setup).toMatch(/window\.WebSocket = /)
    expect(setup).toMatch(/expect\(netCalls, [^)]*\)\.toEqual\(\[\]\)/)
  })
})
