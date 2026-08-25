import { describe, it, expect } from 'vitest'
import { parse } from '@vue/compiler-sfc'

const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

/**
 * Every SFC must PARSE — which is not the same question as "does it type-check".
 *
 * `vue-tsc -b` and all 2544 unit tests passed on a `ViewerWindow.vue` whose template was missing two
 * end tags; the only thing that noticed was Vite, in the browser, as a red overlay (Dominik,
 * 2026-08-25). Type-checking works from the compiled render function and a template that fails to
 * compile simply contributes nothing to check, so a structural break reads as "no errors here".
 *
 * The parse is the same `@vue/compiler-sfc` entry point Vite's plugin calls, so anything this accepts
 * the dev server accepts. It costs milliseconds and it is the difference between finding a broken tag
 * in CI and finding it by opening the page.
 */
describe('every SFC parses', () => {
  it('the glob resolved', () => {
    expect(Object.keys(SFC).length).toBeGreaterThan(50)
  })

  it('has no unclosed or mismatched tags', () => {
    const broken: string[] = []
    for (const [path, src] of Object.entries(SFC)) {
      for (const e of parse(src).errors) {
        broken.push(`${path}:${(e as { loc?: { start: { line: number } } }).loc?.start.line ?? '?'} ${e.message}`)
      }
    }
    expect(broken).toEqual([])
  })
})
