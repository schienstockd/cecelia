import { describe, it, expect } from 'vitest'
import { definedTokens, referencedTokens, findDeadTokenRefs } from './cssTokens'

describe('definedTokens', () => {
  it('collects declarations and ignores references', () => {
    const css = ':root { --cc-text: #fff; --cc-fs-sm: 0.75rem; }\n.x { color: var(--cc-text); }'
    expect(definedTokens(css)).toEqual(new Set(['--cc-text', '--cc-fs-sm']))
  })

  it('ignores declarations inside comments', () => {
    expect(definedTokens('/* --cc-ghost: 1px; */ :root { --cc-real: 2px; }'))
      .toEqual(new Set(['--cc-real']))
  })
})

describe('referencedTokens', () => {
  it('records whether a reference carries a fallback', () => {
    expect(referencedTokens('a { color: var(--cc-a); background: var(--cc-b, #888); }')).toEqual([
      { token: '--cc-a', hasFallback: false },
      { token: '--cc-b', hasFallback: true },
    ])
  })

  it('ignores token-shaped text in CSS, HTML and line comments', () => {
    expect(referencedTokens('/* var(--cc-x) */')).toEqual([])
    expect(referencedTokens('<!-- var(--cc-*) would be undefined -->')).toEqual([])
    expect(referencedTokens('// e.g. var(--cc-text-muted, #888)')).toEqual([])
  })

  it('does not let a URL swallow the rest of its line', () => {
    expect(referencedTokens('/* see https://x.dev */ a { color: var(--cc-a); }'))
      .toEqual([{ token: '--cc-a', hasFallback: false }])
    expect(referencedTokens('a { background: url(https://x.dev/i.png) var(--cc-b); }'))
      .toEqual([{ token: '--cc-b', hasFallback: false }])
  })

  it('only considers our own prefix', () => {
    expect(referencedTokens('a { color: var(--p-primary-color); }')).toEqual([])
  })
})

describe('findDeadTokenRefs', () => {
  it('flags a reference the stylesheet never declares, fallback or not', () => {
    const dead = findDeadTokenRefs(':root { --cc-text-dim: #7d8590; }', [
      { path: 'ok.vue',      text: 'a { color: var(--cc-text-dim); }' },
      { path: 'masked.vue',  text: 'a { color: var(--cc-text-muted, #888); }' },
      { path: 'broken.vue',  text: 'a { background: var(--cc-surface); }' },
    ])
    expect(dead).toEqual([
      { path: 'masked.vue', token: '--cc-text-muted', hasFallback: true },
      { path: 'broken.vue', token: '--cc-surface',    hasFallback: false },
    ])
  })
})

// ── The real check: no component may reference a token style.css doesn't declare ──────────────────

// Sources are pulled in with Vite's raw glob rather than node's fs, so this test needs no
// @types/node and the tier stays fs-free (see docs/DEV.md → Tests).
const RAW = import.meta.glob('/src/**/*.{vue,ts,css}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

describe('the app stylesheet', () => {
  it('declares every --cc-* token the sources reference', () => {
    const sources = Object.entries(RAW)
      .filter(([path]) => !path.endsWith('.test.ts'))
      .map(([path, text]) => ({ path, text }))

    expect(sources.length).toBeGreaterThan(100)   // the glob resolved; not a vacuous pass

    const dead = findDeadTokenRefs(RAW['/src/style.css'], sources)
    // Reported as readable lines so a failure names the file and token directly.
    expect(dead.map(d => `${d.path}: ${d.token}`)).toEqual([])
  })
})
