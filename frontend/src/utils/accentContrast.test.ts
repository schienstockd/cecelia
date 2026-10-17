import { describe, it, expect } from 'vitest'
import { styleBlocks, cssRules } from './cssScenarios'

// White text on a solid fill uses one of the two FILL tokens: --cc-accent-fill (5.12:1) or
// --cc-danger-fill (5.86:1). On the light accent it is 2.72:1 and on --cc-danger 3.76:1 — under WCAG
// AA's 4.5:1 — and the saturated --cc-accent-strong clashed beside the red. Checked per rule, and for
// a modifier rule whose base class sets the white text.
const SOURCES = import.meta.glob('/src/**/*.{vue,css}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

const ACCENT_FILL = /background(?:-color)?:\s*(?:var\(--cc-(?:accent|accent-strong|danger)\)|#b91c1c\b|#ef4444\b|#f87171\b|#dc2626\b)/i
const WHITE_TEXT  = /(?:^|[;\s{])color:\s*(?:#fff\b|#ffffff\b|white\b)/i

describe('white text sits only on a fill token', () => {
  it('found the sources', () => {
    expect(Object.keys(SOURCES).length).toBeGreaterThan(100)
  })

  it('every white-text fill uses --cc-accent-fill or --cc-danger-fill', () => {
    const bad: string[] = []
    for (const [path, text] of Object.entries(SOURCES)) {
      const blocks = path.endsWith('.css') ? [text] : styleBlocks(text)
      const rules = blocks.flatMap(b => cssRules(b))
      // White text set on a BASE class, the fill on its modifier (`.wn-kind` + `.wn-kind-update`).
      const whiteBases = new Set(rules.filter(r => WHITE_TEXT.test(r.body)).map(r => r.selector.trim()))
      const inheritsWhite = (sel: string) => {
        const m = /^\.([\w-]+)$/.exec(sel.trim())
        if (!m) return false
        const parts = m[1].split('-')
        for (let i = parts.length - 1; i > 0; i--) {
          if (whiteBases.has('.' + parts.slice(0, i).join('-'))) return true
        }
        return false
      }
      for (const r of rules) {
        if (!ACCENT_FILL.test(r.body)) continue
        const sameRule = WHITE_TEXT.test(r.body)
        const fromBase = !/(?:^|[;\s{])color:/.test(r.body) && inheritsWhite(r.selector)
        if (sameRule || fromBase) bad.push(`${path} :: ${r.selector}`)
      }
    }
    expect(bad, 'use background: var(--cc-accent-fill) / var(--cc-danger-fill) under white text').toEqual([])
  })
})
