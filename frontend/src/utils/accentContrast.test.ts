import { describe, it, expect } from 'vitest'
import { styleBlocks, cssRules } from './cssScenarios'

// White text on --cc-accent (#a78bfa) is 2.72:1 — under WCAG AA's 4.5:1 for text and even the 3:1
// large-text floor. A filled accent surface that carries white text uses --cc-accent-strong (5.70:1),
// as .cc-btn-primary does. Checked per rule: the background and the white text in the SAME rule.
const SOURCES = import.meta.glob('/src/**/*.{vue,css}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

const ACCENT_FILL = /background(?:-color)?:\s*var\(--cc-accent\)/
const WHITE_TEXT  = /(?:^|[;\s{])color:\s*(?:#fff\b|#ffffff\b|white\b)/i

describe('white text never sits on the light accent', () => {
  it('found the sources', () => {
    expect(Object.keys(SOURCES).length).toBeGreaterThan(100)
  })

  it('every white-on-accent fill uses --cc-accent-strong', () => {
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
    expect(bad, 'use background: var(--cc-accent-strong) under white text').toEqual([])
  })
})
