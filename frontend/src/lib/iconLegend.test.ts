import { describe, it, expect } from 'vitest'
import { ICON_LEGEND, iconMeaning, legendGlyphs } from './iconLegend'

// The ratchet that makes the glossary a REFERENCE rather than a snapshot: every glyph actually rendered
// under `frontend/src` must be explained, and every explained glyph must actually be used. A new icon
// fails the suite until somebody says what it means.
//
// Sources are read raw (the trick uiCopy.test.ts / navGroups.test.ts use) because the glyph is a string
// in a class attribute, not something a type can reach.
// The installed icon set, read as text — the only way to know a glyph exists before it renders blank.
const ICONS_CSS = Object.values(
  import.meta.glob('/node_modules/primeicons/primeicons.css', { query: '?raw', import: 'default', eager: true }),
).join('') as string

const SRC = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

// Comments are stripped first: several components document their icon prop with an example
// (`icon?: string // e.g. 'pi-cog'`), and a glossary that had to list those would be listing prose.
function stripComments(src: string): string {
  return src
    .replace(/<!--[\s\S]*?-->/g, '')
    .replace(/\/\*[\s\S]*?\*\//g, '')
    .replace(/(^|[\s({[;,=])\/\/[^\n]*/g, '$1')
}

/** Every glyph rendered by the app, mapped to the files that render it. */
function renderedGlyphs(): Map<string, string[]> {
  const out = new Map<string, string[]>()
  for (const [path, raw] of Object.entries(SRC)) {
    if (path.endsWith('.test.ts') || path.endsWith('/iconLegend.ts')) continue
    for (const m of stripComments(raw).matchAll(/pi-[a-z0-9-]+/g)) {
      const g = m[0]
      if (g === 'pi-spin') continue          // a MODIFIER (`pi-spin pi-spinner`), not a glyph
      out.set(g, [...(out.get(g) ?? []), path.replace('/src/', '')])
    }
  }
  return out
}

describe('the icon glossary', () => {
  it('reads the sources', () => {
    // If the glob breaks, every assertion below passes vacuously.
    expect(Object.keys(SRC).length).toBeGreaterThan(100)
    expect(renderedGlyphs().size).toBeGreaterThan(80)
  })

  it('explains every glyph the app renders', () => {
    const known = legendGlyphs()
    const missing = [...renderedGlyphs().keys()].filter(g => !known.has(g)).sort()
    expect(missing, 'add these to ICON_LEGEND with what they mean (frontend/src/lib/iconLegend.ts)')
      .toEqual([])
  })

  it('explains nothing the app has stopped using', () => {
    const rendered = renderedGlyphs()
    const dead = [...legendGlyphs()].filter(g => !rendered.has(g)).sort()
    expect(dead, 'these are listed but rendered nowhere — drop them from ICON_LEGEND').toEqual([])
  })

  // The check that matters most, and the one nothing had: PrimeIcons renders a MISSING glyph as an empty
  // box, silently. Four invented names — `pi-ruler`, `pi-layer-group`, `pi-mouse-pointer`,
  // `pi-grip-vertical` — had been shipping blank icons in the physical-size dialog, the metadata panel,
  // the delete dialog and the chain palette until the glossary put them side by side (Dominik spotted
  // all four by eye, 2026-08-17). The installed stylesheet is the authority.
  it('only names glyphs PrimeIcons actually provides', () => {
    const css = ICONS_CSS
    expect(css.length, 'primeicons.css did not load').toBeGreaterThan(1000)
    const provided = new Set([...css.matchAll(/\.(pi-[a-z0-9-]+):before/g)].map(m => m[1]))
    const fake = [...renderedGlyphs().keys()].filter(g => !provided.has(g)).sort()
    expect(fake, 'these render as an empty box — no such glyph in primeicons').toEqual([])
  })

  it('gives each glyph exactly one meaning', () => {
    const seen = new Map<string, number>()
    for (const f of ICON_LEGEND) for (const i of f.icons) seen.set(i.icon, (seen.get(i.icon) ?? 0) + 1)
    expect([...seen].filter(([, n]) => n > 1).map(([g]) => g)).toEqual([])
  })

  it('writes each meaning as one short line, in the user’s words', () => {
    // Same budget as a tooltip (docs/UI.md → UI copy): one line, no second sentence.
    const bad = ICON_LEGEND.flatMap(f => f.icons)
      .filter(i => i.means.length > 78 || /\.\s+[A-Z]/.test(i.means) || !i.means)
      .map(i => `${i.icon}: ${i.means}`)
    expect(bad).toEqual([])
  })

  it('never explains a glyph in terms of class names', () => {
    // A reader of the glossary is looking at a symbol, not at our markup — `pi-spin` is our business
    // (it is in this module's header, and in docs/UI.md), not theirs (Dominik, 2026-08-17).
    const leaks = ICON_LEGEND.flatMap(f => [
      ...f.icons.filter(i => /\bpi-/.test(i.means)).map(i => i.icon),
      ...(/\bpi-/.test(f.note ?? '') ? [`${f.title} (note)`] : []),
    ])
    expect(leaks, 'say what it means, not which class does it').toEqual([])
  })

  it('looks a glyph up by name', () => {
    expect(iconMeaning('pi-trash')?.means).toBe('Delete')
    expect(iconMeaning('pi-not-a-glyph')).toBeUndefined()
  })
})
