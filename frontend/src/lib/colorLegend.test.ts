import { describe, it, expect } from 'vitest'
import palettesJson from '../plots/palettes.json'
import { COLOR_LEGEND, colorMeaning, legendTokens } from './colorLegend'

// The ratchet that makes the colour glossary a REFERENCE rather than a snapshot:
//   * every colour `--cc-*` token declared on `:root` in `style.css` must be explained here
//   * every listed `--cc-*` token must be declared in `style.css`
//   * every palette / heat / track-mode key in `palettes.json` must be explained here
//   * every listed `palette:*` / `heat-ramp` / `track-mode:*` id must exist in the JSON
// A new colour token, a new palette, or a new track mode fails the suite until somebody says what
// it means — same pattern as `iconLegend.test.ts`.

const STYLE_CSS = Object.values(
  import.meta.glob('/src/style.css', { query: '?raw', import: 'default', eager: true }),
).join('') as string

/** Every `--cc-*` custom property declared on the top-level `:root { }` block whose value looks
 *  like a colour (a hex or an `rgb()`/`rgba()`). We match the top-level `:root` block by the first
 *  brace after `:root { … }` — declarations further down (inside media queries, `.cc-light` etc.)
 *  are colour OVERRIDES, not new tokens, and must not be double-counted here. */
function declaredColorTokens(): string[] {
  // Find the first `:root {` block. The design tokens all live in one — style.css line 15.
  const start = STYLE_CSS.indexOf(':root')
  expect(start, 'style.css has no `:root` block').toBeGreaterThan(-1)
  const openBrace = STYLE_CSS.indexOf('{', start)
  // Walk to the matching close brace. `:root` in style.css has no nested blocks, so linear scan
  // with a brace counter is enough.
  let depth = 0, i = openBrace
  for (; i < STYLE_CSS.length; i++) {
    if (STYLE_CSS[i] === '{') depth++
    else if (STYLE_CSS[i] === '}' && --depth === 0) break
  }
  // Strip block comments BEFORE parsing declarations — the tokens are heavily commented
  // (`--cc-accent: purple is form/control chrome, so …` is prose inside a `/* */`), and the raw
  // regex would swallow one of those pseudo-declarations up to the next real `;`, hiding the real
  // token that lives on the line after the comment. Bit us on `--cc-guide` on the first run.
  const block = STYLE_CSS.slice(openBrace + 1, i).replace(/\/\*[\s\S]*?\*\//g, '')
  const tokens: string[] = []
  const seen = new Set<string>()
  const decl = /(--cc-[a-z0-9-]+)\s*:\s*([^;]+);/g
  for (const m of block.matchAll(decl)) {
    const [, name, rawValue] = m
    const value = rawValue.trim().split('/*')[0].trim()
    // Colour heuristic: a hex, an `rgb(…)` / `rgba(…)`, `hsl(…)`, or a `color(…)` function.
    // Font stack, sizes, radii, and layout dimensions all fail this check.
    if (!/^#[0-9a-fA-F]{3,8}$|^rgba?\(|^hsla?\(|^color\(/.test(value)) continue
    if (seen.has(name)) continue
    seen.add(name)
    tokens.push(name)
  }
  return tokens
}

/** Every non-var id the JSON provides — palettes, heat ramp, and track modes — in the token shape
 *  the legend uses. */
function jsonTokens(): string[] {
  const out: string[] = []
  for (const name of Object.keys(palettesJson.palettes)) out.push(`palette:${name}`)
  out.push('heat-ramp')
  for (const m of palettesJson.trackColorModes) out.push(`track-mode:${m}`)
  return out
}

describe('the colour glossary', () => {
  it('reads style.css', () => {
    // If the raw glob breaks, every assertion below passes vacuously.
    expect(STYLE_CSS.length, 'style.css did not load').toBeGreaterThan(1000)
    expect(declaredColorTokens().length,
           'no colour `--cc-*` tokens found — did the regex break?').toBeGreaterThan(10)
  })

  it('explains every colour token declared on :root', () => {
    const known = legendTokens()
    const missing = declaredColorTokens().filter(t => !known.has(t)).sort()
    expect(missing, 'add these to COLOR_LEGEND with what they mean (frontend/src/lib/colorLegend.ts)')
      .toEqual([])
  })

  it('explains nothing that :root no longer declares', () => {
    const declared = new Set(declaredColorTokens())
    const listedVars = [...legendTokens()].filter(t => t.startsWith('--cc-'))
    const dead = listedVars.filter(t => !declared.has(t)).sort()
    expect(dead, 'these are listed but not declared on :root — drop them from COLOR_LEGEND').toEqual([])
  })

  it('covers every palette + track mode + the heat ramp in palettes.json', () => {
    const known = legendTokens()
    const missing = jsonTokens().filter(t => !known.has(t)).sort()
    expect(missing, 'palettes.json added a palette/mode with no entry in COLOR_LEGEND').toEqual([])
  })

  it('lists no palette / mode the JSON does not carry', () => {
    const known = new Set(jsonTokens())
    const listed = [...legendTokens()].filter(t =>
      t.startsWith('palette:') || t === 'heat-ramp' || t.startsWith('track-mode:'))
    const stale = listed.filter(t => !known.has(t)).sort()
    expect(stale, 'these are listed but the JSON does not carry them — drop or fix').toEqual([])
  })

  it('gives each token exactly one meaning', () => {
    const seen = new Map<string, number>()
    for (const f of COLOR_LEGEND) for (const e of f.entries) seen.set(e.token, (seen.get(e.token) ?? 0) + 1)
    expect([...seen].filter(([, n]) => n > 1).map(([t]) => t)).toEqual([])
  })

  it('writes each meaning as one short line, in the user\'s words', () => {
    // Same budget as a tooltip (docs/ui/COPY.md): one line, no second sentence.
    const bad = COLOR_LEGEND.flatMap(f => f.entries)
      .filter(e => e.means.length > 88 || /\.\s+[A-Z]/.test(e.means) || !e.means)
      .map(e => `${e.token}: ${e.means}`)
    expect(bad).toEqual([])
  })

  it('looks a token up by name', () => {
    expect(colorMeaning('--cc-sev-ok')?.means).toBe('Passed')
    expect(colorMeaning('palette:cecelia')?.swatch.kind).toBe('palette')
    expect(colorMeaning('heat-ramp')?.swatch.kind).toBe('gradient')
    expect(colorMeaning('--cc-not-a-token')).toBeUndefined()
  })
})
