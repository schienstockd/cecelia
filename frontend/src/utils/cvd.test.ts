import { describe, it, expect } from 'vitest'
import { deltaE, cvdSeparation, CVD_TARGET, CVD_FLOOR, NORMAL_FLOOR } from './cvd'
import { COLOUR_SCHEMES } from './colourScheme'

describe('deltaE — golden values from the dataviz validator (Machado 2009 + OKLab)', () => {
  // `validate_palette.js --pairs all` printed these for the pre-audit severity trios
  it('matches the reference under deuteranopia and normal vision', () => {
    expect(deltaE('#d03b3b', '#0ca30c', 'deutan')).toBeCloseTo(4.1, 1)
    expect(deltaE('#c42b2b', '#9c5a00', 'deutan')).toBeCloseTo(1.8, 1)
    expect(deltaE('#c42b2b', '#9c5a00')).toBeCloseTo(11.8, 1)
  })
  it('is zero for one colour and symmetric', () => {
    expect(deltaE('#123456', '#123456', 'protan')).toBe(0)
    expect(cvdSeparation('#ff0000', '#00aa00')).toBeCloseTo(cvdSeparation('#00aa00', '#ff0000'), 6)
  })
})

// ── The real check: the app's state colours, in every scheme, read from style.css ──────────────────

const CSS = Object.values(import.meta.glob('/src/style.css', { query: '?raw', import: 'default', eager: true }))[0] as string

/** Every `--cc-*` value a scheme resolves to: the plain :root blocks, then that scheme's override block. */
function schemeTokens(scheme: string): Record<string, string> {
  const css = CSS.replace(/\/\*[\s\S]*?\*\//g, ' ')
  const out: Record<string, string> = {}
  const take = (body: string) => { for (const m of body.matchAll(/(--cc-[\w-]+)\s*:\s*([^;]+);/g)) out[m[1]] = m[2].trim() }
  for (const m of css.matchAll(/:root\s*\{([^}]*)\}/g)) take(m[1])
  if (scheme !== 'dark') {
    const block = new RegExp(`:root\\[data-theme="${scheme}"\\]\\s*\\{([^}]*)\\}`).exec(css)
    if (!block) throw new Error(`no token block for scheme ${scheme}`)
    take(block[1])
  }
  return out
}
const hexOf = (t: Record<string, string>, name: string): string => {
  const v = t[name]
  const ref = /^var\((--cc-[\w-]+)\)$/.exec(v ?? '')
  if (ref) return hexOf(t, ref[1])
  if (!/^#[0-9a-fA-F]{6}$/.test(v ?? '')) throw new Error(`${name} is not a plain hex: ${v}`)
  return v
}

// Colours that carry a STATE and must be told apart by colour, whatever else is on screen.
const MUST_SEPARATE: Record<string, string[]> = {
  severity:   ['--cc-sev-ok', '--cc-sev-warn', '--cc-sev-fail', '--cc-active'],
  statusInk:  ['--cc-ink-red', '--cc-ink-amber', '--cc-ink-green', '--cc-ink-blue'],
  attention:  ['--cc-warn', '--cc-danger'],
  statusDots: ['--cc-dot-ok', '--cc-dot-warn', '--cc-dot-fail', '--cc-dot-info'],
}
// Category hues that always sit beside a text label — a second cue, so the floor applies.
const LABELLED: Record<string, string[]> = {
  categoryInk: ['--cc-ink-red', '--cc-ink-amber', '--cc-ink-green', '--cc-ink-blue', '--cc-ink-cyan'],
}

function pairs(names: string[]): Array<[string, string]> {
  return names.flatMap((a, i) => names.slice(i + 1).map(b => [a, b] as [string, string]))
}

describe.each(COLOUR_SCHEMES.map(s => s.value))('the %s scheme', scheme => {
  const t = schemeTokens(scheme)

  it.each(Object.entries(MUST_SEPARATE))('keeps %s apart under colour blindness (ΔE ≥ 8) and normal vision (≥ 15)', (_g, names) => {
    const bad = pairs(names).flatMap(([a, b]) => {
      const cvd = cvdSeparation(hexOf(t, a), hexOf(t, b)), normal = deltaE(hexOf(t, a), hexOf(t, b))
      return cvd < CVD_TARGET || normal < NORMAL_FLOOR ? [`${a} ↔ ${b}: cvd ${cvd.toFixed(1)}, normal ${normal.toFixed(1)}`] : []
    })
    expect(bad).toEqual([])
  })

  it.each(Object.entries(LABELLED))('keeps labelled %s above the floor (ΔE ≥ 6)', (_g, names) => {
    const bad = pairs(names).flatMap(([a, b]) => {
      const cvd = cvdSeparation(hexOf(t, a), hexOf(t, b))
      return cvd < CVD_FLOOR ? [`${a} ↔ ${b}: cvd ${cvd.toFixed(1)}`] : []
    })
    expect(bad).toEqual([])
  })
})
