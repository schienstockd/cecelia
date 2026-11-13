import { describe, it, expect } from 'vitest'
import { schemeTokens, tokenHex } from './cssTokens'
import { composite, contrastRatio, WCAG_AA } from './colour'
import { COLOUR_SCHEMES } from './colourScheme'
import CSS from '../style.css?raw'

// Text colours must read (WCAG AA, 4.5:1) on every ground a panel can put them on, in every scheme.
const GROUNDS = ['--cc-bg', '--cc-surface-1', '--cc-surface-2']
const TEXT = [
  '--cc-text', '--cc-text-dim', '--cc-accent', '--cc-accent-soft', '--cc-kiwi', '--cc-kiwi-soft', '--cc-claude',
  '--cc-sev-ok', '--cc-sev-warn', '--cc-sev-fail', '--cc-active', '--cc-warn', '--cc-danger',
  '--cc-ink-red', '--cc-ink-amber', '--cc-ink-green', '--cc-ink-blue', '--cc-ink-cyan',
  '--cc-stage-data', '--cc-stage-populations', '--cc-stage-explore', '--cc-stage-analysis',
]
// [ink, fill] — a solid or tinted surface and the text style.css puts on it.
const ON_FILL: Array<[string, string]> = [
  ['--cc-on-solid', '--cc-accent-fill'], ['--cc-on-solid', '--cc-danger-fill'],
  ['--cc-on-primary', '--cc-primary-fill'], ['--cc-on-primary', '--cc-next-fill'],
  ['--cc-accent-soft', '--cc-accent-tint'], ['--cc-kiwi-soft', '--cc-kiwi-tint'],
]

describe.each(COLOUR_SCHEMES.map(s => s.value))('the %s scheme', scheme => {
  const t = schemeTokens(CSS, scheme)
  const ratio = (a: string, b: string) => contrastRatio(tokenHex(t, a), tokenHex(t, b))

  it('every text colour reads on every ground', () => {
    const bad = TEXT.flatMap(f => GROUNDS.map(g => [f, g, ratio(f, g)] as const))
      .filter(([, , r]) => r < WCAG_AA).map(([f, g, r]) => `${f} on ${g}: ${r.toFixed(2)}`)
    expect(bad).toEqual([])
  })

  // `.cc-label-chip-<tone>`: the box is the tone's hue at 12% over surface-1 (opaque), the word its ink.
  // Read off style.css so a new tone, or a change to the mix, is checked too.
  it('every label-chip tone reads on its own box', () => {
    const mix = /\.cc-label-chip\[[^\]]*\]\s*\{[^}]*color-mix\(in srgb, var\(--chip-hue\) (\d+)%, var\((--[\w-]+)\)\)/.exec(CSS)
    expect(mix).not.toBeNull()
    const [pct, ground] = [Number(mix![1]) / 100, mix![2]]
    const tones = [...CSS.matchAll(/\.cc-label-chip-(\w+)\s*\{\s*--chip-hue:\s*var\((--[\w-]+)\);\s*color:\s*var\((--[\w-]+)\)/g)]
    expect(tones.length).toBeGreaterThanOrEqual(5)
    const bad = tones.map(([, tone, hue, ink]) => {
      const box = composite(tokenHex(t, hue), tokenHex(t, ground), pct)
      return [tone, contrastRatio(tokenHex(t, ink), box)] as const
    }).filter(([, r]) => r < WCAG_AA).map(([tone, r]) => `${tone}: ${r.toFixed(2)}`)
    expect(bad).toEqual([])
  })

  it('every fill carries its ink', () => {
    const bad = ON_FILL.map(([f, g]) => [f, g, ratio(f, g)] as const)
      .filter(([, , r]) => r < WCAG_AA).map(([f, g, r]) => `${f} on ${g}: ${r.toFixed(2)}`)
    expect(bad).toEqual([])
  })
})

it('the Next button uses the ink its fill is checked with', () => {
  expect(/\.cc-btn-next\s*\{[^}]*color:\s*var\(--cc-on-primary\)/.test(CSS)).toBe(true)
})
