import { describe, it, expect } from 'vitest'
import { deltaE, cvdSeparation, CVD_TARGET, CVD_FLOOR, NORMAL_FLOOR } from './cvd'
import { COLOUR_SCHEMES } from './colourScheme'
import { schemeTokens as readScheme, tokenHex } from './cssTokens'
import { contrastRatio } from './colour'
import { POPULATION_PALETTE } from '../plots/plot'
import { PLOT_GROUND_DARK } from '../plots/overlays'

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

const schemeTokens = (scheme: string) => readScheme(CSS, scheme)
const hexOf = tokenHex

// Colours that carry a STATE and must be told apart by colour, whatever else is on screen.
const MUST_SEPARATE: Record<string, string[]> = {
  severity:   ['--cc-sev-ok', '--cc-sev-warn', '--cc-sev-fail', '--cc-active'],
  statusInk:  ['--cc-ink-red', '--cc-ink-amber', '--cc-ink-green', '--cc-ink-blue'],
  attention:  ['--cc-warn', '--cc-danger'],
  // the identity hues left after the merge (one AI colour + Claude Code's clay for the side-by-side cards,
  // neutral viewer + checks) against the states
  identity:   ['--cc-accent', '--cc-kiwi', '--cc-claude', '--cc-selected', '--cc-sev-ok', '--cc-sev-warn', '--cc-sev-fail', '--cc-active'],
}
// Pairs that fail and are kept on purpose — each always has a second cue where they meet.
const ACCEPTED: Record<string, string> = {
  '--cc-kiwi ↔ --cc-sev-ok': 'Kiwi is always a labelled pill or ✦; the nearest hue that clears the ok-green is a blue that meets the violet instead',
  '--cc-accent ↔ --cc-active': 'the running light is a labelled task status; violet is control chrome (light scheme only)',
  // Claude Code's clay is only the edge, ticks and schematic of its own titled card beside Kiwi's —
  // never a state, and no severity light sits on those cards
  '--cc-claude ↔ --cc-sev-ok': 'clay marks the titled Claude Code card only, never a state',
  '--cc-claude ↔ --cc-sev-warn': 'clay marks the titled Claude Code card only, never a state',
  '--cc-claude ↔ --cc-sev-fail': 'clay marks the titled Claude Code card only, never a state',
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
      if (`${a} ↔ ${b}` in ACCEPTED) return []
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

it('every accepted exception still fails somewhere (else drop it)', () => {
  for (const pair of Object.keys(ACCEPTED)) {
    const [a, b] = pair.split(' ↔ ')
    const failsIn = COLOUR_SCHEMES.map(s => schemeTokens(s.value)).filter(t => {
      const x = hexOf(t, a), y = hexOf(t, b)
      return cvdSeparation(x, y) < CVD_TARGET || deltaE(x, y) < NORMAL_FLOOR
    })
    expect(failsIn.length, pair).toBeGreaterThan(0)
  }
})

// ── The population palette: data, not chrome, so one list for every scheme and every ground ────────
const JULIA = Object.values(import.meta.glob('/../app/src/gating/popmanager/clustering_colour.jl',
  { query: '?raw', import: 'default', eager: true }))[0] as string | undefined

const CONSOLE = Object.values(import.meta.glob('/../share/console_palette.json',
  { query: '?raw', import: 'default', eager: true }))[0] as string | undefined

describe('the population palette', () => {
  const p = POPULATION_PALETTE
  it('has 12 colours', () => expect(p.length).toBe(12))
  it('keeps every pair of the first 8 apart under colour blindness and normal vision', () => {
    const bad = pairs(p.slice(0, 8)).flatMap(([a, b]) => {
      const cvd = cvdSeparation(a, b), normal = deltaE(a, b)
      return cvd < CVD_TARGET || normal < NORMAL_FLOOR ? [`${a} ↔ ${b}: cvd ${cvd.toFixed(1)}, normal ${normal.toFixed(1)}`] : []
    })
    expect(bad).toEqual([])
  })
  // The last 4 sit at the labelled floor: a pop always carries its name.
  it('keeps all 12 above the labelled floor (cvd ≥ 6, normal ≥ 13)', () => {
    const bad = pairs([...p]).flatMap(([a, b]) => {
      const cvd = cvdSeparation(a, b), normal = deltaE(a, b)
      return cvd < CVD_FLOOR || normal < 13 ? [`${a} ↔ ${b}: cvd ${cvd.toFixed(1)}, normal ${normal.toFixed(1)}`] : []
    })
    expect(bad).toEqual([])
  })
  it('shows on the dark plot ground and a black image (≥ 3:1)', () => {
    const bad = p.flatMap(c => [PLOT_GROUND_DARK, '#000000']
      .filter(g => contrastRatio(c, g) < 3).map(g => `${c} on ${g}`))
    expect(bad).toEqual([])
  })
  // 3:1 on white AND black leaves only a narrow luminance band, and 12 well-separated colours in it are
  // all high-chroma (the previous palette — users found it garish). Paul Tol's bright scheme, made for
  // white paper, bottoms out at 1.8:1 (#66CCEE); that is the floor here.
  it('shows on a white export (≥ 1.8:1, Tol bright\'s own floor)', () => {
    expect(p.filter(c => contrastRatio(c, '#ffffff') < 1.8)).toEqual([])
  })
  it('opens with the agents-pane hues (Tol bright, the badges in share/console_palette.json)', () => {
    expect(CONSOLE, 'console_palette.json not found').toBeTruthy()
    const badges: string[] = JSON.parse(CONSOLE!).badges
    // a near match, not equality: the badges are tuned for a terminal, the palette for the grounds above
    const far = badges.slice(0, 6).flatMap((b, i) => deltaE(b, p[i]) > 8 ? [`${b} vs ${p[i]}: ${deltaE(b, p[i]).toFixed(1)}`] : [])
    expect(far).toEqual([])
  })
  it('is the list Julia colours categories with', () => {
    expect(JULIA, 'clustering_colour.jl not found').toBeTruthy()
    const m = /const POPULATION_PALETTE = \[([^\]]*)\]/.exec(JULIA!)
    expect(m?.[1].match(/#[0-9A-Fa-f]{6}/g)?.map(h => h.toUpperCase())).toEqual(p.map(h => h.toUpperCase()))
  })
})
