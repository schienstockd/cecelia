import { describe, it, expect } from 'vitest'
import {
  styleBlocks, cssRules, scenarioFor, findReimplementedScenarios, findRawValues,
  findHandRolledIconButtons, findRawColours, colourTokens, findRestatedInputBase, inputBase,
} from './cssScenarios'

describe('styleBlocks', () => {
  it('extracts every style block of an SFC, and passes plain CSS through', () => {
    expect(styleBlocks('<template>x</template><style scoped>.a{}</style>')).toEqual(['.a{}'])
    expect(styleBlocks('<style>.a{}</style><style scoped>.b{}</style>')).toEqual(['.a{}', '.b{}'])
    expect(styleBlocks('.plain { color: red }')).toEqual(['.plain { color: red }'])
  })
})

describe('cssRules', () => {
  it('splits selectors from bodies', () => {
    expect(cssRules('.a { color: red; } .b, .c { top: 0 }')).toEqual([
      { selector: '.a', body: ' color: red; ' },
      { selector: '.b, .c', body: ' top: 0 ' },
    ])
  })

  it('does not let a comment leak into the next selector', () => {
    expect(cssRules('/* note */ .a { top: 0 }').map(r => r.selector)).toEqual(['.a'])
  })

  it('descends into at-rules instead of treating them as one rule', () => {
    expect(cssRules('@media (max-width: 5px) { .a { top: 0 } }').map(r => r.selector)).toEqual(['.a'])
  })
})

describe('scenarioFor', () => {
  const of = (selector: string, body: string) => scenarioFor({ selector, body })

  it('names the canonical utility a rule re-implements', () => {
    expect(of('.x-hint', 'color: var(--cc-text-dim); font-size: 0.7rem;')).toBe('muted')
    expect(of('.x-empty', 'color: var(--cc-text-dim); padding: 1rem;')).toBe('empty')
    expect(of('.x-head', 'text-transform: uppercase; letter-spacing: 0.06em; color: var(--cc-text-dim);'))
      .toBe('eyebrow')
  })

  it('ignores rules that already take colour from a utility', () => {
    expect(of('.x-hint', 'font-size: var(--cc-fs-xs); font-style: italic;')).toBeNull()
    expect(of('.x-empty p', 'margin: 0; font-size: 0.8rem;')).toBeNull()
  })

  // Using the scale tokens is necessary but not sufficient: this rule is still `.cc-muted` longhand.
  it('still flags a re-declaration built from tokens', () => {
    expect(of('.x', 'color: var(--cc-text-dim); font-size: var(--cc-fs-sm);')).toBe('muted')
  })

  // A dim colour + a size also describes every ghost/icon button, whose canonical form is
  // `.cc-btn-ghost` rather than `.cc-muted`. Flagging those would send readers to the wrong utility.
  it('does not flag controls', () => {
    expect(of('.x-btn', 'color: var(--cc-text-dim); font-size: 0.7rem;')).toBeNull()
    expect(of('.x-thing', 'color: var(--cc-text-dim); font-size: 0.7rem; cursor: pointer;')).toBeNull()
    expect(of('.x-thing', 'color: var(--cc-text-dim); font-size: 0.7rem; background: #000;')).toBeNull()
  })
})

describe('findRawColours', () => {
  const tokens = { '#a78bfa': '--cc-accent', '#ef4444': '--cc-danger' }
  const find = (css: string) =>
    findRawColours([{ path: 'x.vue', text: `<style scoped>${css}</style>` }], tokens)
      .map(r => `${r.hex} → ${r.token}`)

  it('flags a literal that a token already holds, and ignores one that no token holds', () => {
    expect(find('.a { color: #a78bfa }')).toEqual(['#a78bfa → --cc-accent'])
    expect(find('.a { color: #123456 }')).toEqual([])
  })

  it('is case-insensitive but never truncates an 8-digit hex to match a 6-digit token', () => {
    expect(find('.a { color: #A78BFA }')).toEqual(['#a78bfa → --cc-accent'])
    expect(find('.a { background: #a78bfa14 }')).toEqual([])   // an alpha tint is its own value
  })

  it('flags a hex fallback whatever its value, and counts it only once', () => {
    expect(find('.a { color: var(--cc-accent, #a855f7) }')).toEqual(['#a855f7 → --cc-accent (fallback)'])
    expect(find('.a { color: var(--cc-accent, #a78bfa) }')).toEqual(['#a78bfa → --cc-accent (fallback)'])
  })

  it('exempts plain white/black — not a scale, and what .cc-btn-primary itself uses', () => {
    expect(find('.a { color: #fff } .b { color: #000000 }')).toEqual([])
  })
})

describe('findRestatedInputBase', () => {
  const base = { color: 'var(--cc-text)', border: '1px solid var(--cc-border)' }
  const find = (text: string) =>
    findRestatedInputBase([{ path: 'x.vue', text }], base).map(r => `${r.selector} | ${r.decl}`)

  it('flags a declaration equal to the base, and leaves a different value alone', () => {
    expect(find('<template><input class="a"></template><style>.a { color: var(--cc-text) }</style>'))
      .toEqual(['.a | color: var(--cc-text)'])
    expect(find('<template><input class="a"></template><style>.a { color: var(--cc-accent) }</style>'))
      .toEqual([])
  })

  // `\b(select)\b` matches inside `.chip-select` — a hyphen is a word boundary. That swept a chip
  // wrapper and a plain button into the first run of this check.
  it('does not mistake an element name inside a class name for the element', () => {
    expect(find('<style>.chip-select { color: var(--cc-text) }</style>')).toEqual([])
    expect(find('<style>.select-flagged-btn { color: var(--cc-text) }</style>')).toEqual([])
    expect(find('<style>select { color: var(--cc-text) }</style>')).toEqual(['select | color: var(--cc-text)'])
  })

  // The rule's SUBJECT is its last compound. A sibling/descendant of an input is not the input.
  it('judges the subject, not any part of the selector', () => {
    const t = '<template><input class="ci"></template><style>.ci:checked ~ .track { color: var(--cc-text) }</style>'
    expect(find(t)).toEqual([])
  })
})

describe('inputBase', () => {
  it('takes the resting rule only — folding in :focus would flag every focus rule in the app', () => {
    const css = `
      select, input[type="text"], textarea { color: var(--cc-text); background: var(--cc-surface-2); }
      select:focus, input[type="text"]:focus, textarea:focus { border-color: var(--cc-accent); }`
    const b = inputBase(css)
    expect(b['color']).toBe('var(--cc-text)')
    expect(b['border-color']).toBeUndefined()
  })
})

// ── The ratchet ───────────────────────────────────────────────────────────────────────────────────
//
// ~130 rules still hand-roll a scenario that `docs/UI.md` has a utility for. Migrating them all at once
// would be a churn diff across 45 files, and the plan explicitly warns off that. But the thing actually
// worth preventing is NEW divergence, and that doesn't require the backlog to be empty first — it
// requires the count to never rise. So: a per-file baseline that may shrink and must never grow.
//
// Touching a file in this list? Migrate its rules and lower the number. Adding a file? Use the utility
// instead. The failure message tells you which.
//
// The four chain-node files that briefly appeared here are gone again, and how they showed up is worth
// remembering: the `muted` matcher keys on `color: var(--cc-text-dim)` and never matched the
// `color: var(--cc-text-dim, #8b8ca7)` spelling, so stripping the dead hex fallbacks revealed six
// long-standing rules this check had been blind to. That was the third blind spot of one shape — a
// matcher pinned to ONE spelling of a value the codebase writes more than one way (the others: only
// reading <style> blocks, so inline `style="font-size:…"` was invisible; and keying on a *literal*
// size, so tokenising a rule silently un-flagged it). When adding a matcher, ask which other spellings
// of the same declaration exist: token vs literal, with fallback vs without, scoped vs inline.
const BASELINE: Record<string, number> = {
  'components/AppSidebar.vue': 2,
  'components/ClaudeOverviewDialog.vue': 2,
  'components/CohortCheckButton.vue': 1,
  'components/CopyDialog.vue': 1,
  'components/CropDialog.vue': 1,
  'components/CropPanel.vue': 1,
  'components/ErrorConsole.vue': 3,
  'components/FileBrowser.vue': 3,
  'components/ImageMetadataDialog.vue': 3,
  'components/ImageTable.vue': 3,
  'components/LabLogPanel.vue': 5,
  'components/LegacyMigrateDialog.vue': 2,
  'components/ModuleLayout.vue': 5,
  'components/PackagesDialog.vue': 5,
  'components/PhysicalSizeDialog.vue': 1,
  'components/PoolThrottle.vue': 2,
  'components/ProjectPanel.vue': 5,
  'components/SetBar.vue': 2,
  'components/ViewerPanel.vue': 5,
  'components/canvas/InteractivePanel.vue': 1,
  'components/canvas/LayoutCanvas.vue': 3,
  'components/canvas/PlateBuilder.vue': 3,
  'components/canvas/PlotOptions.vue': 1,
  'components/canvas/PopulationManager.vue': 4,
  'components/canvas/SummaryCanvas.vue': 1,
  'components/canvas/SummaryPanel.vue': 3,
  'components/plots/GateMontage.vue': 2,
  'components/plots/GateScatterCell.vue': 3,
  'components/plots/ImageStripView.vue': 2,
  'components/plots/PlotSpinner.vue': 1,
  'components/plots/UmapView.vue': 2,
  'modules/AnimationModule.vue': 6,
  'modules/ChainModule.vue': 11,
  'modules/MoviesModule.vue': 2,
  'modules/SettingsModule.vue': 3,
  'modules/SetupModule.vue': 2,
  'modules/TasksModule.vue': 6,
  'modules/batchmovies/BatchMoviesPanel.vue': 4,
  'modules/cluster/ClusterPlots.vue': 1,
  'modules/gate/GatePlotPanel.vue': 2,
  'modules/gate/GatingPlots.vue': 1,
  'modules/metadata/MetadataPanel.vue': 4,
  'tasks/ParamRenderer.vue': 5,
  'tasks/TaskList.vue': 5,
  'tasks/TaskRunner.vue': 2,
}

const RAW = import.meta.glob('/src/**/*.{vue,css}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

describe('hand-rolled UX scenarios', () => {
  it('never increase — see docs/UI.md for the canonical utility', () => {
    const sources = Object.entries(RAW)
      .filter(([path]) => path !== '/src/style.css')          // style.css DEFINES the utilities
      .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

    expect(sources.length).toBeGreaterThan(100)               // the glob resolved

    const counts: Record<string, number> = {}
    for (const hit of findReimplementedScenarios(sources)) {
      counts[hit.path] = (counts[hit.path] ?? 0) + 1
    }

    const regressions: string[] = []
    const improvements: string[] = []
    for (const path of new Set([...Object.keys(BASELINE), ...Object.keys(counts)])) {
      const was = BASELINE[path] ?? 0
      const now = counts[path] ?? 0
      if (now > was) regressions.push(`${path}: ${was} → ${now} (use the utility, don't re-declare it)`)
      if (now < was) improvements.push(`${path}: ${was} → ${now} (lower the BASELINE entry)`)
    }

    expect(regressions).toEqual([])
    // Improvements fail too, on purpose: an un-updated baseline silently stops ratcheting.
    expect(improvements).toEqual([])
  })
})

describe('icon-only buttons', () => {
  // 116 sites carried 60 distinct class names — but only TWO shapes and four size steps, so they are
  // all `.cc-btn` + `-bare`|`-ghost` + `-icon` now.
  //
  // The ten hand-rolled `.seg` buttons that used to sit here are gone: they were a joined button
  // STRIP, not a segmented select, so they became `.cc-btn-group` + ordinary `.cc-btn` children
  // rather than the `ChipSelect` swap the plan had prescribed (there was no value to v-model).
  //
  // What remains is two FULL-HEIGHT STRIP controls. In markup these look exactly like icon-only
  // buttons — one <i> and nothing else — but their rule sets a width and NO height, so they stretch
  // as a flex child to fill the panel edge / tab strip. `.cc-btn-icon`'s fixed square collapsed both
  // to a chip at the top. Markup alone cannot distinguish them, so they are exempt by name: a
  // <button> that stretches is not a square icon button.
  //
  // Pinned explicitly, path and all: deriving the allow-list from the findings would make the check
  // tautological, catching a new hand-rolled button only via the total count.
  const SEG_BUTTONS = [
    'components/ModuleLayout.vue | right-handle',       // full-height right-panel collapse strip
    'components/canvas/TabbedCanvas.vue | tab-add',     // full-height "+" cell in the tab strip
  ]

  it('are built from .cc-btn', () => {
    const sources = Object.entries(RAW).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))
    const found = findHandRolledIconButtons(sources).map(b => `${b.path} | ${b.classAttr}`)
    expect(found.sort()).toEqual([...SEG_BUTTONS].sort())
  })
})

describe('raw colours', () => {
  // Colour was the last scale with no ratchet at all: `cssTokens.test.ts` catches referencing a token
  // that doesn't exist, and `findRawValues` catches literal sizes/radii, but nothing caught a literal
  // colour — so 67 hex values sat in scoped CSS duplicating a token exactly (16 × #a78bfa, which IS
  // --cc-accent), plus 33 dead `var(--token, #hex)` fallbacks reporting the wrong value.
  //
  // Deliberately narrow, like the dropped `card` matcher: most raw hex in this app is a genuine
  // one-off (chart series, chain node hues) and nothing in the stylesheet distinguishes those from a
  // system colour. An EXACT match to a declared token is not a judgement call, so this check has no
  // false positives and therefore needs no allow-list to rot. It is an exact list, not a count.
  it('are always tokens — never a literal that a token already holds, never a hex fallback', () => {
    const tokens = colourTokens(RAW['/src/style.css'] ?? '')
    expect(Object.keys(tokens).length).toBeGreaterThan(10)   // style.css resolved with its tokens

    const sources = Object.entries(RAW)
      .filter(([path]) => path !== '/src/style.css')          // style.css DECLARES the tokens
      .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

    const found = findRawColours(sources, tokens).map(r => `${r.path} | ${r.selector} | ${r.hex} → ${r.token}`)
    expect(found.sort()).toEqual([])
  })
})

describe('form controls', () => {
  // The input/select/textarea base is one size, and until `.cc-input-dense`/`-micro` existed there was
  // no way to make a control smaller except to write a class — at which point sites re-typed the base's
  // border/colour/background too. 67 such declarations across 19 files were removed; they were no-ops
  // by the cascade, so the removal is provably neutral.
  //
  // An exact list, not a ratchet: a value match makes a declaration redundant with ONE real exception —
  // re-stating the base defensively to beat a more specific rule, or because the class is shared with a
  // non-input element. Each survivor says which.
  const ALLOWED = [
    // .cby-swatch is on BOTH a <span> (static swatch) and an <input type="color"> (editable one).
    // A <span> gets nothing from the input base, so this border is load-bearing for that half.
    'components/ViewerPanel.vue | .cby-swatch | border: 1px solid var(--cc-border)',
  ]

  it('never re-state what the global input base already gives', () => {
    const base = inputBase(RAW['/src/style.css'] ?? '')
    expect(base['background']).toBeTruthy()          // the base rule was found and parsed
    expect(base['border-color']).toBeUndefined()     // ...and the :focus rule was NOT folded into it

    const sources = Object.entries(RAW)
      .filter(([path]) => path !== '/src/style.css')
      .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

    const found = findRestatedInputBase(sources, base)
      .map(r => `${r.path} | ${r.selector} | ${r.decl}`)
    expect(found.sort()).toEqual([...ALLOWED].sort())
  })
})

describe('raw sizes and radii', () => {
  // Unlike the scenario backlog above, this one is DONE: the ~770 literal font-sizes and radii are all
  // on the scales now, so the bar is an exact list rather than a shrinking count. One documented
  // exception survives — see the inline comment at that line for why it is genuinely off-scale.
  const ALLOWED = [
    'components/ChainQcNode.vue | .qc-bar | border-radius: 1px',
  ]

  it('are always scale tokens', () => {
    const sources = Object.entries(RAW).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))
    const found = findRawValues(sources).map(r => `${r.path} | ${r.selector} | ${r.decl}`)
    expect(found.sort()).toEqual(ALLOWED.sort())
  })
})
