import { describe, it, expect } from 'vitest'
import {
  styleBlocks, cssRules, scenarioFor, findReimplementedScenarios, findRawValues,
  findHandRolledIconButtons, findRawColours, colourTokens, findRestatedInputBase, inputBase,
  findScopedUtilityOverride, utilityRules, SCENARIO_HINT, findShadowedUtilities,
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

  // On an <i>, `font-size` is the GLYPH size, not a step on the text scale — so a dim icon is not
  // muted text, and there is no utility to send it to.
  it('does not flag an icon', () => {
    expect(of('.expand-icon', 'color: var(--cc-text-dim); font-size: var(--cc-fs-2xs);')).toBeNull()
    expect(of('.search-wrap .pi-search', 'color: var(--cc-text-dim); font-size: var(--cc-fs-sm);')).toBeNull()
  })

  // Nothing in the CSS separates a hint from a readout that never got `tabular-nums`, so the hint
  // names both rather than talking a migrator out of the tabular figures.
  it('offers .cc-readout alongside .cc-muted, because the matcher cannot tell them apart', () => {
    expect(SCENARIO_HINT.muted).toContain('.cc-readout')
  })
})

describe('findScopedUtilityOverride', () => {
  const utils = { 'cc-muted': new Set(['color', 'font-size']) }
  const find = (css: string) =>
    findScopedUtilityOverride([{ path: 'x.vue', text: `<style scoped>${css}</style>` }], utils)
      .map(r => `${r.selector} | ${r.decl}`)

  it('flags a scoped rule re-declaring the global utility', () => {
    expect(find('.cc-muted { color: var(--cc-text-dim); font-size: var(--cc-fs-sm) }'))
      .toEqual(['.cc-muted | color: var(--cc-text-dim)', '.cc-muted | font-size: var(--cc-fs-sm)'])
  })

  // docs/UI.md explicitly asks sites to compose the utility and add their own layout, so all three of
  // these have to stay legal or the check would forbid the documented pattern.
  it('allows per-site layout, descendants, and a modifier compound', () => {
    expect(find('.cc-muted { margin-top: 0.3rem; flex: 1 }')).toEqual([])
    expect(find('.panel .cc-muted { color: var(--cc-text-dim) }')).toEqual([])
    expect(find('.cc-btn-bare.viewer-green { color: var(--cc-text-dim) }')).toEqual([])
  })

  it('reads each selector in a comma list on its own', () => {
    expect(find('.lm-name, .cc-muted { font-size: var(--cc-fs-sm) }'))
      .toEqual(['.cc-muted | font-size: var(--cc-fs-sm)'])
  })
})

describe('utilityRules', () => {
  it('indexes bare .cc-* rules only — a compound is not a utility declaration', () => {
    const u = utilityRules('.cc-muted { color: red; font-size: 1px } .cc-btn.on { outline: 0 }')
    expect([...u['cc-muted']]).toEqual(['color', 'font-size'])
    expect(u['cc-btn']).toBeUndefined()
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

// ── Hand-rolled scenarios: an exact list, and it is down to one ───────────────────────────────────
//
// This was a per-file BASELINE of ~310 rules that "may shrink and must never grow", because migrating
// them all at once would have been a churn diff across 45 files. It is now empty except for one
// genuinely un-adoptable site, so the ratchet has become what it was always converging on: an exact
// list. That is a stronger bar — a count-based baseline silently permits swapping one violation for
// another within a file, and it stops meaning anything once it reaches zero.
//
// It is now EMPTY. The last survivor was `AnimationModule`'s `.tl-group .tl-rowhead`, exempted on the
// grounds that the base `.tl-rowhead` set both `color` and `font-size` so no global utility could win.
// That was true but it was the wrong conclusion: the base had no business owning either — the colour
// was redundant (nothing above the timeline dims) and the size belonged on the cells. Asking "why is
// this one different?" was enough to dissolve it. Treat a new entry here the same way: "the utility
// cannot apply" is usually a fact about the *base* rule, and the base rule is yours to change.
const ALLOWED_SCENARIOS: string[] = []

const RAW = import.meta.glob('/src/**/*.{vue,css}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

describe('hand-rolled UX scenarios', () => {
  it('do not exist — every one is migrated', () => {
    const sources = Object.entries(RAW)
      .filter(([path]) => path !== '/src/style.css')          // style.css DEFINES the utilities
      .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

    expect(sources.length).toBeGreaterThan(100)               // the glob resolved

    // Name the rule AND the utility it wants: a file and a count leave the next reader re-deriving
    // which rule moved and where it should go — by grep, which got a wrong number every time.
    const found = findReimplementedScenarios(sources)
      .map(h => `${h.path} :: ${h.scenario} :: ${h.selector}`)
    const extra = found.filter(f => !ALLOWED_SCENARIOS.includes(f))
      .map(f => `${f}  → use ${SCENARIO_HINT[findReimplementedScenarios(sources)
        .find(h => `${h.path} :: ${h.scenario} :: ${h.selector}` === f)!.scenario]}`)

    expect(extra).toEqual([])
    // Fails on improvement too: if the survivor is ever restructured, drop it from the list.
    expect(found.sort()).toEqual([...ALLOWED_SCENARIOS].sort())
  })
})

describe('scoped overrides of a global utility', () => {
  // The purest form of the thing this whole area exists to prevent, and it got past every other check:
  // `LegacyMigrateDialog` adopted `class="cc-muted"` in its template — correctly — and then carried a
  // byte-identical `.cc-muted { … }` in its scoped CSS, because the migration renamed the old `.lm-sub`
  // rule alongside the class instead of deleting it. Scoping adds `[data-v-…]`, so the copy outranks the
  // global and that one component silently stops tracking the utility.
  //
  // Must be EMPTY, with no allow-list: composition (`.panel .cc-muted`), per-site layout
  // (`.cc-muted { margin-top }`) and modifier compounds (`.cc-btn-bare.viewer-green`) are all legal by
  // construction, so anything this reports is the bug itself.
  it('never exist — compose the utility, add only layout', () => {
    const utils = utilityRules(RAW['/src/style.css'] ?? '')
    expect(Object.keys(utils).length).toBeGreaterThan(10)     // style.css resolved with its utilities

    const sources = Object.entries(RAW)
      .filter(([path]) => path !== '/src/style.css')           // style.css DECLARES them
      .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

    const found = findScopedUtilityOverride(sources, utils)
      .map(r => `${r.path} | ${r.selector} | ${r.decl}`)
    expect(found.sort()).toEqual([])
  })
})

describe('the shared size ladder', () => {
  // Two classes of equal specificity are decided by SOURCE ORDER, and `.cc-muted`, `.cc-empty`,
  // `.cc-readout` and `.cc-eyebrow` each set a font-size of their own. So `.cc-fs-*` only wins if it
  // is declared after all of them. The first cut of the collapse put the ladder above them: markup
  // right, class names right, and every `.cc-eyebrow .cc-fs-2xs` silently rendering at the eyebrow's
  // 11px. Nothing caught it — the classes were all present and correct, which is all the other checks
  // look at. Hence an explicit ordering assertion.
  it('is declared after every scenario that sets a font-size of its own', () => {
    const css = RAW['/src/style.css'] ?? ''
    const at = (sel: string) => {
      const i = css.search(new RegExp(`^\\${sel}\\s*\\{`, 'm'))
      expect(i, `${sel} not found in style.css`).toBeGreaterThan(-1)
      return i
    }
    const firstStep = at('.cc-fs-lg')
    for (const scenario of ['.cc-muted', '.cc-empty', '.cc-readout', '.cc-eyebrow']) {
      expect(at(scenario), `${scenario} must be declared BEFORE the .cc-fs-* ladder, or it wins the ` +
        'equal-specificity tie and the size modifier silently does nothing').toBeLessThan(firstStep)
    }
  })
})

describe('shadowed utilities', () => {
  // The failure mode that adopting a utility INTRODUCES. Scoped CSS weighs (0,2,0) with Vue's
  // [data-v-…]; a global utility weighs (0,1,0). So a scoped class that used to win a same-specificity
  // tie on source order starts winning outright — and the utility you just added does nothing.
  // `TaskList`'s log placeholder hit this during the migration that emptied the list above.
  //
  // An exact list: the check cannot distinguish a deliberate override from an accident, so each
  // survivor says which it is. All of these deliberately set a DIFFERENT value.
  const ALLOWED = [
    // hints that are muted in colour but italic and a tier down — the file comments say so
    'components/CopyDialog.vue | cc-muted shadowed by copy-hint on font-size',
    'components/CropPanel.vue | cc-muted shadowed by crop-hint on font-size',
    'components/ProjectPanel.vue | cc-muted shadowed by pp-io-hint on font-size',
    'modules/cluster/ClusterHeatmapPanel.vue | cc-muted shadowed by feat-empty on font-size',
    // an error line: muted layout, danger colour
    'components/canvas/SummaryPanel.vue | cc-muted shadowed by sp-err on color',
    // tick labels size off the dynamic --gate-font, so only the colour comes from the utility
    'components/plots/GateScatterCell.vue | cc-muted shadowed by xtick-lbl on font-size',
    'components/plots/GateScatterCell.vue | cc-muted shadowed by ytick-lbl on font-size',
    // `color: inherit` on purpose — the label follows the section header's colour, which changes on
    // hover and when open, rather than sitting at the eyebrow's fixed dim.
    'components/CollapsibleSection.vue | cc-eyebrow shadowed by cs-label on color',
    // Same VALUE as the utility's base, so the shadow changes nothing — and load-bearing for the two
    // sibling spans that carry `.sel-count` without `.cc-muted` and would otherwise inherit body size.
    // (Its `color` was NOT deliberate and is gone: it was silently un-dimming the placeholder.)
    'components/FileBrowser.vue | cc-muted shadowed by sel-count on font-size',
  ]

  it('are all deliberate — a utility that loses to its own element is a no-op', () => {
    const utils = utilityRules(RAW['/src/style.css'] ?? '')
    const sources = Object.entries(RAW)
      .filter(([path]) => path !== '/src/style.css')
      .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

    const found = findShadowedUtilities(sources, utils)
      .map(s => `${s.path} | ${s.utility} shadowed by ${s.by} on ${s.props.join(',')}`)
    expect(found.sort()).toEqual([...ALLOWED].sort())
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
    'components/WhatNewCard.vue | wn-nav wn-nav-prev',  // full-height left-edge tip-nav overlay
    'components/WhatNewCard.vue | wn-nav wn-nav-next',  // full-height right-edge tip-nav overlay
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
  // The input/select/textarea base is one size, and until the density steps existed there was
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
    // Same shape: .mono is on the REPL <input> and on plain <span>s. The span inherits body size, so
    // dropping this would push those spans up a tier. (`.field-input.mono` needs both classes and is
    // input-only, so that one was removed.)
    'modules/SettingsModule.vue | .mono | font-size: var(--cc-fs-sm)',
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
