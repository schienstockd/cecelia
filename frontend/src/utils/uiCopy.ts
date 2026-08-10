// UI copy budget — the enforceable half of `docs/UI.md` → *UI copy — keep it short*.
//
// The rule ("default to no explanatory text; one short line where orientation isn't self-evident")
// was review-only, and review does not hold: 56 of 175 task-parameter tips and 41 of 568 tooltip
// strings had drifted past 90 characters, the worst at 332 and 228, before anyone noticed. These
// helpers make the two mechanical parts of it checkable, in the same shape as `cssScenarios` — an
// exact allow-list rather than a count, so a violation can't be swapped for another silently.
//
// What is NOT checkable, and stays a review question: whether a tooltip should exist at all, and
// whether a short line is the *right* short line.

/** Longest a single piece of UI copy may be. Matches the budget table in `docs/UI.md`. */
export const COPY_MAX = 90

// A trailing dot on one of these is an abbreviation, not a sentence end — "e.g. HMM state" must not
// read as two sentences. This list is why the check can be strict about periods at all.
const ABBREV = /(?:^|[\s(])(?:e\.g|i\.e|etc|vs|cf|approx|fig|no|Dr|St)\.$/i

/** Collapse whitespace and unescape the quote form Vue attributes use, so lengths are comparable. */
export function normalise(text: string): string {
  return text.replace(/\\'/g, "'").split(/\s+/).filter(Boolean).join(' ')
}

/**
 * Whether `text` runs to more than one sentence.
 *
 * A tooltip is one line: what the control does. Two sentences means the second one is explaining,
 * which is what `docs/` is for. Splits on a period followed by whitespace + a capital or `(`,
 * ignoring known abbreviations.
 */
export function isMultiSentence(text: string): boolean {
  const s = normalise(text)
  for (const m of s.matchAll(/\S*\.\s+(?=[A-Z(])/g)) {
    if (!ABBREV.test(m[0].trimEnd())) return true
  }
  return false
}

/** Whether `text` exceeds the budget. */
export function isTooLong(text: string): boolean {
  return normalise(text).length > COPY_MAX
}

// `${…}` is interpolated at render time and its width is unknowable here, so it is dropped rather
// than guessed. A tooltip that is only just inside the budget plus a long uid could still render
// wide; that is a review question, not something this can decide.
const stripInterpolation = (s: string) => s.replace(/\$\{[^}]*\}/g, '')

// Every string literal inside a binding expression: '…' or `…`.
const LITERAL = /'((?:[^'\\]|\\.)*)'|`([^`]*)`/g

/**
 * The tooltip strings an SFC renders.
 *
 * IMPORTANT — this reads the string LITERALS inside each binding, not the binding expression. That
 * distinction is the whole reason this function exists: measuring the expression counts
 * `flaggedActive ? 'Deselect flagged images' : 'Select all N flagged image(s)'` as one 95-character
 * violation when both branches a user actually sees are well inside budget. Doing it the naive way
 * over-reported by ~78% (73 "violations" against a true 41) and would have sent a reader off to
 * "fix" ternaries that were already fine.
 *
 * A binding with no literal at all (`v-tooltip="label"`) contributes nothing — the text lives
 * wherever that variable is built, and is checked there if it is a literal.
 */
export function tooltipStrings(src: string): string[] {
  const out: string[] = []
  for (const bind of src.matchAll(/v-tooltip(?:\.[a-z]+)*\s*=\s*("|')([\s\S]*?)\1/g)) {
    for (const lit of bind[2].matchAll(LITERAL)) {
      const text = normalise(stripInterpolation(lit[1] ?? lit[2] ?? ''))
      if (text) out.push(text)
    }
  }
  return out
}

/**
 * `hint` / `no-set-hint` props an SFC passes to `ModuleLayout` — the first-use callout copy.
 * Static attribute form only (`hint="…"`); a bound `:hint` is an expression and is left to review.
 */
export function hintStrings(src: string): string[] {
  const out: string[] = []
  for (const m of src.matchAll(/(?<![:\w-])(?:no-set-)?hint\s*=\s*"([^"]*)"/g)) {
    const text = normalise(m[1])
    if (text) out.push(text)
  }
  return out
}

// Attributes that carry copy a user reads. `title` and `aria-label` are here because they are read
// out loud or on hover even though they never render as visible text.
const COPY_ATTRS = 'label|header|title|placeholder|emptyMessage|empty-message|acceptLabel|rejectLabel|aria-label'

/**
 * Copy passed through an attribute — `label="Crop image"`, `:header="editing ? 'Edit' : 'New'"`.
 *
 * Both forms are read: the static attribute directly, and the string literals inside a bound one
 * (same reason as `tooltipStrings` — the expression is not what the user sees).
 */
export function attrStrings(src: string): string[] {
  const out: string[] = []
  for (const m of src.matchAll(new RegExp(`(?<![:\\w-])(${COPY_ATTRS})\\s*=\\s*"([^"]*)"`, 'g'))) {
    const text = normalise(stripInterpolation(m[2]))
    if (text) out.push(text)
  }
  for (const m of src.matchAll(new RegExp(`:(${COPY_ATTRS})\\s*=\\s*("|')([\\s\\S]*?)\\2`, 'g'))) {
    for (const lit of m[3].matchAll(LITERAL)) {
      const text = normalise(stripInterpolation(lit[1] ?? lit[2] ?? ''))
      if (text) out.push(text)
    }
  }
  return out
}

/**
 * Bare text nodes inside `<template>` — button captions, headings, empty states. The largest copy
 * surface in the app and, until now, the only one nothing could see.
 *
 * CAUTION on the tag regex. Stripping tags with the obvious `/<[^>]*>/` is WRONG here: an arrow
 * function in a handler (`@blur="save(v => set(v))"`) contains a `>` that ends the match early and
 * leaks the rest of the attribute list out as "text". That inflated this bucket by roughly 3× when
 * measured. The pattern below steps over quoted attribute values instead, and the guard drops any
 * run still carrying markup — belt and braces, because a parse leak here reads as real copy.
 */
const TAG = /<\/?[A-Za-z][^>"']*(?:(?:"[^"]*"|'[^']*')[^>"']*)*>/g

export function textStrings(src: string): string[] {
  const tpl = src.match(/<template>([\s\S]*)<\/template>/)?.[1] ?? ''
  const out: string[] = []
  for (const line of tpl.replace(/<!--[\s\S]*?-->/g, '').replace(TAG, '\n').split('\n')) {
    const text = normalise(stripInterpolation(line.replace(/\{\{[^}]*\}\}/g, '')))
    if (text.length < 3 || !/[A-Za-z]{2}/.test(text)) continue
    if (/^[a-z]+(?:_[a-z]+)+$/.test(text)) continue            // snake_case → an identifier, not copy
    if (/[<>{}]|=["']|\.\w+\(|\bv-[a-z]/.test(text)) continue  // markup that survived the strip
    out.push(text)
  }
  return out
}

// A capital mid-string is only evidence of Title Case if the word isn't EXPECTED to carry one.
// Without this allowance "Calculate UMAP", "Use Dask" and "Flatten Z" all read as violations and the
// signal is mostly noise — 16 real hits hid behind 23 raw ones.
const PROPER = /^(?:Cellpose|Bayesian|Dask|Cecelia|Leiden|Python|Julia|ImageJ|Fiji|OME|Napari|Zarr|Pluto|Rscript)$/
const expectedCap = (w: string) => /^[A-Z0-9+&/–-]+$/.test(w) || w.length === 1 || PROPER.test(w)

/**
 * Whether a label is Title Cased rather than the house sentence case.
 *
 * A word after a separator starts a new phrase, so its capital is expected too — `Spatial / Time`
 * and `Segment + Measure` are two parallel labels, not Title Case. The word BEFORE the separator is
 * still judged normally, so `AF + Drift Correction` is correctly flagged on "Correction".
 */
const SEPARATOR = /^[/+&–—|]+$/

export function isTitleCase(text: string): boolean {
  const tokens = normalise(text).split(' ')
  const words = tokens.filter((w) => /^[A-Za-z]/.test(w) || SEPARATOR.test(w))
  const judged = words
    .map((w, i) => ({ w, afterSep: i > 0 && SEPARATOR.test(words[i - 1]!) }))
    .filter(({ w }, i) => i > 0 && !SEPARATOR.test(w))
  if (!judged.length) return false
  return judged.some(({ w, afterSep }) => /^[A-Z]/.test(w) && !afterSep && !expectedCap(w))
    && judged.every(({ w, afterSep }) => /^[A-Z]/.test(w) || afterSep || expectedCap(w))
}

// ── Tooltip COVERAGE — the other half of the rule ────────────────────────────────────────────────
//
// Everything above measures the copy that EXISTS. This measures the copy that DOESN'T: which inputs
// a user can change with no hover help on them at all. `docs/UI.md` asks for CellProfiler-style tip
// DENSITY — if a control does something non-obvious it has a tooltip — and that half was pure review
// until now, so it drifted the way review-only rules do: a panel gets tooltips on six of its ten
// rows and nobody sees the four. Length had a ratchet; presence didn't.
//
// This deliberately answers a NARROW question — "does this control have hover help?" — because that
// is the part a machine can decide. Whether a tooltip is the RIGHT tooltip is still a review
// question, same as `isTooLong` can't tell you a short line is a good one.

/**
 * Controls this checks. Inputs the user SETS — a value goes in, state changes.
 *
 * Buttons with a CAPTION are deliberately absent: "Run" / "Delete set" is already its own help, so
 * requiring a tooltip on all 152 of them produces tautologies — the "generated screen" noise the copy
 * budget exists to prevent. An input's value has no caption, which is why inputs are in. Icon-only
 * buttons have no caption either and are handled separately below.
 */
const CONTROL = /^(?:input|select|textarea|CcToggle|SwatchSelect|RangeSlider|ChipSelect|CcCycleButton)$/

/**
 * Controls that a tipped HEADING covers, instead of needing a tooltip of their own.
 *
 * A chip row is not one hit target, it is many small ones, and a tooltip anchored to the row renders
 * ON TOP of the chips — so the hover help hides the things you were about to click. That makes the
 * blanket "every settable control carries its own `v-tooltip`" rule actively wrong here, rather than
 * merely redundant (Dominik, 2026-08-07, seeing it on the channel selection).
 *
 * They are also always rendered under a label or heading that says what the set is — the param row's
 * label and its info icon, a section heading — and that is where the explanation belongs. So a chip
 * select counts as covered when a tipped label precedes it inside the same parent, which is the
 * ordinary label-then-control shape.
 *
 * PER-OPTION `tip`s COUNT AS COVERAGE TOO — a reversal of the rule below, and the reason the two
 * checks had to be amended together. The old reasoning ("they explain the individual choices, not
 * what the control is for") is right about a WORD-labelled row and exactly backwards about an
 * icon-only one, where the per-option tip is the only thing naming a glyph, and the group tooltip
 * that the presence rule demanded is the one that covers those glyphs on hover. Requiring the group
 * tooltip and forbidding the overlay cannot both hold; per-option tips are the coverage, and
 * `duplicateTooltips` reports the row that has both. A chip select with NEITHER is still reported.
 */
// `CcCycleButton` joined this set when the nested-tooltip sweep landed: it renders ONE button whose
// tooltip is the current option's `tip`, so per-option tips are its real hover help — and a
// `v-tooltip` on the component would sit on top of them, which is the double this set exists to
// catch. Same reasoning as ChipSelect, different widget.
const HEADING_COVERED = /^(?:ChipSelect|SwatchSelect|CcToggle|CcCycleButton)$/

/**
 * The tooltip EXPRESSION as written — `'Reload'`, `param.tip`, `` `${n} cells` `` — or `null`.
 *
 * Compared as source text, not as a rendered string, because the duplicate that matters is written
 * as one: `<label v-tooltip.left="param.tip">` above `<CcToggle v-tooltip.right="param.tip">` is the
 * same tip twice, and no parser has to evaluate anything to see it.
 */
const TOOLTIP_ATTR = /v-tooltip(?:\.[A-Za-z]+)*\s*=\s*"([^"]*)"/
const tooltipExpr = (attrs: string): string | null => TOOLTIP_ATTR.exec(attrs)?.[1]?.trim() ?? null

/**
 * Does this chip row's `:options` carry per-option `tip`s?
 *
 * The awkward half of the chip-row rule, and the reason per-option tips were excluded from coverage
 * to begin with: they are written in the SCRIPT (`const AXIS_OPTIONS = [{…, tip: '…'}]`, or a
 * `computed`) and reach the template as a bound identifier, so a template-only pass cannot see them.
 * This resolves the identifier back into the script block and looks for a `tip:` key.
 *
 * Deliberately conservative — `null` means "cannot tell", never "no tips". An identifier that is a
 * PROP, or options built somewhere this cannot follow, must not be reported as a violation on a
 * guess; a false positive here sends someone to delete a tooltip that was the only help there was.
 */
const OPTIONS_ATTR = /:options\s*=\s*"([^"]*)"/
const IDENTIFIER = /^[A-Za-z_$][\w$]*$/
// The next top-level binding — where a `const NAME = …` definition ends, for our purposes.
const NEXT_BINDING = /\n(?:const|let|var|function|async function|type|interface|export)\b/

export function hasPerOptionTips(script: string, attrs: string): boolean | null {
  const opts = OPTIONS_ATTR.exec(attrs)?.[1]?.trim()
  if (!opts) return null                                  // no `:options` binding at all
  if (!IDENTIFIER.test(opts)) return /\btip\s*:/.test(opts)   // an inline array literal
  const at = script.search(new RegExp(`\\b(?:const|let|var)\\s+${opts}\\b`))
  if (at < 0) return null                                 // a prop, an import — cannot tell
  const rest = script.slice(at + 1)
  const end = rest.search(NEXT_BINDING)
  return /\btip\s*:/.test(end < 0 ? rest : rest.slice(0, end))
}

/**
 * A `<button>` whose entire content is an icon — `<button><i class="pi pi-trash" /></button>`.
 *
 * This is the CellProfiler case at its purest: no caption, no value, nothing on screen to read, so a
 * tooltip is the ONLY thing standing between the user and guessing what a glyph does. It is also the
 * rule the codebase already follows without being asked — 139 of 150 icon-only buttons carried one
 * before this check existed, which is why the remaining 11 read as oversights rather than a style
 * this ratchet is imposing.
 *
 * Non-greedy, and `<button>` never nests, so it cannot run past its own close tag.
 *
 * `{{ … }}` IS a caption and must be left in. Stripping interpolation the way the copy extractors do
 * is wrong here: those measure text width, this asks whether the user sees any words at all, and
 * `<button><span>{{ group.heading }}</span><i class="pi-chevron-down" /></button>` renders a visible
 * label. Dropping it flagged four captioned buttons — sidebar group headings, a menu row labelled
 * `{{ pageIconFor()!.tip }}` — as bare icons.
 */
const BUTTON = /<button((?:[^>"']|"[^"]*"|'[^']*')*?)>([\s\S]*?)<\/button>/g
const isIconOnly = (body: string) =>
  /<i\b|\bpi-/.test(body) && !body.replace(/<[^>]*>/g, '').trim()

// A control the user never sets a value on, so there is nothing to explain.
const NOT_A_SETTING = /type\s*=\s*"(?:hidden|file|submit|button|reset)"/

// The primitives' OWN definitions. `CcToggle.vue` contains the `<input type="checkbox">` that every
// toggle in the app renders through; its tooltip belongs at the CALL SITE (that is what the file's
// own header comment says), so counting the internal input would report one permanent violation that
// no caller can fix. Same for the other wrappers.
const PRIMITIVE_SFC = /(?:^|\/)(?:CcToggle|ChipSelect|SwatchSelect|RangeSlider|CcCycleButton)\.vue$/

// Steps OVER quoted attribute values, so a `>` inside a handler (`@input="f(v => g(v))"`) doesn't
// end the tag early. Same hazard, and same fix, as the `TAG` regex used by `textStrings`.
const OPEN_TAG = /<(\/?)([A-Za-z][\w.-]*)((?:[^>"']|"[^"]*"|'[^']*')*?)(\/?)>/g

// HTML void elements never nest, so they must not be pushed onto the ancestor stack — an `<input>`
// left on it would swallow every following sibling into a phantom subtree.
const VOID = /^(?:area|base|br|col|embed|hr|img|input|link|meta|source|track|wbr)$/i

export interface UncoveredControl {
  /** Tag name as written — `select`, `CcToggle`, … */
  tag: string
  /** 1-based line within the file, so the report points at something you can open. */
  line: number
}

export interface DuplicateTooltip extends UncoveredControl {
  /** The redundant tooltip expression, as written. */
  tooltip: string
  /** Which double: it repeats the heading, or it sits on top of per-option tips. */
  why: 'heading' | 'per-option'
}

/**
 * The settable controls — and icon-only buttons — in an SFC with no hover help reachable from them.
 *
 * A control counts as COVERED when the tooltip is on the control itself **or on any element it sits
 * inside**. The ancestor rule is not a convenience — it is how most of this app is already written:
 *
 *     <label class="po-row" v-tooltip.left="'X tick-label angle'"><span>X angle</span>
 *       <input type="range" … /></label>
 *
 * The row carries the tooltip and the input is a child, so the user does get help on hover. Checking
 * the tag alone calls that a violation and over-reports by ~90% (155 hits against a true 82), which
 * is enough noise to make the whole signal ignorable — the same failure `tooltipStrings` avoids by
 * reading literals instead of expressions.
 *
 * ONLY `v-tooltip` COUNTS. A native `title=` is not coverage: it renders as the browser's own
 * tooltip — unstyled, slow to appear, invisible to the copy ratchets — so accepting it would let a
 * control satisfy this check while looking nothing like the rest of the app. (Most `title=` in the
 * codebase is a component PROP anyway — `BaseModal`, `ModulePage`, `ConfirmDeleteButton` — not a
 * native tooltip at all.)
 *
 * PER-OPTION `tip`s DO COUNT, for `HEADING_COVERED` controls only — see the note there for why that
 * reversed. `hasPerOptionTips` resolves the `:options` identifier back into the script to find them,
 * and answers `null` when it cannot follow the binding (a prop, an import), which counts as no
 * coverage rather than as a guess. Everything else — a plain `select`, an `input` — still needs its
 * own `v-tooltip`.
 *
 * @param src   full SFC source
 * @param path  the file's path — used only to skip the wrapper primitives' own definitions
 */
/**
 * `CcToggle`s with no accessible NAME — no `label` prop, no slot text, no `aria-label`.
 *
 * A tooltip is not a name. `CcToggle` renders a `<label>` wrapping a `display:none` checkbox, so with
 * the caption outside the component (`<span>Legend</span><CcToggle …/>`, which is how the plot options
 * and the movie rows are written) the control has NO text content and a screen reader announces an
 * unlabelled checkbox. 17 call sites were in that state when the `ariaLabel` prop was added.
 *
 * Deliberately NOT part of `scanTooltips`: that answers "can the user find out what this does on
 * hover", and its ancestor rule is what makes a captioned row count as covered. This asks a different
 * question — does the control carry its own name — and an ancestor cannot supply one.
 *
 * Scoped to `CcToggle` because it is the primitive whose real input is hidden. A native `<input>` or
 * `<select>` inside a `<label>` is named by that label without any help.
 */
export const unnamedToggles = (src: string, path = ''): UncoveredControl[] => {
  if (PRIMITIVE_SFC.test(path)) return []
  const tpl = src.match(/<template>([\s\S]*)<\/template>/)?.[1] ?? ''
  const out: UncoveredControl[] = []
  const re = /<CcToggle((?:[^>"']|"[^"]*"|'[^']*')*?)(\/?)>/g
  let m: RegExpExecArray | null
  while ((m = re.exec(tpl)) !== null) {
    const [, attrs, selfClosing] = m
    if (/(?:^|\s):?label\s*=|(?:^|\s):?aria-label\s*=/.test(attrs)) continue
    // slot text counts: `<CcToggle …>Use discrete GPU</CcToggle>` names the control the same way
    if (!selfClosing) {
      const close = tpl.indexOf('</CcToggle>', re.lastIndex)
      if (close !== -1 && tpl.slice(re.lastIndex, close).trim()) continue
    }
    out.push({ tag: 'CcToggle', line: tpl.slice(0, m.index).split('\n').length })
  }
  return out
}

export const uncoveredControls = (src: string, path = ''): UncoveredControl[] =>
  scanTooltips(src, path).uncovered

/**
 * Controls that repeat their heading's tooltip verbatim.
 *
 * The complement of `uncoveredControls`, and the same underlying fact seen from the other side: a
 * tipped heading COVERS a chip row / swatch / toggle, so a second identical tooltip on the control
 * adds nothing — and worse, it renders ON TOP of the control (Dominik, 2026-08-07: "it overlays the
 * toggle button"). The blanket "every settable control carries its own `v-tooltip`" rule produced
 * exactly this, so the two checks have to move together or fixing one re-breaks the other.
 *
 * Scoped to `HEADING_COVERED` — the controls where the tooltip lands on the hit target. A `<select>`
 * or `<input>` repeating its label is redundant but harmless, and widening this would turn a
 * "something is broken" signal into a style sweep.
 *
 * **This catches ONE of the two doubles a chip row can have, so a green run here does not mean there
 * are none.** The other is a group `v-tooltip` alongside per-option `tip`s, which say the same thing
 * in different words — no expression comparison can match them, and the per-option tips live in the
 * script where this template parser cannot see them. Seven such rows were measured on 2026-08-07.
 * Closing that gap reverses the "per-option tips don't count" paragraph on `uncoveredControls`, so it
 * is an amendment to both checks at once: `docs/TODO.md` → *Widen the chip-row tooltip rule*.
 */
export const duplicateTooltips = (src: string, path = ''): DuplicateTooltip[] =>
  scanTooltips(src, path).duplicates

/**
 * Tooltips whose HOVER AREAS nest: an element with its own `v-tooltip` inside an ancestor that also
 * has one. Hovering the inner control fires both, and they overlap on screen.
 *
 * A different failure from `duplicateTooltips`, which compares TEXT — here the two tips usually say
 * different things, so no expression comparison can see it. What makes it visible is the ancestor
 * stack this walker already keeps: `scanTooltips` was using a tipped ancestor only to mark the child
 * as *covered* (hover help is reachable, so don't report it as uncovered), which is exactly the
 * arrangement that produces the double. Same fact, opposite verdict, so it is read off the same walk.
 *
 * SIBLINGS are fine and are not reported: two tipped elements side by side can never both be hovered.
 * The fix is always to move the outer tip onto a leaf no control sits on top of — see `docs/UI.md`.
 */
export const nestedTooltips = (src: string, path = ''): DuplicateTooltip[] =>
  scanTooltips(src, path).nested

function scanTooltips(src: string, path = ''):
    { uncovered: UncoveredControl[]; duplicates: DuplicateTooltip[]; nested: DuplicateTooltip[] } {
  const none = { uncovered: [], duplicates: [], nested: [] }
  if (PRIMITIVE_SFC.test(path)) return none
  const tpl = src.match(/<template>([\s\S]*)<\/template>/)?.[1] ?? ''
  if (!tpl) return none

  // Blank out comments rather than deleting them, so offsets stay usable for line numbers.
  const clean = tpl.replace(/<!--[\s\S]*?-->/g, (m) => m.replace(/[^\n]/g, ' '))
  const script = src.match(/<script[^>]*>([\s\S]*?)<\/script>/)?.[1] ?? ''
  const tplAt = src.indexOf(tpl)
  const lineAt = (i: number) => src.slice(0, tplAt + i).split('\n').length

  // Which `<button>`s are icon-only, keyed by the offset of their `<`. Collected in a separate pass
  // because that verdict needs the element's CONTENT, which the tag walk below never sees — it reads
  // a stream of tags, not a tree.
  const iconButtonAt = new Set<number>()
  for (const b of clean.matchAll(BUTTON)) if (isIconOnly(b[2]!)) iconButtonAt.add(b.index!)

  const out: UncoveredControl[] = []
  const dupes: DuplicateTooltip[] = []
  const nested: DuplicateTooltip[] = []
  // `tippedSibling` is per-DEPTH: a tipped <label> marks the row it opens, and the chip select that
  // follows it inside the same parent is covered by it. Reset on entering/leaving a parent so a
  // heading cannot leak coverage into an unrelated block. `siblingExpr` carries that heading's
  // tooltip source alongside it, for the duplicate check below.
  const open: { tag: string; tipped: boolean; tippedSibling: boolean; siblingExpr: string | null }[] = []
  let tippedSibling = false
  let siblingExpr: string | null = null
  for (const m of clean.matchAll(OPEN_TAG)) {
    const [, closing, tag, attrs, selfClosing] = m
    if (closing) {
      // Pop to the matching open tag. An unclosed `<div>` (v-if branches, or a template this regex
      // mis-reads) would otherwise leave the stack wrong for the rest of the file.
      const at = open.map((e) => e.tag).lastIndexOf(tag!)
      if (at >= 0) {
        tippedSibling = open[at]!.tippedSibling
        siblingExpr = open[at]!.siblingExpr
        open.length = at
      }
      continue
    }
    const tipped = /v-tooltip/.test(attrs!)
    const settable = CONTROL.test(tag!) && !NOT_A_SETTING.test(attrs!)
    const perOption = HEADING_COVERED.test(tag!) ? hasPerOptionTips(script, attrs!) : null
    const covered = tipped || open.some((e) => e.tipped) ||
                    (HEADING_COVERED.test(tag!) && (tippedSibling || perOption === true))
    if ((settable || iconButtonAt.has(m.index!)) && !covered)
      out.push({ tag: tag!, line: lineAt(m.index!) })
    // Two ways a chip row ends up explained twice, and both put the second tooltip ON TOP of the
    // chips. (a) it repeats its heading word for word; (b) it sits over per-option tips that are
    // already naming each choice — the case an expression comparison can never see, because the two
    // say the same thing in different words.
    const expr = tipped ? tooltipExpr(attrs!) : null
    if (HEADING_COVERED.test(tag!) && expr !== null) {
      if (expr === siblingExpr)
        dupes.push({ tag: tag!, line: lineAt(m.index!), tooltip: expr, why: 'heading' })
      else if (perOption === true)
        dupes.push({ tag: tag!, line: lineAt(m.index!), tooltip: expr, why: 'per-option' })
    }
    // Hover areas that nest — read off the SAME ancestor stack `covered` uses above, with the
    // opposite verdict. Only a real ANCESTOR counts; `tippedSibling` is deliberately not consulted.
    if (tipped && open.some((e) => e.tipped))
      nested.push({ tag: tag!, line: lineAt(m.index!), tooltip: expr ?? '', why: 'heading' })
    if (tipped) { tippedSibling = true; siblingExpr = expr }
    if (!selfClosing && !VOID.test(tag!)) {
      // INHERITED into the child scope, not reset: the heading is the param row's label and the
      // chips often sit one wrapper deeper (`channel-select-wrap`). Restored on the close tag, so a
      // heading covers its own row and nothing after it.
      open.push({ tag: tag!, tipped, tippedSibling, siblingExpr })
    }
  }
  return { uncovered: out, duplicates: dupes, nested }
}

// Task-JSON `tip` fields carry the same budget, but they are backend files (`app/src/tasks/**`) and
// the frontend never owns a copy of a task spec. That half of the ratchet lives with them, in
// `app/test/runtests.jl` — keeping this suite to `frontend/src` and free of node builtins (reading
// outside the Vite root would mean adding `@types/node` for one glob). The task-spec COVERAGE rule
// ("every leaf param carries a `tip`") lives there too, for the same reason.
//
// The whole-corpus view across all three sources — SFCs, task specs and Julia QC text — is
// `scripts/ui_copy_inventory.mjs`, which imports this module rather than re-implementing it.
