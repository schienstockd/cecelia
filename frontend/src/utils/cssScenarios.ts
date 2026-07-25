// Re-implemented-scenario detector.
//
// `docs/UI.md` says: use `.cc-muted` / `.cc-empty` / `.cc-eyebrow` / `.cc-card` rather than declaring
// the same role again in scoped CSS. That rule was only ever enforced by review — and review across
// fresh context windows is exactly what kept failing (see docs/todo/UX_PRIMITIVES_PLAN.md). So the
// scenarios are detectable here instead: a scoped rule that spells out the *defining* declarations of
// a canonical utility IS that utility, hand-rolled.
//
// Deliberately narrow. Each matcher keys on the combination that makes the role unambiguous, not on a
// single property — plenty of rules legitimately set only a colour or only a size. False positives are
// worse than misses here, because the allow-list they'd force is where this kind of check rots.

// NOT linted: `card`. Measured it — `surface + 1px border + radius` is the shape of at least five
// different roles (card, input, chip, badge, icon-button), and scoped CSS carries nothing that tells
// them apart; ~60% of the matches wanted `.cc-btn`/`ChipSelect`/the global input base rather than
// `.cc-card`. A check that wrong either gets ignored or grows an allow-list that stops meaning
// anything, so card chrome stays a review-time rule in docs/UI.md.
export type Scenario = 'muted' | 'empty' | 'eyebrow'

export interface CssRule {
  selector: string
  body:     string
}

/** Contents of every `<style>` block in an SFC (or the whole text for a plain .css file). */
export function styleBlocks(text: string): string[] {
  if (!text.includes('<style')) return [text]
  const out: string[] = []
  for (const m of text.matchAll(/<style[^>]*>([\s\S]*?)<\/style>/g)) out.push(m[1])
  return out
}

/**
 * Split CSS into `{selector, body}` rules. Bodies that themselves contain a block (`@media`,
 * `@supports`) are recursed into, so rules nested in an at-rule are still seen.
 */
export function cssRules(css: string): CssRule[] {
  const out: CssRule[] = []
  // Block comments only — `//` is not a CSS comment, and stripping it would eat `url(//…)`.
  css = css.replace(/\/\*[\s\S]*?\*\//g, ' ')
  let selStart = 0, depth = 0, bodyStart = -1

  for (let i = 0; i < css.length; i++) {
    const ch = css[i]
    if (ch === '{') {
      if (depth === 0) bodyStart = i
      depth++
    } else if (ch === '}') {
      depth--
      if (depth === 0) {
        const selector = css.slice(selStart, bodyStart).trim()
        const body     = css.slice(bodyStart + 1, i)
        if (body.includes('{')) out.push(...cssRules(body))   // at-rule wrapper
        else if (selector && !selector.startsWith('@')) out.push({ selector, body })
        selStart = i + 1
      }
    }
  }
  return out
}

const has = (body: string, re: RegExp) => re.test(body)

const DIM_COLOUR   = /color:\s*var\(--cc-text-dim\)/
// ANY font-size, token-valued included: `color: dim` + `font-size: var(--cc-fs-sm)` is still
// `.cc-muted` spelled out longhand. Using the scale tokens is necessary, not sufficient.
const ANY_SIZE     = /font-size:/
const UPPERCASE    = /text-transform:\s*uppercase/
const TRACKING     = /letter-spacing:/

// A dim colour + a size also describes every ghost/icon BUTTON in the app — whose canonical form is
// `.cc-btn-ghost`, not `.cc-muted`. So the text scenarios only fire on rules that are purely text:
// nothing interactive, no box chrome, and not a control-shaped selector.
const INTERACTIVE  = /cursor:|background(-color)?:|border(-\w+)?:|transition:|:hover|appearance:/
const CONTROL_NAME = /(btn|button|toggle|tab|chip|input|select|gear|caret|swatch)\b/i

const isTextOnly = (rule: CssRule) =>
  !INTERACTIVE.test(rule.body) && !CONTROL_NAME.test(rule.selector)

/**
 * Which canonical utility this rule re-implements, if any.
 *
 * - muted   dim colour + a hard-coded size = `.cc-muted` (+ a density modifier)
 * - empty   an `*-empty*` selector re-declaring the dim colour = `.cc-empty*`
 * - eyebrow uppercase + tracking + dim = `.cc-eyebrow`
 *
 * All three key on the dim colour, because that is what actually marks a rule as *owning* the role.
 * A child rule inside an adopted empty (`.foo-empty p { margin: 0; font-size: 0.8rem }`) is styling
 * its own contents, not re-implementing the scenario, and must not be flagged.
 */
export function scenarioFor(rule: CssRule): Scenario | null {
  const { selector, body } = rule
  if (!isTextOnly(rule) || !has(body, DIM_COLOUR)) return null

  if (has(body, UPPERCASE) && has(body, TRACKING)) return 'eyebrow'
  if (/-empty|empty-/.test(selector))              return 'empty'
  if (has(body, ANY_SIZE))                         return 'muted'
  return null
}

// ── Icon-only buttons ─────────────────────────────────────────────────────────────────────────────

/** A `<button>` whose entire content is one icon, and which isn't using the `.cc-btn` family. */
export interface IconButton {
  path:      string
  classAttr: string
}

// A button whose whole content is a single <i>. Style blocks and HTML comments are removed first — a
// usage example inside a doc comment is not a call site (ConfirmButton's docs read as one).
const ICON_BUTTON = /<button\b([^>]*)>\s*<i\b[^>]*\/>\s*<\/button>/g

/**
 * Icon-only buttons not built from `.cc-btn`. Measured before unifying: 116 sites carried 60 distinct
 * class names, but only TWO shapes (boxed / bare) and four size steps — so the canonical form is
 * `.cc-btn` + `-bare`|`-ghost` + `-icon` (+ a size step), not a per-file class.
 */
export function findHandRolledIconButtons(
  sources: Array<{ path: string, text: string }>,
): IconButton[] {
  const out: IconButton[] = []
  for (const { path, text } of sources) {
    const template = text
      .replace(/<style[^>]*>[\s\S]*?<\/style>/g, ' ')
      .replace(/<!--[\s\S]*?-->/g, ' ')
    for (const m of template.matchAll(ICON_BUTTON)) {
      const cls = /\bclass="([^"]*)"/.exec(m[1])
      // no class at all is also hand-rolled — it renders as a raw browser button
      if (!cls || !/\bcc-btn\b/.test(cls[1])) out.push({ path, classAttr: cls?.[1] ?? '(no class)' })
    }
  }
  return out
}

// ── Raw values ────────────────────────────────────────────────────────────────────────────────────

/** A literal `font-size` / `border-radius` where a scale token exists. */
export interface RawValue {
  path:     string
  selector: string
  decl:     string
}

// Above this the value is display type / a bespoke geometry, not a step on the small-text scale.
const DISPLAY_PX = 15.0

const toPx = (v: number, unit: string) => (unit === 'rem' ? v * 16 : v)

/**
 * Literal sizes and radii still hand-written in scoped CSS. The scales in `style.css` carry every tier
 * the app actually uses, so a literal here is a value that won't track the system — the same class of
 * defect as a dead token, just silent instead of broken.
 *
 * Exempt: display type (> 15px), pill radii (>= 100), `0`, and `em` — an `em` size is deliberately
 * relative to its container (a legend that scales with the export), which is the opposite of a
 * hand-written constant, and a fixed token would break it.
 */
export function findRawValues(sources: Array<{ path: string, text: string }>): RawValue[] {
  const out: RawValue[] = []
  const LITERAL = /(font-size|border-radius):\s*([0-9.]+)(rem|px|em|%)/g

  const keep = (prop: string, num: number, unit: string) =>
    !(num === 0 || unit === '%' || unit === 'em' || num >= 100) &&
    !(prop === 'font-size' && toPx(num, unit) > DISPLAY_PX)

  for (const { path, text } of sources) {
    for (const block of styleBlocks(text)) {
      for (const rule of cssRules(block)) {
        for (const m of rule.body.matchAll(LITERAL)) {
          if (keep(m[1], parseFloat(m[2]), m[3])) out.push({ path, selector: rule.selector, decl: m[0] })
        }
      }
    }
    // Inline `style="font-size:0.7rem"` in the TEMPLATE is just as hand-rolled, and lives outside every
    // <style> block — a blind spot that hid four icon sizes. Inline declarations can reference the
    // tokens too (they inherit from :root), so there's no reason to exempt them.
    const template = text.replace(/<style[^>]*>[\s\S]*?<\/style>/g, ' ')
    for (const m of template.matchAll(LITERAL)) {
      if (keep(m[1], parseFloat(m[2]), m[3])) out.push({ path, selector: '(inline style)', decl: m[0] })
    }
  }
  return out
}

export interface ScenarioHit {
  path:     string
  selector: string
  scenario: Scenario
}

/** Every rule across the given sources that re-implements a canonical scenario. */
export function findReimplementedScenarios(
  sources: Array<{ path: string, text: string }>,
): ScenarioHit[] {
  const out: ScenarioHit[] = []
  for (const { path, text } of sources) {
    for (const block of styleBlocks(text)) {
      for (const rule of cssRules(block)) {
        const scenario = scenarioFor(rule)
        if (scenario) out.push({ path, selector: rule.selector, scenario })
      }
    }
  }
  return out
}

/** Stable `path::selector` key, for comparing against an allow-list. */
export const hitKey = (h: ScenarioHit) => `${h.path}::${h.selector}`
