// Design-token integrity check.
//
// Every `--cc-*` custom property is declared once in `style.css` (the design system) and referenced
// from scoped component CSS. A reference to a token that was never declared is silently broken: the
// declaration becomes "invalid at computed-value time", so the property falls back to its `unset`
// value rather than to anything sensible. With a fallback (`var(--cc-text-muted, #888)`) that shows
// up as a slightly-wrong hard-coded colour that never tracks the theme; WITHOUT one it drops the
// property entirely — e.g. `background: var(--cc-surface)` on a `<select>` reset both the fill and
// the global custom-caret background-image, leaving an arrowless, transparent dropdown.
//
// Neither failure mode throws, so nothing catches it at build time. Hence this checker, run over the
// real sources by `cssTokens.test.ts`. Only `--cc-*` is checked — `--p-*` (PrimeVue) and other
// vendor tokens are declared outside our stylesheet.

// PrimeVue declares its own `--p-*` tokens in vendor CSS we don't parse, so they're out of scope.
const VENDOR = /^--p-/

/**
 * Strip block, HTML and line comments — token-shaped text in prose or in a commented-out line is not
 * a live reference. Line comments are matched only when `//` isn't preceded by `:`, so a `https://`
 * URL doesn't truncate the rest of its line.
 */
export function stripComments(text: string): string {
  return text
    .replace(/\/\*[\s\S]*?\*\//g, ' ')
    .replace(/<!--[\s\S]*?-->/g, ' ')
    .replace(/(^|[^:])\/\/[^\n]*/g, '$1')
}

/**
 * Token names DECLARED in a source, i.e. appearing on the left of `--foo: value`.
 *
 * Scans the whole file, not just its `<style>` block, because a component may legitimately declare a
 * custom property inline for a dynamic value — `:style="{ '--gate-font': \`${fontSize}px\` }"` — and
 * those are real declarations. Hence the optional quote before the colon.
 */
export function definedTokens(css: string): Set<string> {
  const out = new Set<string>()
  for (const m of stripComments(css).matchAll(/(--[A-Za-z0-9_-]+)['"`]?\s*:/g)) out.add(m[1])
  return out
}

export interface TokenRef {
  token:       string
  hasFallback: boolean
}

/** Token names REFERENCED via `var(--foo)` / `var(--foo, fallback)`. */
export function referencedTokens(text: string): TokenRef[] {
  const out: TokenRef[] = []
  for (const m of stripComments(text).matchAll(/var\(\s*(--[A-Za-z0-9_-]+)\s*(,?)/g)) {
    if (!VENDOR.test(m[1])) out.push({ token: m[1], hasFallback: m[2] === ',' })
  }
  return out
}

export interface DeadTokenRef extends TokenRef {
  path: string
}

/**
 * References to custom properties that nothing declares — neither the global stylesheet nor the
 * referencing file itself (a component-local token, static or inline, is perfectly valid).
 *
 * @param styleCss  contents of the stylesheet that owns the global tokens (`style.css`)
 * @param sources   every file that may reference tokens (including the stylesheet itself)
 */
export function findDeadTokenRefs(
  styleCss: string,
  sources: Array<{ path: string, text: string }>,
): DeadTokenRef[] {
  const global = definedTokens(styleCss)
  const out: DeadTokenRef[] = []
  for (const { path, text } of sources) {
    const local = definedTokens(text)
    for (const ref of referencedTokens(text)) {
      if (!global.has(ref.token) && !local.has(ref.token)) out.push({ path, ...ref })
    }
  }
  return out
}
