// Small regex builder for the Metadata panel's "extract via regex" — most people only need "split
// the filename by a separator and take the Nth (or last) field". This generates the regex SOURCE
// string for that case (which is written into the visible regex field, so the user sees/edits the
// real thing and learns), plus a preview that mirrors how the panel applies it.

export type FieldPos = 'first' | 'second' | 'third' | 'last'

const SPECIAL = /[.*+?^${}()|[\]\\]/g
/** Escape a char for use OUTSIDE a character class (as a literal). */
function escLiteral(c: string): string { return c.replace(SPECIAL, '\\$&') }
/** Escape a char for use INSIDE a [...] class. */
function escClass(c: string): string { return c.replace(/[\]^\\-]/g, '\\$&').replace(/\./g, '\\.') }

const POS_INDEX: Record<Exclude<FieldPos, 'last'>, number> = { first: 1, second: 2, third: 3 }

/**
 * Regex source capturing the chosen field of `sample.split(sep)`. `stripExt` excludes a trailing
 * `.extension` from the captured token (so "Image2-testB.tif" split by "-", last → "testB", not
 * "testB.tif"). Returns "" for an empty separator. The captured value is the first group.
 */
export function buildFieldRegex(sep: string, pos: FieldPos, stripExt: boolean): string {
  if (!sep) return ''
  const sc   = escClass(sep)
  const sl   = escLiteral(sep)
  const cap  = stripExt ? `[^${sc}.]+` : `[^${sc}]+`   // captured token (optionally dot-free)
  const any  = `[^${sc}]*`                              // a whole skipped field
  const tail = stripExt ? '(?:\\.[^.]+)?' : ''         // optional trailing .ext to drop
  if (pos === 'last') return `(?:^|${sl})(${cap})${tail}$`
  const n = POS_INDEX[pos]
  return '^' + `${any}${sl}`.repeat(n - 1) + `(${cap})`
}

// ── Look-around builder ─────────────────────────────────────────────────────────
// The other common case: "the digits after M", "the letter after the number" — i.e. extract a token
// bounded by context on one/both sides, without capturing that context. Generated as a zero-width
// lookbehind `(?<=…)` / lookahead `(?=…)` around the token, so the whole match IS the wanted value
// (e.g. "M1a" → (?<=M)\d+ → "1", (?<=\d)[a-z]+ → "a").

// A context side (lookbehind/lookahead) is a literal `text` AND/OR a class that VARIES — combined
// `text + class` so you can anchor on "M then a number" for M1b/M2a/M4f: text "M" + class 'digits'
// → `M\d+` → `(?<=M\d+)`. Either half may be empty.
export type CtxClass = 'none' | 'digits' | 'letters' | 'lower' | 'upper'
export type ExtractKind = 'digits' | 'letters' | 'lower' | 'upper' | 'word' | 'custom'
export interface Ctx { text: string; cls: CtxClass }

const CLASS_SRC: Record<'digits' | 'letters' | 'lower' | 'upper' | 'word', string> = {
  digits: '\\d+', letters: '[A-Za-z]+', lower: '[a-z]+', upper: '[A-Z]+', word: '\\w+',
}

function ctxSrc(c: Ctx): string {
  return escLiteral(c.text.trim()) + (c.cls === 'none' ? '' : CLASS_SRC[c.cls])
}
// The captured token: a class shortcut, or a raw custom pattern (advanced — not escaped).
function extractSrc(kind: ExtractKind, custom: string): string {
  return kind === 'custom' ? custom.trim() : CLASS_SRC[kind]
}

/**
 * Regex source that matches `extract` only when preceded by `before` and/or followed by `after`,
 * using zero-width lookbehind/lookahead so the match equals the token. Each context side is a
 * `text + class` combination (either may be empty). Returns "" if the token is empty.
 */
export function buildLookaroundRegex(before: Ctx, extract: { kind: ExtractKind; text: string }, after: Ctx): string {
  const tok = extractSrc(extract.kind, extract.text)
  if (!tok) return ''
  const lb = ctxSrc(before)
  const la = ctxSrc(after)
  return (lb ? `(?<=${lb})` : '') + tok + (la ? `(?=${la})` : '')
}

/**
 * Apply a regex source to a sample; returns capture group 1 if the pattern has one, else the whole
 * match, else "" (no match / invalid). Mirrors the panel's extraction so preview == applied result.
 */
export function extractWith(regexSrc: string, sample: string): string {
  if (!regexSrc) return ''
  let re: RegExp
  try { re = new RegExp(regexSrc) } catch { return '' }
  const m = sample.match(re)
  if (!m) return ''
  return m[1] ?? m[0]
}
