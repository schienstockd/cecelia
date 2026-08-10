// Small regex builder for the Metadata panel's "extract via regex" — most people only need "split
// the filename by a separator and take the Nth (or last) field". This generates the regex SOURCE
// string for that case (which is written into the visible regex field, so the user sees/edits the
// real thing and learns), plus a preview that mirrors how the panel applies it.

export type FieldPos = 'first' | 'second' | 'third' | 'thirdLast' | 'secondLast' | 'last'

const SPECIAL = /[.*+?^${}()|[\]\\]/g
/** Escape a char for use OUTSIDE a character class (as a literal). */
function escLiteral(c: string): string { return c.replace(SPECIAL, '\\$&') }
/** Escape a char for use INSIDE a [...] class. */
function escClass(c: string): string { return c.replace(/[\]^\\-]/g, '\\$&').replace(/\./g, '\\.') }

const FROM_START: Record<'first' | 'second' | 'third', number>      = { first: 1, second: 2, third: 3 }
const FROM_END:   Record<'last' | 'secondLast' | 'thirdLast', number> = { last: 1, secondLast: 2, thirdLast: 3 }
const isFromEnd = (p: FieldPos): p is 'last' | 'secondLast' | 'thirdLast' => p in FROM_END

/**
 * Regex source capturing the chosen field of `sample.split(sep)`. `sep` is one or more separator
 * CHARACTERS — any of them splits (so the folder separator can be `/\` and cover both platforms).
 * `stripExt` excludes a trailing `.extension` from the captured token (so "Image2-testB.tif" split
 * by "-", last → "testB", not "testB.tif"). Returns "" for an empty separator. The captured value
 * is the first group.
 *
 * Positions count from the start (first/second/third) OR from the end (last/secondLast/thirdLast).
 * From-the-end matters for the *path* source: an absolute path has a variable number of leading
 * folders, so "the folder the image sits in" (…/20260714/M1b-MERTK.ori → 20260714) is only
 * reachable as the 2nd-last field.
 *
 * `stripExt` applies to the LAST field only — that is the one an extension can be on. Anywhere else
 * it would just mangle a legitimately dotted token: a `2026.07.16` date folder taken as the 2nd-last
 * field of a path matched nothing at all while the dot-free token class was applied to it.
 */
export function buildFieldRegex(sep: string, pos: FieldPos, stripExt: boolean): string {
  if (!sep) return ''
  const chars = [...new Set(sep.split(''))]
  const sc   = chars.map(escClass).join('')                            // inside [...]
  const sl   = chars.length === 1 ? escLiteral(chars[0]) : `[${sc}]`   // as a literal
  const ext  = stripExt && pos === 'last'              // only the last field carries the extension
  const cap  = ext ? `[^${sc}.]+` : `[^${sc}]+`        // captured token (optionally dot-free)
  const any  = `[^${sc}]*`                              // a whole skipped field
  const tail = ext ? '(?:\\.[^.]+)?' : ''              // optional trailing .ext to drop
  if (isFromEnd(pos)) {
    const k = FROM_END[pos]
    return `(?:^|${sl})(${cap})${tail}` + `(?:${sl}${any})`.repeat(k - 1) + '$'
  }
  const n = FROM_START[pos]
  return '^' + `${any}${sl}`.repeat(n - 1) + `(${cap})`
}

// ── What the regex runs on ──────────────────────────────────────────────────────

export type RegexSource = 'name' | 'path'

/**
 * The string the regex is applied to. `path` means the image's ORIGINAL source location (`oriPath`,
 * from `meta.ori_path`) — deliberately NOT `filepath`, which is the *converted* OME-Zarr filename
 * inside the project (`ccidImage.ome.zarr`, `ccidDriftCorrected.ome.zarr`, …) and carries none of
 * the acquisition information the user is extracting. The point of the path source is the upstream
 * folders: `…/20260714/M1b-MERTK.ori` → the imaging date.
 *
 * Falls back to the name when the image has no recorded source path (pre-`ori_path` data); the
 * panel's live preview shows the string that will actually be matched.
 */
export function regexSampleFor(
  img: { name: string; oriPath?: string | null }, source: RegexSource,
): string {
  return source === 'path' ? (img.oriPath || img.name) : img.name
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
