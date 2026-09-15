// Normalises a filesystem path pasted into a text input: trims, then strips a matched pair of
// surrounding straight/curly quotes. macOS Finder's "Copy as Pathname" wraps the result in single
// quotes, and users paste that verbatim into the legacy-migrate dialog — the literal quotes then
// become part of the string sent to the server, which turns "spaces in the path" into "path does
// not exist".
const QUOTE_PAIRS: Array<[string, string]> = [
  ["'", "'"],
  ['"', '"'],
  ['‘', '’'],  // curly single ‘ ’
  ['“', '”'],  // curly double “ ”
]

export function cleanPathInput(raw: string): string {
  const t = raw.trim()
  if (t.length < 2) return t
  for (const [open, close] of QUOTE_PAIRS) {
    if (t.startsWith(open) && t.endsWith(close)) return t.slice(open.length, t.length - close.length)
  }
  return t
}
