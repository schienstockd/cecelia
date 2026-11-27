/**
 * The ONE free-text search rule for list filters: every whitespace-separated word of `query` must
 * appear (case-insensitive substring) somewhere in `fields`. An empty query matches everything.
 * Shared by the task manager, model vault and project manager searches so every list answers the
 * same query the same way.
 */
export function matchesQuery(fields: readonly (string | null | undefined)[], query: string): boolean {
  const words = query.toLowerCase().split(/\s+/).filter(Boolean)
  if (!words.length) return true
  const hay = fields.filter(Boolean).join(' ').toLowerCase()
  return words.every(w => hay.includes(w))
}

export interface WordRules {
  /** filler words skipped, so "mean velocity between treatments" or "where is the …?" still match */
  stop?: ReadonlySet<string>
  /** a word may instead be met by any of these */
  synonyms?: Readonly<Record<string, readonly string[]>>
  /** a query of ONLY filler words: match everything (a picker that lists it all), or match those
   *  words as typed (a jump box, where "the" listing everything is noise). Default: as typed. */
  onlyFiller?: 'all' | 'as-typed'
}

/**
 * `matchesQuery` for a typed question, applied per word: trailing punctuation is dropped, filler
 * words are skipped, and a word can be met by one of its synonyms. Each caller brings its own lists —
 * the plot picker's (`plots/plotPicker.ts`) and the jump search's (`utils/jumpSearch.ts`).
 */
export function matchesWords(fields: readonly (string | null | undefined)[], query: string, rules: WordRules = {}): boolean {
  const words = query.toLowerCase().split(/\s+/).map(w => w.replace(/[?!.,;:]+$/, '')).filter(Boolean)
  const kept = rules.stop ? words.filter(w => !rules.stop!.has(w)) : words
  if (!kept.length && rules.onlyFiller === 'all') return true
  return (kept.length ? kept : words).every(w => [w, ...(rules.synonyms?.[w] ?? [])].some(t => matchesQuery(fields, t)))
}
