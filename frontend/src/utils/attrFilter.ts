// Filtering a list of rows by their user-defined ATTRIBUTES — the "which of these are the controls at
// 4h" question, asked with chips rather than by reading down a column.
//
// Extracted from `ModuleLayout.vue`, where it was the image table's alone: the chip rows, the
// draft/applied split, the invert, and the matching clause were all inline, tied to `activeSet.images`.
// Nothing about any of it is about images — it needs a bag of `attr` values and nothing else — and the
// Movies page wants the same question asked of movies joined back to their image. So the rule lives
// here (pure, tested), the chrome lives in `components/AttrFilterPanel.vue`, and both pages use them.
//
// The DRAFT/APPLIED split is the part worth naming. Picking chips does not narrow the list; Apply does.
// That is deliberate — narrowing on every chip click makes assembling a multi-attribute filter a fight,
// because each partial selection hides the rows the next chip would have come from.

/** Anything carrying a user-attribute bag — an image, or a movie row joined to one. */
export interface AttrBearing {
  attr?: Record<string, string> | null
}

export interface AttrFilterState {
  /** what the chips currently hold — no effect on the list until applied */
  draft: Record<string, string[]>
  /** what actually narrows it */
  applied: Record<string, string[]>
  /** show the rows that do NOT match */
  invert: boolean
}

export const emptyAttrFilter = (): AttrFilterState => ({ draft: {}, applied: {}, invert: false })

/** Every attribute key present across the rows, sorted — one filter row (and one column) each. */
export function attrKeysOf(rows: readonly AttrBearing[]): string[] {
  const keys = new Set<string>()
  for (const r of rows) for (const k of Object.keys(r.attr ?? {})) keys.add(k)
  return [...keys].sort()
}

/** Key → the distinct values in use, sorted. The chips a user can pick from, and nothing beyond them. */
export function attrValueMap(rows: readonly AttrBearing[]): Record<string, string[]> {
  const map: Record<string, Set<string>> = {}
  for (const r of rows)
    for (const [k, v] of Object.entries(r.attr ?? {})) {
      if (!map[k]) map[k] = new Set()
      if (v != null) map[k].add(String(v))
    }
  return Object.fromEntries(Object.entries(map).map(([k, s]) => [k, [...s].sort()]))
}

/**
 * The chips for one attribute key.
 *
 * A BLANK value is a legitimate filter — "which of these did I never annotate?" — but its chip renders
 * as an unlabelled pill, because `ChipSelect` hides the label span when the label is `''` (that is how
 * icon-only chips work). So the value is kept, since the filter matches on it, and the label is given
 * something to show.
 *
 * Structurally a `ChipOption`; typed inline rather than imported so this module stays free of the
 * component layer (pure logic in `utils/*` — docs/DEV.md → Tests).
 */
export function attrChipOptions(key: string, values: readonly string[]):
    Array<{ value: string; label: string; tip: string }> {
  return values.map(v => v.trim() === ''
    ? { value: v, label: '—', tip: `No ${key} set` }
    : { value: v, label: v, tip: v })
}

/** Is anything actually narrowing the list? A draft nobody applied is not. */
export const attrFilterActive = (s: AttrFilterState): boolean =>
  Object.values(s.applied).some(v => v.length > 0)

/** Has the user picked chips that Apply would do something with? */
export const attrFilterDrafted = (s: AttrFilterState): boolean =>
  Object.values(s.draft).some(v => v.length > 0)

/** The draft promoted to applied, dropping the keys nothing was picked for. */
export const applyAttrFilter = (s: AttrFilterState): AttrFilterState => ({
  ...s,
  applied: Object.fromEntries(Object.entries(s.draft).filter(([, v]) => v.length > 0)),
})

/**
 * The filter with values that no longer exist anywhere dropped.
 *
 * A persisted filter outlives the rows it was picked against — open a different project and
 * `Treatment: MERTK` matches nothing, so the list is empty with only the Filter button to say why. That
 * is indistinguishable from "there is nothing here". Pruning turns it back into no filter at all.
 *
 * Returns the SAME object when nothing changed, so a caller can watch and write only on a real change.
 */
export function pruneAttrFilter(s: AttrFilterState, rows: readonly AttrBearing[]): AttrFilterState {
  const live = attrValueMap(rows)
  const prune = (m: Record<string, string[]>) => {
    const out: Record<string, string[]> = {}
    for (const [k, vals] of Object.entries(m)) {
      const kept = vals.filter(v => live[k]?.includes(v))
      if (kept.length) out[k] = kept
    }
    return out
  }
  const draft = prune(s.draft), applied = prune(s.applied)
  const same = (a: Record<string, string[]>, b: Record<string, string[]>) =>
    JSON.stringify(a) === JSON.stringify(b)
  return same(draft, s.draft) && same(applied, s.applied) ? s : { ...s, draft, applied }
}

/**
 * Does one row pass?
 *
 * ALL keys must match (a row is "control AND 4h"), ANY value within a key (picking two values of one
 * attribute means either) — the reading a row of chips per attribute gives. A missing attribute reads
 * as the empty string, which is what makes the blank chip above work.
 *
 * `invert` flips the whole verdict, not each clause: "show me everything that is NOT control at 4h".
 */
export function matchesAttrFilter(attr: Record<string, string> | null | undefined,
                                  s: AttrFilterState): boolean {
  if (!attrFilterActive(s)) return true
  const matches = Object.entries(s.applied)
    .every(([key, vals]) => vals.includes(String(attr?.[key] ?? '')))
  return s.invert ? !matches : matches
}
