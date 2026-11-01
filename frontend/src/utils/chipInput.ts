/**
 * A list of short, user-invented words typed as chips (`components/ChipInput.vue`), stored as ONE
 * comma-separated string so a task param stays a plain `text` value — the parse rule below matches
 * the backend's (`roi_review_labels` in app/src/tasks/spatialAnalysis/roiDiscovery.jl): split on
 * commas, trim, drop empties and repeats, keep the order.
 */

/** The chips a comma-separated string holds. */
export function parseChips(text: string | null | undefined): string[] {
  return addChips([], text ?? '')
}

/** The stored form of a chip list. */
export function joinChips(chips: string[]): string {
  return chips.join(', ')
}

/** `chips` plus every new word in `raw` (which may itself hold commas, e.g. a paste); `max` caps the
 *  list, and a word already present is not added twice. */
export function addChips(chips: string[], raw: string, max?: number): string[] {
  const out = [...chips]
  for (const part of raw.split(',')) {
    const w = part.trim()
    if (!w || out.includes(w)) continue
    if (max !== undefined && out.length >= max) break
    out.push(w)
  }
  return out
}

/** What a keystroke in the chip field's text box means: commit the draft (Enter, or a typed comma),
 *  take the last chip back (Backspace in an empty box), or nothing. */
export function chipKeyAction(key: string, draft: string): 'commit' | 'pop' | null {
  if (key === 'Enter' || key === ',') return 'commit'
  if (key === 'Backspace' && draft === '') return 'pop'
  return null
}
