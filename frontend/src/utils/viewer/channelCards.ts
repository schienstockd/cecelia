// INVENTORY-EXEMPT: viewer-only helper for the Channels section's compact/expand cards; no other panel lists channels this way.
/** Compact vs expanded channel cards in the viewer's Channels section. Pure — the viewer owns the refs.
 *
 *  `allCompact` is the section-wide mode (the compact-all / expand-all button); `flipped` holds the
 *  channels the user opened or closed against it one at a time. Changing the mode clears the flips, so
 *  "expand all" really does expand every card. */

/** Above this many channels the cards start compact — past ~8 the histograms push most of the list
 *  off screen. Until the user picks a mode for the image with the compact-all button. */
export const COMPACT_ABOVE = 8

export function defaultCompact(nChannels: number): boolean {
  return nChannels > COMPACT_ABOVE
}

export function isCompact(allCompact: boolean, flipped: ReadonlySet<number>, c: number): boolean {
  return allCompact !== flipped.has(c)
}

/** `flipped` with channel `c` flipped — a new set, so a `ref<Set>` assignment triggers. */
export function toggleFlip(flipped: ReadonlySet<number>, c: number): Set<number> {
  const next = new Set(flipped)
  if (next.has(c)) next.delete(c)
  else next.add(c)
  return next
}

/** What the compact-all / expand-all button does next: compact everything unless every card already is. */
export function nextAllCompact(allCompact: boolean, flipped: ReadonlySet<number>, n: number): boolean {
  for (let c = 0; c < n; c++) if (!isCompact(allCompact, flipped, c)) return true
  return false
}
