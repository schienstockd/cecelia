// Open or close a persisted `CollapsibleSection` from outside it, and ask a page to show its plots.
//
// A section with a `storageKey` reads its open state from localStorage ONCE, when it mounts — so a
// caller that wanted the image table folded away for it (Kiwi pointing at a plot below the table,
// 2026-09-24: "the first thing I see is a selected image on the table and I have to scroll down")
// had no way in. `setSectionOpen` writes the same key and tells any mounted section with it to follow.
//
// `revealPlots` is the page-level ask built on it: `ModuleLayout` answers by folding its Images section
// and opening its Plots section — the layout owns those keys, so no caller has to know them.

const SECTION_EVENT = 'cc:section-open'
const REVEAL_PLOTS_EVENT = 'cc:reveal-plots'

export interface SectionOpenDetail { key: string; open: boolean }

/** Set a persisted section open/closed — now if it is mounted, on its next mount otherwise. */
export function setSectionOpen(key: string, open: boolean): void {
  try { localStorage.setItem(key, open ? '1' : '0') } catch { /* private mode — the event still applies */ }
  window.dispatchEvent(new CustomEvent<SectionOpenDetail>(SECTION_EVENT, { detail: { key, open } }))
}

/** Listen for `setSectionOpen` on one key. Returns the unsubscribe. */
export function onSectionOpen(key: string, apply: (open: boolean) => void): () => void {
  const h = (e: Event) => {
    const d = (e as CustomEvent<SectionOpenDetail>).detail
    if (d?.key === key) apply(d.open)
  }
  window.addEventListener(SECTION_EVENT, h)
  return () => window.removeEventListener(SECTION_EVENT, h)
}

/** Ask the current page to bring its plots into view (fold its image table, open its plots). */
export function revealPlots(): void {
  window.dispatchEvent(new CustomEvent(REVEAL_PLOTS_EVENT))
}

/** Answer `revealPlots`. Returns the unsubscribe. */
export function onRevealPlots(apply: () => void): () => void {
  window.addEventListener(REVEAL_PLOTS_EVENT, apply)
  return () => window.removeEventListener(REVEAL_PLOTS_EVENT, apply)
}
