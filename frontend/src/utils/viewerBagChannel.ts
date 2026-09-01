// Cross-window sync for the per-image / per-set viewer state bags.
//
// The browser volume viewer runs in a popup (`lib/popout.ts`), a separate app instance with its OWN
// Pinia store. Writes in the main window (the panel's toggles) don't reach it — the two are separate
// JS contexts. `localStorage` fires a `storage` event in every OTHER same-origin window on set, and
// the settings store already writes these bags to localStorage on every change, so the sync path is
// ready-made: main writes → localStorage → popup receives event → popup rehydrates its ref.
//
// This module is the pure decoder. It's called from the settings store's `storage` listener AND
// tested independently, because the store cannot boot in the test environment (localStorage global
// is not shimmed — pure-logic rule, no jsdom). Extracting the switch out is the pattern
// `lib/openProjectChannel.ts` already establishes for the same reason.

export type ViewerBagKind = 'labelVis' | 'trackVis' | 'branchVis' | 'setPrefs' | 'imageVersion'

export interface ViewerBagEvent {
  kind: ViewerBagKind
  value: unknown
}

/**
 * Interpret a `storage` event as a viewer-bag update, or `null` when it isn't one.
 *
 * `null` for `key` (localStorage.clear() elsewhere) → `null`: the state a clear() would wipe is
 * exactly what should NOT vanish here, since the panel's live edits shouldn't disappear because
 * an unrelated window clear happened.
 *
 * `null` for `newValue` (removeItem elsewhere) → `null`: same reasoning.
 *
 * JSON parse failures → `null`: another window wrote garbage; a stale ref is still better than a
 * crash, and Vue can't render the panel if the ref is undefined.
 */
export function decodeViewerBagEvent(
  key: string | null, newValue: string | null,
): ViewerBagEvent | null {
  if (!key || newValue === null) return null
  let value: unknown
  try { value = JSON.parse(newValue) } catch { return null }
  switch (key) {
    case 'cc.viewerLabelVisibility':  return { kind: 'labelVis',     value }
    case 'cc.viewerTrackVisibility':  return { kind: 'trackVis',     value }
    case 'cc.viewerBranchVisibility': return { kind: 'branchVis',    value }
    case 'cc.viewerSetPrefs':         return { kind: 'setPrefs',     value }
    case 'cc.viewerImageVersion':     return { kind: 'imageVersion', value }
    default:                          return null
  }
}
