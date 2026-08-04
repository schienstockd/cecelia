// Scenario: a side panel made of TWO stacked halves, where either can take the whole panel.
//
// It recurs — the module pages' task runner (function + params + run, over the module's task list) and
// the batch-movies panel (movie config, over the same task list) — and on a laptop screen neither half
// gets enough vertical room in both. So each panel offers three states: `split` (both), `top` (the
// bottom half hidden), `bottom` (the top half hidden).
//
// One rule, one storage convention, one control (`components/PaneExpandBar.vue`) for every panel that
// has this shape, rather than each one growing its own toggle. Halves are named `top`/`bottom` — what
// they CONTAIN differs per panel, their arrangement is what's shared.
//
// The mode is a user-settable option, so it survives a remount (docs/UI.md → *Persisting view state*).
// It lives in localStorage under a per-panel key, alongside the panel widths, and is NOT scoped per
// module or project: it's a preference about how a panel is arranged, not about what's in it.
//
// A consumer puts the mode on its root as `pane-<mode>` and hides each half with ONE CSS rule — see
// `docs/UI.md` → *Two-half side panels*. Deliberately not a per-element boolean: a half is usually several
// sibling elements, and guarding each one means a section added later is silently left visible.
//
// Pure logic here, so the toggle rule and the stored-value guard are testable without mounting a
// component (docs/DEV.md → frontend test scope). `composables/usePaneExpand.ts` is the wrapper to use.

export type PaneExpand = 'split' | 'top' | 'bottom'
export type PaneHalf = 'top' | 'bottom'

const PANES: PaneExpand[] = ['split', 'top', 'bottom']

/** A stored value, or `'split'` for anything unrecognised (absent, stale, hand-edited). */
export function parsePane(raw: unknown): PaneExpand {
  return PANES.includes(raw as PaneExpand) ? raw as PaneExpand : 'split'
}

/**
 * What clicking a half's button does: expand that half, or — if it is already expanded — go back to
 * `split`.
 *
 * One button per half, each its own toggle, so whichever half is hidden its own button is still there to
 * bring it back and there is no state the user can't click their way out of.
 */
export function nextPane(current: PaneExpand, clicked: PaneHalf): PaneExpand {
  return current === clicked ? 'split' : clicked
}

export function loadPane(storageKey: string): PaneExpand {
  try { return parsePane(localStorage.getItem(storageKey)) } catch { return 'split' }
}

export function savePane(storageKey: string, pane: PaneExpand): void {
  try { localStorage.setItem(storageKey, pane) } catch { /* private mode / quota — not worth failing over */ }
}
