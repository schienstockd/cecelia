// Scenario: a side panel made of TWO halves that never fit together — the module pages' task runner
// (function + params + run, and the module's task list), the batch-movies panel (movie config, and the
// batch's task list), the animation panel (controls, and its task list). With a long config neither half
// is usable at half height, so a panel shows ONE half at a time: `top` (the config/runner) or `bottom`
// (the task list). The top half keeps a one-line summary of the hidden list under its action button
// (`components/TaskSummaryLine.vue`).
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

import { profileStorage } from './profileStorage'

export type PaneHalf = 'top' | 'bottom'
/** Which half is showing. Kept as its own name: it is the panel's MODE, a half is what a button names. */
export type PaneExpand = PaneHalf

const PANES: PaneExpand[] = ['top', 'bottom']

/** A stored value, or the panel's default (`'top'` unless it names one) for anything unrecognised —
 *  absent, hand-edited, or a `split` stored when panels could show both halves. */
export function parsePane(raw: unknown, fallback: PaneExpand = 'top'): PaneExpand {
  return PANES.includes(raw as PaneExpand) ? raw as PaneExpand : fallback
}

/**
 * What clicking a half's button does: show that half, or — if it is already showing — flip to the other.
 *
 * One button per half, each its own toggle, so there is no state the user can't click their way out of.
 */
export function nextPane(current: PaneExpand, clicked: PaneHalf): PaneExpand {
  if (current !== clicked) return clicked
  return clicked === 'top' ? 'bottom' : 'top'
}

export function loadPane(storageKey: string, fallback: PaneExpand = 'top'): PaneExpand {
  try { return parsePane(profileStorage.getItem(storageKey), fallback) } catch { return fallback }
}

export function savePane(storageKey: string, pane: PaneExpand): void {
  try { profileStorage.setItem(storageKey, pane) } catch { /* private mode / quota — not worth failing over */ }
}
