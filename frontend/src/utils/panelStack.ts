// Stacking order for the top-level floating panels (see components/FloatingPanel.vue).
//
// Every FloatingPanel used to carry the same hardcoded `z-index: 60`, so when two were open at
// once the winner fell back to DOM order — the Lab log panel is declared after the Viewer panel
// in App.vue, so it always covered the Viewer no matter which one you were actually using. These
// helpers keep a most-recently-raised-last ordering of panel storage keys; the panel you last
// touched ends up at the end of the stack and therefore on top.
//
// Pure array helpers on purpose: the component owns the reactive holder, this owns the logic, so
// the ordering is unit-testable without mounting anything (frontend tests are logic-only — see
// docs/DEV.md → Tests).

// Base z-index for the lowest floating panel. Must stay above page content and the right panel,
// and below BaseModal (500) and TeleportPopover (1000) — a panel is never meant to cover a modal.
// The top panel sits at PANEL_Z_BASE + (number of open panels - 1), so with the handful of panels
// that can be open at once this stays far below the modal layer. Mirrored in docs/UI.md.
export const PANEL_Z_BASE = 60

// Move `key` to the top of the stack. Returns a new array — callers assign it to a ref so Vue
// tracks the change.
export function raisePanel(stack: readonly string[], key: string): string[] {
  const without = stack.filter(k => k !== key)
  without.push(key)
  return without
}

// Drop `key` (panel closed/unmounted) so it doesn't hold a slot in the ordering.
export function dropPanel(stack: readonly string[], key: string): string[] {
  return stack.filter(k => k !== key)
}

// z-index for `key`. An unknown key (not yet raised) sits at the base.
export function panelZ(stack: readonly string[], key: string): number {
  const i = stack.indexOf(key)
  return PANEL_Z_BASE + (i < 0 ? 0 : i)
}
