// The global/local scope toggle every plot canvas offers (summary canvas, analysis board, gating,
// cluster and model plots). Global = one value shared by every plot; local = each plot's own copy.
//
// Switching global → local must not change what is drawn: each plot's local copy starts as the
// global value it was showing, and the user tweaks one plot from there.
//
// Seeded on the USER's switch only, never by watching `scope`: loading a saved canvas that was left
// in local scope also changes `scope`, and must keep its saved per-plot values.

export type PlotScope = 'global' | 'local'

/** A deep, plain copy, so a plot's local value never aliases the global one. */
export const scopeCopy = <T>(v: T): T => (v === undefined ? v : JSON.parse(JSON.stringify(v)))

/** The handler for the scope control: on global → local, `seedLocal` first copies the global values
 *  into every plot's local state, then the scope flips. */
export function scopeSetter(scope: { value: PlotScope }, seedLocal: () => void) {
  return (next: PlotScope) => {
    if (scope.value === 'global' && next === 'local') seedLocal()
    scope.value = next
  }
}
