// The gating page highlights a transient pop (the viewer's `/Pick selection`) on its plots the moment
// it APPEARS — linked brushing, no eye click needed. Only on appearing: every pick (and every gate
// edit) re-broadcasts the tree, and re-adding the highlight on each of those overrode an eye the user
// had switched off. The eye is theirs once the pop is there; a pick that empties the selection removes
// the pop, so the next one appears again and is shown.
import { watch } from 'vue'

/** Call `onAppear(added)` with the paths that are in `paths()` now but were not the previous time —
 *  immediately for whatever is already there, then on every change. */
export function watchAppearingTransientPops(
  paths: () => readonly string[],
  onAppear: (added: string[]) => void,
): void {
  let prev = new Set<string>()
  watch(() => paths().join('\n'), () => {
    const next = paths()
    const added = next.filter(p => !prev.has(p))
    prev = new Set(next)
    if (added.length) onAppear(added)
  }, { immediate: true })
}
