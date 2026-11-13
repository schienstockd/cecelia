import { ref } from 'vue'
import { loadPane, savePane, nextPane, nextPaneExclusive, type PaneExpand, type PaneHalf } from '../utils/paneExpand'

/**
 * Which half of a two-half side panel is expanded, persisted per panel.
 *
 * Pair it with `components/PaneExpandBar.vue` (the control) and a `pane-<mode>` class on the panel root,
 * which one CSS rule per half hides against — see `docs/UI.md` → *Two-half side panels*.
 *
 * ```ts
 * const { pane, toggle } = usePaneExpand('cc-taskrunner-pane-v2', 'top', { exclusive: true })
 * ```
 *
 * `storageKey` is per panel, so the task runner and the batch-movies panel remember their own
 * arrangement. `fallback` is the arrangement before the user picks one. `exclusive` drops `split` —
 * one half or the other, never both (`nextPaneExclusive`); pass the same flag to `PaneExpandBar` so its
 * tooltips match.
 */
export function usePaneExpand(storageKey: string, fallback: PaneExpand = 'split', { exclusive = false } = {}) {
  const pane = ref<PaneExpand>(loadPane(storageKey, fallback, exclusive))

  function toggle(half: PaneHalf) {
    pane.value = exclusive ? nextPaneExclusive(pane.value, half) : nextPane(pane.value, half)
    savePane(storageKey, pane.value)
  }

  return { pane, toggle }
}
