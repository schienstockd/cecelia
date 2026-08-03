import { ref } from 'vue'
import { loadPane, savePane, nextPane, type PaneExpand, type PaneHalf } from '../utils/paneExpand'

/**
 * Which half of a two-half side panel is expanded, persisted per panel.
 *
 * Pair it with `components/PaneExpandBar.vue` (the control) and a `pane-<mode>` class on the panel root,
 * which one CSS rule per half hides against — see `docs/UI.md` → *Two-half side panels*.
 *
 * ```ts
 * const { pane, toggle } = usePaneExpand('cc-taskrunner-pane')
 * ```
 *
 * `storageKey` is per panel, so the task runner and the batch-movies panel remember their own
 * arrangement.
 */
export function usePaneExpand(storageKey: string) {
  const pane = ref<PaneExpand>(loadPane(storageKey))

  function toggle(half: PaneHalf) {
    pane.value = nextPane(pane.value, half)
    savePane(storageKey, pane.value)
  }

  return { pane, toggle }
}
