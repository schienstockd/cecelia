import { ref, computed } from 'vue'
import { loadPane, savePane, nextPane, paneShows, type PaneExpand, type PaneHalf } from '../utils/paneExpand'

/**
 * Which half of a two-half side panel is expanded, persisted per panel.
 *
 * Pair it with `components/PaneExpandBar.vue` (the control) and `v-show` on each half — see
 * `utils/paneExpand.ts` for the scenario and `docs/MODULES.md` → *Either half of the panel can take the
 * whole panel* for the consumer recipe.
 *
 * ```ts
 * const { pane, toggle, showTop, showBottom } = usePaneExpand('cc-taskrunner-pane')
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

  return {
    pane,
    toggle,
    showTop:    computed(() => paneShows(pane.value, 'top')),
    showBottom: computed(() => paneShows(pane.value, 'bottom')),
  }
}
