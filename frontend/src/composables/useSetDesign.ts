// The experiment design of one set, loaded and kept current: refetched when the set changes and after
// any save or attribute rename (`designSaves`). Every view that reads a design goes through this — the
// Design check and the plot canvases (docs/todo/EXPERIMENT_DESIGN_PLAN.md → T1, T2).
import { ref, watch, type Ref } from 'vue'
import type { DesignGrid, ExperimentDesign } from '../utils/experimentDesign'
import { designSaves, fetchDesign } from '../utils/experimentDesignApi'

export function useSetDesign(projectUid: Ref<string | null | undefined>, setUid: Ref<string | null | undefined>) {
  const design = ref<ExperimentDesign | null>(null)
  // the saved design over the set's images as they are now (the Checks cluster's Design check)
  const grid = ref<DesignGrid | null>(null)
  const loaded = ref(false)
  async function load() {
    const p = projectUid.value, s = setUid.value
    loaded.value = false
    design.value = null
    grid.value = null
    if (!p || !s) return
    try {
      const reply = await fetchDesign(p, s)
      if (setUid.value === s) { design.value = reply.design; grid.value = reply.grid; loaded.value = true }
    } catch { /* no design: views behave as before */ loaded.value = true }
  }
  watch(() => [projectUid.value, setUid.value, designSaves.value], load, { immediate: true })
  return { design, grid, loaded, reload: load }
}
