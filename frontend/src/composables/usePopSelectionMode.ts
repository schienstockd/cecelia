import { computed, watch, type Ref } from 'vue'
import { useViewState } from './useViewState'
import { hasStagedChanges, stagedChangeCount as stagedDiff } from '../utils/manualApplyStaging'
import { remapPopKeys } from '../utils/popRenameRemap'

// "Wait for population selection to update plots" mode — persisted per-canvas so summary,
// cluster and gating pages all offer the same opt-in toggle from one composable. Every host
// has its own live selection ref (`gSel` / `highlighted` / `gHL`); the composable takes it
// as an arg and mirrors it into `stagedSel` while the mode is OFF, so a consumer can always
// render off `stagedSel` without branching on the mode. When ON the two diverge until Apply.
//
// UI contract for hosts:
//  • Read `stagedSel` (not `live`) as the value the population picker edits when `manualApply`
//    is true — same shape as SummaryCanvas.selectedGlobal.
//  • Route toggles/edits to `stagedSel` when `manualApply` is true; to `live` otherwise.
//  • Wire `applyStaged` / `discardStaged` to the CanvasSidePanel Apply / Discard chip.
//  • Call `remapStaged(remap)` from the host's pop-rename remap watcher so a rename during a
//    staged edit carries the pending pop over to its new key (docs/todo/POP_SYNC_PLAN.md).
export function usePopSelectionMode(opts: {
  shared: Ref<Record<string, unknown>>
  live: Ref<string[]>
}) {
  const { manualApply, stagedSel } = useViewState(opts.shared, {
    manualApply: false as boolean,
    stagedSel: [] as string[],
  })
  const hasStaged = computed(() => hasStagedChanges(stagedSel.value, opts.live.value))
  const stagedChangeCount = computed(() => stagedDiff(stagedSel.value, opts.live.value))
  const applyStaged = () => { opts.live.value = [...stagedSel.value] }
  const discardStaged = () => { stagedSel.value = [...opts.live.value] }
  // Keep stagedSel = live while the mode is off — flipping ON starts from the current live
  // selection rather than an empty / stale staged bag.
  watch([manualApply, opts.live], ([on, sel]) => { if (!on) stagedSel.value = [...sel] }, { immediate: true })
  // Callers plug this into their pop-rename remap watcher so a rename during a staged edit
  // carries the pending pop over to its new key instead of dropping it.
  const remapStaged = (remap: (k: string) => string | null) => {
    stagedSel.value = remapPopKeys(stagedSel.value, remap)
  }
  return { manualApply, stagedSel, hasStaged, stagedChangeCount, applyStaged, discardStaged, remapStaged }
}
