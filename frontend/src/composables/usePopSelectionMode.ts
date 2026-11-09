import { computed, ref, watch, type Ref } from 'vue'
import { useViewState } from './useViewState'
import { hasStagedChanges, stagedChangeCount as stagedDiff } from '../utils/manualApplyStaging'
import { remapPopKeys } from '../utils/popRenameRemap'
import { createApplyHold } from '../utils/applyHold'

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
//  • Route every refresh a gate write triggers through `deferUntilApply(key, run)` — it runs at
//    once with the mode off, and is held for Apply with it on (`utils/applyHold.ts`). Discard
//    keeps held refreshes: they are server-side changes, not a choice the user can take back.
export function usePopSelectionMode(opts: {
  shared: Ref<Record<string, unknown>>
  live: Ref<string[]>
}) {
  const { manualApply, stagedSel } = useViewState(opts.shared, {
    manualApply: false as boolean,
    stagedSel: [] as string[],
  })
  const heldCount = ref(0)
  const hold = createApplyHold(n => { heldCount.value = n })
  const deferUntilApply = (key: string, run: () => void) => {
    if (manualApply.value) hold.hold(key, run)
    else run()
  }
  const dropHeld = (key: string) => hold.drop(key)
  const hasStaged = computed(() => heldCount.value > 0 || hasStagedChanges(stagedSel.value, opts.live.value))
  const stagedChangeCount = computed(() => heldCount.value + stagedDiff(stagedSel.value, opts.live.value))
  const applyStaged = () => { opts.live.value = [...stagedSel.value]; hold.flush() }
  const discardStaged = () => { stagedSel.value = [...opts.live.value] }
  // Keep stagedSel = live while the mode is off — flipping ON starts from the current live
  // selection rather than an empty / stale staged bag. Flipping OFF runs what was held.
  watch([manualApply, opts.live], ([on, sel]) => {
    if (!on) { stagedSel.value = [...sel]; hold.flush() }
  }, { immediate: true })
  // Callers plug this into their pop-rename remap watcher so a rename during a staged edit
  // carries the pending pop over to its new key instead of dropping it.
  const remapStaged = (remap: (k: string) => string | null) => {
    stagedSel.value = remapPopKeys(stagedSel.value, remap)
  }
  return { manualApply, stagedSel, hasStaged, stagedChangeCount, applyStaged, discardStaged, remapStaged,
           deferUntilApply, dropHeld }
}
