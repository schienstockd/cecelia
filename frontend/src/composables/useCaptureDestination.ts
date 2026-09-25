// KIWI_CAPTURE_AND_BLACKBOARD_PLAN P1 — one source of truth for the DrawSurface capture-destination
// toggles (Attach-to-Kiwi + Send-to-paired). Two parents mount DrawSurface (ViewerWindow's viewer
// overlay + useCanvasShare's plot-canvas overlay); both need to compute the same first-use defaults
// and persist the same last-user-choice into `stores/settings`, so the resolution rule lives here.
//
// Contract: on read, `null` in settings (never toggled) resolves to a state-derived default —
// `attachToKiwi ← settings.kiwiOpen`, `sendToPaired ← isPaired`. Once the user toggles either
// value, the resolved boolean writes back to settings and wins for the next capture.

import { computed, ref, watch, onMounted, onBeforeUnmount, type WritableComputedRef, type Ref } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { usePushStore } from '../stores/push'
import { fetchPushTarget, type PairedState } from '../utils/pushTarget'

export interface CaptureDestinationState {
  attachToKiwi: WritableComputedRef<boolean>
  sendToPaired: WritableComputedRef<boolean>
  /** Live pairing state — parents can read to decide whether to render the Send toggle at all. */
  isPaired: Ref<boolean>
}

/**
 * Self-contained: fetches the paired state for `projectUid` on mount, refreshes on
 * `push_target:changed` broadcasts, and stops on unmount. Parents just consume the two
 * writable computed refs and wire them as v-model to DrawSurface.
 */
export function useCaptureDestination(projectUid: Ref<string> | (() => string)): CaptureDestinationState {
  const settings = useSettingsStore()
  const pushStore = usePushStore()
  const pushTarget = ref<PairedState>({ paired: false })
  const puid = typeof projectUid === 'function' ? projectUid : () => projectUid.value

  const isPaired = computed(() => pushTarget.value.paired)

  const attachToKiwi = computed<boolean>({
    get: () => settings.captureAttachToKiwi ?? settings.kiwiOpen,
    set: v => { settings.captureAttachToKiwi = v },
  })
  const sendToPaired = computed<boolean>({
    get: () => settings.captureSendToPaired ?? isPaired.value,
    set: v => { settings.captureSendToPaired = v },
  })

  async function refresh() {
    const p = puid()
    pushTarget.value = p ? await fetchPushTarget(p) : { paired: false }
  }

  const stop = watch(() => pushStore.tick, () => {
    const evt = pushStore.lastEvent
    if (!evt) return
    const evtUid = (evt as { projectUid?: string }).projectUid
    if (evtUid && evtUid !== puid()) return
    if (evt.kind === 'push_target:changed') void refresh()
  })

  onMounted(() => { void refresh() })
  onBeforeUnmount(() => { stop() })

  return { attachToKiwi, sendToPaired, isPaired }
}
