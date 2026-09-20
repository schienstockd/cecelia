import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref } from 'vue'

// BIDIR Part 4 (Blackboard) — WS `blackboard:changed` dispatcher.
//
// Same thin-dispatcher pattern as `stores/push.ts`: the WS frame lands in `stores/ws.ts::dispatch()`,
// which calls `notify(projectUid)`; subscribers watch `tick` and re-fetch on their own terms. Kept
// separate from `push` because that store's `lastEvent` shape is push-specific (paired/captureId/…),
// and blackboard events only need `projectUid` — a shared bag would grow noise on both sides.
export const useBlackboardStore = defineStore('blackboard', () => {
  const tick = ref(0)
  const lastProjectUid = ref('')

  function notify(projectUid: string) {
    lastProjectUid.value = projectUid
    tick.value++
  }

  return { tick, lastProjectUid, notify }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useBlackboardStore, import.meta.hot))
