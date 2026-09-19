import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref } from 'vue'

// BIDIR Part 5 PR #3 — push pairing / delivery notifications from the backend.
//
// This store is a THIN dispatcher: the WS `push_target:changed` and `push:sent` frames land in
// `stores/ws.ts::dispatch()`, which calls `notify(kind, data)` here. Subscribers watch a bumped
// tick + read a small `lastEvent` bag; they decide (a) whether the event is for their project
// and (b) what to do with it (refetch `/api/push/target`, show a transient "sent ✓" chip, etc).
//
// Kept intentionally small — the four-state chip lives in `ViewerPanel.vue` where the projectUid
// context is already resolved, and per-consumer "am I paired for THIS project" filtering doesn't
// belong in a shared store. Same shape as `labCapture` / `viewer` mark bags: WS in, tick out.
export const usePushStore = defineStore('push', () => {
  const tick = ref(0)  // bumped on every arriving frame (pair changes + push:sent)
  const lastEvent = ref<{ kind: string; projectUid: string; paired?: boolean; captureId?: string; sessionLabel?: string } | null>(null)

  function notify(kind: string, data: Record<string, unknown>) {
    lastEvent.value = {
      kind,
      projectUid: typeof data.projectUid === 'string' ? data.projectUid : '',
      paired:     typeof data.paired     === 'boolean' ? data.paired     : undefined,
      captureId:  typeof data.captureId  === 'string' ? data.captureId  : undefined,
      sessionLabel: typeof data.sessionLabel === 'string' ? data.sessionLabel : undefined,
    }
    tick.value++
  }

  return { tick, lastEvent, notify }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(usePushStore, import.meta.hot))
