import { defineStore, acceptHMRUpdate } from 'pinia'
import { reactive } from 'vue'
import type { LabelOp } from '../lib/labelCorrection'

// The UNCOMMITTED label corrections — queued edits that have not been applied yet.
//
// Sibling of `stores/trackOpsQueue.ts`; keyed on the same `(projectUid, imageUid, valueName)`
// tuple for the same reason (a draft lives at the granularity the thing it edits lives at) so
// two cockpit surfaces on the same labels set share ONE queue and can never each apply half of
// an intended edit. Session-scoped and in-memory. Same rationale as the tracks store — see there.
//
// **Why a peer store instead of one shared queue.** Track ops and label ops don't compose: they
// address different data (`obs.track_id` versus the labels array), and get submitted through
// different composite tasks (`tracking.correct_measures` vs `segment.correct_measures`). One
// mixed queue would force every consumer to filter by kind on every read; two peer stores lets
// each surface (cockpit Tracks tab, cockpit Labels tab) fetch its own queue with no filter.

/** The scope a label queue belongs to — same shape as `trackOpsKey`. */
export function labelOpsKey(projectUid: string, imageUid: string, valueName: string): string {
  return (projectUid && imageUid && valueName) ? `${projectUid}|${imageUid}|${valueName}` : ''
}

export const useLabelOpsQueueStore = defineStore('labelOpsQueue', () => {
  const queues = reactive<Record<string, LabelOp[]>>({})

  const get = (key: string): LabelOp[] => (key ? queues[key] ?? [] : [])
  const set = (key: string, ops: LabelOp[]) => {
    if (!key) return
    // Same delete-when-empty rule as trackOpsQueue — see the note there.
    if (ops.length) queues[key] = ops
    else delete queues[key]
  }
  const clear = (key: string) => set(key, [])

  return { queues, get, set, clear }
})

// Replace the live instance on hot-reload — same principle as `useTrackOpsQueueStore`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useLabelOpsQueueStore, import.meta.hot))
