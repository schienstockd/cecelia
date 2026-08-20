import { defineStore, acceptHMRUpdate } from 'pinia'
import { reactive } from 'vue'
import type { TrackOp } from '../lib/trackCorrection'

// The UNCOMMITTED track corrections — queued edits that have not been applied yet.
//
// **Why a store and not the panel's state bag.** The queue becomes `params.trackOps` of one
// `tracking.correct_measures` run (`lib/trackOpsRun.ts`), so it is an un-run TASK DRAFT, not a view
// option — and it was living in the timeline panel's persisted state, which is keyed by the canvas it
// sits on. The Track canvas keys itself `gate:{popType}:{image}:{g.valueName}`, so touching the
// page-level segmentation select rebound the canvas and took the panel — with the queued edits — out of
// view. Nothing was lost from disk; it was simply unreachable, which for pending work is the same thing.
//
// Sibling of `stores/taskDrafts.ts` and keyed on the same principle: **a draft lives at the granularity
// the thing it edits lives at.** A correction edits one (image, segmentation)'s tracks, so that is the
// key — not whichever canvas or panel happened to author it. Two timeline panels on the same tracked
// label set therefore share ONE queue, which is what the engine already assumes (one queue → one run →
// one journal entry, CORRECTION_PLAN.md → Decision 3b). Two panels with two private queues could each
// apply half of an intended edit.
//
// Session-scoped and in-memory, exactly like `taskDrafts`: pending edits survive navigation and a
// segmentation switch, and a full reload starts clean rather than resurrecting ops the user has
// forgotten authoring against data that may since have been re-tracked.

/** The scope a queue belongs to — what the ops actually edit. Pure; empty until it is known. */
export function trackOpsKey(projectUid: string, imageUid: string, valueName: string): string {
  return (projectUid && imageUid && valueName) ? `${projectUid}|${imageUid}|${valueName}` : ''
}

export const useTrackOpsQueueStore = defineStore('trackOpsQueue', () => {
  const queues = reactive<Record<string, TrackOp[]>>({})

  const get = (key: string): TrackOp[] => (key ? queues[key] ?? [] : [])
  const set = (key: string, ops: TrackOp[]) => {
    if (!key) return
    // an empty queue is DELETED rather than stored: "no pending edits" and "a queue that happens to be
    // empty" are the same state, and keeping the entry would grow one per segmentation ever visited
    if (ops.length) queues[key] = ops
    else delete queues[key]
  }
  const clear = (key: string) => set(key, [])

  return { queues, get, set, clear }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useTrackOpsQueueStore, import.meta.hot))
