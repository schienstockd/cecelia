/**
 * Submit a queue of label edits as ONE task run.
 *
 * Parallel to `lib/trackOpsRun.ts` (see the notes there). Same principle: one code path for every
 * caller so a rename of the composite task, a change to the pool, or a decision to switch which
 * composite runs happens in one place. A second copy would drift immediately (CLAUDE.md →
 * divergent re-implementation).
 *
 * **Runs through the COMPOSITE** — `segment.correct_measures`, not `segment.correct` alone — so
 * `measureLabels` re-runs after the mask edit, keeping the labelProps h5ad and the labels store
 * consistent. Decision 4b's obs carry-over is a known follow-up gap in the composite; running
 * only `segment.correct` here would leave labelProps describing the pre-edit row set, which is
 * worse than the current gap (stale obs on touched cells vs stale everything on every cell).
 */

import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import type { LabelOp } from '../lib/labelCorrection'

/** The composite: apply the ops, then re-measure. Never `segment.correct` on its own. */
export const LABEL_CORRECT_FUN = 'segment.correct_measures'

export interface LabelOpsRun {
  projectUid: string
  setUid: string | null
  imageUid: string
  valueName: string
  ops: readonly LabelOp[]
  /** where a log line is attributed from — 'cockpit', 'review', etc. */
  source?: string
}

/**
 * Queue the run. Returns false (and does nothing) when there's nothing to apply or no image.
 * The caller clears its own queue on true — same pattern as `submitTrackOps`.
 */
export function submitLabelOps(o: LabelOpsRun): boolean {
  if (!o.ops.length || !o.imageUid) return false
  const tasks = useTaskStore()
  const ws = useWsStore()
  const project = useProjectStore()
  const log = useLogStore()

  const params = { valueName: o.valueName, labelOps: JSON.stringify(o.ops) }
  const img = project.imageByUid(o.imageUid)
  const task = tasks.add({
    module: 'segment', label: 'Correct labels', imageUid: o.imageUid,
    imageName: img?.name || o.imageUid,
    status: 'queued' as const, taskName: 'segmentCorrectMeasures', funName: LABEL_CORRECT_FUN,
    params, projectUid: o.projectUid,
  })
  ws.send({ type: 'task:run', taskId: task.id, funName: LABEL_CORRECT_FUN, params,
            imageUid: o.imageUid, projectUid: o.projectUid, setUid: o.setUid ?? '', poolName: 'io' })
  log.info(`Applying ${o.ops.length} label correction(s) — the viewer refreshes when it finishes.`,
           { source: o.source ?? 'cockpit' })
  return true
}
