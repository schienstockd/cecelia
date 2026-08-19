/**
 * Submit a queue of track edits as ONE task run.
 *
 * Extracted from the correction worklist because the track timeline now authors the same ops and the
 * worklist is being retired (docs/todo/TRACK_SCHEME_PLAN.md). Left inline, this would have been
 * deleted from one component and retyped in another — and the two copies would then be free to
 * disagree about the task name, the pool, or whether the composite runs. There is one way to apply a
 * correction; a second is the bug (CLAUDE.md → divergent re-implementation).
 *
 * **One run for the whole queue, through the COMPOSITE.** `tracking.correct_measures`, not
 * `tracking.correct`: correcting tracks without recomputing the track measures leaves every `live.*`
 * column describing the previous assignment, which is worse than not correcting at all because
 * nothing downstream can tell. This is CORRECTION_PLAN.md → Decision 3b, and it is the reason this
 * function exists rather than each caller assembling its own `task:run`.
 */

import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import type { TrackOp } from './trackCorrection'

/** The composite: apply the ops, then re-measure. Never `tracking.correct` on its own. */
export const TRACK_CORRECT_FUN = 'tracking.correct_measures'

export interface TrackOpsRun {
  projectUid: string
  setUid: string | null
  imageUid: string
  valueName: string
  ops: readonly TrackOp[]
  /** where a log line is attributed from — 'correct' or 'tracks' */
  source?: string
}

/**
 * Queue the run. Returns false (and does nothing) when there is nothing to apply or no image.
 *
 * The caller clears its own queue on `true`. It is not cleared here because the queue lives in the
 * caller's persisted panel state and this module has no business reaching into it.
 */
export function submitTrackOps(o: TrackOpsRun): boolean {
  if (!o.ops.length || !o.imageUid) return false
  const tasks = useTaskStore()
  const ws = useWsStore()
  const project = useProjectStore()
  const log = useLogStore()

  const params = { valueName: o.valueName, trackOps: JSON.stringify(o.ops) }
  const img = project.imageByUid(o.imageUid)
  const task = tasks.add({
    module: 'tracking', label: 'Correct tracks', imageUid: o.imageUid,
    imageName: img?.name || o.imageUid,
    status: 'queued' as const, taskName: 'trackCorrectMeasures', funName: TRACK_CORRECT_FUN,
    params, projectUid: o.projectUid,
  })
  // `setUid ?? ''` — a canvas panel legitimately has no set, and sending an explicit `null` used to
  // abort the whole handler (`String(::Nothing)`). The WS boundary now absorbs a null too
  // (`_wstr` in api/src/sockets.jl), but that file is not Revise-tracked, so this keeps Apply working
  // against a server that has not been restarted yet.
  ws.send({ type: 'task:run', taskId: task.id, funName: TRACK_CORRECT_FUN, params,
            imageUid: o.imageUid, projectUid: o.projectUid, setUid: o.setUid ?? '', poolName: 'cpu' })
  log.info(`Applying ${o.ops.length} correction(s) — the plots refresh when it finishes.`,
           { source: o.source ?? 'tracks' })
  return true
}
