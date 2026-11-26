// Why a queued task is still queued — the run(s) holding its pool's slots.
//
// The scheduler is one queue per pool for the whole server: every project, every profile. So a task can
// sit at Queued behind work this tab never sees — another profile's GPU segmentation, in a project that
// isn't open. The task list only shows the open project's rows, so before this the user saw "Queued"
// and nothing else. The answer was already in `GET /api/tasks` (pool, launcher, project for every
// in-flight task); this reads it off the WHOLE snapshot, not the per-project slice `adoptableTasks`
// keeps.
//
// Pure: the snapshot and every name lookup come in through the arguments (`composables/useQueueReasons`
// supplies them from the stores).
import type { InFlightTaskRow } from './runningTasks'
import type { TaskEntry } from '../stores/tasks'
import { taskElapsed } from './taskElapsed'
import { profileNameLabel } from './profileApi'

export interface QueueReason {
  /** one short line under the row: `gpu busy: Segment (alice) · 2 ahead` */
  short: string
  /** the detail: which image, which project, for how long */
  tip: string
}

export interface QueueReasonContext {
  /** the profile this tab runs as — its own runs read "you" */
  selfProfile?: string
  currentProjectUid?: string
  /** imageUid → name, for the open project (another project's images can't be named here) */
  imageName: (uid: string) => string | undefined
  projectName: (uid: string) => string | undefined
  labelFor: (fun: string) => string
  now: number
}

/** Who launched a run, as the reason names them. `''` when the snapshot didn't say. */
function who(by: string | undefined, self: string | undefined): string {
  if (!by) return ''
  if (by === self) return 'you'
  return profileNameLabel(by)
}

function where(r: InFlightTaskRow, ctx: QueueReasonContext): string {
  const img = r.image_uid ? ctx.imageName(r.image_uid) : undefined
  if (!r.project_uid || r.project_uid === ctx.currentProjectUid) return img ?? r.image_uid ?? ''
  return ctx.projectName(r.project_uid) ?? 'another project'
}

/**
 * The reason, or `null` when there's nothing to say: not queued, a cluster run (its queue is the
 * cluster's, shown by the HPC stage), no pool known, or nothing in the snapshot holding the pool (it
 * hasn't been fetched yet, or the slot is just freeing up).
 */
export function queueReason(
  t: Pick<TaskEntry, 'id' | 'status' | 'pool' | 'backendTaskId' | 'hpcStage'>,
  snapshot: InFlightTaskRow[],
  ctx: QueueReasonContext,
): QueueReason | null {
  if (t.status !== 'queued' || t.hpcStage) return null
  const selfId = t.backendTaskId || t.id
  const self = snapshot.find(r => r.id === selfId)
  const pool = self?.pool_name || t.pool
  if (!pool) return null

  const inPool = snapshot.filter(r => r.pool_name === pool && r.id !== selfId)
  const running = inPool.filter(r => r.status === 'running')
  // Ahead = queued in the same pool before this one. Only countable when the snapshot has our own
  // queued_at; without it, say nothing rather than guess.
  const ahead = self?.queued_at
    ? inPool.filter(r => r.status === 'queued' && !!r.queued_at && r.queued_at < self.queued_at!).length
    : 0
  if (!running.length && !ahead) return null

  const label = (r: InFlightTaskRow) => ctx.labelFor(r.fun_name ?? '') || (r.fun_name ?? '').split('.').pop() || 'task'
  const people = [...new Set(running.map(r => who(r.by, ctx.selfProfile)).filter(Boolean))]

  let short = ''
  if (running.length === 1) {
    const p = people[0]
    short = `${pool} busy: ${label(running[0])}${p ? ` (${p})` : ''}`
  } else if (running.length > 1) {
    short = `${pool} busy: ${running.length} running${people.length ? ` (${people.join(', ')})` : ''}`
  }
  if (ahead) short = short ? `${short} · ${ahead} ahead` : `${ahead} ahead in ${pool}`

  const details = running.map(r => {
    const bits = [where(r, ctx), who(r.by, ctx.selfProfile)].filter(Boolean)
    const el = r.started_at ? taskElapsed(new Date(r.started_at), undefined, ctx.now) : null
    return `${label(r)}${bits.length ? ` on ${bits.join(', ')}` : ''}${el ? `, ${el}` : ''}`
  })
  const tip = `Waiting for a free ${pool} slot.`
    + (details.length ? ` Running: ${details.join('; ')}.` : '')
    + (ahead ? ` ${ahead} queued ahead.` : '')
  return { short, tip }
}
