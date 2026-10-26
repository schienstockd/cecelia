// Background jobs the BACKEND starts — the cluster env install, an opt-in pixi env install, a model-weights
// download (`pool = "job"` on their `task:status`). Nothing in the browser launched them, so no Task
// Manager row existed and `setStatus` (which matches by id) dropped every frame: the install ran with
// nothing on screen. The row is made from the job's first `running` frame instead.
//
// These jobs reuse one fixed id per thing (`hpc-env-install`, `model-weights:<model>`), so a second run
// arrives on the first run's finished row. It replaces that row when it started after the row finished;
// an earlier start is a late frame of the old run, which must not bring it back (cancelled is sticky).

import type { TaskEntry } from '../stores/tasks'

/** The fields of a `task:status` frame this needs. */
export interface RailJobFrame {
  taskId: string
  status: string
  fun: string
  pool: string
  startedAt?: Date
}

/** The row's name: what the job does, in the Task Manager's words. */
export function railJobLabel(fun: string): string {
  if (fun === 'hpc:install-env') return 'Install cluster environment'
  const env = /^system:install-env:(.+)$/.exec(fun)
  if (env) return `Install ${env[1]} environment`
  const w = /^system:fetch-weights:(.+)$/.exec(fun)
  if (w) return `Download ${w[1]} weights`
  return fun
}

/** Should this frame make a row? A starting backend job with no live row of its own. */
export function adoptsRailJob(f: RailJobFrame, existing?: Pick<TaskEntry, 'status' | 'finishedAt'>): boolean {
  if (f.pool !== 'job' || !f.taskId || !f.fun) return false
  if (f.status !== 'running' && f.status !== 'queued') return false
  if (!existing) return true
  if (existing.status === 'running' || existing.status === 'queued') return false
  return !!f.startedAt && !!existing.finishedAt && f.startedAt.getTime() > existing.finishedAt.getTime()
}

/** The row for a backend job: no image and no project, so it is in scope whichever project is open. */
export function railJobRow(f: RailJobFrame): Omit<TaskEntry, 'seq' | 'log'> {
  return {
    id: f.taskId, module: 'system', label: railJobLabel(f.fun), imageUid: '', imageName: '',
    status: f.status === 'queued' ? 'queued' : 'running', startedAt: f.startedAt ?? new Date(),
    taskName: f.fun, funName: f.fun, params: {}, projectUid: '', pool: 'job',
    // nothing to re-run it with: it is started from its own button (Settings, a model's Install)
    paramsUnknown: true,
  }
}
