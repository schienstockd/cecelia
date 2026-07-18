// Terminal task/chain-completion predicate (kept out of the .vue SFC so it's unit-testable). A
// finished unit of work = a terminal outcome from either launch path: a module-page task
// (`task:status` done/failed) or a whiteboard chain node (`chain:node:done`/`chain:node:failed`);
// queued/running/cancelled don't count. Consumed by `useTaskCompletionWatch` (the shared backbone for
// "run a debounced callback when work finishes"). NOTE the old auto-Claude "Watch" was removed — this
// predicate now backs Cecelia's own completion triggers, not an observer pass.

const TERMINAL_TASK_STATUS = new Set(['done', 'failed'])

/** Frame types to subscribe to for task/chain completion. */
export const OBSERVER_AUTO_FRAME_TYPES = ['task:status', 'chain:node:done', 'chain:node:failed']

/** Should this WS frame count as a completed unit of work? */
export function isObserverTrigger(frame: { type?: string; status?: string }): boolean {
  const t = frame?.type
  if (t === 'task:status') return TERMINAL_TASK_STATUS.has(String(frame?.status ?? ''))
  return t === 'chain:node:done' || t === 'chain:node:failed'
}
