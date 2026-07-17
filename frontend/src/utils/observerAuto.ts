// "Sit next to me" auto-mode trigger logic (kept out of the .vue SFC so it's unit-testable).
// A finished task worth a (debounced) auto observer pass = a terminal outcome from either launch
// path: a module-page task (`task:status` done/failed) or a whiteboard chain node
// (`chain:node:done`/`chain:node:failed`). queued/running/cancelled don't warrant a pass.
// See docs/todo/OBSERVER_INTEGRATION_PLAN.md (Phase 3).

const TERMINAL_TASK_STATUS = new Set(['done', 'failed'])

/** Frame types the panel subscribes to for auto-mode. */
export const OBSERVER_AUTO_FRAME_TYPES = ['task:status', 'chain:node:done', 'chain:node:failed']

/** Should this WS frame trigger an auto observer pass? */
export function isObserverTrigger(frame: { type?: string; status?: string }): boolean {
  const t = frame?.type
  if (t === 'task:status') return TERMINAL_TASK_STATUS.has(String(frame?.status ?? ''))
  return t === 'chain:node:done' || t === 'chain:node:failed'
}
