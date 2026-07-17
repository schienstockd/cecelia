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

// The event categories the observer COULD watch, for the read-only green/red status row under the
// Watch toggle. `active` = wired as a live trigger today (only task-completion is). The rest are shown
// red with a `note` explaining why — flip `active` (and wire the frame) when a signal exists. Single
// source of truth so adding a trigger is one edit here. See docs/todo/OBSERVER_INTEGRATION_PLAN.md.
export interface ObserverTrigger { key: string; label: string; active: boolean; note: string }
export const OBSERVER_TRIGGERS: ObserverTrigger[] = [
  { key: 'task',       label: 'Task finished', active: true,
    note: 'A task or chain node completes (done or failed) — the current trigger.' },
  { key: 'note',       label: 'Image note',    active: false,
    note: 'A live event exists (image_note_added) but is not wired as a trigger yet.' },
  { key: 'population', label: 'Population',     active: false,
    note: 'No live signal yet — gating changes are captured at digest time, not pushed live.' },
  { key: 'workflow',   label: 'Workflow',      active: false,
    note: 'No live signal for saving a workflow yet.' },
  { key: 'figure',     label: 'Figure',        active: false,
    note: 'No live signal for creating a figure yet.' },
]
