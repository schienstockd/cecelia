// The ONE task-status → visual descriptor (icon + colour + label). Replaces the byte-similar status
// maps that were copied into TasksModule / TaskList / ChainLiveNode with drifted raw hexes
// (#86efac / #fca5a5 / #93c5fd repeated). Task status has FIVE states, so it's a superset of the
// three-state QC severity (lib/severity.ts): `done`/`failed` reuse the CVD-safe severity palette
// (--cc-sev-ok / --cc-sev-fail), `running` is "active" (--cc-active), queued/cancelled are neutral.
// `tone` lets a component tint its own chrome (border/background) consistently with the icon.
import type { TaskStatus } from '../stores/tasks'

export type StatusTone = 'ok' | 'fail' | 'active' | 'neutral'

export interface TaskStatusStyle {
  icon: string     // PrimeVue icon class — the shape-distinct (non-colour) cue
  color: string    // icon/text colour — a CSS var into the validated palette
  tone: StatusTone // for a component's own bg/border tint
  label: string    // text label / tooltip — colour is never the only cue
}

export const TASK_STATUS: Record<TaskStatus, TaskStatusStyle> = {
  queued:    { icon: 'pi-clock',        color: 'var(--cc-text-dim)', tone: 'neutral', label: 'Queued' },
  running:   { icon: 'pi-spin pi-cog',  color: 'var(--cc-active)',   tone: 'active',  label: 'Running' },
  done:      { icon: 'pi-check-circle', color: 'var(--cc-sev-ok)',   tone: 'ok',      label: 'Done' },
  failed:    { icon: 'pi-times-circle', color: 'var(--cc-sev-fail)', tone: 'fail',    label: 'Failed' },
  cancelled: { icon: 'pi-ban',          color: 'var(--cc-text-dim)', tone: 'neutral', label: 'Cancelled' },
}
