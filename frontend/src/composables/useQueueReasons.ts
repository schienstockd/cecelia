import { computed, onScopeDispose, watch, type Ref } from 'vue'
import type { TaskEntry } from '../stores/tasks'
import { useAppControlStore } from '../stores/appControl'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useTaskDefsStore } from '../stores/taskDefs'
import { inFlightSnapshot, subscribeInFlightPoll } from '../utils/inFlightPoll'
import { queueReason, type QueueReason } from '../utils/queueReason'

/**
 * Why each queued row is waiting (`utils/queueReason.ts`), for a task list.
 *
 * Polls the scheduler snapshot only while `tasks` holds a queued row, and stops with the component.
 * Returns the lookup a row mapper calls; pass `now` so elapsed times in the tip advance.
 */
export function useQueueReasons(tasks: Ref<TaskEntry[]>, now: Ref<number>) {
  const appControl  = useAppControlStore()
  const projectMeta = useProjectMetaStore()
  const project     = useProjectStore()
  const defs        = useTaskDefsStore()

  const hasQueued = computed(() => tasks.value.some(t => t.status === 'queued' && !t.hpcStage))
  let release: (() => void) | null = null
  watch(hasQueued, on => {
    if (on && !release) release = subscribeInFlightPoll()
    else if (!on && release) { release(); release = null }
  }, { immediate: true })
  onScopeDispose(() => { release?.(); release = null })

  const imageNames = computed(() => {
    const m: Record<string, string> = {}
    for (const s of project.sets) for (const i of s.images) m[i.uid] = i.name
    return m
  })

  return (t: TaskEntry): QueueReason | null => queueReason(t, inFlightSnapshot.value, {
    selfProfile:       appControl.activeProfileName,
    currentProjectUid: projectMeta.current?.uid,
    imageName:         uid => imageNames.value[uid],
    projectName:       uid => projectMeta.recent.find(p => p.uid === uid)?.name,
    labelFor:          f => defs.labelFor(f),
    now:               now.value,
  })
}
