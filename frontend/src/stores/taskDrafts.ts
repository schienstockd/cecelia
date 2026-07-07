import { defineStore } from 'pinia'
import { reactive } from 'vue'
import type { ParamValues } from '../tasks/types'

// In-progress (un-run) task-param drafts, so typing params then navigating away and back doesn't
// lose them (module pages except ChainModule unmount on navigation, and TaskRunner's paramValues is
// otherwise a bare ref). Session-scoped/in-memory: survives navigation, resets on a full reload —
// after a reload TaskRunner falls back to the server-saved funParams (persisted on Run).
//
// The draft is keyed to MIRROR how funParams are scoped (image → set): a draft lives at the same
// granularity the form loads/saves at — per driving image when exactly one image is selected, else
// per set. See TaskRunner.fetchSavedParams / api_task_fun_params / _remember_fun_params.

// The scope a draft belongs to: the single selected image, else the set. Mirrors funparams resolution.
export function taskDraftScope(drivingImageUid: string, setUid: string): string {
  return drivingImageUid || setUid
}

// Stable draft key. Pure — unit-tested. Empty when project/fun/scope aren't known yet (no draft).
export function taskDraftKey(projectUid: string, funName: string, scopeUid: string): string {
  return (projectUid && funName && scopeUid) ? `${projectUid}|${funName}|${scopeUid}` : ''
}

export const useTaskDraftsStore = defineStore('taskDrafts', () => {
  const drafts = reactive<Record<string, ParamValues>>({})

  const get   = (key: string): ParamValues | undefined => key ? drafts[key] : undefined
  const set   = (key: string, values: ParamValues) => { if (key) drafts[key] = values }
  const clear = (key: string) => { if (key) delete drafts[key] }

  return { drafts, get, set, clear }
})
