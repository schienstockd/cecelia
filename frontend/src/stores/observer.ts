import { defineStore } from 'pinia'
import { ref } from 'vue'
import { observerApi, type ObserverSession } from '../utils/serviceApi'
import { useTaskCompletionWatch } from '../composables/useTaskCompletionWatch'
import { useSettingsStore } from './settings'
import { useProjectMetaStore } from './projectMeta'

// Owns the in-app observer state + the "Watch" auto-runner. It lives in a store (not inside
// LabLogPanel) on purpose: the lab-log panel is `v-if`'d, so a WS subscription placed inside it dies
// the moment the panel closes — which is exactly when Watch should still be running (and the sidebar
// badge is for the closed-panel case). App.vue installs the auto-watch once, for the app lifetime.
export const useObserverStore = defineStore('observer', () => {
  const settings = useSettingsStore()
  const pm = useProjectMetaStore()

  const available = ref(false)
  const models = ref<string[]>(['haiku', 'sonnet', 'opus'])
  const session = ref<ObserverSession | null>(null)
  const busy = ref(false)
  const appendTick = ref(0)              // bumped when a pass appends → the open panel reloads entries

  const projectUid = () => pm.current?.uid ?? ''

  async function refresh() {
    const s = await observerApi.status(projectUid() || undefined)
    available.value = s.available
    if (s.models?.length) models.value = s.models
    session.value = s.session ?? null
  }

  // Run one observer pass (manual button or auto Watch). Records + returns the pass; on a real append
  // it bumps appendTick and, if the panel is closed, lights the sidebar badge with a one-line preview.
  async function runPass(trigger: 'manual' | 'auto') {
    const uid = projectUid()
    if (!uid || busy.value || !available.value) return null
    busy.value = true
    try {
      const res = await observerApi.feedback(uid, settings.labLogObserverModel, trigger)
      if (res?.available === false) { available.value = false; return res }
      if (res?.session) session.value = res.session
      if (res?.appended) {
        appendTick.value++
        if (!settings.labLogPanelOpen && res.appendedLine) {
          settings.labLogUnseen = String(res.appendedLine).replace(/^[-*]\s*/, '').trim()
        }
      }
      return res
    } catch { return null }
    finally { busy.value = false }
  }

  async function clear() {
    if (!projectUid()) return
    const res = await observerApi.clear(projectUid())
    session.value = res?.session ?? null
  }

  // "Watch": run an auto pass after a task finishes. POLICY only — the subscribe/debounce MECHANISM
  // is the shared useTaskCompletionWatch backbone (so a future background watcher reuses it).
  const watcher = useTaskCompletionWatch({
    enabled: () => settings.labLogObserverAuto && available.value && !busy.value && !!projectUid(),
    onComplete: () => runPass('auto'),
  })
  const installAutoWatch = () => watcher.install()

  return { available, models, session, busy, appendTick, refresh, runPass, clear, installAutoWatch }
})
