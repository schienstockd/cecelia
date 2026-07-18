import { defineStore } from 'pinia'
import { ref } from 'vue'
import { observerApi, type ObserverSession } from '../utils/serviceApi'
import { useSettingsStore } from './settings'
import { useProjectMetaStore } from './projectMeta'

// Owns the in-app observer state. Claude is ON-DEMAND ONLY — the "Ask Claude" button runs a pass;
// there is no auto-firing "Watch" (removed: most task completions had nothing worth flagging, so the
// auto passes were token noise). Deterministic reporting is Cecelia's job (capture_context! digests +
// QC), not Claude's. Lives in a store (not LabLogPanel, which is `v-if`'d) so state + the closed-panel
// badge survive the panel closing.
export const useObserverStore = defineStore('observer', () => {
  const settings = useSettingsStore()
  const pm = useProjectMetaStore()

  const available = ref(false)
  const models = ref<string[]>(['haiku', 'sonnet', 'opus'])
  const prompt = ref('')                 // the exact instructions the observer runs under (transparency)
  const session = ref<ObserverSession | null>(null)
  const busy = ref(false)
  const appendTick = ref(0)              // bumped when a pass appends → the open panel reloads entries

  const projectUid = () => pm.current?.uid ?? ''

  async function refresh() {
    const s = await observerApi.status(projectUid() || undefined)
    available.value = s.available
    if (s.models?.length) models.value = s.models
    if (s.prompt) prompt.value = s.prompt
    session.value = s.session ?? null
  }

  // Run one observer pass (the on-demand "Ask Claude" button). Records + returns the pass; on a real
  // append it bumps appendTick and, if the panel is closed, lights the sidebar badge with a preview.
  async function runPass() {
    const uid = projectUid()
    if (!uid || busy.value || !available.value) return null
    busy.value = true
    try {
      const res = await observerApi.feedback(uid, settings.labLogObserverModel, 'manual')
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

  return { available, models, prompt, session, busy, appendTick, refresh, runPass, clear }
})
