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
  // Path of the MCP config Cecelia generates for the spawned agent — reused verbatim as the
  // `claude --mcp-config <path>` line the info panel offers if one-click setup fails.
  const mcpConfigPath = ref('')
  // Is the user's OWN terminal set up? 'missing' | 'stale' | 'current', read from Claude Code's config
  // by the backend. Drives which button the lab-log toolbar shows (setup vs Chat to Claude), so it must
  // come from the real config — never from optimistic local state after a click.
  const terminalState = ref('')
  const session = ref<ObserverSession | null>(null)
  const busy = ref(false)
  const appendTick = ref(0)              // bumped when a pass appends → the open panel reloads entries

  const projectUid = () => pm.current?.uid ?? ''

  async function refresh() {
    const s = await observerApi.status(projectUid() || undefined)
    available.value = s.available
    if (s.models?.length) models.value = s.models
    if (s.mcpConfigPath) mcpConfigPath.value = s.mcpConfigPath
    terminalState.value = s.terminal?.state ?? ''
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
          settings.labLogUnseenKind = 'claude'   // sparkles badge (Cecelia digests use the bell)
          settings.labLogUnseenLevel = ''
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

  // One-click terminal setup (the lab-log toolbar's button until it's done): register the observer MCP
  // in the user's own Claude Code config so plain `claude` has the tools. Idempotent — clicking it again
  // re-syncs a stale entry. Detection is a config-file read on the backend (see _observer_terminal_state);
  // we deliberately never shell out to `claude mcp list`, which health-checks every server.
  const registering = ref(false)
  const registerError = ref('')
  async function registerMcp() {
    if (registering.value) return
    registering.value = true
    registerError.value = ''
    try {
      const res = await observerApi.register()
      // Trust the config read-back, not the exit code: `terminalState` is what the UI branches on.
      terminalState.value = res?.terminal?.state ?? terminalState.value
      if (res?.ok !== true) registerError.value = String(res?.error || 'Setup failed')
    } catch {
      registerError.value = 'Setup failed — is Cecelia still running?'
    } finally { registering.value = false }
  }

  return { available, models, mcpConfigPath, terminalState, session, busy, appendTick,
           registering, registerError,
           refresh, runPass, clear, registerMcp }
})
