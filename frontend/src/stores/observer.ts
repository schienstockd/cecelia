import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref } from 'vue'
import { observerApi } from '../utils/serviceApi'

// The assistant's availability, its model allow-list and the user's terminal setup — what Kiwi's
// prompt box and the lab log's setup button both read. The lab log's one-off "Ask Claude" pass (and
// its session / activity log) was removed 2026-09-24: asking is Kiwi's job now, and a terminal session
// still writes the lab log through append_lab_log. Deterministic reporting is Cecelia's job
// (capture_context! digests + QC). A store so the state survives the v-if'd panels closing.
export const useObserverStore = defineStore('observer', () => {
  const available = ref(false)
  const models = ref<string[]>(['haiku', 'sonnet', 'opus'])
  // Path of the MCP config Cecelia generates for the spawned agent — reused verbatim as the
  // `claude --mcp-config <path>` line the info panel offers if one-click setup fails.
  const mcpConfigPath = ref('')
  // Is the user's OWN terminal set up? 'missing' | 'stale' | 'shadowed' | 'current', from Claude's config
  // by the backend. Drives which button the lab-log toolbar shows (setup vs Chat to Claude), so it must
  // come from the real config — never from optimistic local state after a click.
  const terminalState = ref('')

  async function refresh() {
    const s = await observerApi.status()
    available.value = s.available
    if (s.models?.length) models.value = s.models
    if (s.mcpConfigPath) mcpConfigPath.value = s.mcpConfigPath
    terminalState.value = s.terminal?.state ?? ''
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
      // A clean exit that still isn't 'current' means something we don't own blocked it — most often a
      // per-folder entry we couldn't remove. Show the server's message; silence here is what made the
      // button look broken while reporting success.
      else if (terminalState.value && terminalState.value !== 'current')
        registerError.value = String(res?.message || '')
    } catch {
      registerError.value = 'Setup failed — is Cecelia still running?'
    } finally { registering.value = false }
  }

  return { available, models, mcpConfigPath, terminalState, registering, registerError,
           refresh, registerMcp }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useObserverStore, import.meta.hot))
