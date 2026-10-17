// What Claude can reach from this profile: the rows of `/api/mcp/connections`, classified by
// `utils/mcpConnections.ts`. Split in two because they live on two surfaces — Cecelia's own
// `cecelia-observer` row is part of this profile's Kiwi setup (Preferences → Kiwi), the rest are
// Settings → Other MCP connections. Both read the active profile's Claude config, so both are
// personal, not machine-wide (docs/todo/SETTINGS_SCOPE_PLAN.md).
import { ref, computed, watch, onMounted } from 'vue'
import { useObserverStore } from '../stores/observer'
import { useKiwiStore } from '../stores/kiwi'
import { useSettingsStore } from '../stores/settings'
import { mcpRows, type McpConnection } from '../utils/mcpConnections'
import { isAuthError } from '../utils/observerSetup'

export function useMcpConnections() {
  const observer = useObserverStore()
  const kiwi = useKiwiStore()
  const settings = useSettingsStore()
  const raw = ref<McpConnection[]>([])

  // The last Kiwi turn failing on auth is the one signal that the CLI is installed but not logged in.
  const authFailed = computed(() => {
    const last = kiwi.turns[kiwi.turns.length - 1]
    return !!last && last.status === 'failed' && isAuthError(last.error || last.reply?.errors?.[0] || '')
  })
  const rows = computed(() =>
    mcpRows(raw.value, observer.terminalState, settings.hiddenMcpAccounts,
            { available: observer.available, authFailed: authFailed.value, path: observer.cliPath }))
  const observerRow = computed(() => rows.value.find(r => r.name === 'cecelia-observer'))
  const otherRows = computed(() => rows.value.filter(r => r.name !== 'cecelia-observer'))
  // The registered install's *checkout name* — the parent of the always-literal `mcp/` leaf, which
  // is the distinguishing bit ("cecelia-feijoa" vs "cecelia-mcp-row"). The full path goes in a tooltip.
  const observerInstallShort = computed(() => {
    const p = observerRow.value?.installPath || ''
    if (!p) return ''
    const parts = p.split(/[\\/]/).filter(Boolean)
    const leaf = parts[parts.length - 1] || ''
    return leaf === 'mcp' ? (parts[parts.length - 2] || leaf) : leaf
  })

  async function load() {
    try { raw.value = (await (await fetch('/api/mcp/connections')).json())?.connections ?? [] }
    catch { raw.value = [] }
  }
  onMounted(() => { void load(); void observer.refresh() })
  // re-read once a setup attempt settles, so the dot reflects the config rather than the click
  watch(() => observer.registering, busy => { if (!busy) void load() })

  // "I just installed it": re-probe the backend (it searches PATH + the installers' dirs every call,
  // so no restart is needed) and re-read the config the install may have created.
  const rechecking = ref(false)
  async function recheck() {
    rechecking.value = true
    try { await Promise.all([observer.refresh(), load()]) }
    finally { rechecking.value = false }
  }

  return { observerRow, otherRows, observerInstallShort, rechecking, recheck }
}
