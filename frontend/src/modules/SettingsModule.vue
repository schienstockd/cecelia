<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useCopyFlash } from '../composables/useCopyFlash'
import PackagesDialog from '../components/PackagesDialog.vue'
import ConfirmButton from '../components/ConfirmButton.vue'
import { napariState, notebooksState, previewState, stateInfo, formatUptime, type ServiceState } from '../utils/serviceStatus'
import { notebooksApi, napariApi, previewApi } from '../utils/serviceApi'
import { useAppControlStore } from '../stores/appControl'
import { useCustomModulesStore, type PluginEntry } from '../stores/customModules'
import { useViewProfilesStore, ALL_PROFILE_ID } from '../stores/viewProfiles'
import ViewProfileEditor from '../components/ViewProfileEditor.vue'
import ChipSelect from '../components/ChipSelect.vue'
import { fetchStorageSummary, reclaimStorage, formatBytes, debrisLine, fetchCompressor, setCompressor,
         fetchStoreLayout, setStoreLayout,
         type StorageSummary, type CompressorSettings, type StoreLayoutSettings } from '../utils/storage'
import { useWsStore } from '../stores/ws'
import { quitConfirmTooltip, quitConfirmLabel } from '../utils/quitWarning'
import { runningTaskCount } from '../utils/runningTasks'
import { useTaskStore } from '../stores/tasks'
import { useObserverStore } from '../stores/observer'
import { mcpRows, type McpConnection } from '../utils/mcpConnections'
import { isAuthError } from '../utils/observerSetup'
import { claudeChatCommand } from '../lib/claudeOverview'
import CcToggle from '../components/CcToggle.vue'
import CcProgressBar from '../components/CcProgressBar.vue'
import SelectionTable, { type SelectionColumn } from '../components/SelectionTable.vue'

const showPackages = ref(false)

// ── Storage ─────────────────────────────────────────────────────────────────
// On-demand scan (walking every image store is expensive — never auto-run on open). Surfaces
// reclaimable ORIGINAL imports of images whose drift/AF/cellpose-corrected variant is now active,
// and frees them in one click (backend keeps the corrected variant working). See utils/storage.ts.
const storage      = ref<StorageSummary | null>(null)
const storageScan  = ref(false)
const storageBusy  = ref(false)
const storageError = ref('')

// Image-store compression (advanced). Server-side setting, not a browser preference — it decides how
// every store the backend writes is encoded, so it lives in custom.toml like the pool limits, and the
// choice list is served rather than duplicated here.
const layout      = ref<StoreLayoutSettings | null>(null)
const layoutBusy  = ref(false)
const layoutError = ref('')
// A table, not chips, for the same reason the compressor is one: the trade-off is the only reason
// there is a choice, so the measured numbers belong on screen at the point of deciding.
const LAYOUT_COLUMNS: SelectionColumn[] = [
  { key: 'label', label: 'Layout' },
  { key: 'keys',  label: 'Chunk key' },
  { key: 'dirs',  label: 'Dirs' },
  { key: 'size',  label: 'On disk' },
  { key: 'read',  label: 'Read' },
]
function layoutTip(row: Record<string, any>) { return String(row.detail ?? '') }
async function loadLayout() {
  try { layout.value = await fetchStoreLayout() } catch (e) { layoutError.value = String(e) }
}
async function changeLayout(name: string) {
  if (!layout.value || name === layout.value.current) return
  layoutBusy.value = true; layoutError.value = ''
  try { layout.value.current = await setStoreLayout(name) }
  catch (e) { layoutError.value = e instanceof Error ? e.message : String(e) }
  finally { layoutBusy.value = false }
}

const compressor      = ref<CompressorSettings | null>(null)
const compressorBusy  = ref(false)
const compressorError = ref('')

async function loadCompressor() {
  try { compressor.value = await fetchCompressor() }
  catch { /* advanced, optional — a failure here must not break the Settings page */ }
}

// Column labels only — every value is a display string the backend measured and formatted.
const COMPRESSOR_COLUMNS: SelectionColumn[] = [
  { key: 'label', label: 'Codec' },
  { key: 'size',  label: 'Store' },
  { key: 'ratio', label: 'vs raw' },
  { key: 'write', label: 'Write' },
  { key: 'read',  label: 'Read/plane' },
  { key: 'url',   label: 'Docs', kind: 'link' },
]

function compressorTip(row: Record<string, any>) {
  return row.name === compressor.value?.default ? 'The measured default' : 'Use this codec for new stores'
}

async function changeCompressor(name: string) {
  if (!compressor.value || name === compressor.value.current) return
  compressorBusy.value = true; compressorError.value = ''
  try { compressor.value.current = await setCompressor(name) }
  catch (e: any) { compressorError.value = e?.message ?? 'Could not change compression' }
  finally { compressorBusy.value = false }
}

async function scanStorage() {
  const uid = projectMeta.current?.uid
  if (!uid) return
  storageScan.value = true; storageError.value = ''
  try { storage.value = await fetchStorageSummary(uid) }
  catch (e: any) { storageError.value = e?.message ?? 'Scan failed' }
  finally { storageScan.value = false }
}

async function reclaimAll() {
  const uid = projectMeta.current?.uid
  if (!uid || !storage.value?.reclaimable.length) return
  storageBusy.value = true; storageError.value = ''
  try {
    await reclaimStorage(uid, storage.value.reclaimable.map(r => r.imageUid))
    await scanStorage()   // re-scan so the numbers reflect what was freed
  } catch (e: any) {
    storageError.value = e?.message ?? 'Reclaim failed'
  } finally {
    storageBusy.value = false
  }
}

const projectMeta = useProjectMetaStore()
const settings    = useSettingsStore()
const appCtl      = useAppControlStore()
const customModules = useCustomModulesStore()

// ── View profiles ────────────────────────────────────────────────────────────
// A curated sidebar: which module pages show, in which order (docs/todo/VIEW_PROFILES_PLAN.md). The
// selection is per user; the definitions are files under <config_dir>/profiles/, authored by the
// editor below. Refreshed when the panel opens so a hand-dropped file shows up without a restart.
const viewProfiles = useViewProfilesStore()
const showProfileEditor = ref(false)
// One chip per profile, plus the implicit "All pages". ChipSelect is the canonical inline selector
// (docs/UI.md) — a handful of named profiles is a chip row, not a dropdown. No per-option tips: the
// labels ARE the profile names, and a tip per chip would fire on top of the control's own tooltip.
const profileOptions = computed(() => [
  { value: ALL_PROFILE_ID, label: 'All pages' },
  ...viewProfiles.profiles.map(p => ({ value: p.id, label: p.label })),
])
onMounted(() => viewProfiles.refresh())

// ── Custom modules ───────────────────────────────────────────────────────────
// User drop-in tasks (docs/CUSTOM_MODULES.md). Reload rescans the config dir for NEWLY dropped .jl;
// edits to already-loaded modules still need a server restart. Refresh status when this panel opens.
onMounted(() => customModules.refresh())

// Display module paths relative to the drop-in root (…/modules/sources/) — the absolute prefix is
// noise; the full path stays in the tooltip. Falls back to the configured dir, then the raw path.
function shortModulePath(p: string): string {
  const m = p.match(/modules[\\/]sources[\\/](.+)$/)
  if (m) return m[1].replace(/\\/g, '/')
  const dir = customModules.dir
  if (dir && p.startsWith(dir)) return p.slice(dir.length).replace(/^[\\/]+/, '')
  return p
}

// ── Plugins ──────────────────────────────────────────────────────────────────
// A plugin is one directory of custom modules installed from a URL (docs/todo/PLUGINS_PLAN.md).
// Install and remove both return the SAME payload the status endpoint does, so the store is refreshed
// from the response rather than by a follow-up fetch that could race the reload.
// Columns for the two plugin tables. A table rather than stacked rows because each entry carries
// several comparable facts (version, what it ships) plus a row-scoped action — and `.save-btn` is
// `display:flex`, i.e. block-level, so in a plain row the button dropped onto its own line under the
// name it belonged to. SelectionTable is THE canonical table (docs/ui/PRIMITIVES.md);
// hand-rolling one here is the exact mistake it exists to prevent.
const PLUGIN_COLUMNS: SelectionColumn[] = [
  { key: 'name',       label: 'Plugin' },
  { key: 'version',    label: 'Version', fixed: true },
  { key: 'categories', label: 'Provides' },
]
const REGISTRY_COLUMNS: SelectionColumn[] = [
  { key: 'name',        label: 'Plugin' },
  { key: 'description', label: 'What it does' },
]

// Everything wrong with a plugin, as ONE line. The API keeps the three apart on purpose — a manifest
// that would not parse, a version mismatch, and a `contributions` block that disagrees with the
// directory fail for unrelated reasons — but the user is asking one question, "is this thing OK".
const pluginFaults = (row: PluginEntry): string =>
  [row.stale ? STALE_MSG : null, row.warning, ...(row.problems ?? [])].filter(Boolean).join(' · ')

// Both processes on purpose. Task code runs in the detached runner (docs/RUNNER.md), which loaded its
// own copy and does NOT come back on a backend restart — so naming only the backend sends the user to
// restart the one process that was not the problem. That is the exact 40 minutes this message exists
// to save: an updated plugin ran its OLD handler against its NEW form, and the error named a param
// the form no longer had.
const STALE_MSG = 'updated on disk — restart the backend, then the runner row, to load it'

const pluginUrl  = ref('')
const pluginRef  = ref('')
const pluginBusy = ref(false)
const pluginMsg  = ref('')
// Updating an already-loaded plugin needs a server restart — Julia cannot redefine a struct in place,
// so `load_custom_modules!` skips a file it has already loaded. The server tells us which case it was;
// saying nothing here is how "my edit did nothing" becomes a support question (Decision 7).
const pluginRestart = ref(false)

// ONE install path, used by both the URL field and a row in the curated list. The list used to just
// FILL the field above — invisible indirection: the button was two sections away from its own effect,
// so it read as doing nothing. A row installs itself now, behind the same confirm.
async function installPlugin(url: string, ref = '') {
  const u = url.trim()
  if (!u) return
  pluginBusy.value = true; pluginMsg.value = ''; pluginRestart.value = false
  try {
    const res = await fetch('/api/plugins/install', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ url: u, ref: ref.trim() }),
    })
    const data = await res.json()
    if (!res.ok) { pluginMsg.value = data.error ?? `HTTP ${res.status}`; return }
    customModules.apply(data)
    pluginRestart.value = !!data.restartRequired
    // Only clear the URL field when it is what was submitted — clearing it after installing a listed
    // plugin would wipe a URL the user had half-typed for something else.
    if (u === pluginUrl.value.trim()) { pluginUrl.value = ''; pluginRef.value = '' }
  } catch (e) {
    pluginMsg.value = String(e)
  } finally { pluginBusy.value = false }
}

// Install (or UPDATE) an example plugin straight from this checkout — no network, no GitHub.
// `docs/examples/plugins/<name>/` is the SOURCE the published repo is mirrored from, so on a checkout
// it is the newest copy by definition. Without this, updating a plugin you are editing meant pushing
// to GitHub and pulling the same files back, and the window where those two disagree is how a form
// three commits stale reached the screen while the fix sat in the worktree.
async function installLocalPlugin(name: string) {
  pluginBusy.value = true; pluginMsg.value = ''; pluginRestart.value = false
  try {
    const res = await fetch('/api/plugins/install-local', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name }),
    })
    const data = await res.json()
    if (!res.ok) { pluginMsg.value = data.error ?? `HTTP ${res.status}`; return }
    customModules.apply(data)
    pluginRestart.value = !!data.restartRequired
  } catch (e) {
    pluginMsg.value = String(e)
  } finally { pluginBusy.value = false }
}

// Registry entries that also exist in this checkout, so the row can offer the local install.
const bundledNames = computed(() => new Set(customModules.bundled.map(b => b.name)))

async function removePlugin(name: string) {
  pluginBusy.value = true; pluginMsg.value = ''
  try {
    const res = await fetch('/api/plugins/remove', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name }),
    })
    const data = await res.json()
    // 409 = one of its tasks is still running. That is a state, not a mistake, so it reads as a
    // message rather than an error the user has to interpret.
    if (!res.ok) { pluginMsg.value = data.error ?? `HTTP ${res.status}`; return }
    customModules.apply(data)
  } catch (e) {
    pluginMsg.value = String(e)
  } finally { pluginBusy.value = false }
}

const editName = ref(projectMeta.current?.name ?? '')
const saving   = ref(false)
const saved    = ref(false)

// Reset when project changes
watch(() => projectMeta.current?.name, n => { editName.value = n ?? '' })

async function saveName() {
  if (!editName.value.trim() || editName.value === projectMeta.current?.name) return
  saving.value = true
  const ok = await projectMeta.renameProject(editName.value)
  saving.value = false
  if (ok) {
    saved.value = true
    setTimeout(() => { saved.value = false }, 2000)
  }
}

// Project ID copy — shared helper, so it flashes like every other copy button (it previously had no
// confirmation at all) and keeps working when the Clipboard API is unavailable.
const { isCopied: uidCopied, copy } = useCopyFlash()
function copyUid() {
  if (projectMeta.current?.uid) copy(projectMeta.current.uid)
}

// ── Software updates ───────────────────────────────────────────────────────
// State + actions live in the shared appControl store — the SAME source the header badge reads, so
// there's one update check, not a per-surface re-implementation. Re-check when this panel opens.
onMounted(() => appCtl.checkUpdate())

// ── Diagnostics + debug console ──────────────────────────────────────────────
interface Diag {
  threads: number; julia: string; version: string; commit?: string; commitCurrent?: string; stale?: boolean; projectsDir: string
  startedAt?: number; uptimeSeconds?: number
  memFreeGB: number; memTotalGB: number; gcLiveMB: number
  host: string; port: number; loopback: boolean
  replEnabled: boolean; replAvailable: boolean; dev: boolean
  napariPort: number; previewPort: number; notebooksPort: number; runnerPort: number
}
const diag = ref<Diag | null>(null)
const diagBusy = ref(false)
const replToggle = ref(false)   // mirrors the server's runtime enable flag (diag.replEnabled)
async function loadDiag() {
  diagBusy.value = true
  try {
    diag.value = await (await fetch('/api/diagnostics')).json() as Diag
    replToggle.value = !!diag.value?.replEnabled
  } catch { diag.value = null }
  finally { diagBusy.value = false }
}
// flip the server-side runtime flag; loopback bind is still required for the console to work (server-side)
async function toggleRepl() {
  const enabled = replToggle.value
  try {
    await fetch('/api/repl/config', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ enabled }),
    })
  } catch { /* ignore — loadDiag re-syncs the true state */ }
  await loadDiag()
}

// gated debug REPL (only rendered when the server reports replEnabled)
interface ReplEntry { code: string; ok: boolean; value?: string; output?: string; error?: string }
const replCode = ref('')
const replBusy = ref(false)
const replLog = ref<ReplEntry[]>([])
async function runRepl() {
  const code = replCode.value.trim()
  if (!code || replBusy.value) return
  replBusy.value = true
  try {
    const res = await fetch('/api/repl', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ code }),
    })
    const d = await res.json() as { ok?: boolean; value?: string; output?: string; error?: string }
    replLog.value.push({ code, ok: res.ok && !!d.ok, value: d.value, output: d.output, error: d.error })
    if (res.ok) replCode.value = ''
  } catch (e) {
    replLog.value.push({ code, ok: false, error: String(e) })
  } finally { replBusy.value = false }
}
// ⌘/Ctrl+Enter runs (plain Enter stays a newline — it's a multi-line editor)
function replKeydown(e: KeyboardEvent) {
  if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') { e.preventDefault(); runRepl() }
}
onMounted(loadDiag)
onMounted(loadCompressor)
onMounted(loadLayout)

// ── System: service control panel ─────────────────────────────────────────────
// Live status of the backend's child processes + per-component and global controls. Status is
// ephemeral UI state (polled) → plain refs, not persisted view state. Pure status→state mapping
// lives in utils/serviceStatus.ts (unit-tested); here we only poll, act, and pick which buttons show.
const napariRaw = ref<{ alive?: boolean; starting?: boolean; bridgeUptimeSeconds?: number | null; bridgeStale?: boolean } | null>(null)
const notebooksRaw = ref<{ running?: boolean; starting?: boolean } | null>(null)
// The task-preview worker. It gets a row because it holds a cellpose model in GPU memory, and the
// toggle that starts it lives on the task page — reachable only while you are there with a previewable
// task selected. Without this row, a preview left running has no off switch.
const previewRaw = ref<{ alive?: boolean; starting?: boolean; imageUid?: string | null } | null>(null)
// The detached task runner. It gets a row for a reason none of the others have: it deliberately
// SURVIVES a backend restart, so it can be running code you have already changed. `commit`/`stale`
// are the only way to tell "my fix isn't working" from "my fix isn't loaded".
interface RunnerStatus {
  enabled?: boolean; running?: boolean; port?: number; pid?: number; adopted?: boolean
  commit?: string; stale?: boolean; protocolMismatch?: boolean; uptimeSeconds?: number; busy?: boolean
  settable?: boolean
}
const runnerRaw = ref<RunnerStatus | null>(null)
const runnerSt = computed<ServiceState>(() => runnerRaw.value?.running ? 'running' : 'stopped')
const napariSt = computed<ServiceState>(() => napariState(napariRaw.value))
const notebooksSt = computed<ServiceState>(() => notebooksState(notebooksRaw.value))
const previewSt = computed<ServiceState>(() => previewState(previewRaw.value))
const projectUid = computed(() => projectMeta.current?.uid ?? '')

// ── Data patches (project-scoped maintenance scripts) ──────────────────────────
// The run streams over the task WS rail (task:log/progress/status keyed by the taskStore entry id),
// so it shows live output + progress here AND in the Tasks list, with a working Stop.
const ws = useWsStore()
const taskStore = useTaskStore()
interface PatchDef { id: string; title: string; description: string }
const patches = ref<PatchDef[]>([])
const patchRunId = ref<Record<string, string>>({})   // patch id → active run's taskStore entry id

onMounted(async () => {
  try {
    const res = await fetch('/api/maintenance/patches')
    if (res.ok) patches.value = ((await res.json()).patches ?? []) as PatchDef[]
  } catch { /* no patches surfaced */ }
})

const patchRun = (patchId: string) => {
  const id = patchRunId.value[patchId]
  return id ? (taskStore.tasks.find(t => t.id === id) ?? null) : null
}
const patchBusy = (patchId: string) => {
  const r = patchRun(patchId)
  return !!r && (r.status === 'running' || r.status === 'queued')
}
function runPatch(p: PatchDef, apply: boolean) {
  const uid = projectMeta.current?.uid
  if (!uid || patchBusy(p.id)) return
  const entry = taskStore.add({
    module: 'maintenance', label: `${p.title}${apply ? '' : ' (dry-run)'}`,
    imageUid: '', imageName: '', status: 'queued',
    taskName: p.id, funName: `maintenance.${p.id}`, params: { apply }, projectUid: uid,
    startedAt: new Date(),
  })
  patchRunId.value = { ...patchRunId.value, [p.id]: entry.id }
  ws.send({ type: 'maintenance:run', taskId: entry.id, patchId: p.id, projectUid: uid, apply })
}
function cancelPatch(patchId: string) {
  const id = patchRunId.value[patchId]
  if (id) ws.send({ type: 'maintenance:cancel', taskId: id })
}
// the port serving THIS window (Vite :5173 in dev; the backend :8080 in prod) — the GUI isn't a
// controllable service, we just show it so the full picture of occupied ports is visible.
const guiPort = computed(() => location.port || (location.protocol === 'https:' ? '443' : '80'))

const svcBusy = ref('')     // which row's action is in flight ('napari' | 'notebooks' | 'app')
const svcMsg = ref('')

// ── Napari discrete-GPU toggle ──────────────────────────────────────────────
// Persisted in the settings store (localStorage); the backend holds the authoritative launch-time
// flag. `gpuSupported` is false off Linux (there GPU choice is an OS/driver setting → toggle is a
// no-op). Flipping it POSTs the flag and restarts napari (if running) so it takes effect now.
const gpuSupported = ref(true)
const gpuBusy = ref(false)
async function loadGpu() {
  try {
    const d = await (await fetch('/api/napari/gpu')).json()
    gpuSupported.value = d.supported !== false
  } catch { /* leave optimistic default; toggle still works */ }
}
async function toggleGpu() {
  gpuBusy.value = true; svcMsg.value = ''
  const which = settings.napariDiscreteGpu ? 'discrete' : 'default'
  try {
    const res = await fetch('/api/napari/gpu', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ enabled: settings.napariDiscreteGpu }),
    })
    const d = await res.json()
    if (d.needsRestart) {
      await napariApi.restart()
      svcMsg.value = `Napari restarting on the ${which} GPU — reopen the image to reload its layers.`
    } else {
      svcMsg.value = `Napari will use the ${which} GPU next time it starts.`
    }
  } catch {
    svcMsg.value = 'Could not update the GPU setting.'
  } finally { gpuBusy.value = false; setTimeout(pollServices, 500) }
}
onMounted(loadGpu)

async function pollServices() {
  try { napariRaw.value = await (await fetch('/api/napari/status')).json() } catch { napariRaw.value = null }
  try { notebooksRaw.value = await (await fetch('/api/notebooks/status')).json() } catch { notebooksRaw.value = null }
  try { previewRaw.value = await previewApi.status() } catch { previewRaw.value = null }
  try { runnerRaw.value = await (await fetch('/api/runner/status')).json() } catch { runnerRaw.value = null }
}
let svcTimer: number | undefined
onMounted(() => { pollServices(); svcTimer = window.setInterval(pollServices, 4000) })
onUnmounted(() => { if (svcTimer) window.clearInterval(svcTimer) })

// ── MCP connections ────────────────────────────────────────────────────────────
// What Claude can reach. NOT polled: the config only changes when the user (or our own setup button)
// edits it, so it's read on open and after a registration attempt. Row model in utils/mcpConnections.ts.
const observer = useObserverStore()
const mcpRaw = ref<McpConnection[]>([])
const hiddenAccountConnectors = computed(() => settings.hiddenMcpAccounts)
// The Claude Code CLI row leads the list. Its "not detected / not logged in" state used to be a
// banner in the lab-log panel; with the connections panel here, that banner was a second home for
// the same fact — and the one further from where you act on it.
const observerAuthFailed = computed(() => {
  const last = observer.session?.passes?.[0]
  return !!last && !last.ok && isAuthError(last.note)
})
const mcpConnectionRows = computed(() =>
  mcpRows(mcpRaw.value, observer.terminalState, settings.hiddenMcpAccounts,
          { available: observer.available, authFailed: observerAuthFailed.value }))
function hideAccountConnector(name: string) {
  if (!settings.hiddenMcpAccounts.includes(name)) settings.hiddenMcpAccounts.push(name)
}
async function loadMcpConnections() {
  try { mcpRaw.value = (await (await fetch('/api/mcp/connections')).json())?.connections ?? [] }
  catch { mcpRaw.value = [] }
}
onMounted(() => { loadMcpConnections(); observer.refresh() })
// re-read once a setup attempt settles, so the dot reflects the config rather than the click
watch(() => observer.registering, busy => { if (!busy) loadMcpConnections() })

// The failed-setup fallback command lives here now (diagnostics), not in the lab-log toolbar.
const { isCopied: observerCmdCopied, copy: copyCmdFlash } = useCopyFlash()
const observerFallbackCommand = computed(() => claudeChatCommand(observer.mcpConfigPath))
const copyObserverFallback = () => copyCmdFlash(observerFallbackCommand.value)

async function napariAction(kind: 'restart' | 'stop') {
  svcBusy.value = 'napari'; svcMsg.value = ''
  try {
    await (kind === 'restart' ? napariApi.restart() : napariApi.close())
    svcMsg.value = kind === 'restart' ? 'Napari restarting — reopen the image to reload its layers.' : 'Napari stopped.'
  } catch { svcMsg.value = 'Napari action failed.' }
  finally { svcBusy.value = ''; setTimeout(pollServices, 500) }
}
// Stop only. Starting a preview means previewing SOMETHING — it needs a task's params and an open
// image, which this panel has neither of; a Start button here would either do nothing visible or need
// to invent params. The task page owns starting; this owns the off switch.
async function previewStop() {
  svcBusy.value = 'preview'; svcMsg.value = ''
  try {
    await previewApi.stop()
    svcMsg.value = 'Preview stopped — GPU memory released.'
  } catch { svcMsg.value = 'Could not stop the preview worker.' }
  finally { svcBusy.value = ''; setTimeout(pollServices, 500) }
}
// Restart REFUSES while the runner is busy, and that refusal is the feature: the runner holds work
// this app does not, so discarding it silently is the one thing this button must never do. The second
// press is the user overriding, with the count in front of them.
// The opt-in itself. Off does NOT stop a busy runner — see the endpoint; the row keeps showing it
// until it drains, which is the truth rather than a tidier lie.
async function runnerToggle(on: boolean) {
  svcBusy.value = 'runner'; svcMsg.value = ''
  try {
    const res = await fetch('/api/runner/enabled', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ enabled: on }),
    })
    const d = await res.json()
    svcMsg.value = res.ok ? d.message : (d.error ?? 'Could not change the task runner setting.')
  } catch { svcMsg.value = 'Could not change the task runner setting.' }
  finally { svcBusy.value = ''; setTimeout(pollServices, 500) }
}
async function runnerRestart(force = false) {
  svcBusy.value = 'runner'; svcMsg.value = ''
  try {
    const res = await fetch('/api/runner/restart', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ force }),
    })
    const d = await res.json()
    svcMsg.value = res.ok ? 'Task runner restarting…'
      : `${d.error ?? 'Could not restart the task runner.'} Press Restart again to stop them anyway.`
  } catch { svcMsg.value = 'Could not restart the task runner.' }
  finally { svcBusy.value = ''; setTimeout(pollServices, 500) }
}
async function notebooksAction(kind: 'start' | 'stop' | 'restart') {
  svcBusy.value = 'notebooks'; svcMsg.value = ''
  try {
    if (kind === 'stop') await notebooksApi.shutdown()
    else if (kind === 'start') await notebooksApi.launch(projectUid.value)
    else await notebooksApi.restart(projectUid.value)
    svcMsg.value = kind === 'stop' ? 'Notebooks stopped.' : kind === 'start' ? 'Notebooks starting…' : 'Notebooks restarting…'
  } catch { svcMsg.value = 'Notebooks action failed.' }
  finally { svcBusy.value = ''; setTimeout(pollServices, 500) }
}
// app-level actions (Quit / dev Restart) live in the shared appControl store — same logic the sidebar
// footer uses. We mirror its status into svcMsg and refresh the pills once the backend is back.
async function appRestart() {
  svcMsg.value = 'Backend restarting…'
  await appCtl.restartBackend()
  svcMsg.value = appCtl.message
  pollServices()
}
// Quit reports what it will kill — shutdown exits the backend without waiting for in-flight work.
// Same builders as the sidebar footer so the two entry points can't drift.
const quitTasks   = ref(0)
const quitConfirm = computed(() => quitConfirmTooltip(quitTasks.value))
const quitLabel   = computed(() => quitConfirmLabel(quitTasks.value))
async function armQuit(arm: () => void) {
  arm()
  quitTasks.value = await runningTaskCount()
}
async function quitApp() {
  await appCtl.quit()
  svcMsg.value = appCtl.message
}
// dev worktree switch: relaunch the backend from another checkout (backend/:8080 only)
onMounted(() => appCtl.refreshWorktrees())
// folder name of a worktree path — labels the option so the PRIMARY ("main") checkout is
// recognisable even when it's on a feature branch (the branch label alone can't identify it).
function wtFolder(path: string): string { return path.split('/').filter(Boolean).pop() ?? path }
async function switchWt(path: string) {
  if (!path) return
  svcMsg.value = 'Switching worktree…'
  await appCtl.switchWorktree(path)
  svcMsg.value = appCtl.message
  pollServices()
}
</script>

<template>
  <div class="settings-page">
    <div class="settings-cols">
    <div class="settings-col">

    <!-- ── Project ─────────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Project</h2>

      <template v-if="projectMeta.current">
        <!-- Name (grows) + Project ID (6 chars, narrow) on one row -->
        <div class="field-pair">
          <div class="field field-grow">
            <label class="field-label">Name</label>
            <div class="field-row">
              <input
                class="field-input"
                v-tooltip.right="'Project name shown in the header and picker'"
                v-model="editName"
                @keydown.enter="saveName"
                placeholder="Project name"
              />
              <button
                class="save-btn"
                :disabled="saving || !editName.trim() || editName === projectMeta.current?.name"
                @click="saveName"
                v-tooltip.right="'Apply the new project name'"
              >
                <i :class="['pi', saved ? 'pi-check' : saving ? 'pi-spin pi-spinner' : 'pi-check']" />
                {{ saved ? 'Applied' : 'Apply' }}
              </button>
            </div>
          </div>

          <div class="field field-id">
            <label class="field-label">Project ID</label>
            <div class="field-row">
              <input class="field-input mono" :value="projectMeta.current.uid" readonly
                     v-tooltip.bottom="'Read-only unique identifier used internally'" />
              <button class="icon-btn cc-btn cc-btn-bare cc-btn-icon cc-btn-lg" @click="copyUid"
                v-tooltip.left="uidCopied() ? 'Copied!' : 'Copy project ID'">
                <i :class="['pi', uidCopied() ? 'pi-check' : 'pi-copy']" />
              </button>
            </div>
          </div>
        </div>
      </template>

      <p v-else class="no-project cc-muted cc-fs-md">No project open. Open or create a project first.</p>
    </section>

    <!-- ── Interface ───────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Interface</h2>

      <div class="field">
        <CcToggle class="toggle-row" v-model="settings.taskListAutoFollow" label="Auto-follow running tasks in task manager"
          v-tooltip.right="'When a task starts running, automatically select it in the task manager log panel'" />
      </div>

      <div class="field">
        <CcToggle class="toggle-row" v-model="settings.autoRefreshOnTask" label="Auto-refresh plots when tasks finish"
          v-tooltip.right="'Reload plots automatically when a task finishes'" />
      </div>

      <!-- View profile: curate the sidebar down to the pages this user actually works on. Hidden
           pages stay reachable by URL — this declutters, it does not restrict. -->
      <div class="field">
        <label class="field-label">View profile</label>
        <div class="field-row">
          <!-- Edit FIRST: the chip row grows with every profile, and a trailing button would drift
               right (and eventually wrap) as it does. A fixed control belongs on the fixed side. -->
          <button class="save-btn" @click="showProfileEditor = true"
                  v-tooltip.right="'Create or change a profile'">
            <i class="pi pi-pencil" /> Edit
          </button>
          <ChipSelect :options="profileOptions" :model-value="settings.viewProfile"
                      aria-label="Active view profile"
                      v-tooltip.right="'Show only the pages this profile lists'"
                      @update:model-value="viewProfiles.select($event as string)" />
        </div>
        <span class="field-hint cc-muted cc-fs-xs">
          Hides sidebar pages you don't use. Hidden pages still open by URL.
        </span>
        <span v-for="e in viewProfiles.errors" :key="e.file" class="field-hint cc-muted-warn cc-fs-xs">
          {{ e.file }}: {{ e.error }}
        </span>
      </div>
    </section>

    <!-- ── Software updates ────────────────────────────────────────────── -->
    <section class="settings-section" data-guide="settings.updates">
      <h2 class="section-title">Software updates</h2>

      <div class="field">
        <label class="field-label">Version</label>
        <div class="field-row">
          <input class="field-input mono" :value="appCtl.updateCurrent || '—'" readonly
                 v-tooltip.right="'Cecelia version currently running'" />
          <button
            class="save-btn"
            :disabled="appCtl.updateChecking"
            @click="appCtl.checkUpdate"
            v-tooltip.right="'Check GitHub for a newer release'"
          >
            <i :class="['pi', appCtl.updateChecking ? 'pi-spin pi-spinner' : 'pi-refresh']" />
            {{ appCtl.updateChecking ? 'Checking…' : 'Check' }}
          </button>
        </div>
        <span v-if="!appCtl.updateAvailable && appCtl.updateCurrent && !appCtl.updateMsg" class="field-hint cc-muted cc-fs-xs">
          You're on the latest version.
        </span>
      </div>

      <!-- per-user install: in-app update -->
      <div v-if="appCtl.updateAvailable && appCtl.canApplyUpdate" class="field">
        <button
          class="save-btn"
          :disabled="appCtl.updateBusy"
          @click="appCtl.applyUpdate"
          v-tooltip.right="`Download ${appCtl.updateLatest} and stage it; restart Cecelia to finish`"
        >
          <i :class="['pi', appCtl.updateBusy ? 'pi-spin pi-spinner' : 'pi-download']" />
          {{ appCtl.updateBusy ? 'Updating…' : `Update to ${appCtl.updateLatest}` }}
        </button>
      </div>

      <!-- shared system-wide install: updates are admin-only (see docs/todo/ONBOARDING_PLAN.md D4/D5) -->
      <span v-else-if="appCtl.updateAvailable && appCtl.updateScope === 'system'" class="field-hint cc-muted cc-fs-xs">
        {{ appCtl.updateLatest }} is available. This is a shared installation — updates must be run by
        an administrator (re-run the install-system script).
      </span>

      <span v-if="appCtl.updateMsg" class="field-hint cc-muted cc-fs-xs">{{ appCtl.updateMsg }}</span>
    </section>

    <!-- ── Storage ──────────────────────────────────────────────────────── -->
    <section class="settings-section" data-guide="settings.storage">
      <h2 class="section-title">Storage</h2>

      <!-- Advanced: what compression the image stores we write use. A TABLE, not a dropdown — the
           trade-off is the only reason there is a choice, so the measured numbers belong on screen at
           the point of deciding. Every value is a display string from the backend. -->
      <div v-if="compressor" class="field">
        <div class="cmp-head">
          <span class="svc-name">Image compression</span>
          <span class="field-hint cc-muted cc-fs-xs">{{ compressor.measuredOn }}</span>
        </div>
        <SelectionTable :columns="COMPRESSOR_COLUMNS" :rows="compressor.choices"
                        :model-value="compressor.current" :disabled="compressorBusy"
                        :row-tooltip="compressorTip"
                        @update:model-value="changeCompressor" />
        <span class="field-hint cc-muted cc-fs-xs">Applies to new stores only</span>
      </div>
      <span v-if="compressorError" class="field-hint cc-muted-error cc-fs-xs">{{ compressorError }}</span>

      <!-- Store LAYOUT. Same shape as the compressor above, deliberately: same kind of decision, so
           the measured numbers go on screen rather than behind a chip. The rows are the three VIABLE
           combinations of NGFF version + separator, not two independent controls — flat keys and NGFF
           0.5 cannot be combined (bioformats2raw silently writes zarr v2 for that pair), so the
           impossible state is simply unreachable instead of something to warn about. -->
      <div v-if="layout" class="field">
        <div class="cmp-head">
          <span class="svc-name">Store layout</span>
          <span class="field-hint cc-muted cc-fs-xs">{{ layout.measuredOn }}</span>
        </div>
        <SelectionTable :columns="LAYOUT_COLUMNS" :rows="layout.choices"
                        :model-value="layout.current" :disabled="layoutBusy"
                        :row-tooltip="layoutTip"
                        @update:model-value="changeLayout" />
        <span class="field-hint cc-muted cc-fs-xs">Default for new imports; existing images keep theirs</span>
      </div>
      <span v-if="layoutError" class="field-hint cc-muted cc-fs-xs" style="color: var(--cc-sev-fail);">{{ layoutError }}</span>

      <div class="field">
        <div class="field-row">
          <!-- The tour anchors HERE rather than on the "Free up space" button: that one is behind
               `v-if="storage.reclaimable.length"`, so it does not exist until a scan has run and never
               exists on a project with nothing to reclaim — an anchor the guide could point at only
               sometimes. Scan is always present, and it is the first click either way. -->
          <button class="save-btn" data-guide="settings.storageScan"
                  :disabled="storageScan || !projectMeta.current" @click="scanStorage"
                  v-tooltip.top="'Scan this project on disk (may take a moment for large projects)'">
            <i :class="['pi', storageScan ? 'pi-spin pi-spinner' : 'pi-search']" />
            {{ storage ? 'Re-scan' : 'Scan storage' }}
          </button>
        </div>
        <span v-if="!storage && !storageScan" class="field-hint cc-muted cc-fs-xs">
          Scan for disk usage and superseded image versions that can be freed.
        </span>
        <span v-if="storageError" class="field-hint cc-muted-error cc-fs-xs">{{ storageError }}</span>
      </div>

      <template v-if="storage">
        <div class="stor-line">
          <span v-tooltip.top="'Total size of the image OME-ZARRs in this project'">Images in project</span>
          <strong>{{ formatBytes(storage.imageBytes) }}</strong>
          <span>Disk free</span><strong>{{ formatBytes(storage.diskAvailable) }} / {{ formatBytes(storage.diskTotal) }}</strong>
        </div>

        <!-- Derived output, reported but never freed from here: dropping analysis is a deliberate
             per-image act in the Import page's Delete modal (IMAGE_DELETE_PLAN Decision 5). -->
        <div v-if="storage.analysisBytes" class="stor-line">
          <span v-tooltip.top="'Segmentations, measurements, gating, clustering and spatial output'">Analysis</span>
          <strong>{{ formatBytes(storage.analysisBytes) }}</strong>
          <span class="field-hint cc-muted cc-fs-xs">
            drop it per image in Import → Delete → All analysis
          </span>
        </div>

        <!-- Leftovers a cancelled/crashed run abandoned — bytes nothing in the UI can reach. Shown
             here so the cleanup announces itself instead of waiting to be found in Data patches. Not
             actionable from this box on purpose: the patch shows the list before deleting anything. -->
        <div v-if="debrisLine(storage.debris)" class="stor-line">
          <span v-tooltip.top="'Free these in Data patches → Remove leftover stores'">Leftover from cancelled runs</span>
          <strong>{{ debrisLine(storage.debris) }}</strong>
          <span v-if="storage.debris?.activeSkipped" class="field-hint cc-muted cc-fs-xs">
            +{{ storage.debris.activeSkipped }} in use, not counted
          </span>
        </div>

        <div v-if="storage.reclaimable.length" class="stor-reclaim">
          <div class="stor-reclaim-head">
            Reclaimable <strong>{{ formatBytes(storage.reclaimableBytes) }}</strong>
            <span class="field-hint cc-muted cc-fs-xs">({{ storage.reclaimable.length }} image{{ storage.reclaimable.length > 1 ? 's' : '' }} with superseded versions; the active version is kept)</span>
          </div>
          <ul class="stor-list">
            <li v-for="r in storage.reclaimable.slice(0, 8)" :key="r.imageUid">
              <span class="stor-name">{{ r.name || r.imageUid }}</span>
              <span class="stor-size">{{ formatBytes(r.bytes) }}</span>
              <span class="field-hint cc-muted cc-fs-xs"
                    v-tooltip.top="'Frees: ' + (r.versions?.map(v => v.valueName).join(', ') ?? '') + ' — keeps ' + r.activeVersion">
                → keeps {{ r.activeVersion }}
              </span>
            </li>
            <li v-if="storage.reclaimable.length > 8" class="field-hint cc-muted cc-fs-xs">…{{ storage.reclaimable.length - 8 }} more</li>
          </ul>
          <ConfirmButton @confirm="reclaimAll" v-slot="{ armed, arm, confirm, cancel }">
            <button v-if="!armed" class="save-btn danger" :disabled="storageBusy" @click="arm"
                    v-tooltip.top="'Delete every non-active image version'">
              <i :class="['pi', storageBusy ? 'pi-spin pi-spinner' : 'pi-trash']" /> Free up space
            </button>
            <template v-else>
              <button class="save-btn danger" @click="confirm">
                <i class="pi pi-trash" /> Free {{ formatBytes(storage.reclaimableBytes) }} across {{ storage.reclaimable.length }} image{{ storage.reclaimable.length > 1 ? 's' : '' }}
              </button>
              <button class="save-btn ghost" @click="cancel">Cancel</button>
            </template>
          </ConfirmButton>
        </div>
        <span v-else class="field-hint cc-muted cc-fs-xs">Nothing to reclaim — every image has only its active version.</span>
      </template>
    </section>

    <section class="settings-section">
      <h2 class="section-title">Custom modules</h2>
      <div class="field">
        <label class="field-label">Modules directory</label>
        <div class="field-row">
          <input class="field-input mono" :value="customModules.dir || '—'" readonly
                 v-tooltip.right="'Where drop-in task modules are loaded from'" />
          <button
            class="save-btn"
            :disabled="customModules.loading"
            @click="customModules.reload"
            v-tooltip.right="'Rescan for newly dropped modules; edits need a server restart'"
          >
            <i :class="['pi', customModules.loading ? 'pi-spin pi-spinner' : 'pi-refresh']" />
            {{ customModules.loading ? 'Reloading…' : 'Reload' }}
          </button>
        </div>
        <span class="field-hint cc-muted cc-fs-xs">
          Drop tasks into this folder to add them without a rebuild — see docs/CUSTOM_MODULES.md.
        </span>
      </div>

      <div v-if="customModules.modules.length" class="cm-list">
        <div v-for="m in customModules.modules" :key="m.path" class="cm-row">
          <span class="svc-pill" :class="m.status === 'ok' ? 'ok' : 'err'">
            <span class="dot" /> {{ m.status === 'ok' ? 'loaded' : 'error' }}
          </span>
          <span class="cm-path mono" v-tooltip.top="m.error || m.path">{{ shortModulePath(m.path) }}</span>
        </div>
      </div>
      <span v-else class="field-hint cc-muted cc-fs-xs">No custom modules loaded.</span>

      <!-- fun_name clashes: NOT load failures, so they cannot show in the list above (those files
           loaded fine — they just lost the name). Without this the task is simply absent from the UI
           with nothing anywhere saying why. -->
      <div v-if="customModules.clashes.length" class="cm-list">
        <div v-for="c in customModules.clashes" :key="c.path + c.funName" class="cm-row">
          <span class="svc-pill err"><span class="dot" /> clash</span>
          <span class="cm-path mono"
                v-tooltip.top="`${c.path} — ${c.winner ?? 'a built-in'} keeps this name`">
            {{ c.funName }} — {{ c.winnerTier }} wins
          </span>
        </div>
      </div>
    </section>

    <!-- ── Plugins (module sets installed from a URL) ─────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Plugins</h2>
      <p class="field-hint cc-muted cc-fs-xs">
        Module sets installed from a URL. Plugin code runs unsandboxed, with full access to this machine.
      </p>

      <div class="field">
        <label class="field-label">Install from URL</label>
        <div class="field-row">
          <input class="field-input mono" v-model="pluginUrl" :disabled="pluginBusy"
                 placeholder="https://github.com/owner/repo"
                 v-tooltip.right="'A git repo or a tarball URL'" />
          <input class="field-input mono" v-model="pluginRef" :disabled="pluginBusy"
                 placeholder="tag or commit"
                 v-tooltip.right="'Pin a tag or commit; blank takes the default branch'" />
          <ConfirmButton @confirm="installPlugin(pluginUrl, pluginRef)" v-slot="{ armed, arm, confirm, cancel }">
            <button v-if="!armed" class="save-btn" :disabled="pluginBusy || !pluginUrl.trim()" @click="arm"
                    v-tooltip.top="'Fetch and install — this code is not sandboxed'">
              <i :class="['pi', pluginBusy ? 'pi-spin pi-spinner' : 'pi-download']" /> Install
            </button>
            <template v-else>
              <button class="save-btn danger" @click="confirm"
                      v-tooltip.top="'Runs with full access to this machine'">
                <i class="pi pi-download" /> Confirm install
              </button>
              <button class="save-btn ghost" @click="cancel">Cancel</button>
            </template>
          </ConfirmButton>
        </div>
        <span v-if="pluginMsg" class="field-hint cc-fs-xs">
          <i class="pi pi-exclamation-triangle" /> {{ pluginMsg }}
        </span>
        <span v-if="pluginRestart" class="field-hint cc-muted cc-fs-xs">
          Restart the server to pick up the update.
        </span>
      </div>

      <SelectionTable v-if="customModules.plugins.length" class="plugin-table"
                      selection-mode="none" id-key="name" density="compact" fit="fill"
                      :columns="PLUGIN_COLUMNS" :rows="customModules.plugins">
        <template #cell-name="{ row }">
          <span v-tooltip.top="row.error || row.description || row.dir">{{ row.name }}</span>
          <i v-if="pluginFaults(row)" class="pi pi-exclamation-triangle"
             v-tooltip.top="pluginFaults(row)" />
        </template>
        <!-- categories is an array; the table renders values verbatim by design, so join it here
             rather than teaching the table to format (it would become a second formatter). -->
        <template #cell-categories="{ row }">{{ row.categories.join(', ') || '—' }}</template>
        <template #actions="{ row }">
          <ConfirmButton @confirm="removePlugin(row.name)" v-slot="{ armed, arm, confirm, cancel }">
            <button v-if="!armed" class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="pluginBusy"
                    @click="arm" v-tooltip.top="'Remove this plugin'"><i class="pi pi-trash" /></button>
            <template v-else>
              <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="confirm"
                      v-tooltip.top="'Confirm remove'"><i class="pi pi-check" /></button>
              <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="cancel"
                      v-tooltip.top="'Cancel'"><i class="pi pi-times" /></button>
            </template>
          </ConfirmButton>
        </template>
      </SelectionTable>
      <span v-else class="field-hint cc-muted cc-fs-xs">No plugins installed.</span>

      <!-- The curated list. Not a search index: these are the ones we vouch for; anything else goes
           in the URL field above. -->
      <div v-if="customModules.registry.length" class="field">
        <label class="field-label">Available</label>
        <SelectionTable class="plugin-table" selection-mode="none" id-key="name"
                        density="compact" fit="fill"
                        :columns="REGISTRY_COLUMNS" :rows="customModules.registry">
          <template #actions="{ row }">
            <!-- In this checkout: install or UPDATE from disk. Offered even when already installed —
                 that IS the case it exists for, editing a plugin and wanting the app to see it. -->
            <button v-if="bundledNames.has(row.name)" class="cc-btn cc-btn-ghost cc-btn-icon"
                    :disabled="pluginBusy" @click="installLocalPlugin(row.name)"
                    v-tooltip.top="row.installed ? 'Update from this checkout — no network'
                                                 : 'Install from this checkout — no network'">
              <i :class="['pi', pluginBusy ? 'pi-spin pi-spinner' : 'pi-folder-open']" />
            </button>
            <span v-if="row.installed" class="cc-muted cc-fs-xs">installed</span>
            <ConfirmButton v-else @confirm="installPlugin(row.url, row.ref ?? '')"
                           v-slot="{ armed, arm, confirm, cancel }">
              <button v-if="!armed" class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="pluginBusy"
                      @click="arm" v-tooltip.top="'Install — this code is not sandboxed'">
                <i :class="['pi', pluginBusy ? 'pi-spin pi-spinner' : 'pi-download']" />
              </button>
              <template v-else>
                <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="confirm"
                        v-tooltip.top="'Confirm install — runs with full access to this machine'">
                  <i class="pi pi-check" /></button>
                <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="cancel"
                        v-tooltip.top="'Cancel'"><i class="pi pi-times" /></button>
              </template>
            </ConfirmButton>
          </template>
        </SelectionTable>
      </div>
    </section>

    <!-- ── Data patches (project-scoped maintenance scripts) ──────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Data patches</h2>
      <p class="field-hint cc-muted cc-fs-xs">One-off fixes to the open project's data. Dry-run first to see what would change.</p>
      <div v-if="!projectMeta.current" class="field-hint cc-muted cc-fs-xs">Open a project to run patches.</div>
      <div v-for="p in patches" :key="p.id" class="patch-row">
        <div class="patch-head">
          <span class="patch-title">{{ p.title }}</span>
          <span class="patch-actions">
            <button class="save-btn" :disabled="!projectMeta.current || patchBusy(p.id)" @click="runPatch(p, false)"
                    v-tooltip.top="'List what would change — writes nothing'">
              <i :class="['pi', patchBusy(p.id) ? 'pi-spin pi-spinner' : 'pi-search']" /> Dry-run
            </button>
            <ConfirmButton @confirm="runPatch(p, true)" v-slot="{ armed, arm, confirm, cancel }">
              <button v-if="!armed" class="save-btn danger" :disabled="!projectMeta.current || patchBusy(p.id)" @click="arm"
                      v-tooltip.top="'Write changes to this project\'s data'"><i class="pi pi-play" /> Apply</button>
              <template v-else>
                <button class="save-btn danger" @click="confirm"><i class="pi pi-play" /> Confirm apply</button>
                <button class="save-btn ghost" @click="cancel">Cancel</button>
              </template>
            </ConfirmButton>
            <button v-if="patchBusy(p.id)" class="save-btn ghost" @click="cancelPatch(p.id)"><i class="pi pi-times" /> Stop</button>
          </span>
        </div>
        <span class="field-hint cc-muted cc-fs-xs">{{ p.description }}</span>
        <div v-if="patchRun(p.id)" class="patch-run">
          <CcProgressBar v-if="patchRun(p.id)!.progress != null" class="patch-bar" size="bar"
            :value="patchRun(p.id)!.progress" :aria-label="`${p.title} progress`" />
          <pre class="repl-log patch-log">{{ patchRun(p.id)!.log.join('\n') }}</pre>
          <span class="field-hint cc-muted cc-fs-xs">status: {{ patchRun(p.id)!.status }}</span>
        </div>
      </div>
    </section>

    </div>
    <div class="settings-col">

    <!-- ── System (service control panel) ──────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">System
        <span v-if="diag?.dev" class="svc-tag" v-tooltip.top="'Development server (pixi run dev, Revise hot-reload)'">dev</span>
      </h2>

      <div class="svc-row">
        <span class="svc-name">Application</span>
        <span class="svc-pill ok"><span class="dot" /> Running</span>
        <span class="svc-port cc-muted cc-fs-xs" v-tooltip.top="'Backend HTTP/WS server'">:{{ diag?.port ?? '8080' }}</span>
        <span class="svc-actions">
          <button v-if="diag?.dev" class="save-btn" :disabled="appCtl.busy" @click="appRestart"
                  v-tooltip.top="'Restart the backend (dev); the page reconnects'">
            <i :class="['pi', appCtl.busy ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Restart
          </button>
          <ConfirmButton @confirm="quitApp" v-slot="{ armed, arm, confirm, cancel }">
            <button v-if="!armed" class="save-btn danger" :disabled="appCtl.busy" @click="armQuit(arm)"
                    v-tooltip.top="'Stop napari, notebooks and the backend, then exit Cecelia'">
              <i class="pi pi-power-off" /> Quit
            </button>
            <template v-else>
              <button class="save-btn danger" @click="confirm"
                      v-tooltip.top="quitConfirm"><i class="pi pi-power-off" /> {{ quitLabel }}</button>
              <button class="save-btn ghost" @click="cancel">Cancel</button>
            </template>
          </ConfirmButton>
        </span>
      </div>

      <!-- dev worktree switch: relaunch the backend from another git worktree (avoids the console).
           Backend :8080 only — a frontend-only branch still needs its own Vite (see docs/DEV.md). -->
      <div v-if="diag?.dev && appCtl.canSwitch && appCtl.worktrees.length > 1" class="svc-row">
        <span class="svc-name">Worktree</span>
        <select class="wt-select" :disabled="appCtl.busy"
                :value="appCtl.worktrees.find(w => w.current)?.path ?? ''"
                @change="switchWt(($event.target as HTMLSelectElement).value)"
                v-tooltip.top="'Relaunch the backend from another git worktree (dev)'">
          <option v-for="w in appCtl.worktrees" :key="w.path" :value="w.path">
            {{ wtFolder(w.path) }} — {{ w.branch }}{{ w.primary ? ' (main)' : '' }}{{ w.current ? ' (current)' : '' }}
          </option>
        </select>
      </div>

      <div class="svc-row">
        <span class="svc-name">Napari viewer</span>
        <span class="svc-pill" :class="stateInfo(napariSt).tone"><span class="dot" /> {{ stateInfo(napariSt).label }}</span>
        <span class="svc-port cc-muted cc-fs-xs" v-tooltip.top="'Napari bridge WebSocket'">:{{ diag?.napariPort ?? '7655' }}</span>
        <span class="svc-actions">
          <button class="save-btn" :disabled="svcBusy === 'napari'" @click="napariAction('restart')"
                  v-tooltip.top="'Close and relaunch the napari bridge (picks up bridge code changes)'">
            <i :class="['pi', svcBusy === 'napari' ? 'pi-spin pi-spinner' : 'pi-refresh']" />
            {{ napariSt === 'stopped' ? 'Start' : 'Restart' }}
          </button>
          <button v-if="napariSt !== 'stopped'" class="save-btn ghost" :disabled="svcBusy === 'napari'"
                  @click="napariAction('stop')"><i class="pi pi-stop-circle" /> Stop</button>
        </span>
      </div>

      <!-- discrete-GPU toggle: launches the napari bridge on the dGPU (hybrid-graphics machines).
           Linux only; disabled with a hint elsewhere. Flipping it restarts napari to apply. -->
      <div class="field" style="margin: 0.2rem 0 0.6rem;">
        <CcToggle class="toggle-row" :disabled="!gpuSupported || gpuBusy"
               :model-value="settings.napariDiscreteGpu"
               @update:model-value="settings.napariDiscreteGpu = $event; toggleGpu()"
               v-tooltip.right="'Render napari on the discrete GPU; restarts napari (Linux only)'">
          Use discrete GPU for napari
          <i v-if="gpuBusy" class="pi pi-spin pi-spinner" style="font-size:var(--cc-fs-xs);" />
        </CcToggle>
        <span v-if="!gpuSupported" class="field-hint cc-muted cc-fs-xs">
          Only configurable on Linux — on this system the GPU is selected by the OS/driver.
        </span>
      </div>

      <div class="svc-row">
        <span class="svc-name">Notebooks</span>
        <span class="svc-pill" :class="stateInfo(notebooksSt).tone"><span class="dot" /> {{ stateInfo(notebooksSt).label }}</span>
        <span class="svc-port cc-muted cc-fs-xs" v-tooltip.top="'Pluto notebook server'">:{{ diag?.notebooksPort ?? '7660' }}</span>
        <span class="svc-actions">
          <button v-if="notebooksSt === 'stopped'" class="save-btn" :disabled="svcBusy === 'notebooks' || !projectUid"
                  @click="notebooksAction('start')"
                  v-tooltip.top="projectUid ? 'Start the Pluto notebook server' : 'Open a project first'">
            <i :class="['pi', svcBusy === 'notebooks' ? 'pi-spin pi-spinner' : 'pi-play']" /> Start
          </button>
          <template v-else>
            <button class="save-btn" :disabled="svcBusy === 'notebooks' || !projectUid" @click="notebooksAction('restart')">
              <i :class="['pi', svcBusy === 'notebooks' ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Restart
            </button>
            <button class="save-btn ghost" :disabled="svcBusy === 'notebooks'" @click="notebooksAction('stop')">
              <i class="pi pi-stop-circle" /> Stop
            </button>
          </template>
        </span>
      </div>

      <!-- Task preview worker. Stop only — starting means previewing something, which needs a task's
           params and an open image (the task page owns that). This is the off switch, and it exists
           because a warm cellpose model holds GPU memory. -->
      <div class="svc-row">
        <span class="svc-name">Task preview</span>
        <span class="svc-pill" :class="stateInfo(previewSt).tone"><span class="dot" /> {{ stateInfo(previewSt).label }}</span>
        <span class="svc-port cc-muted cc-fs-xs" v-tooltip.top="'Task-preview worker WebSocket'">:{{ diag?.previewPort ?? '7656' }}</span>
        <span class="svc-actions">
          <button v-if="previewSt !== 'stopped'" class="save-btn ghost" :disabled="svcBusy === 'preview'"
                  @click="previewStop()"
                  v-tooltip.top="'Stop the preview worker and free its GPU memory'">
            <i :class="['pi', svcBusy === 'preview' ? 'pi-spin pi-spinner' : 'pi-stop-circle']" /> Stop
          </button>
          <span v-else class="cc-muted cc-fs-xs">Starts from a task's preview toggle</span>
        </span>
      </div>

      <!-- Task runner. Only shown when enabled (CECELIA_RUNNER=1) — an always-visible row for a
           process most installs don't run would be noise. -->
      <div class="svc-row" v-if="diag?.dev && runnerRaw">
        <span class="svc-name">Task runner</span>
        <span class="svc-pill" :class="stateInfo(runnerSt).tone"><span class="dot" /> {{ stateInfo(runnerSt).label }}</span>
        <span class="svc-port cc-muted cc-fs-xs" v-tooltip.top="'Runs tasks in its own process, so a backend restart does not stop them'">:{{ diag?.runnerPort ?? '7657' }}</span>
        <span class="svc-actions">
          <span v-if="runnerRaw?.stale" class="diag-stale-note"
                v-tooltip.bottom="'Still running ' + runnerRaw.commit">
            <i class="pi pi-exclamation-triangle" /> old code
          </span>
          <span v-else-if="runnerRaw?.adopted" class="cc-muted cc-fs-xs"
                v-tooltip.bottom="'This runner was already going when the backend started — it outlived a restart'">adopted</span>
          <button v-if="runnerSt !== 'stopped'" class="save-btn" :disabled="svcBusy === 'runner'"
                  @click="runnerRestart(!!runnerRaw?.busy && svcMsg.includes('again'))"
                  v-tooltip.top="'Restart the runner to load current code — refuses while it still has work'">
            <i :class="['pi', svcBusy === 'runner' ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Restart
          </button>
          <span v-else-if="runnerRaw?.enabled" class="cc-muted cc-fs-xs">Starts with the backend</span>
          <span v-else class="cc-muted cc-fs-xs">Off — tasks run in the backend</span>
        </span>
      </div>

      <!-- dev only, like the Restart button above it and for the same reason: without Restart the
           runner buys a prod user nothing, while its failure modes all land on them. -->
      <div class="field" v-if="diag?.dev" style="margin: 0.2rem 0 0.6rem;">
        <CcToggle class="toggle-row" :disabled="svcBusy === 'runner' || runnerRaw?.settable === false"
               :model-value="!!runnerRaw?.enabled"
               @update:model-value="runnerToggle($event)"
               v-tooltip.right="'Run tasks in a separate process so a backend restart does not stop them'">
          Run tasks in a separate process
        </CcToggle>
        <span v-if="runnerRaw?.settable === false" class="field-hint cc-muted cc-fs-xs">
          CECELIA_RUNNER is set for this session, so it overrides this setting.
        </span>
      </div>

      <!-- read-only: not a service you control, shown so the full port picture is visible -->
      <div class="svc-row">
        <span class="svc-name">Frontend (GUI)</span>
        <span class="svc-pill ok"><span class="dot" /> This window</span>
        <span class="svc-port cc-muted cc-fs-xs" v-tooltip.top="diag?.dev ? 'Vite dev server (proxies to the backend)' : 'served by the backend'">:{{ guiPort }}</span>
      </div>

      <span class="field-hint cc-muted cc-fs-xs">Cecelia occupies these ports — don't bind other services (e.g. a Jupyter kernel) to them.</span>
      <span v-if="svcMsg" class="field-hint cc-muted cc-fs-xs">{{ svcMsg }}</span>
    </section>

    <!-- ── MCP connections ─────────────────────────────────────────────── -->
    <!-- What Claude can reach. Machine rows come from the user's Claude config (real state); account
         rows are managed by their claude.ai account and are NOT detectable from here, so they carry
         no dot — listed so people discover Cecelia can use them. Row model: utils/mcpConnections.ts -->
    <section class="settings-section">
      <h2 class="section-title">MCP connections</h2>

      <!-- the hint hangs off the pill + the (ellipsable) detail, NEVER the row: a tooltip on a
           container that also holds tooltipped buttons fires both at once -->
      <div v-for="r in mcpConnectionRows" :key="r.kind + r.name" class="mcp-row">
        <span class="svc-name">{{ r.name }}</span>
        <span class="svc-pill" :class="r.tone" v-tooltip.top="r.hint"><span class="dot" /> {{ r.label }}</span>
        <span class="mcp-detail cc-muted cc-fs-xs" v-tooltip.top="r.hint">
          {{ r.detail }}
          <a v-if="r.href" :href="r.href" target="_blank" rel="noopener">Setup guide ↗</a>
        </span>
        <button v-if="r.name === 'cecelia-observer' && r.tone === 'warn'" class="cc-btn"
                :disabled="observer.registering" @click="observer.registerMcp()"
                v-tooltip.top="'Register the Cecelia MCP in your Claude config'">
          <i class="pi pi-download" /> {{ observer.registering ? 'Setting up…' : 'Set up' }}
        </button>
        <button v-else-if="r.dismissable" class="cc-btn cc-btn-bare cc-btn-icon"
                @click="hideAccountConnector(r.name)" v-tooltip.left="'Hide — not used here'">
          <i class="pi pi-times" />
        </button>
        <span v-else />
      </div>

      <!-- one-click setup FAILED: the resolved command to run by hand. Diagnostics live here now, so
           the lab-log toolbar keeps only the action. -->
      <div v-if="observer.registerError" class="svc-row-note cc-row cc-row-tight">
        <strong class="cc-fs-xs">{{ observer.registerError }}</strong>
        <template v-if="observerFallbackCommand">
          <code class="cc-fs-2xs">{{ observerFallbackCommand }}</code>
          <button class="cc-btn cc-btn-bare cc-btn-icon" @click="copyObserverFallback"
                  v-tooltip.left="observerCmdCopied() ? 'Copied!' : 'Copy command'">
            <i :class="observerCmdCopied() ? 'pi pi-check' : 'pi pi-copy'" />
          </button>
        </template>
      </div>

      <span v-if="hiddenAccountConnectors.length" class="field-hint cc-muted cc-fs-xs">
        Hidden: {{ hiddenAccountConnectors.join(', ') }}
        <button class="cc-btn cc-btn-bare cc-fs-xs" @click="settings.hiddenMcpAccounts = []">show again</button>
      </span>
    </section>

    <!-- ── Diagnostics ─────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Diagnostics</h2>

      <div v-if="diag" class="diag-grid">
        <span>Version</span><span class="mono">{{ diag.version }}</span>
        <span v-if="diag.commit">Commit</span>
        <span v-if="diag.commit" class="mono" :class="{ 'diag-stale': diag.stale }">
          {{ diag.commit }}
          <span v-if="diag.stale" class="diag-stale-note"
                v-tooltip.bottom="`Backend is behind your files (HEAD ${diag.commitCurrent}) — restart it`">
            <i class="pi pi-exclamation-triangle" /> stale
          </span>
        </span>
        <span>Backend up</span><span class="mono">{{ formatUptime(diag.uptimeSeconds) }}</span>
        <span>Napari bridge</span>
        <span class="mono" :class="{ 'diag-stale': napariRaw?.bridgeStale }">
          <template v-if="napariSt === 'running'">up {{ formatUptime(napariRaw?.bridgeUptimeSeconds) }}</template>
          <template v-else>{{ stateInfo(napariSt).label }}</template>
          <span v-if="napariRaw?.bridgeStale" class="diag-stale-note"
                v-tooltip.bottom="'Napari is running old code — restart it (System panel above) and reopen the image'">
            <i class="pi pi-exclamation-triangle" /> stale
          </span>
        </span>
        <span>Server threads</span><span class="mono">{{ diag.threads }}</span>
        <span>Julia</span><span class="mono">{{ diag.julia }}</span>
        <span>Memory</span><span class="mono">{{ diag.memFreeGB }} / {{ diag.memTotalGB }} GB free · GC live {{ diag.gcLiveMB }} MB</span>
        <span>Host</span><span class="mono">{{ diag.host }}:{{ diag.port }}</span>
        <span>Projects dir</span><span class="mono">{{ diag.projectsDir }}</span>
      </div>

      <div class="field-row" style="margin-top:0.6rem; gap:0.5rem">
        <button class="save-btn" :disabled="diagBusy" @click="loadDiag" v-tooltip.right="'Re-read server diagnostics'">
          <i :class="['pi', diagBusy ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Refresh
        </button>
        <button class="save-btn" @click="showPackages = true" v-tooltip.right="'List every installed Python (pixi) and Julia package'">
          <i class="pi pi-box" /> Packages…
        </button>
      </div>
      <span v-if="diag && diag.threads > 1" class="field-hint cc-muted cc-fs-xs">Multithreaded API active ({{ diag.threads }} threads).</span>
      <span v-else-if="diag" class="field-hint cc-muted cc-fs-xs">Single-threaded — relaunch the API with <code>-t auto</code> for parallelism.</span>
    </section>

    <!-- ── Developer ───────────────────────────────────────────────────── -->
    <section v-if="diag" class="settings-section">
      <h2 class="section-title">Developer</h2>

      <div class="field">
        <CcToggle class="toggle-row" label="Enable debug console"
          :model-value="replToggle" @update:model-value="replToggle = $event; toggleRepl()"
          v-tooltip.right="'Julia console in the running server; loopback-bound only'" />
      </div>

      <!-- toggle is on but the server is network-bound → eval is refused server-side (loopback required) -->
      <span v-if="replToggle && !diag.loopback" class="field-hint cc-muted cc-fs-xs">
        The server is bound to <code>{{ diag.host }}</code>, so the console is disabled for safety.
        Relaunch loopback-only to use it: <code>CECELIA_HOST=127.0.0.1 CECELIA_REPL=1 pixi run dev</code>.
      </span>
    </section>

    <!-- ── Debug console — only when BOTH gates pass: flag on AND loopback bind ─── -->
    <section v-if="diag?.replAvailable" class="settings-section">
      <h2 class="section-title">Debug console</h2>
      <span class="field-hint cc-muted cc-fs-xs">
        Evaluates Julia in the running server — full access, use with care.
        Concurrent task logs may briefly appear here during a run.
      </span>

      <div v-if="replLog.length" class="repl-log">
        <div v-for="(e, i) in replLog" :key="i" class="repl-entry">
          <div class="repl-code">» {{ e.code }}</div>
          <pre v-if="e.output" class="repl-out">{{ e.output }}</pre>
          <pre v-if="e.value" class="repl-val">{{ e.value }}</pre>
          <pre v-if="e.error" class="repl-err">{{ e.error }}</pre>
        </div>
      </div>

      <textarea
        class="repl-input mono"
        v-tooltip.top="'Julia to evaluate in the running server'"
        v-model="replCode"
        rows="3"
        spellcheck="false"
        placeholder="Threads.nthreads()"
        @keydown="replKeydown"
      />
      <div class="field-row" style="margin-top:0.5rem">
        <button class="save-btn" :disabled="replBusy || !replCode.trim()" @click="runRepl">
          <i :class="['pi', replBusy ? 'pi-spin pi-spinner' : 'pi-play']" /> Run (⌘/Ctrl+Enter)
        </button>
      </div>
    </section>

    </div>
    </div>

    <PackagesDialog v-if="showPackages" @close="showPackages = false" />
    <ViewProfileEditor v-if="showProfileEditor" @close="showProfileEditor = false" />
  </div>
</template>

<style scoped>
.settings-page {
  max-width: 1180px;
  padding: 2rem 2.5rem;
}

/* two columns: existing settings left, diagnostics/developer right. Collapses to one on narrow
   viewports. `align-items: start` so the columns don't stretch to equal height. */
.settings-cols {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 0 3rem;
  align-items: start;
}
.settings-col { min-width: 0; }
@media (max-width: 860px) {
  .settings-cols { grid-template-columns: 1fr; gap: 0; }
}

.cmp-head {
  display: flex;
  align-items: baseline;
  gap: 0.6rem;
  margin-bottom: 0.35rem;
}
.settings-section {
  margin-bottom: 2rem;
}

.section-title {
  font-size: var(--cc-fs-sm);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--cc-text-dim);
  margin: 0 0 0.85rem;
  padding-bottom: 0.4rem;
  border-bottom: 1px solid var(--cc-border);
}

.field {
  margin-bottom: 1.1rem;
}

/* Name + Project ID share one row; Name grows, ID is sized to its 6-char uid */
.field-pair { display: flex; gap: 0.75rem; align-items: flex-start; }
.field-pair .field { margin-bottom: 1.1rem; }
.field-grow { flex: 1; min-width: 0; }
.field-id { flex: 0 0 auto; }
.field-id .field-input.mono { flex: 0 0 auto; width: 7ch; }

.field-label {
  display: block;
  font-size: var(--cc-fs-sm);
  font-weight: 600;
  color: var(--cc-text);
  margin-bottom: 0.3rem;
}

.field-row {
  display: flex;
  gap: 0.5rem;
  align-items: center;
}

/* visual styling from the global form base (style.css) */
.field-input { flex: 1; }
.field-input[readonly] { color: var(--cc-text-dim); cursor: default; }
.field-input.mono { font-family: var(--cc-mono); }

.field-hint { display: block; margin-top: 0.25rem; }

/* Storage box */
.stor-line {
  display: grid;
  grid-template-columns: auto 1fr;
  gap: 0.2rem 0.6rem;
  font-size: var(--cc-fs-md);
  margin: 0.4rem 0 0.6rem;
}
.stor-line strong { justify-self: end; }
.stor-reclaim { margin-top: 0.5rem; }
.stor-reclaim-head { font-size: var(--cc-fs-md); margin-bottom: 0.35rem; }
.stor-reclaim-head .field-hint { display: inline; margin-left: 0.35rem; }
.stor-list {
  list-style: none;
  margin: 0 0 0.5rem;
  padding: 0;
  max-height: 9rem;
  overflow-y: auto;
}
.stor-list li {
  display: flex;
  align-items: baseline;
  gap: 0.5rem;
  font-size: var(--cc-fs-sm);
  padding: 0.1rem 0;
}
.stor-name { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.stor-size { font-variant-numeric: tabular-nums; color: var(--cc-text); }
.stor-list .field-hint { display: inline; margin: 0; }

.save-btn {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: var(--cc-fs-sm);
  padding: 0.35rem 0.7rem;
  border-radius: var(--cc-radius-sm);
  border: 1px solid var(--cc-accent);
  background: var(--cc-accent);
  color: #fff;
  cursor: pointer;
  transition: opacity 0.12s;
  flex-shrink: 0;
}
.save-btn:disabled { opacity: 0.4; cursor: not-allowed; }
.save-btn:not(:disabled):hover { opacity: 0.85; }

/* .icon-btn → cc-btn cc-btn-bare cc-btn-icon cc-btn-lg */
.icon-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }

.toggle-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: var(--cc-fs-md);
  color: var(--cc-text);
  cursor: pointer;
  user-select: none;
}
.toggle-row input { accent-color: var(--cc-accent); cursor: pointer; }
.toggle-row.disabled { opacity: 0.5; cursor: not-allowed; }
.toggle-row.disabled input { cursor: not-allowed; }

/* system control panel: aligned grid — name · status pill · port · actions */
.svc-row { display: grid; grid-template-columns: 8rem 7rem 3.5rem 1fr; align-items: center;
  column-gap: 0.6rem; margin-bottom: 0.55rem; }
.svc-name { font-size: var(--cc-fs-md); color: var(--cc-text); }
.wt-select { padding: 2px 6px; max-width: 18rem; }
.svc-pill { justify-self: start; display: inline-flex; align-items: center; gap: 0.35rem; font-size: var(--cc-fs-sm);
  color: var(--cc-text-dim); padding: 0.1rem 0.55rem; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-pill);
  white-space: nowrap; }
.svc-pill .dot { width: 7px; height: 7px; border-radius: var(--cc-radius-pill); background: var(--cc-text-dim); }
.svc-pill.ok .dot   { background: var(--cc-viewer); }
.svc-pill.warn .dot { background: var(--cc-sev-warn); }
.svc-pill.idle .dot { background: var(--cc-text-dim); }
.svc-tag { font-size: var(--cc-fs-2xs); font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--cc-accent); border: 1px solid var(--cc-accent); border-radius: var(--cc-radius-xs); padding: 0 0.3rem; }
.svc-port { justify-self: start; font-family: var(--cc-mono); }
/* MCP rows: ONE line each — name, pill, a short detail that ellipses rather than wrapping, and the
   row's single action. Its own grid rather than `.svc-row`'s, whose 3rd column is sized for a port
   number (`:8080`) and wrapped anything longer onto a second line. */
.mcp-row { display: grid; grid-template-columns: 9rem 7.5rem 1fr auto; align-items: center;
  column-gap: 0.6rem; margin-bottom: 0.3rem; min-height: 1.7rem; }
.mcp-detail { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; min-width: 0; }
/* a failed-setup note under the MCP rows — warn-toned so it can't be read as another status row */
.svc-row-note { margin: 0 0 0.55rem; padding: 0.3rem 0.5rem; border-left: 3px solid var(--cc-sev-warn);
  background: var(--cc-surface-2); border-radius: var(--cc-radius-sm); }
.svc-row-note strong { color: var(--cc-sev-warn); }
.svc-row-note code { font-family: var(--cc-mono); overflow-x: auto; white-space: nowrap; min-width: 0; }
.svc-actions { display: flex; gap: 0.4rem; justify-content: flex-end; }
.save-btn.ghost { background: transparent; color: var(--cc-text-dim); border-color: var(--cc-border); }
.save-btn.ghost:not(:disabled):hover { color: var(--cc-text); }
.save-btn.danger { background: var(--cc-danger); border-color: var(--cc-danger); }

/* diagnostics key/value grid */
.diag-grid {
  display: grid;
  grid-template-columns: max-content 1fr;
  gap: 0.3rem 0.9rem;
  font-size: var(--cc-fs-sm);
  color: var(--cc-text);
}
.diag-grid > span:nth-child(odd) { color: var(--cc-text-dim); }
.mono { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); word-break: break-all; }
.field-hint code, .diag-grid code { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); }
/* stale-process flag: amber value + a small chip (problem short; the action is in the tooltip) */
.diag-stale { color: var(--cc-sev-warn); }
.diag-stale-note { margin-left: 0.4rem; font-size: var(--cc-fs-xs); color: var(--cc-sev-warn); white-space: nowrap; cursor: default; }
.diag-stale-note .pi { font-size: var(--cc-fs-xs); }

/* debug console */
.repl-log {
  max-height: 320px;
  overflow: auto;
  margin: 0.6rem 0;
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
  background: var(--cc-surface-1);
  padding: 0.4rem 0.6rem;
}
/* data patches */
.patch-row { padding: 0.5rem 0; border-bottom: 1px solid var(--cc-border); }
.patch-row:last-child { border-bottom: none; }
.patch-head { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.25rem; }
.patch-title { font-size: var(--cc-fs-md); font-weight: 600; color: var(--cc-text); flex: 1; }
.patch-actions { display: flex; align-items: center; gap: 0.4rem; flex-shrink: 0; }
.patch-run { margin-top: 0.4rem; }
.patch-bar { margin-bottom: 0.35rem; }   /* geometry only — the bar itself is CcProgressBar */
.patch-log { max-height: 200px; font-family: var(--cc-mono); font-size: var(--cc-fs-sm); color: var(--cc-text); white-space: pre-wrap; }

.repl-entry { padding: 0.35rem 0; border-bottom: 1px solid var(--cc-border); }
.repl-entry:last-child { border-bottom: none; }
.repl-code { font-family: var(--cc-mono); font-size: var(--cc-fs-sm); color: var(--cc-accent); white-space: pre-wrap; }
.repl-out, .repl-val, .repl-err {
  margin: 0.2rem 0 0; font-family: var(--cc-mono); font-size: var(--cc-fs-sm);
  white-space: pre-wrap; word-break: break-word;
}
.repl-out { color: var(--cc-text-dim); }
.repl-val { color: var(--cc-text); }
.repl-err { color: var(--cc-danger); }
.repl-input {
  width: 100%; resize: vertical; font-family: var(--cc-mono);
  padding: 0.5rem; border-radius: var(--cc-radius-sm);
}
</style>
