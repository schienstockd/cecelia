<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import PackagesDialog from '../components/PackagesDialog.vue'
import ConfirmButton from '../components/ConfirmButton.vue'
import { napariState, notebooksState, stateInfo, formatUptime, type ServiceState } from '../utils/serviceStatus'
import { notebooksApi, napariApi } from '../utils/serviceApi'
import { useAppControlStore } from '../stores/appControl'
import { useCustomModulesStore } from '../stores/customModules'
import { fetchStorageSummary, reclaimStorage, formatBytes, type StorageSummary } from '../utils/storage'
import { useWsStore } from '../stores/ws'
import { useTaskStore } from '../stores/tasks'

const showPackages = ref(false)

// ── Storage ─────────────────────────────────────────────────────────────────
// On-demand scan (walking every image store is expensive — never auto-run on open). Surfaces
// reclaimable ORIGINAL imports of images whose drift/AF/cellpose-corrected variant is now active,
// and frees them in one click (backend keeps the corrected variant working). See utils/storage.ts.
const storage      = ref<StorageSummary | null>(null)
const storageScan  = ref(false)
const storageBusy  = ref(false)
const storageError = ref('')

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

// ── Custom modules ───────────────────────────────────────────────────────────
// User drop-in tasks (docs/CUSTOM_MODULES.md). Reload rescans the config dir for NEWLY dropped .jl;
// edits to already-loaded modules still need a server restart. Refresh status when this panel opens.
onMounted(() => customModules.refresh())

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

function copyUid() {
  if (projectMeta.current?.uid)
    navigator.clipboard.writeText(projectMeta.current.uid)
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
  napariPort: number; notebooksPort: number
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

// ── System: service control panel ─────────────────────────────────────────────
// Live status of the backend's child processes + per-component and global controls. Status is
// ephemeral UI state (polled) → plain refs, not persisted view state. Pure status→state mapping
// lives in utils/serviceStatus.ts (unit-tested); here we only poll, act, and pick which buttons show.
const napariRaw = ref<{ alive?: boolean; starting?: boolean; bridgeUptimeSeconds?: number | null; bridgeStale?: boolean } | null>(null)
const notebooksRaw = ref<{ running?: boolean; starting?: boolean } | null>(null)
const napariSt = computed<ServiceState>(() => napariState(napariRaw.value))
const notebooksSt = computed<ServiceState>(() => notebooksState(notebooksRaw.value))
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
}
let svcTimer: number | undefined
onMounted(() => { pollServices(); svcTimer = window.setInterval(pollServices, 4000) })
onUnmounted(() => { if (svcTimer) window.clearInterval(svcTimer) })

async function napariAction(kind: 'restart' | 'stop') {
  svcBusy.value = 'napari'; svcMsg.value = ''
  try {
    await (kind === 'restart' ? napariApi.restart() : napariApi.close())
    svcMsg.value = kind === 'restart' ? 'Napari restarting — reopen the image to reload its layers.' : 'Napari stopped.'
  } catch { svcMsg.value = 'Napari action failed.' }
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
    <h1 class="page-title">Settings</h1>

    <div class="settings-cols">
    <div class="settings-col">

    <!-- ── Project ─────────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Project</h2>

      <template v-if="projectMeta.current">
        <div class="field">
          <label class="field-label">Name</label>
          <div class="field-row">
            <input
              class="field-input"
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
              <i :class="['pi', saved ? 'pi-check' : saving ? 'pi-spin pi-cog' : 'pi-check']" />
              {{ saved ? 'Applied' : 'Apply' }}
            </button>
          </div>
        </div>

        <div class="field">
          <label class="field-label">Project ID</label>
          <div class="field-row">
            <input class="field-input mono" :value="projectMeta.current.uid" readonly />
            <button class="icon-btn" @click="copyUid" v-tooltip.right="'Copy project ID'">
              <i class="pi pi-copy" />
            </button>
          </div>
          <span class="field-hint">Read-only unique identifier used internally.</span>
        </div>
      </template>

      <p v-else class="no-project">No project open. Open or create a project first.</p>
    </section>

    <!-- ── Interface ───────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Interface</h2>

      <div class="field">
        <label class="toggle-row" v-tooltip.right="'When a task starts running, automatically select it in the task manager log panel.'">
          <input type="checkbox" v-model="settings.taskListAutoFollow" />
          <span class="toggle-label">Auto-follow running tasks in task manager</span>
        </label>
      </div>

      <div class="field">
        <label class="toggle-row" v-tooltip.right="'Keep plots in sync with your data: when a task finishes, any plot or population list showing the affected image(s) reloads on its own. Turn off to keep plots steady while you work — they update next time you open or change them.'">
          <input type="checkbox" v-model="settings.autoRefreshOnTask" />
          <span class="toggle-label">Auto-refresh plots when tasks finish</span>
        </label>
      </div>
    </section>

    <!-- ── Software updates ────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Software updates</h2>

      <div class="field">
        <label class="field-label">Version</label>
        <div class="field-row">
          <input class="field-input mono" :value="appCtl.updateCurrent || '—'" readonly />
          <button
            class="save-btn"
            :disabled="appCtl.updateChecking"
            @click="appCtl.checkUpdate"
            v-tooltip.right="'Check GitHub for a newer release'"
          >
            <i :class="['pi', appCtl.updateChecking ? 'pi-spin pi-cog' : 'pi-refresh']" />
            {{ appCtl.updateChecking ? 'Checking…' : 'Check' }}
          </button>
        </div>
        <span v-if="!appCtl.updateAvailable && appCtl.updateCurrent && !appCtl.updateMsg" class="field-hint">
          You're on the latest version.
        </span>
      </div>

      <!-- per-user install: in-app update -->
      <div v-if="appCtl.updateAvailable && appCtl.canApplyUpdate" class="field">
        <button
          class="save-btn"
          :disabled="appCtl.updateBusy"
          @click="appCtl.applyUpdate"
          v-tooltip.right="`Download ${appCtl.updateLatest} and stage it; restart Cecelia to finish.`"
        >
          <i :class="['pi', appCtl.updateBusy ? 'pi-spin pi-cog' : 'pi-download']" />
          {{ appCtl.updateBusy ? 'Updating…' : `Update to ${appCtl.updateLatest}` }}
        </button>
      </div>

      <!-- shared system-wide install: updates are admin-only (see docs/todo/ONBOARDING_PLAN.md D4/D5) -->
      <span v-else-if="appCtl.updateAvailable && appCtl.updateScope === 'system'" class="field-hint">
        {{ appCtl.updateLatest }} is available. This is a shared installation — updates must be run by
        an administrator (re-run the install-system script).
      </span>

      <span v-if="appCtl.updateMsg" class="field-hint">{{ appCtl.updateMsg }}</span>
    </section>

    <!-- ── Custom modules ──────────────────────────────────────────────── -->
    <!-- ── Storage ──────────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Storage</h2>

      <div class="field">
        <div class="field-row">
          <button class="save-btn" :disabled="storageScan || !projectMeta.current" @click="scanStorage"
                  v-tooltip.top="'Scan this project on disk (may take a moment for large projects)'">
            <i :class="['pi', storageScan ? 'pi-spin pi-cog' : 'pi-search']" />
            {{ storage ? 'Re-scan' : 'Scan storage' }}
          </button>
        </div>
        <span v-if="!storage && !storageScan" class="field-hint">
          Scan to see disk usage and superseded image versions that can be freed (everything except the active one).
        </span>
        <span v-if="storageError" class="field-hint" style="color: var(--cc-sev-fail, #c0392b);">{{ storageError }}</span>
      </div>

      <template v-if="storage">
        <div class="stor-line">
          <span v-tooltip.top="'Total size of the image OME-ZARRs in this project (not labels or other analysis data)'">Images in project</span>
          <strong>{{ formatBytes(storage.imageBytes) }}</strong>
          <span>Disk free</span><strong>{{ formatBytes(storage.diskAvailable) }} / {{ formatBytes(storage.diskTotal) }}</strong>
        </div>

        <div v-if="storage.reclaimable.length" class="stor-reclaim">
          <div class="stor-reclaim-head">
            Reclaimable <strong>{{ formatBytes(storage.reclaimableBytes) }}</strong>
            <span class="field-hint">({{ storage.reclaimable.length }} image{{ storage.reclaimable.length > 1 ? 's' : '' }} with superseded versions; the active version is kept)</span>
          </div>
          <ul class="stor-list">
            <li v-for="r in storage.reclaimable.slice(0, 8)" :key="r.imageUid">
              <span class="stor-name">{{ r.name || r.imageUid }}</span>
              <span class="stor-size">{{ formatBytes(r.bytes) }}</span>
              <span class="field-hint"
                    v-tooltip.top="'Frees: ' + (r.versions?.map(v => v.valueName).join(', ') ?? '') + ' — keeps ' + r.activeVersion">
                → keeps {{ r.activeVersion }}
              </span>
            </li>
            <li v-if="storage.reclaimable.length > 8" class="field-hint">…{{ storage.reclaimable.length - 8 }} more</li>
          </ul>
          <ConfirmButton @confirm="reclaimAll" v-slot="{ armed, arm, confirm, cancel }">
            <button v-if="!armed" class="save-btn danger" :disabled="storageBusy" @click="arm"
                    v-tooltip.top="'Delete every non-active image version (incl. the original import); the active version is kept'">
              <i :class="['pi', storageBusy ? 'pi-spin pi-cog' : 'pi-trash']" /> Free up space
            </button>
            <template v-else>
              <button class="save-btn danger" @click="confirm">
                <i class="pi pi-trash" /> Free {{ formatBytes(storage.reclaimableBytes) }} across {{ storage.reclaimable.length }} image{{ storage.reclaimable.length > 1 ? 's' : '' }}
              </button>
              <button class="save-btn ghost" @click="cancel">Cancel</button>
            </template>
          </ConfirmButton>
        </div>
        <span v-else class="field-hint">Nothing to reclaim — every image has only its active version.</span>
      </template>
    </section>

    <section class="settings-section">
      <h2 class="section-title">Custom modules</h2>
      <div class="field">
        <label class="field-label">Modules directory</label>
        <div class="field-row">
          <input class="field-input mono" :value="customModules.dir || '—'" readonly />
          <button
            class="save-btn"
            :disabled="customModules.loading"
            @click="customModules.reload"
            v-tooltip.right="'Rescan for newly dropped modules. Edits to already-loaded modules need a server restart.'"
          >
            <i :class="['pi', customModules.loading ? 'pi-spin pi-cog' : 'pi-refresh']" />
            {{ customModules.loading ? 'Reloading…' : 'Reload' }}
          </button>
        </div>
        <span class="field-hint">
          Drop tasks into this folder to add them without a rebuild — see docs/CUSTOM_MODULES.md.
        </span>
      </div>

      <div v-if="customModules.modules.length" class="cm-list">
        <div v-for="m in customModules.modules" :key="m.path" class="cm-row">
          <span class="svc-pill" :class="m.status === 'ok' ? 'ok' : 'err'">
            <span class="dot" /> {{ m.status === 'ok' ? 'loaded' : 'error' }}
          </span>
          <span class="cm-path mono" v-tooltip.top="m.error || m.path">{{ m.path }}</span>
        </div>
      </div>
      <span v-else class="field-hint">No custom modules loaded.</span>
    </section>

    <!-- ── Data patches (project-scoped maintenance scripts) ──────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Data patches</h2>
      <p class="field-hint">One-off fixes applied to the currently open project's data. Dry-run first to see what would change.</p>
      <div v-if="!projectMeta.current" class="field-hint">Open a project to run patches.</div>
      <div v-for="p in patches" :key="p.id" class="patch-row">
        <div class="patch-head">
          <span class="patch-title">{{ p.title }}</span>
          <span class="patch-actions">
            <button class="save-btn" :disabled="!projectMeta.current || patchBusy(p.id)" @click="runPatch(p, false)"
                    v-tooltip.top="'List what would change — writes nothing'">
              <i :class="['pi', patchBusy(p.id) ? 'pi-spin pi-cog' : 'pi-search']" /> Dry-run
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
        <span class="field-hint">{{ p.description }}</span>
        <div v-if="patchRun(p.id)" class="patch-run">
          <div v-if="patchRun(p.id)!.progress != null" class="patch-bar">
            <span :style="{ width: (patchRun(p.id)!.progress! * 100) + '%' }" /></div>
          <pre class="repl-log patch-log">{{ patchRun(p.id)!.log.join('\n') }}</pre>
          <span class="field-hint">status: {{ patchRun(p.id)!.status }}</span>
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
        <span class="svc-port" v-tooltip.top="'Backend HTTP/WS server'">:{{ diag?.port ?? '8080' }}</span>
        <span class="svc-actions">
          <button v-if="diag?.dev" class="save-btn" :disabled="appCtl.busy" @click="appRestart"
                  v-tooltip.top="'Restart the backend server (dev): the supervisor relaunches it, page reconnects when it is back'">
            <i :class="['pi', appCtl.busy ? 'pi-spin pi-cog' : 'pi-refresh']" /> Restart
          </button>
          <ConfirmButton @confirm="quitApp" v-slot="{ armed, arm, confirm, cancel }">
            <button v-if="!armed" class="save-btn danger" :disabled="appCtl.busy" @click="arm"
                    v-tooltip.top="'Stop napari, notebooks and the backend, then exit Cecelia'">
              <i class="pi pi-power-off" /> Quit
            </button>
            <template v-else>
              <button class="save-btn danger" @click="confirm"
                      v-tooltip.top="'Confirm — stop napari, notebooks and the backend'"><i class="pi pi-power-off" /> Quit everything</button>
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
                v-tooltip.top="'Relaunch the backend from another git worktree (dev). The page reconnects when it is back.'">
          <option v-for="w in appCtl.worktrees" :key="w.path" :value="w.path">
            {{ wtFolder(w.path) }} — {{ w.branch }}{{ w.primary ? ' (main)' : '' }}{{ w.current ? ' (current)' : '' }}
          </option>
        </select>
      </div>

      <div class="svc-row">
        <span class="svc-name">Napari viewer</span>
        <span class="svc-pill" :class="stateInfo(napariSt).tone"><span class="dot" /> {{ stateInfo(napariSt).label }}</span>
        <span class="svc-port" v-tooltip.top="'Napari bridge WebSocket'">:{{ diag?.napariPort ?? '7655' }}</span>
        <span class="svc-actions">
          <button class="save-btn" :disabled="svcBusy === 'napari'" @click="napariAction('restart')"
                  v-tooltip.top="'Close and relaunch the napari bridge (picks up bridge code changes)'">
            <i :class="['pi', svcBusy === 'napari' ? 'pi-spin pi-cog' : 'pi-refresh']" />
            {{ napariSt === 'stopped' ? 'Start' : 'Restart' }}
          </button>
          <button v-if="napariSt !== 'stopped'" class="save-btn ghost" :disabled="svcBusy === 'napari'"
                  @click="napariAction('stop')"><i class="pi pi-stop" /> Stop</button>
        </span>
      </div>

      <!-- discrete-GPU toggle: launches the napari bridge on the dGPU (hybrid-graphics machines).
           Linux only; disabled with a hint elsewhere. Flipping it restarts napari to apply. -->
      <div class="field" style="margin: 0.2rem 0 0.6rem;">
        <label class="toggle-row"
               :class="{ disabled: !gpuSupported || gpuBusy }"
               v-tooltip.right="'Render napari on the discrete GPU (hybrid graphics). Restarts napari to apply. Linux only.'">
          <input type="checkbox" v-model="settings.napariDiscreteGpu"
                 :disabled="!gpuSupported || gpuBusy" @change="toggleGpu" />
          <span class="toggle-label">Use discrete GPU for napari</span>
          <i v-if="gpuBusy" class="pi pi-spin pi-cog" style="font-size:0.7rem;" />
        </label>
        <span v-if="!gpuSupported" class="field-hint">
          Only configurable on Linux — on this system the GPU is selected by the OS/driver.
        </span>
      </div>

      <div class="svc-row">
        <span class="svc-name">Notebooks</span>
        <span class="svc-pill" :class="stateInfo(notebooksSt).tone"><span class="dot" /> {{ stateInfo(notebooksSt).label }}</span>
        <span class="svc-port" v-tooltip.top="'Pluto notebook server'">:{{ diag?.notebooksPort ?? '7660' }}</span>
        <span class="svc-actions">
          <button v-if="notebooksSt === 'stopped'" class="save-btn" :disabled="svcBusy === 'notebooks' || !projectUid"
                  @click="notebooksAction('start')"
                  v-tooltip.top="projectUid ? 'Launch the Pluto notebook server' : 'Open a project first'">
            <i :class="['pi', svcBusy === 'notebooks' ? 'pi-spin pi-cog' : 'pi-play']" /> Start
          </button>
          <template v-else>
            <button class="save-btn" :disabled="svcBusy === 'notebooks' || !projectUid" @click="notebooksAction('restart')">
              <i :class="['pi', svcBusy === 'notebooks' ? 'pi-spin pi-cog' : 'pi-refresh']" /> Restart
            </button>
            <button class="save-btn ghost" :disabled="svcBusy === 'notebooks'" @click="notebooksAction('stop')">
              <i class="pi pi-stop" /> Stop
            </button>
          </template>
        </span>
      </div>

      <!-- read-only: not a service you control, shown so the full port picture is visible -->
      <div class="svc-row">
        <span class="svc-name">Frontend (GUI)</span>
        <span class="svc-pill ok"><span class="dot" /> This window</span>
        <span class="svc-port" v-tooltip.top="diag?.dev ? 'Vite dev server (proxies to the backend)' : 'served by the backend'">:{{ guiPort }}</span>
      </div>

      <span class="field-hint">Cecelia occupies these ports — don't bind other services (e.g. a Jupyter kernel) to them.</span>
      <span v-if="svcMsg" class="field-hint">{{ svcMsg }}</span>
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
                v-tooltip.bottom="`Backend runs an older commit than your files (HEAD ${diag.commitCurrent}) — restart it to load the latest.`">
            <i class="pi pi-exclamation-triangle" /> stale
          </span>
        </span>
        <span>Backend up</span><span class="mono">{{ formatUptime(diag.uptimeSeconds) }}</span>
        <span>Napari bridge</span>
        <span class="mono" :class="{ 'diag-stale': napariRaw?.bridgeStale }">
          <template v-if="napariSt === 'running'">up {{ formatUptime(napariRaw?.bridgeUptimeSeconds) }}</template>
          <template v-else>{{ stateInfo(napariSt).label }}</template>
          <span v-if="napariRaw?.bridgeStale" class="diag-stale-note"
                v-tooltip.bottom="'Napari is running old code — restart it (System panel above) and reopen the image.'">
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
          <i :class="['pi', diagBusy ? 'pi-spin pi-cog' : 'pi-refresh']" /> Refresh
        </button>
        <button class="save-btn" @click="showPackages = true" v-tooltip.right="'List every installed Python (pixi) and Julia package'">
          <i class="pi pi-box" /> Packages…
        </button>
      </div>
      <span v-if="diag && diag.threads > 1" class="field-hint">Multithreaded API active ({{ diag.threads }} threads).</span>
      <span v-else-if="diag" class="field-hint">Single-threaded — relaunch the API with <code>-t auto</code> for parallelism.</span>
    </section>

    <!-- ── Developer ───────────────────────────────────────────────────── -->
    <section v-if="diag" class="settings-section">
      <h2 class="section-title">Developer</h2>

      <div class="field">
        <label class="toggle-row" v-tooltip.right="'Show a Julia console that evaluates code in the running server. Only works when the server is loopback-bound (127.0.0.1); a network-bound server refuses it regardless.'">
          <input type="checkbox" v-model="replToggle" @change="toggleRepl" />
          <span class="toggle-label">Enable debug console</span>
        </label>
      </div>

      <!-- toggle is on but the server is network-bound → eval is refused server-side (loopback required) -->
      <span v-if="replToggle && !diag.loopback" class="field-hint">
        The server is bound to <code>{{ diag.host }}</code>, so the console is disabled for safety.
        Relaunch loopback-only to use it: <code>CECELIA_HOST=127.0.0.1 CECELIA_REPL=1 pixi run dev</code>.
      </span>
    </section>

    <!-- ── Debug console — only when BOTH gates pass: flag on AND loopback bind ─── -->
    <section v-if="diag?.replAvailable" class="settings-section">
      <h2 class="section-title">Debug console</h2>
      <span class="field-hint">
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
        v-model="replCode"
        rows="3"
        spellcheck="false"
        placeholder="Threads.nthreads()"
        @keydown="replKeydown"
      />
      <div class="field-row" style="margin-top:0.5rem">
        <button class="save-btn" :disabled="replBusy || !replCode.trim()" @click="runRepl">
          <i :class="['pi', replBusy ? 'pi-spin pi-cog' : 'pi-play']" /> Run (⌘/Ctrl+Enter)
        </button>
      </div>
    </section>

    </div>
    </div>

    <PackagesDialog v-if="showPackages" @close="showPackages = false" />
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

.page-title {
  font-size: 1.1rem;
  font-weight: 700;
  color: var(--cc-text);
  margin: 0 0 1.75rem;
}

.settings-section {
  margin-bottom: 2rem;
}

.section-title {
  font-size: 0.72rem;
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

.field-label {
  display: block;
  font-size: 0.76rem;
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
.field-input.mono { font-family: var(--cc-mono); font-size: 0.74rem; }

.field-hint {
  display: block;
  font-size: 0.7rem;
  color: var(--cc-text-dim);
  margin-top: 0.25rem;
}

/* Storage box */
.stor-line {
  display: grid;
  grid-template-columns: auto 1fr;
  gap: 0.2rem 0.6rem;
  font-size: 0.8rem;
  margin: 0.4rem 0 0.6rem;
}
.stor-line strong { justify-self: end; }
.stor-reclaim { margin-top: 0.5rem; }
.stor-reclaim-head { font-size: 0.82rem; margin-bottom: 0.35rem; }
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
  font-size: 0.76rem;
  padding: 0.1rem 0;
}
.stor-name { flex: 1; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.stor-size { font-variant-numeric: tabular-nums; color: var(--cc-text); }
.stor-list .field-hint { display: inline; margin: 0; }

.save-btn {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  font-size: 0.76rem;
  padding: 0.35rem 0.7rem;
  border-radius: 0.35rem;
  border: 1px solid var(--cc-accent);
  background: var(--cc-accent);
  color: #fff;
  cursor: pointer;
  transition: opacity 0.12s;
  flex-shrink: 0;
}
.save-btn:disabled { opacity: 0.4; cursor: not-allowed; }
.save-btn:not(:disabled):hover { opacity: 0.85; }

.icon-btn {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  font-size: 0.82rem;
  padding: 0.3rem 0.4rem;
  border-radius: 0.25rem;
  flex-shrink: 0;
}
.icon-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }

.toggle-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.82rem;
  color: var(--cc-text);
  cursor: pointer;
  user-select: none;
}
.toggle-row input { accent-color: var(--cc-accent); cursor: pointer; }
.toggle-row.disabled { opacity: 0.5; cursor: not-allowed; }
.toggle-row.disabled input { cursor: not-allowed; }

.no-project {
  font-size: 0.8rem;
  color: var(--cc-text-dim);
}

/* system control panel: aligned grid — name · status pill · port · actions */
.svc-row { display: grid; grid-template-columns: 8rem 7rem 3.5rem 1fr; align-items: center;
  column-gap: 0.6rem; margin-bottom: 0.55rem; }
.svc-name { font-size: 0.8rem; color: var(--cc-text); }
.wt-select { font-size: 0.8rem; padding: 2px 6px; max-width: 18rem; }
.svc-pill { justify-self: start; display: inline-flex; align-items: center; gap: 0.35rem; font-size: 0.72rem;
  color: var(--cc-text-dim); padding: 0.1rem 0.55rem; border: 1px solid var(--cc-border); border-radius: 999px;
  white-space: nowrap; }
.svc-pill .dot { width: 7px; height: 7px; border-radius: 50%; background: var(--cc-text-dim); }
.svc-pill.ok .dot   { background: #22c55e; }
.svc-pill.warn .dot { background: #f59e0b; }
.svc-pill.idle .dot { background: var(--cc-text-dim); }
.svc-tag { font-size: 0.62rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--cc-accent); border: 1px solid var(--cc-accent); border-radius: 3px; padding: 0 0.3rem; }
.svc-port { justify-self: start; font-family: var(--cc-mono); font-size: 0.68rem; color: var(--cc-text-dim); }
.svc-actions { display: flex; gap: 0.4rem; justify-content: flex-end; }
.save-btn.ghost { background: transparent; color: var(--cc-text-dim); border-color: var(--cc-border); }
.save-btn.ghost:not(:disabled):hover { color: var(--cc-text); }
.save-btn.danger { background: var(--cc-danger, #ef4444); border-color: var(--cc-danger, #ef4444); }

/* diagnostics key/value grid */
.diag-grid {
  display: grid;
  grid-template-columns: max-content 1fr;
  gap: 0.3rem 0.9rem;
  font-size: 0.78rem;
  color: var(--cc-text);
}
.diag-grid > span:nth-child(odd) { color: var(--cc-text-dim); }
.mono { font-family: var(--cc-mono); font-size: 0.74rem; word-break: break-all; }
.field-hint code, .diag-grid code { font-family: var(--cc-mono); font-size: 0.72rem; }
/* stale-process flag: amber value + a small chip (problem short; the action is in the tooltip) */
.diag-stale { color: var(--cc-warn); }
.diag-stale-note { margin-left: 0.4rem; font-size: 0.68rem; color: var(--cc-warn); white-space: nowrap; cursor: default; }
.diag-stale-note .pi { font-size: 0.66rem; }

/* debug console */
.repl-log {
  max-height: 320px;
  overflow: auto;
  margin: 0.6rem 0;
  border: 1px solid var(--cc-border);
  border-radius: 0.35rem;
  background: var(--cc-surface-1);
  padding: 0.4rem 0.6rem;
}
/* data patches */
.patch-row { padding: 0.5rem 0; border-bottom: 1px solid var(--cc-border); }
.patch-row:last-child { border-bottom: none; }
.patch-head { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.25rem; }
.patch-title { font-size: 0.82rem; font-weight: 600; color: var(--cc-text); flex: 1; }
.patch-actions { display: flex; align-items: center; gap: 0.4rem; flex-shrink: 0; }
.patch-run { margin-top: 0.4rem; }
.patch-bar { height: 4px; border-radius: 2px; background: var(--cc-surface-2); overflow: hidden; margin-bottom: 0.35rem; }
.patch-bar span { display: block; height: 100%; background: var(--cc-accent); transition: width 0.2s; }
.patch-log { max-height: 200px; font-family: var(--cc-mono); font-size: 0.72rem; color: var(--cc-text); white-space: pre-wrap; }

.repl-entry { padding: 0.35rem 0; border-bottom: 1px solid var(--cc-border); }
.repl-entry:last-child { border-bottom: none; }
.repl-code { font-family: var(--cc-mono); font-size: 0.74rem; color: var(--cc-accent); white-space: pre-wrap; }
.repl-out, .repl-val, .repl-err {
  margin: 0.2rem 0 0; font-family: var(--cc-mono); font-size: 0.72rem;
  white-space: pre-wrap; word-break: break-word;
}
.repl-out { color: var(--cc-text-dim); }
.repl-val { color: var(--cc-text); }
.repl-err { color: var(--cc-danger, #f87171); }
.repl-input {
  width: 100%; resize: vertical; font-family: var(--cc-mono); font-size: 0.76rem;
  padding: 0.5rem; border-radius: 0.35rem;
}
</style>
