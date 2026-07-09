<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import PackagesDialog from '../components/PackagesDialog.vue'
import { napariState, notebooksState, stateInfo, type ServiceState } from '../utils/serviceStatus'
import { useAppControlStore } from '../stores/appControl'

const showPackages = ref(false)

const projectMeta = useProjectMetaStore()
const settings    = useSettingsStore()
const appCtl      = useAppControlStore()

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
const verCurrent = ref('')
const verLatest = ref<string | null>(null)
const updateAvailable = ref(false)
const checking = ref(false)
const updateBusy = ref(false)
const updateMsg = ref('')

async function checkUpdates() {
  checking.value = true
  updateMsg.value = ''
  try {
    const d = await (await fetch('/api/update/check')).json()
    verCurrent.value = d.current ?? ''
    verLatest.value = d.latest ?? null
    updateAvailable.value = !!d.updateAvailable
    if (d.error) updateMsg.value = d.error
  } catch {
    updateMsg.value = 'Could not reach the update server.'
  } finally {
    checking.value = false
  }
}

async function applyUpdate() {
  if (!verLatest.value || updateBusy.value) return
  updateBusy.value = true
  updateMsg.value = ''
  try {
    const res = await fetch('/api/update/apply', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ version: verLatest.value }),
    })
    const d = await res.json()
    updateMsg.value = res.ok
      ? (d.message ?? `Update ${verLatest.value} staged — restart Cecelia to finish.`)
      : (d.error ?? 'Update failed.')
    if (res.ok) updateAvailable.value = false
  } catch {
    updateMsg.value = 'Update failed (could not reach the server).'
  } finally {
    updateBusy.value = false
  }
}

onMounted(checkUpdates)

// ── Diagnostics + debug console ──────────────────────────────────────────────
interface Diag {
  threads: number; julia: string; version: string; projectsDir: string
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
const napariRaw = ref<{ alive?: boolean; starting?: boolean } | null>(null)
const notebooksRaw = ref<{ running?: boolean; starting?: boolean } | null>(null)
const napariSt = computed<ServiceState>(() => napariState(napariRaw.value))
const notebooksSt = computed<ServiceState>(() => notebooksState(notebooksRaw.value))
const projectUid = computed(() => projectMeta.current?.uid ?? '')
// the port serving THIS window (Vite :5173 in dev; the backend :8080 in prod) — the GUI isn't a
// controllable service, we just show it so the full picture of occupied ports is visible.
const guiPort = computed(() => location.port || (location.protocol === 'https:' ? '443' : '80'))

const svcBusy = ref('')     // which row's action is in flight ('napari' | 'notebooks' | 'app')
const svcMsg = ref('')
const showQuitConfirm = ref(false)

async function pollServices() {
  try { napariRaw.value = await (await fetch('/api/napari/status')).json() } catch { napariRaw.value = null }
  try { notebooksRaw.value = await (await fetch('/api/notebooks/status')).json() } catch { notebooksRaw.value = null }
}
let svcTimer: number | undefined
onMounted(() => { pollServices(); svcTimer = window.setInterval(pollServices, 4000) })
onUnmounted(() => { if (svcTimer) window.clearInterval(svcTimer) })

const svcPost = (url: string, body?: object) =>
  fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body ?? {}) })

async function napariAction(kind: 'restart' | 'stop') {
  svcBusy.value = 'napari'; svcMsg.value = ''
  try {
    await svcPost(kind === 'restart' ? '/api/napari/restart' : '/api/napari/close')
    svcMsg.value = kind === 'restart' ? 'Napari restarting — reopen the image to reload its layers.' : 'Napari stopped.'
  } catch { svcMsg.value = 'Napari action failed.' }
  finally { svcBusy.value = ''; setTimeout(pollServices, 500) }
}
async function notebooksAction(kind: 'start' | 'stop' | 'restart') {
  svcBusy.value = 'notebooks'; svcMsg.value = ''
  try {
    if (kind === 'stop') await svcPost('/api/notebooks/shutdown')
    else await svcPost(kind === 'start' ? '/api/notebooks/launch' : '/api/notebooks/restart', { projectUid: projectUid.value })
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
  showQuitConfirm.value = false
  await appCtl.quit()
  svcMsg.value = appCtl.message
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
          <input class="field-input mono" :value="verCurrent || '—'" readonly />
          <button
            class="save-btn"
            :disabled="checking"
            @click="checkUpdates"
            v-tooltip.right="'Check GitHub for a newer release'"
          >
            <i :class="['pi', checking ? 'pi-spin pi-cog' : 'pi-refresh']" />
            {{ checking ? 'Checking…' : 'Check' }}
          </button>
        </div>
        <span v-if="!updateAvailable && verCurrent && !updateMsg" class="field-hint">
          You're on the latest version.
        </span>
      </div>

      <div v-if="updateAvailable" class="field">
        <button
          class="save-btn"
          :disabled="updateBusy"
          @click="applyUpdate"
          v-tooltip.right="`Download ${verLatest} and stage it; restart Cecelia to finish.`"
        >
          <i :class="['pi', updateBusy ? 'pi-spin pi-cog' : 'pi-download']" />
          {{ updateBusy ? 'Updating…' : `Update to ${verLatest}` }}
        </button>
      </div>

      <span v-if="updateMsg" class="field-hint">{{ updateMsg }}</span>
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
          <button class="save-btn danger" :disabled="appCtl.busy" @click="showQuitConfirm = true"
                  v-tooltip.top="'Stop napari, notebooks and the backend, then exit Cecelia'">
            <i class="pi pi-power-off" /> Quit
          </button>
        </span>
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

      <div v-if="showQuitConfirm" class="svc-confirm">
        <span>Quit Cecelia — stop napari, notebooks and the backend?</span>
        <button class="save-btn danger" @click="quitApp"><i class="pi pi-power-off" /> Quit everything</button>
        <button class="save-btn ghost" @click="showQuitConfirm = false">Cancel</button>
      </div>
    </section>

    <!-- ── Diagnostics ─────────────────────────────────────────────────── -->
    <section class="settings-section">
      <h2 class="section-title">Diagnostics</h2>

      <div v-if="diag" class="diag-grid">
        <span>Version</span><span class="mono">{{ diag.version }}</span>
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

.no-project {
  font-size: 0.8rem;
  color: var(--cc-text-dim);
}

/* system control panel: aligned grid — name · status pill · port · actions */
.svc-row { display: grid; grid-template-columns: 8rem 7rem 3.5rem 1fr; align-items: center;
  column-gap: 0.6rem; margin-bottom: 0.55rem; }
.svc-name { font-size: 0.8rem; color: var(--cc-text); }
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
.svc-confirm { display: flex; align-items: center; gap: 0.5rem; margin-top: 0.6rem; font-size: 0.78rem; color: var(--cc-text);
  background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: 0.35rem; padding: 0.5rem 0.6rem; }

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
