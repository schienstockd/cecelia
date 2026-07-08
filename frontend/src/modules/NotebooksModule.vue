<script setup lang="ts">
// Notebooks — the Pluto notebook Playground: launch the (pure-Julia) notebook server and manage this
// project's notebooks (add / describe / snapshot / delete + duplicate examples). The server runs as
// its own process (api/src/notebooks_api.jl); this page launches/probes it and opens it in a new tab.
// See docs/todo/NOTEBOOK_PLAYGROUND_PLAN.md.
import { ref, computed, onMounted, onUnmounted } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import NotebookTable from '../components/NotebookTable.vue'

const projectMeta = useProjectMetaStore()

type ServerState = 'unknown' | 'stopped' | 'starting' | 'running'
const server = ref<ServerState>('unknown')
const url = ref('http://localhost:7660/')
const secret = ref('')
const errorMsg = ref('')

// Fast-plot sysimage (pluto/deps.so): built on first visit, in the background, so notebooks are
// usable immediately (slow first plot until it lands). See api_notebooks_build_sysimage.
// 'stale' = an image built for a different Julia/package set (after an update) — rebuilt like 'absent'.
type SysimageState = 'unknown' | 'absent' | 'stale' | 'building' | 'ready' | 'error'
const sysimage = ref<SysimageState>('unknown')

// Pluto requires a secret (see launch.jl); append it so the browser is authorized.
const homeUrl = computed(() => secret.value ? `${url.value}?secret=${encodeURIComponent(secret.value)}` : url.value)

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const hasProject = computed(() => projectMeta.hasProject)
const serverRunning = computed(() => server.value === 'running')

let poll: number | undefined
let buildPoll: number | undefined

function stopPoll() { if (poll) { clearInterval(poll); poll = undefined } }
function stopBuildPoll() { if (buildPoll) { clearInterval(buildPoll); buildPoll = undefined } }

// Kick off the background build when the cache is missing ('absent') OR outdated after a package
// update ('stale'). Non-fatal: if it can't start (env not set up), notebooks still launch — just
// with a slow first plot.
async function maybeBuildSysimage() {
  if (sysimage.value !== 'absent' && sysimage.value !== 'stale') return
  try {
    const res = await fetch('/api/notebooks/build-sysimage', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: '{}',
    })
    const d = await res.json().catch(() => ({}))
    if (res.ok) sysimage.value = d.status ?? 'building'
  } catch { /* silent — the fallback (slow first plot) still works */ }
  if (sysimage.value === 'building') startBuildPoll()
}

// Poll until the build finishes (~10 min) — slow cadence, it's a long job.
function startBuildPoll() {
  stopBuildPoll()
  buildPoll = window.setInterval(async () => {
    await refreshStatus()
    if (sysimage.value === 'ready' || sysimage.value === 'error') stopBuildPoll()
  }, 5000)
}

function startPolling() {
  stopPoll()
  poll = window.setInterval(async () => {
    await refreshStatus()
    // stop once it's up OR a startup error surfaced (e.g. env not set up)
    if (server.value === 'running' || errorMsg.value) {
      stopPoll()
      if (errorMsg.value) server.value = 'stopped'
    }
  }, 2000)
}

async function refreshStatus() {
  try {
    const d = await (await fetch('/api/notebooks/status')).json()
    url.value = d.url ?? url.value
    secret.value = d.secret ?? ''
    server.value = d.running ? 'running' : (d.starting ? 'starting' : 'stopped')
    sysimage.value = d.sysimage ?? 'unknown'
    if (d.error) errorMsg.value = d.error
    else if (d.running) errorMsg.value = ''
  } catch {
    server.value = 'stopped'
  }
}

async function launch() {
  if (!projectUid.value) return
  errorMsg.value = ''
  server.value = 'starting'
  try {
    const res = await fetch('/api/notebooks/launch', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value }),
    })
    const d = await res.json().catch(() => ({}))
    if (!res.ok) throw new Error(d.error ?? `HTTP ${res.status}`)
    url.value = d.url ?? url.value
    secret.value = d.secret ?? ''
    if (!d.starting) { server.value = 'running'; return }
    startPolling()
  } catch (e) {
    server.value = 'stopped'
    errorMsg.value = e instanceof Error ? e.message : String(e)
  }
}

async function restart() {
  if (!projectUid.value) return
  errorMsg.value = ''
  server.value = 'starting'
  try {
    const res = await fetch('/api/notebooks/restart', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value }),
    })
    const d = await res.json().catch(() => ({}))
    if (!res.ok) throw new Error(d.error ?? `HTTP ${res.status}`)
    url.value = d.url ?? url.value
    secret.value = d.secret ?? ''
    if (!d.starting) { server.value = 'running'; return }
    startPolling()
  } catch (e) {
    server.value = 'stopped'
    errorMsg.value = e instanceof Error ? e.message : String(e)
  }
}

async function shutdown() {
  stopPoll()
  try {
    const res = await fetch('/api/notebooks/shutdown', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: '{}',
    })
    const d = await res.json().catch(() => ({}))
    if (!res.ok) errorMsg.value = d.error ?? `HTTP ${res.status}`
  } catch (e) {
    errorMsg.value = e instanceof Error ? e.message : String(e)
  }
  await refreshStatus()
}

onMounted(async () => { await refreshStatus(); maybeBuildSysimage() })
onUnmounted(() => { stopPoll(); stopBuildPoll() })
</script>

<template>
  <div class="notebooks-page">
    <header class="nb-header">
      <h1><i class="pi pi-book" /> Notebooks</h1>
      <p class="nb-sub">
        Pure-Julia downstream analysis with <strong>Pluto</strong> — load objects, pull cell tables
        via <code>pop_df</code>, plot, and export. Runs in its own Julia session.
      </p>
    </header>

    <div v-if="!hasProject" class="nb-empty">
      <i class="pi pi-lock" /> Open or create a project first.
    </div>

    <template v-else>
      <!-- Server launch / status -->
      <section class="nb-section">
        <div class="nb-server-row">
          <span class="nb-status" :class="`is-${server}`">
            <i class="pi" :class="server === 'running' ? 'pi-circle-fill'
                                 : server === 'starting' ? 'pi-spin pi-spinner'
                                 : 'pi-circle'" />
            {{ server === 'running' ? 'Server running'
             : server === 'starting' ? 'Starting…'
             : server === 'stopped' ? 'Not running' : 'Checking…' }}
          </span>

          <button v-if="server !== 'running'" class="cc-btn cc-btn-primary"
                  :disabled="server === 'starting'" @click="launch">
            <i class="pi pi-play" /> Launch server
          </button>

          <template v-else>
            <a class="cc-btn cc-btn-primary" :href="homeUrl" target="_blank" rel="noopener">
              <i class="pi pi-external-link" /> Open Notebooks
            </a>
            <button class="cc-btn cc-btn-ghost" @click="restart" v-tooltip.top="'Stop and relaunch the notebook server'">
              <i class="pi pi-refresh" /> Restart
            </button>
            <button class="cc-btn cc-btn-ghost" @click="shutdown" v-tooltip.top="'Stop the notebook server'">
              <i class="pi pi-power-off" /> Shut down
            </button>
          </template>
        </div>
        <p v-if="server === 'starting'" class="nb-hint">
          First launch precompiles — this can take up to a minute.
        </p>
        <p v-if="errorMsg" class="nb-error"><i class="pi pi-exclamation-triangle" /> {{ errorMsg }}</p>

        <!-- Fast-plot cache build (background): first run, or a rebuild after a package update -->
        <div v-if="sysimage === 'building'" class="nb-note">
          <i class="pi pi-spin pi-cog" />
          <span>
            <strong>Preparing notebooks — building the fast-plot cache (~10 min, in the background).</strong>
            This runs once after install and again after an update. Keep working — notebooks are usable
            now; only the <em>first</em> plot is slow until it finishes. The cache is picked up
            automatically the next time the server launches.
          </span>
        </div>
        <p v-else-if="sysimage === 'error'" class="nb-hint">
          <i class="pi pi-info-circle" /> Couldn't build the fast-plot cache — notebooks still work,
          just with a slower first plot.
        </p>
      </section>

      <!-- Notebook registry (Phase 3: manage + version) -->
      <section class="nb-section">
        <h2>Notebooks</h2>
        <NotebookTable :project-uid="projectUid" :server-url="url" :server-secret="secret" :server-running="serverRunning" />
      </section>
    </template>
  </div>
</template>

<style scoped>
.notebooks-page { padding: 1.25rem 1.5rem; max-width: 980px; }
.nb-header h1 { display: flex; align-items: center; gap: .5rem; margin: 0 0 .25rem; font-size: 1.4rem; }
.nb-sub { color: var(--cc-text-muted, #888); margin: 0 0 1rem; max-width: 640px; }
.nb-empty { display: flex; align-items: center; gap: .5rem; color: var(--cc-text-muted, #888); padding: 2rem 0; }
.nb-section { margin-bottom: 1.5rem; }
.nb-section h2 { font-size: 1.05rem; margin: 0 0 .5rem; }
.nb-server-row { display: flex; align-items: center; gap: 1rem; }
.nb-status { display: inline-flex; align-items: center; gap: .4rem; font-size: .9rem; }
.nb-status.is-running .pi { color: #3fb950; }
.nb-status.is-stopped .pi, .nb-status.is-unknown .pi { color: var(--cc-text-muted, #888); }
.nb-hint { color: var(--cc-text-muted, #888); font-size: .85rem; margin: .5rem 0 0; }
.nb-error { color: #f0883e; font-size: .85rem; margin: .5rem 0 0; display: flex; align-items: center; gap: .4rem; }
.nb-note {
  display: flex; align-items: flex-start; gap: .55rem; margin: .75rem 0 0; padding: .65rem .8rem;
  font-size: .85rem; line-height: 1.4; border-radius: 6px;
  color: var(--cc-text, #cdd9e5); background: rgba(88, 166, 255, .08); border: 1px solid rgba(88, 166, 255, .25);
}
.nb-note .pi { margin-top: .1rem; color: #58a6ff; }
</style>
