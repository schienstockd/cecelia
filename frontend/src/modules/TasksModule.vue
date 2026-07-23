<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useTaskStore, type TaskEntry, type TaskStatus } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useSettingsStore } from '../stores/settings'
import TeleportPopover from '../components/TeleportPopover.vue'
import PoolThrottle from '../components/PoolThrottle.vue'
import { moduleColor } from '../utils/taskModule'

const tasks    = useTaskStore()
const ws       = useWsStore()
const settings = useSettingsStore()

// live scheduler throttle — a quick popover off the toolbar (not buried in Settings)
const throttleBtn  = ref<HTMLElement | null>(null)
const throttleOpen = ref(false)

const selectedId   = ref<string | null>(null)
const statusFilter = ref<'all' | 'active' | 'done' | 'failed' | 'cancelled'>('all')
const logEl        = ref<HTMLElement | null>(null)
const copied       = ref(false)

const selected = computed(() => tasks.tasks.find(t => t.id === selectedId.value) ?? null)

const filtered = computed(() => {
  return tasks.tasks.filter(t => {
    if (statusFilter.value === 'all')    return true
    if (statusFilter.value === 'active') return t.status === 'running' || t.status === 'queued'
    return t.status === statusFilter.value
  })
})

// Honour jump requests from sidebar
onMounted(() => {
  if (tasks.jumpToId) {
    selectedId.value = tasks.jumpToId
    tasks.jumpToId   = null
  }
})

// Listen directly to the WS event — no reactive tracking edge cases
function onTaskStatus(data: Record<string, unknown>) {
  if (!settings.taskListAutoFollow) return
  const s = String(data.status ?? '')
  if (s === 'running' || s === 'queued')
    selectedId.value = String(data.taskId ?? '')
}
onMounted(() => ws.on('task:status', onTaskStatus))
onUnmounted(() => ws.off('task:status', onTaskStatus))

// Auto-scroll log on new lines
watch(
  () => selected.value?.log.length,
  () => nextTick(() => { if (logEl.value) logEl.value.scrollTop = logEl.value.scrollHeight })
)

function select(t: TaskEntry) {
  selectedId.value = t.id
}

function cancelTask(t: TaskEntry) {
  if (t.chainRunId) {
    tasks.cancelChainRun(t.chainRunId)
    ws.send({ type: 'chain:cancel', runId: t.chainRunId })
  } else {
    tasks.cancel(t.id)
    ws.send({ type: 'task:cancel', taskId: t.id })
  }
}

function rerun(t: TaskEntry) {
  tasks.restart(t.id)
  ws.send({ type: 'task:restart', taskId: t.id, funName: t.funName,
            params: t.params, imageUid: t.imageUid, projectUid: t.projectUid })
}

// Rerun goes through the scheduler (task:restart → handle_task_run), so it only applies to
// scheduler-backed tasks. A data patch (module 'maintenance') is a non-scheduler producer of the same
// task frames — it has no fun_name the scheduler knows, so it can't be rerun here (relaunch it from
// Settings → Data patches instead).
const canRerun = (t: TaskEntry) =>
  (t.status === 'done' || t.status === 'failed' || t.status === 'cancelled') && t.module !== 'maintenance'

async function copyLog() {
  if (!selected.value?.log.length) return
  await navigator.clipboard.writeText(selected.value.log.join('\n'))
  copied.value = true
  setTimeout(() => { copied.value = false }, 1500)
}

function elapsed(t: TaskEntry) {
  if (!t.startedAt) return null
  const end = t.finishedAt ?? new Date()
  const s   = Math.round((end.getTime() - t.startedAt.getTime()) / 1000)
  return s < 60 ? `${s}s` : `${Math.floor(s / 60)}m ${s % 60}s`
}

const statusCfg: Record<TaskStatus, { cls: string; icon: string; tip: string }> = {
  queued:    { cls: 'st-queued',    icon: 'pi-clock',        tip: 'Queued' },
  running:   { cls: 'st-running',   icon: 'pi-spin pi-cog',  tip: 'Running' },
  done:      { cls: 'st-done',      icon: 'pi-check-circle', tip: 'Done' },
  failed:    { cls: 'st-failed',    icon: 'pi-times-circle', tip: 'Failed' },
  cancelled: { cls: 'st-cancelled', icon: 'pi-ban',          tip: 'Cancelled' },
}


const FILTERS = [
  { key: 'all',       label: 'All' },
  { key: 'active',    label: 'Active' },
  { key: 'done',      label: 'Done' },
  { key: 'failed',    label: 'Failed' },
  { key: 'cancelled', label: 'Cancelled' },
] as const
</script>

<template>
  <div class="tm-shell">

    <!-- ── Toolbar ─────────────────────────────────────────────────────── -->
    <div class="tm-toolbar">
      <span class="tm-title">Task Manager</span>

      <div class="filter-chips">
        <button
          v-for="f in FILTERS" :key="f.key"
          class="chip"
          :class="{ active: statusFilter === f.key }"
          @click="statusFilter = f.key"
        >{{ f.label }}</button>
      </div>

      <label class="follow-toggle" v-tooltip.left="'Automatically select the newest running task'">
        <input type="checkbox" v-model="settings.taskListAutoFollow" />
        Auto-follow
      </label>

      <button ref="throttleBtn" class="tm-throttle" :class="{ active: throttleOpen }"
        @click="throttleOpen = !throttleOpen"
        v-tooltip.left="'Throttle — how many tasks of each kind run at once'">
        <i class="pi pi-sliders-h" />
      </button>
      <TeleportPopover v-model="throttleOpen" :anchor="throttleBtn" placement="bottom-end">
        <PoolThrottle />
      </TeleportPopover>
    </div>

    <!-- ── Body ───────────────────────────────────────────────────────── -->
    <div class="tm-body">

      <!-- Task list -->
      <div class="tm-list">
        <div v-if="filtered.length === 0" class="tm-empty">No tasks.</div>

        <div
          v-for="t in filtered" :key="t.id"
          class="tm-row"
          :class="[statusCfg[t.status].cls, { selected: t.id === selectedId }]"
          @click="select(t)"
        >
          <i :class="['pi', statusCfg[t.status].icon, 'row-icon']"
            v-tooltip.right="statusCfg[t.status].tip" />

          <div class="row-body">
            <div class="row-top">
              <span class="mod-pill" :style="{ background: moduleColor(t.module) + '33', color: moduleColor(t.module) }">
                {{ t.module }}
              </span>
              <span v-if="t.chainRunId" class="chain-pill"
                v-tooltip.right="`Chain: ${t.chainName ?? t.chainRunId} / ${t.chainRunId}`">
                <i class="pi pi-sitemap" />{{ t.chainName || t.chainRunId }}
              </span>
              <span class="row-label">
                <span class="row-seq">#{{ t.seq }}</span>
                {{ t.label }}
              </span>
              <span v-if="elapsed(t)" class="row-elapsed">{{ elapsed(t) }}</span>
            </div>
            <div class="row-image">{{ t.imageName }}</div>
          </div>

          <div class="row-actions" @click.stop>
            <button v-if="t.status === 'running' || t.status === 'queued'"
              class="ra-btn danger" @click="cancelTask(t)"
              v-tooltip.left="t.chainRunId ? 'Stop chain run' : 'Cancel task'">
              <i class="pi pi-times" />
            </button>
            <button v-if="canRerun(t)"
              class="ra-btn" @click="rerun(t)" v-tooltip.left="'Rerun'">
              <i class="pi pi-replay" />
            </button>
            <button v-if="t.status === 'done' || t.status === 'failed' || t.status === 'cancelled'"
              class="ra-btn" @click="tasks.remove(t.id)" v-tooltip.left="'Dismiss'">
              <i class="pi pi-trash" />
            </button>
          </div>
        </div>
      </div>

      <!-- Log panel -->
      <div class="tm-log-panel">
        <template v-if="selected">
          <div class="log-header">
            <i :class="['pi', statusCfg[selected.status].icon, 'log-status-icon', statusCfg[selected.status].cls]" />
            <div class="log-title-block">
              <div class="log-title-row">
                <span class="log-title">#{{ selected.seq }} — {{ selected.label }}</span>
                <span v-if="selected.chainRunId" class="chain-pill sm"
                  v-tooltip.right="`Chain: ${selected.chainName ?? selected.chainRunId} / ${selected.chainRunId}`">
                  <i class="pi pi-sitemap" />{{ selected.chainName || selected.chainRunId }}
                </span>
              </div>
              <span class="log-image">{{ selected.imageName }}</span>
            </div>
            <span v-if="elapsed(selected)" class="log-elapsed">{{ elapsed(selected) }}</span>
            <div class="log-actions">
              <button class="ra-btn" @click="copyLog" v-tooltip.left="copied ? 'Copied!' : 'Copy log'">
                <i :class="['pi', copied ? 'pi-check' : 'pi-copy']" />
              </button>
              <button v-if="selected.status === 'running' || selected.status === 'queued'"
                class="ra-btn danger" @click="cancelTask(selected)"
                v-tooltip.left="selected.chainRunId ? 'Stop chain run' : 'Cancel task'">
                <i class="pi pi-times" />
              </button>
              <button v-if="canRerun(selected)"
                class="ra-btn" @click="rerun(selected)" v-tooltip.left="'Rerun'">
                <i class="pi pi-replay" />
              </button>
            </div>
          </div>

          <div v-if="selected.status === 'running' && selected.progress !== undefined" class="log-progress">
            <div class="log-progress-fill" :style="{ width: `${(selected.progress * 100).toFixed(1)}%` }" />
          </div>

          <pre ref="logEl" class="log-body">{{ selected.log.join('\n') || '— no output yet —' }}</pre>
        </template>

        <div v-else class="log-empty">
          <i class="pi pi-list-check" />
          <span>Select a task to view its log.</span>
        </div>
      </div>

    </div>
  </div>
</template>

<style scoped>
.tm-shell {
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: hidden;
}

/* ── Toolbar ──────────────────────────────────────────────────────────── */
.tm-toolbar {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 0.5rem 1rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
  background: var(--cc-surface-1);
}
.tm-title {
  font-size: 0.82rem;
  font-weight: 600;
  color: var(--cc-text);
  flex-shrink: 0;
}
.filter-chips {
  display: flex;
  gap: 0.3rem;
  flex: 1;
}
.chip {
  font-size: 0.72rem;
  padding: 0.18rem 0.55rem;
  border-radius: 1rem;
  border: 1px solid var(--cc-border);
  background: none;
  color: var(--cc-text-dim);
  cursor: pointer;
  transition: background 0.1s, color 0.1s;
}
.chip:hover  { background: var(--cc-surface-2); color: var(--cc-text); }
.chip.active { background: var(--cc-accent); border-color: var(--cc-accent); color: #fff; }

.follow-toggle {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  cursor: pointer;
  flex-shrink: 0;
  user-select: none;
}
.follow-toggle input { accent-color: var(--cc-accent); cursor: pointer; }

.tm-throttle {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 1.7rem;
  height: 1.7rem;
  border: 1px solid var(--cc-border);
  border-radius: 0.3rem;
  background: none;
  color: var(--cc-text-dim);
  cursor: pointer;
  flex-shrink: 0;
  transition: background 0.1s, color 0.1s;
}
.tm-throttle:hover  { background: var(--cc-surface-2); color: var(--cc-text); }
.tm-throttle.active { background: var(--cc-accent); border-color: var(--cc-accent); color: #fff; }

/* ── Body ─────────────────────────────────────────────────────────────── */
.tm-body {
  display: flex;
  flex: 1;
  overflow: hidden;
}

/* ── Task list ────────────────────────────────────────────────────────── */
.tm-list {
  width: 340px;
  flex-shrink: 0;
  border-right: 1px solid var(--cc-border);
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  gap: 1px;
  background: var(--cc-border);
}
.tm-empty {
  padding: 2rem 1rem;
  font-size: 0.78rem;
  color: var(--cc-text-dim);
  text-align: center;
  background: var(--cc-bg);
}

.tm-row {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.45rem 0.6rem;
  background: var(--cc-surface-1);
  cursor: pointer;
  transition: background 0.1s;
  position: relative;
}
.tm-row:hover    { background: var(--cc-surface-2); }
.tm-row.selected { background: var(--cc-surface-2); }
.tm-row.selected::before {
  content: '';
  position: absolute;
  left: 0; top: 0; bottom: 0;
  width: 3px;
  background: var(--cc-accent);
  border-radius: 0 2px 2px 0;
}

.row-icon { font-size: 0.82rem; flex-shrink: 0; }
.st-running .row-icon { color: #93c5fd; }
.st-done    .row-icon { color: #86efac; }
.st-failed  .row-icon { color: #fca5a5; }
.st-queued  .row-icon { color: #71717a; }
.st-cancelled .row-icon { color: #52525b; }

.row-body  { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.row-top   { display: flex; align-items: center; gap: 0.35rem; min-width: 0; }
.mod-pill  {
  font-size: 0.6rem;
  font-weight: 700;
  padding: 0.05rem 0.3rem;
  border-radius: 0.2rem;
  flex-shrink: 0;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}
.row-label {
  font-size: 0.76rem;
  font-weight: 500;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  flex: 1;
}
.row-seq { font-size: 0.62rem; font-family: var(--cc-mono); color: var(--cc-text-dim); margin-right: 0.2rem; }
.chain-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.2rem;
  font-size: 0.58rem;
  font-weight: 700;
  padding: 0.05rem 0.3rem;
  border-radius: 0.2rem;
  background: #a78bfa22;
  color: #a78bfa;
  flex-shrink: 0;
  max-width: 7rem;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  text-transform: uppercase;
  letter-spacing: 0.03em;
}
.chain-pill .pi { font-size: 0.55rem; flex-shrink: 0; }
.chain-pill.sm { font-size: 0.62rem; padding: 0.1rem 0.4rem; max-width: 10rem; }
.log-title-row { display: flex; align-items: center; gap: 0.4rem; min-width: 0; }
.row-elapsed { font-size: 0.65rem; font-family: var(--cc-mono); color: var(--cc-text-dim); flex-shrink: 0; }
.row-image { font-size: 0.68rem; color: var(--cc-text-dim); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }

.row-actions {
  display: flex;
  gap: 0.1rem;
  flex-shrink: 0;
  opacity: 0;
  transition: opacity 0.1s;
}
.tm-row:hover .row-actions { opacity: 1; }

/* ── Log panel ────────────────────────────────────────────────────────── */
.tm-log-panel {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  background: var(--cc-bg);
}
.log-empty {
  flex: 1;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
  color: var(--cc-text-dim);
  font-size: 0.82rem;
}
.log-empty .pi { font-size: 1.5rem; opacity: 0.3; }

.log-header {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.55rem 0.9rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
  background: var(--cc-surface-1);
}
.log-status-icon { font-size: 0.9rem; flex-shrink: 0; }
.log-status-icon.st-running { color: #93c5fd; }
.log-status-icon.st-done    { color: #86efac; }
.log-status-icon.st-failed  { color: #fca5a5; }
.log-status-icon.st-queued  { color: #71717a; }
.log-status-icon.st-cancelled { color: #52525b; }

.log-title-block { flex: 1; min-width: 0; display: flex; flex-direction: column; }
.log-title { font-size: 0.82rem; font-weight: 600; color: var(--cc-text); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.log-image { font-size: 0.7rem; color: var(--cc-text-dim); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.log-elapsed { font-size: 0.7rem; font-family: var(--cc-mono); color: var(--cc-text-dim); flex-shrink: 0; }
.log-actions { display: flex; gap: 0.15rem; flex-shrink: 0; }

.log-progress {
  height: 3px;
  background: var(--cc-surface-2);
  flex-shrink: 0;
}
.log-progress-fill {
  height: 100%;
  background: var(--cc-accent);
  transition: width 0.25s ease;
  min-width: 2px;
}

.log-body {
  flex: 1;
  overflow-y: auto;
  margin: 0;
  padding: 0.75rem 1rem;
  font-family: var(--cc-mono);
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  background: var(--cc-console-bg, var(--cc-bg));
  white-space: pre-wrap;
  word-break: break-all;
  line-height: 1.55;
}

/* ── Shared button style ──────────────────────────────────────────────── */
.ra-btn {
  background: none;
  border: none;
  cursor: pointer;
  color: var(--cc-text-dim);
  font-size: 0.72rem;
  padding: 0.2rem 0.3rem;
  border-radius: 0.2rem;
}
.ra-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.ra-btn.danger:hover { background: #7f1d1d55; color: #fca5a5; }
</style>
