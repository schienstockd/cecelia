<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useTaskStore, type TaskEntry } from '../stores/tasks'
import { TASK_STATUS } from '../lib/taskStatus'
import { useWsStore } from '../stores/ws'
import { useSettingsStore } from '../stores/settings'
import TeleportPopover from '../components/TeleportPopover.vue'
import PoolThrottle from '../components/PoolThrottle.vue'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import CcToggle from '../components/CcToggle.vue'
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

// status icon/colour/label come from the ONE canonical map (lib/taskStatus.ts)


const FILTERS: ChipOption[] = [
  { value: 'all',       label: 'All' },
  { value: 'active',    label: 'Active' },
  { value: 'done',      label: 'Done' },
  { value: 'failed',    label: 'Failed' },
  { value: 'cancelled', label: 'Cancelled' },
]
</script>

<template>
  <div class="tm-shell">

    <!-- ── Toolbar ─────────────────────────────────────────────────────── -->
    <div class="tm-toolbar">
      <span class="tm-title">Task Manager</span>

      <ChipSelect
        class="filter-chips" :options="FILTERS" :model-value="statusFilter"
        aria-label="Filter tasks by status"
        @update:model-value="v => statusFilter = v as typeof statusFilter" />

      <CcToggle class="follow-toggle" v-model="settings.taskListAutoFollow" label="Auto-follow"
        v-tooltip.left="'Automatically select the newest running task'" />

      <button ref="throttleBtn" class="tm-throttle cc-btn cc-btn-bare cc-btn-icon"
        :class="{ 'cc-btn-on cc-btn-on-solid': throttleOpen }"
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
          :class="{ selected: t.id === selectedId }"
          @click="select(t)"
        >
          <i :class="['pi', TASK_STATUS[t.status].icon, 'row-icon']"
            :style="{ color: TASK_STATUS[t.status].color }"
            v-tooltip.right="TASK_STATUS[t.status].label" />

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
                <span class="row-seq cc-muted cc-fs-2xs">#{{ t.seq }}</span>
                {{ t.label }}
              </span>
              <span v-if="elapsed(t)" class="row-elapsed cc-muted cc-fs-2xs">{{ elapsed(t) }}</span>
            </div>
            <div class="row-image cc-muted cc-fs-xs">{{ t.imageName }}</div>
          </div>

          <div class="row-actions" @click.stop>
            <button v-if="t.status === 'running' || t.status === 'queued'"
              class="ra-btn cc-btn cc-btn-bare cc-btn-icon danger" @click="cancelTask(t)"
              v-tooltip.left="t.chainRunId ? 'Stop chain run' : 'Cancel task'">
              <i class="pi pi-times" />
            </button>
            <button v-if="canRerun(t)"
              class="ra-btn cc-btn cc-btn-bare cc-btn-icon" @click="rerun(t)" v-tooltip.left="'Rerun'">
              <i class="pi pi-replay" />
            </button>
            <button v-if="t.status === 'done' || t.status === 'failed' || t.status === 'cancelled'"
              class="ra-btn cc-btn cc-btn-bare cc-btn-icon" @click="tasks.remove(t.id)" v-tooltip.left="'Dismiss'">
              <i class="pi pi-trash" />
            </button>
          </div>
        </div>
      </div>

      <!-- Log panel -->
      <div class="tm-log-panel">
        <template v-if="selected">
          <div class="log-header">
            <i :class="['pi', TASK_STATUS[selected.status].icon, 'log-status-icon']"
               :style="{ color: TASK_STATUS[selected.status].color }" />
            <div class="log-title-block">
              <div class="log-title-row">
                <span class="log-title">#{{ selected.seq }} — {{ selected.label }}</span>
                <span v-if="selected.chainRunId" class="chain-pill sm"
                  v-tooltip.right="`Chain: ${selected.chainName ?? selected.chainRunId} / ${selected.chainRunId}`">
                  <i class="pi pi-sitemap" />{{ selected.chainName || selected.chainRunId }}
                </span>
              </div>
              <span class="log-image cc-muted cc-fs-xs">{{ selected.imageName }}</span>
            </div>
            <span v-if="elapsed(selected)" class="log-elapsed cc-muted cc-fs-xs">{{ elapsed(selected) }}</span>
            <div class="log-actions">
              <button class="ra-btn cc-btn cc-btn-bare cc-btn-icon" @click="copyLog" v-tooltip.left="copied ? 'Copied!' : 'Copy log'">
                <i :class="['pi', copied ? 'pi-check' : 'pi-copy']" />
              </button>
              <button v-if="selected.status === 'running' || selected.status === 'queued'"
                class="ra-btn cc-btn cc-btn-bare cc-btn-icon danger" @click="cancelTask(selected)"
                v-tooltip.left="selected.chainRunId ? 'Stop chain run' : 'Cancel task'">
                <i class="pi pi-times" />
              </button>
              <button v-if="canRerun(selected)"
                class="ra-btn cc-btn cc-btn-bare cc-btn-icon" @click="rerun(selected)" v-tooltip.left="'Rerun'">
                <i class="pi pi-replay" />
              </button>
            </div>
          </div>

          <div v-if="selected.status === 'running' && selected.progress !== undefined" class="log-progress">
            <div class="log-progress-fill" :style="{ width: `${(selected.progress * 100).toFixed(1)}%` }" />
          </div>

          <pre ref="logEl" class="log-body">{{ selected.log.join('\n') || '— no output yet —' }}</pre>
        </template>

        <div v-else class="log-empty cc-empty">
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
  font-size: var(--cc-fs-md);
  font-weight: 600;
  color: var(--cc-text);
  flex-shrink: 0;
}
.filter-chips {
  flex: 1;
}

.follow-toggle {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  font-size: var(--cc-fs-sm);
  color: var(--cc-text-dim);
  cursor: pointer;
  flex-shrink: 0;
  user-select: none;
}
.follow-toggle input { accent-color: var(--cc-accent); cursor: pointer; }

.tm-throttle { transition: background 0.1s, color 0.1s; }   /* + cc-btn cc-btn-bare cc-btn-icon */
.tm-throttle:hover  { background: var(--cc-surface-2); color: var(--cc-text); }

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
  font-size: var(--cc-fs-sm);
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

.row-icon { font-size: var(--cc-fs-md); flex-shrink: 0; }
/* status icon colour is inline from TASK_STATUS (lib/taskStatus.ts) */

.row-body  { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 0.1rem; }
.row-top   { display: flex; align-items: center; gap: 0.35rem; min-width: 0; }
.mod-pill  {
  font-size: var(--cc-fs-2xs);
  font-weight: 700;
  padding: 0.05rem 0.3rem;
  border-radius: var(--cc-radius-xs);
  flex-shrink: 0;
  text-transform: uppercase;
  letter-spacing: 0.04em;
}
.row-label {
  font-size: var(--cc-fs-sm);
  font-weight: 500;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  flex: 1;
}
.row-seq { font-family: var(--cc-mono); margin-right: 0.2rem; }
.chain-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.2rem;
  font-size: var(--cc-fs-3xs);
  font-weight: 700;
  padding: 0.05rem 0.3rem;
  border-radius: var(--cc-radius-xs);
  background: #a78bfa22;
  color: var(--cc-accent);
  flex-shrink: 0;
  max-width: 7rem;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  text-transform: uppercase;
  letter-spacing: 0.03em;
}
.chain-pill .pi { font-size: var(--cc-fs-3xs); flex-shrink: 0; }
.chain-pill.sm { font-size: var(--cc-fs-2xs); padding: 0.1rem 0.4rem; max-width: 10rem; }
.log-title-row { display: flex; align-items: center; gap: 0.4rem; min-width: 0; }
.row-elapsed { font-family: var(--cc-mono); flex-shrink: 0; }
.row-image { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }

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
.log-empty { flex: 1; }
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
.log-status-icon { font-size: var(--cc-fs-lg); flex-shrink: 0; }
/* log-status-icon colour is inline from TASK_STATUS (lib/taskStatus.ts) */

.log-title-block { flex: 1; min-width: 0; display: flex; flex-direction: column; }
.log-title { font-size: var(--cc-fs-md); font-weight: 600; color: var(--cc-text); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.log-image { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.log-elapsed { font-family: var(--cc-mono); flex-shrink: 0; }
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
  font-size: var(--cc-fs-sm);
  color: var(--cc-text-dim);
  background: var(--cc-console-bg, var(--cc-bg));
  white-space: pre-wrap;
  word-break: break-all;
  line-height: 1.55;
}

/* ── Shared button style ──────────────────────────────────────────────── */
/* .ra-btn → cc-btn cc-btn-bare cc-btn-icon */
.ra-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.ra-btn.danger:hover { background: #7f1d1d55; color: #fca5a5; }
</style>
