<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useTaskStore, type TaskEntry } from '../stores/tasks'
import { TASK_STATUS } from '../lib/taskStatus'
import { useCopyFlash } from '../composables/useCopyFlash'
import { useWsStore } from '../stores/ws'
import { useSettingsStore } from '../stores/settings'
import { useProjectMetaStore } from '../stores/projectMeta'
import TeleportPopover from '../components/TeleportPopover.vue'
import PoolThrottle from '../components/PoolThrottle.vue'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import CcToggle from '../components/CcToggle.vue'
import CcProgressBar from '../components/CcProgressBar.vue'
import SelectionTable, { type SelectionColumn } from '../components/SelectionTable.vue'
import { taskRows } from '../utils/taskRows'
import { usePanelResize } from '../composables/usePanelResize'
import { moduleTagStyle } from '../utils/taskModule'
import { fetchLogBackfill } from '../utils/taskLogBackfill'
import { useNowTick } from '../composables/useNowTick'
import { taskElapsed } from '../utils/taskElapsed'
import { canRerunTask } from '../utils/taskRerun'
// the foreign-project LABEL now comes off the row (utils/taskRows.ts); the scope predicate stays here
import { taskInScope } from '../utils/taskScope'

const tasks    = useTaskStore()
const ws       = useWsStore()
const settings = useSettingsStore()
const projectMeta = useProjectMetaStore()

// live scheduler throttle — a quick popover off the toolbar (not buried in Settings)
const throttleBtn  = ref<HTMLElement | null>(null)
const throttleOpen = ref(false)

const selectedId   = ref<string | null>(null)
const statusFilter = ref<'all' | 'active' | 'done' | 'failed' | 'cancelled'>('all')
const logEl        = ref<HTMLElement | null>(null)
// shared copy+flash helper (docs/ui/PRIMITIVES.md)
const { isCopied: copied, copy } = useCopyFlash()

const selected = computed(() => tasks.tasks.find(t => t.id === selectedId.value) ?? null)

// Scoped to the open project by default — the rule, its two exceptions and why they exist live in
// `utils/taskScope.ts`. In short: the task store is not cleared when a project is opened (a run keeps
// reporting into the tab that launched it), so after a switch this list was showing the previous
// project's rows with nothing on them to say so.
const inScope = (t: TaskEntry) =>
  taskInScope(t, projectMeta.current?.uid, settings.tasksThisProjectOnly)

const filtered = computed(() => {
  return tasks.tasks.filter(t => {
    if (!inScope(t)) return false
    if (statusFilter.value === 'all')    return true
    if (statusFilter.value === 'active') return t.status === 'running' || t.status === 'queued'
    return t.status === statusFilter.value
  })
})

// shared formatter + shared 1s clock, so a running task's elapsed advances without a frame arriving.
// Declared ABOVE `rows`, which reads it — see utils/setupOrder.ts for why order matters here.
const now = useNowTick()
const elapsed = (t: TaskEntry) => taskElapsed(t.startedAt, t.finishedAt, now.value)

// The table reads row FIELDS (and sorts by them), so the entries are flattened by the shared mapper —
// the same one the per-module list uses, so the two lists can't drift again. Where the two surfaces
// genuinely differ they differ in their `#cell-*` slots, not here (utils/taskRows.ts).
const rows = computed(() => taskRows(filtered.value, {
  currentProjectUid: projectMeta.current?.uid,
  thisProjectOnly:   settings.tasksThisProjectOnly,
  nameOfProject:     uid => projectMeta.recent.find(p => p.uid === uid)?.name,
  now:               now.value,
}))

// Starting widths, not minimums: the table is `fit="fill"` (the default), so it is exactly the pane's
// width and the leftover is shared out. `fit="content"` was wrong here — it makes the declared sum a
// MIN width, which overflowed this pane by 184px and pushed Time and the progress bar off-screen.
// The list/log divider — the shared resize composable, with the handle on the list's RIGHT edge.
// Wider by default than the hand-rolled list's 340px, because this one carries six columns.
const { widthStyle: listWidthStyle, onResizeStart: onListResizeStart } =
  usePanelResize({ min: 280, max: 760, default: 440, storageKey: 'cc-tasks-list-width', edge: 'right' })

const TM_COLUMNS: SelectionColumn[] = [
  // no label, and out of the resize path: an icon and a bar are their own width
  { key: 'status',   label: '',       fixed: true, width: 24 },
  { key: 'module',   label: 'Module', sortable: true, ellipsis: true, width: 70 },
  { key: 'task',     label: 'Task',   sortable: true, ellipsis: true, width: 130 },
  { key: 'image',    label: 'Image',  sortable: true, ellipsis: true, width: 100 },
  { key: 'progress', label: '',       fixed: true, width: 36 },
  // `elapsed` is `4m 12s`, which sorts BEFORE `59s` as text — hence the raw-ms sort key
  { key: 'elapsed',  label: 'Time',   sortable: true, sortKey: 'elapsedMs', width: 44 },
]

// A row that has just gone out of scope must not stay open in the detail pane — the log below would
// then belong to a task the list no longer shows.
watch(() => filtered.value.some(t => t.id === selectedId.value), inList => {
  if (!inList) selectedId.value = null
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

// Pull the whole run's output from `{img}/logs/{fun}.log` and put it in place of what this tab has.
// The file is the complete record; the store's copy is only whatever frames this tab was awake for.
// Per ROW, not per panel: the fetch outlives the selection (a big log over a slow link), so a
// panel-wide flag spun on whichever row you switched to and disabled its button.
const syncingId = ref<string | null>(null)
const syncing = (t: TaskEntry) => syncingId.value === t.id
async function syncLogFromDisk(t: TaskEntry) {
  syncingId.value = t.id
  try {
    const lines = await fetchLogBackfill({
      projectUid: t.projectUid, imageUid: t.imageUid, funName: t.funName, startedAt: t.startedAt,
    })
    if (lines.length) tasks.setLog(t.id, lines)
  } finally { if (syncingId.value === t.id) syncingId.value = null }
}

function select(t: TaskEntry) {
  selectedId.value = t.id
  // An adopted row (rebuilt after a reconnect) only has lines from the moment this tab connected — the
  // rest is on disk. Fetched when the row is actually opened rather than on adoption, so twenty rows
  // don't fire twenty requests for output nobody looked at.
  //
  // Gated on `logSynced`, NOT on the log being empty. A backend restart adopts rows that are already
  // producing output, so live lines land within the second and the empty-log test then declined to
  // fetch — which is why opening the row after a restart showed only the last few minutes of a
  // two-hour run. A still-running row re-syncs on each open, since the file has grown since the last.
  if (t.adopted && (!t.logSynced || t.status === 'running')) void syncLogFromDisk(t)
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

// Shared with the per-module task list — one predicate, in utils/taskRerun.ts (see its header).
const canRerun = (t: TaskEntry) => canRerunTask(t)

async function copyLog() {
  if (!selected.value?.log.length) return
  await copy(selected.value.log.join('\n'))
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
        v-tooltip.bottom="'Show only tasks in this state'"
        @update:model-value="v => statusFilter = v as typeof statusFilter" />

      <CcToggle class="follow-toggle" v-model="settings.tasksThisProjectOnly" label="This project"
        v-tooltip.bottom="'Hide tasks from other projects'" />

      <CcToggle class="follow-toggle" v-model="settings.taskListAutoFollow" label="Auto-follow"
        v-tooltip.left="'Automatically select the newest running task'" />

      <button ref="throttleBtn" class="tm-throttle cc-btn cc-btn-bare cc-btn-icon"
        :class="{ 'cc-btn-on cc-btn-on-solid': throttleOpen }"
        @click="throttleOpen = !throttleOpen"
        v-tooltip.left="'Throttle — how many tasks run at once, and how wide each may go'">
        <i class="pi pi-sliders-h" />
      </button>
      <TeleportPopover v-model="throttleOpen" :anchor="throttleBtn" placement="bottom-end">
        <PoolThrottle />
      </TeleportPopover>
    </div>

    <!-- ── Body ───────────────────────────────────────────────────────── -->
    <div class="tm-body">

      <!-- Task list — the canonical table (docs/UI.md). `single`: a row IS selected here, and what it
           selects is what the log pane shows. The selected-row highlight is the table's own amber
           `--cc-selected`; this page used to hand-roll the same left-rule idiom in `--cc-accent`
           (purple = form-control chrome). See docs/todo/TASK_LIST_UNIFICATION_PLAN.md.
           `@row-click` rather than `@update:model-value` — clicking the row that is already selected
           emits no model change, and re-opening a row is what triggers its log backfill. -->
      <div class="tm-list" :style="listWidthStyle">
        <!-- drag the list/log divider (persisted). Handle on the list's RIGHT edge, hence
             `edge: 'right'` — dragging right widens the list. OUTSIDE the scrolling half, or it
             scrolls away with the rows. -->
        <div class="tm-divider" @mousedown="onListResizeStart"
          v-tooltip.right="'Drag to resize the list'" />
        <div class="tm-list-scroll">
        <SelectionTable
          class="tm-table" selection-mode="single" density="compact"
          :columns="TM_COLUMNS" :rows="rows"
          id-key="id" :model-value="selectedId ?? undefined"
          sort-storage-key="cc.tasks.sort" column-width-key="cc.tasks.colw"
          actions-width="2.9rem"
          :row-tooltip="r => r.projectLabel ? `${r.task} — from ${r.projectLabel}` : r.task"
          @row-click="r => select(r.entry)">

          <template #cell-status="{ row: r }">
            <i :class="['pi', TASK_STATUS[r.status].icon, 'row-icon']"
              :style="{ color: TASK_STATUS[r.status].color }"
              v-tooltip.right="TASK_STATUS[r.status].label" />
          </template>

          <template #cell-module="{ row: r }">
            <span class="cc-module-tag" :style="moduleTagStyle(r.module)">
              <span class="cc-module-tag-mod">{{ r.module }}</span>
            </span>
          </template>

          <template #cell-task="{ row: r }">
            <span class="row-seq cc-muted cc-fs-2xs">#{{ r.seq }}</span>
            <span v-if="r.chainLabel" class="chain-pill" v-tooltip.right="r.chainTip">
              <i class="pi pi-sitemap" />{{ r.chainLabel }}
            </span>
            {{ r.task }}
          </template>

          <template #cell-image="{ row: r }">
            <span class="tm-image">
              <span v-if="r.projectLabel" class="cc-muted"
                v-tooltip.right="'From another project'">{{ r.projectLabel }} ·</span>
              <span class="cc-uid tm-uid">{{ r.imageUid }}</span>{{ r.image }}
            </span>
          </template>

          <!-- blank unless there is a fraction to show — an empty cell says "no reading", a 0% bar
               would claim one. Same column on the per-module list. -->
          <template #cell-progress="{ row: r }">
            <CcProgressBar v-if="r.hasProgress" :value="r.progress" :aria-label="`${r.task} progress`" />
            <span v-else />
          </template>

          <template #cell-elapsed="{ row: r }">
            <span class="row-elapsed cc-muted cc-fs-2xs">{{ r.elapsed }}</span>
          </template>

          <template #actions="{ row: r }">
            <button v-if="r.status === 'running' || r.status === 'queued'"
              class="ra-btn cc-btn cc-btn-bare cc-btn-icon danger" @click="cancelTask(r.entry)"
              v-tooltip.left="r.chainLabel ? 'Stop chain run' : 'Cancel task'">
              <i class="pi pi-times" />
            </button>
            <button v-if="r.canRerun"
              class="ra-btn cc-btn cc-btn-bare cc-btn-icon" @click="rerun(r.entry)" v-tooltip.left="'Rerun'">
              <i class="pi pi-replay" />
            </button>
            <button v-if="r.status === 'done' || r.status === 'failed' || r.status === 'cancelled'"
              class="ra-btn cc-btn cc-btn-bare cc-btn-icon" @click="tasks.remove(r.id)" v-tooltip.left="'Dismiss'">
              <i class="pi pi-trash" />
            </button>
          </template>

          <template #empty>No tasks.</template>
        </SelectionTable>
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
              <button class="ra-btn cc-btn cc-btn-bare cc-btn-icon" @click="copyLog" v-tooltip.left="copied() ? 'Copied!' : 'Copy log'">
                <i :class="['pi', copied() ? 'pi-check' : 'pi-copy']" />
              </button>
              <!-- The tab's copy of a log has holes wherever the socket was down; the file on disk does
                   not. Offered for any row with a start (an adopted one syncs on open anyway) so a run
                   this tab launched and then lost the backend under can still be read in full. -->
              <button v-if="selected.startedAt" class="ra-btn cc-btn cc-btn-bare cc-btn-icon"
                :disabled="syncing(selected)" @click="syncLogFromDisk(selected)"
                v-tooltip.left="'Reload log from disk'">
                <i :class="['pi', syncing(selected) ? 'pi-spin pi-spinner' : 'pi-refresh']" />
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

          <CcProgressBar v-if="selected.status === 'running' && selected.progress !== undefined"
            class="log-progress" :value="selected.progress"
            :aria-label="`${selected.label} progress`" />

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
/* The pane; the rows are SelectionTable's, at `compact` density. Width comes from `usePanelResize`
   (draggable + persisted) rather than a constant — 340px could not hold six columns and any one
   number is wrong for someone. `overflow: auto` is a backstop for a user who drags the COLUMNS wider
   than the pane; the fill layout means it is otherwise dormant. */
.tm-list {
  flex-shrink: 0;
  border-right: 1px solid var(--cc-border);
  position: relative;
  overflow: hidden;              /* the scrolling is the inner half's, so the divider can't scroll away */
}
.tm-list-scroll { height: 100%; overflow: auto; }
/* the divider: a grab strip on the pane's right edge, over the border it sits on */
.tm-divider {
  position: absolute; top: 0; right: 0; bottom: 0;
  width: 5px;
  cursor: col-resize;
  z-index: 4;                      /* above the table's sticky header */
}
.tm-divider:hover { background: var(--cc-accent); opacity: 0.35; }

.row-icon { font-size: var(--cc-fs-md); flex-shrink: 0; }
/* status icon colour is inline from TASK_STATUS (lib/taskStatus.ts) */

.row-seq { font-family: var(--cc-mono); margin-right: 0.2rem; }
.chain-pill {
  display: inline-flex;
  align-items: center;
  gap: 0.2rem;
  font-size: var(--cc-fs-3xs);
  font-weight: 700;
  padding: 0.05rem 0.3rem;
  border-radius: var(--cc-radius-xs);
  /* purple is right here — this is a BADGE (form/control chrome), not a row selection */
  background: color-mix(in srgb, var(--cc-accent) 13%, transparent);
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

/* the image cell: uid chip then name, the name taking the leftover */
.tm-image { display: flex; align-items: center; gap: 0.25rem; min-width: 0; }
/* + .cc-uid — this site's own half is only the chip (see TaskList's twin) */
.tm-uid {
  flex-shrink: 0;
  background: var(--cc-surface-2);
  padding: 0 0.2rem;
  border-radius: var(--cc-radius-xs);
}

/* Row actions were hover-revealed here (`opacity: 0`) and always visible in the per-module list — a
   difference nobody chose. `#actions` has no hover-reveal, so adopting the table settles it toward
   always visible on both (docs/todo/TASK_LIST_UNIFICATION_PLAN.md → Decision 8). */
/* `actions-width` is sized to what can appear AT ONCE, not to the number of buttons declared: cancel
   shows only while running/queued and rerun/dismiss only once terminal, so the most a row ever shows
   is two. Reserving for three left a running row's lone ✕ floating at the far edge of an empty column
   (Dominik, 2026-08-15). */
.tm-table :deep(.sel-actions) .cc-btn { padding: 0.15rem 0.25rem; }
.tm-table :deep(.sel-actions) > * + * { margin-left: 0.15rem; }

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

/* geometry only — the bar itself is CcProgressBar */
.log-progress { flex-shrink: 0; }

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
