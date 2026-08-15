<!--
  Shows tasks for a given module, with live status, log expand, cancel, dismiss.

  The HEADING and its two list-wide actions (cancel everything in flight, clear everything finished)
  live here rather than in the host. They were hand-rolled inside `TaskRunner`, so `BatchMoviesPanel`
  — the other place that embeds this list — had a list you could neither tidy nor cancel wholesale
  (Dominik, 2026-08-10). A list-wide action belongs to the list, not to whoever placed it.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import SelectionTable, { type SelectionColumn } from '../components/SelectionTable.vue'
import { taskRows } from '../utils/taskRows'
import { useRouter } from 'vue-router'
import { useTaskStore, type TaskEntry, type TaskStatus } from '../stores/tasks'
import { TASK_STATUS } from '../lib/taskStatus'
import { useCopyFlash } from '../composables/useCopyFlash'
import { useWsStore } from '../stores/ws'
import { useProjectMetaStore } from '../stores/projectMeta'
import { fetchLogBackfill } from '../utils/taskLogBackfill'
import { useNowTick } from '../composables/useNowTick'
// `taskElapsed` / `canRerunTask` are still the canonical helpers — they are just called by the row
// mapper now (utils/taskRows.ts) rather than here, so both lists get the same answer.
import CcProgressBar from '../components/CcProgressBar.vue'

const props = withDefaults(defineProps<{
  module: string
  /** Hide the heading row — for a host that already titles the list itself. */
  bare?: boolean
}>(), { bare: false })
const tasks       = useTaskStore()
const ws          = useWsStore()
const router      = useRouter()
const projectMeta = useProjectMetaStore()

const expanded  = ref<Set<string>>(new Set())
// shared copy+flash helper, keyed per task row (docs/UI.md → UX-primitive catalog)
const { isCopied, copy } = useCopyFlash()

// Scoped to the current project — otherwise switching projects leaves the previous project's
// (e.g. cancelled) tasks visible in this module's list.
const items = computed(() => tasks.forModule(props.module, projectMeta.current?.uid))

// ── list-wide actions ────────────────────────────────────────────────────────
const activeTasks = computed(() =>
  items.value.filter(t => t.status === 'running' || t.status === 'queued'))

// Cancel everything in flight. A CHAIN run is cancelled once for the whole run, not once per node —
// otherwise a chain of ten sends ten cancels for one thing.
function cancelAll() {
  const cancelledChainRuns = new Set<string>()
  for (const t of activeTasks.value) {
    if (t.chainRunId) {
      if (cancelledChainRuns.has(t.chainRunId)) continue
      cancelledChainRuns.add(t.chainRunId)
      tasks.cancelChainRun(t.chainRunId)
      ws.send({ type: 'chain:cancel', runId: t.chainRunId })
    } else {
      tasks.cancel(t.id)
      ws.send({ type: 'task:cancel', taskId: t.id })
    }
  }
}
const clearFinished = () => tasks.clearFinished(props.module, projectMeta.current?.uid)


async function toggleLog(t: TaskEntry) {
  const id = t.id
  if (expanded.value.has(id)) expanded.value.delete(id)
  else expanded.value.add(id)
  expanded.value = new Set(expanded.value)
  // An adopted row only has what arrived since this tab connected; the rest is on disk. Fetched on the
  // first open, not on adoption — twenty rows must not fire twenty requests for output nobody opened.
  if (t.adopted && !t.log.length && expanded.value.has(id)) {
    const lines = await fetchLogBackfill({
      projectUid: t.projectUid, imageUid: t.imageUid, funName: t.funName, startedAt: t.startedAt,
    })
    if (lines.length) tasks.setLog(id, lines)
  }
}

// icon + colour from the canonical map (lib/taskStatus.ts); tooltips stay local (more descriptive here)
const TIP: Record<TaskStatus, string> = {
  queued:    'Waiting to run.',
  running:   'Task is running.',
  done:      'Completed successfully.',
  failed:    'Task failed. Expand to see log.',
  cancelled: 'Cancelled.',
}

function rerun(t: TaskEntry) {
  tasks.restart(t.id)
  ws.send({
    type:       'task:restart',
    taskId:     t.id,
    funName:    t.funName,
    params:     t.params,
    imageUid:   t.imageUid,
    projectUid: t.projectUid,
  })
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

function jumpToTask(t: TaskEntry) {
  tasks.jumpToId = t.id
  router.push('/tasks')
}

async function copyLog(t: TaskEntry) {
  if (!t.log.length) return
  await copy(t.log.join('\n'), t.id)     // keyed flash — one row at a time
}

// shared 1s clock, so a running task's elapsed advances without a frame arriving. Declared ABOVE
// `rows`, which reads it — see utils/setupOrder.ts for why order matters.
const now = useNowTick()

// The table reads row FIELDS (and sorts by them), so entries are flattened by the shared mapper — the
// same one the /tasks manager uses, which is what keeps the two lists from drifting again. The list is
// already scoped to one project by `forModule`, so `thisProjectOnly` is true and no row is labelled.
const rows = computed(() => taskRows(items.value, {
  currentProjectUid: projectMeta.current?.uid,
  thisProjectOnly:   true,
  nameOfProject:     () => undefined,
  now:               now.value,
}))

// THREE columns, not six. `headerless` means no sort and no column resize (both live in a header
// cell), so widths here are not starting hints for anything to drag — the table is auto-layout and
// `task` simply takes what the other two leave. Image and progress are inside the task cell (the
// original two-row entry); at 280px, columns of their own were squeezed to nothing.
const TL_COLUMNS: SelectionColumn[] = [
  { key: 'status',  label: '', fixed: true, width: 22 },
  { key: 'task',    label: '' },
  { key: 'elapsed', label: '', fixed: true, width: 40 },
]
</script>

<template>
  <div class="task-list">
    <div v-if="!bare" class="tasks-heading">
      <h3 class="section-heading cc-eyebrow cc-fs-2xs">Tasks</h3>
      <div class="tasks-heading-actions">
        <button v-if="activeTasks.length" class="clear-btn cc-btn cc-btn-bare cc-btn-icon danger"
          @click="cancelAll"
          v-tooltip.left="`Cancel all ${activeTasks.length} running/queued task(s) in this module`">
          <i class="pi pi-times-circle" />
        </button>
        <button class="clear-btn cc-btn cc-btn-bare cc-btn-icon" @click="clearFinished"
          v-tooltip.left="'Remove all completed and failed tasks from the list'">
          <i class="pi pi-filter-slash" />
        </button>
      </div>
    </div>
    <!-- The canonical table (docs/UI.md), same as the /tasks manager — `none` because a row here is
         not "selected": the buttons act, and the log opens in place. The panel is narrow, so
         `fit="content"` + the wrapper's `overflow-x` lets the columns outgrow it and
         `column-width-key` makes them draggable and persistent.
         `#row-detail` carries ONLY the expanded log, so `is-expanded` means what it says; the
         running-task bar is its own column (docs/todo/TASK_LIST_UNIFICATION_PLAN.md → Decision 7b). -->
    <div class="tl-scroll">
      <SelectionTable
        class="tl-table" selection-mode="none" density="compact" headerless
        :columns="TL_COLUMNS" :rows="rows" id-key="id"
        actions-width="5.5rem"
        :row-tooltip="r => r.task"
        :row-class="r => `tone-${TASK_STATUS[r.status].tone} st-${r.status}`"
        :is-expanded="r => expanded.has(r.id)">

        <template #cell-status="{ row: r }">
          <i :class="['pi', TASK_STATUS[r.status].icon, 'task-icon']"
            :style="{ color: TASK_STATUS[r.status].color }"
            v-tooltip.left="TIP[r.status]" />
        </template>

        <!-- The original TWO-ROW entry: label on top, image beneath, the running bar under both. One
             cell rather than three columns — at 280px an Image column is squeezed to nothing, and the
             uid + name were never a thing you sort by here (Dominik, 2026-08-15). -->
        <template #cell-task="{ row: r }">
          <div class="tl-entry">
            <span class="tl-title">
              <button class="jump-btn cc-btn cc-btn-bare cc-btn-icon" @click.stop="jumpToTask(r.entry)"
                v-tooltip.right="'Open in task manager'">
                <i class="pi pi-arrow-left" />
              </button>
              <span class="task-seq cc-muted cc-fs-2xs">#{{ r.seq }}</span>
              <i v-if="r.chainLabel" class="pi pi-sitemap chain-badge" v-tooltip.right="r.chainTip" />
              <span class="tl-label">{{ r.task }}</span>
            </span>
            <span class="tl-sub cc-muted cc-fs-2xs" v-tooltip.right="`UID: ${r.imageUid}`">
              <span class="cc-uid task-uid">{{ r.imageUid }}</span>{{ r.image }}
            </span>
            <CcProgressBar v-if="r.hasProgress" :value="r.progress"
              :aria-label="`${r.task} progress`" />
          </div>
        </template>

        <template #cell-elapsed="{ row: r }">
          <span class="task-elapsed cc-muted cc-fs-2xs"
            v-tooltip.left="r.entry.startedAt ? `Started ${r.entry.startedAt.toLocaleTimeString()}` : ''">
            {{ r.elapsed }}
          </span>
        </template>

        <template #actions="{ row: r }">
          <button v-if="r.entry.log.length || r.entry.adopted"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon" @click="toggleLog(r.entry)"
            v-tooltip.left="expanded.has(r.id) ? 'Hide log' : 'Show task log'">
            <i :class="['pi', expanded.has(r.id) ? 'pi-chevron-up' : 'pi-chevron-down']" />
          </button>

          <button v-if="r.status === 'running' || r.status === 'queued'"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon danger" @click="cancelTask(r.entry)"
            v-tooltip.left="r.chainLabel ? 'Stop chain run' : 'Cancel this task'">
            <i class="pi pi-times" />
          </button>

          <button v-if="r.canRerun"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon" @click="rerun(r.entry)"
            v-tooltip.left="'Rerun this task with the same parameters'">
            <i class="pi pi-replay" />
          </button>

          <button v-if="r.entry.log.length"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon" @click="copyLog(r.entry)"
            v-tooltip.left="isCopied(r.id) ? 'Copied!' : 'Copy log to clipboard'">
            <i :class="['pi', isCopied(r.id) ? 'pi-check' : 'pi-copy']" />
          </button>

          <button v-if="r.status === 'done' || r.status === 'failed' || r.status === 'cancelled'"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon" @click="tasks.remove(r.id)"
            v-tooltip.left="'Dismiss this task from the list'">
            <i class="pi pi-trash" />
          </button>
        </template>

        <template #row-detail="{ row: r }">
          <pre class="task-log">{{ r.entry.log.join('\n') || 'No log output yet.' }}</pre>
        </template>

        <template #empty>No tasks yet — select images and click Run.</template>
      </SelectionTable>
    </div>
  </div>
</template>

<style scoped>
.task-list {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  /* Shrink to the host rather than forcing it wide. A flex item's default `min-width: auto` is the
     min-content width of its children, and a task row's is real (a status icon, five row buttons, an
     image name) — so in a narrow side panel this list pushed its host wider and the horizontal
     scrollbar landed on the WHOLE panel. Hosts still decide where the overflow goes: TaskRunner
     clips + scrolls it in `.tasks-scroll`, BatchMoviesPanel in `.bm-tasks`. */
  min-width: 0;
}

/* The table fills this panel (`fit` default), so it does not normally overflow; this is the backstop
   for a user who drags the columns wider, and it keeps that overflow HERE rather than on the host —
   the same containment the card stack needed (see `.task-list`). */
.tl-scroll { min-width: 0; overflow-x: auto; }

/* the heading row — sticky so the two list-wide actions stay reachable in a long list */
.tasks-heading {
  display: flex; align-items: center; justify-content: space-between;
  margin-bottom: 0.5rem; flex-shrink: 0;
}
.tasks-heading .section-heading { margin-bottom: 0; }
.tasks-heading-actions { display: flex; gap: 0.15rem; }
/* .clear-btn → cc-btn cc-btn-bare cc-btn-icon */
.clear-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.clear-btn.danger:hover { background: #7f1d1d55; color: #fca5a5; }

/* Per-status row tint, via SelectionTable's `rowClass` (the ImageTable `.row-excluded` precedent).
   Keyed off `TASK_STATUS[...].tone`, which exists for exactly this — a component tinting its own
   chrome consistently with the icon — so the tints come from the same tokens as the status lights
   instead of the four raw hexes the card stack carried. `done` had only a border colour, which a
   table row has nothing to do with; the status icon already says it. */
.tl-table :deep(tr.tone-active) { background: color-mix(in srgb, var(--cc-active) 10%, transparent); }
.tl-table :deep(tr.tone-fail)   { background: color-mix(in srgb, var(--cc-sev-fail) 10%, transparent); }
.tl-table :deep(tr.st-cancelled) { opacity: 0.6; }
/* the row buttons are dense here — the panel is 280px by default */
.tl-table :deep(.sel-actions) .cc-btn { padding: 0.2rem 0.25rem; }

.task-icon { font-size: var(--cc-fs-md); flex-shrink: 0; }
/* status icon colour is inline from TASK_STATUS (lib/taskStatus.ts) */

/* ── The two-row entry ────────────────────────────────────────────────────────
   Title line, image line, and the running bar under both — the card stack's anatomy, in one cell.
   Each line clips itself, so the surrounding `td`'s `nowrap` never has to hold them apart. */
.tl-entry { display: flex; flex-direction: column; gap: 0.05rem; min-width: 0; padding: 0.1rem 0; }
.tl-title { display: flex; align-items: center; gap: 0.25rem; min-width: 0; }
.tl-label {
  font-weight: 600;
  color: var(--cc-text);
  overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
}
/* + .cc-muted .cc-fs-2xs — the size is the ladder's, not a scoped `font-size` that would shadow the
   utility it sits on (the cssScenarios ratchet flags exactly that, and did) */
.tl-sub {
  display: flex; align-items: center; gap: 0.25rem; min-width: 0;
  overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
}
.task-seq { font-family: var(--cc-mono); flex-shrink: 0; }
.chain-badge {
  font-size: var(--cc-fs-2xs);
  color: var(--cc-accent);
  flex-shrink: 0;
}
/* + .cc-uid (mono/tracking/dim/clip). This site's own half: the chip, because here the uid leads the
   line and needs separating from the name that follows it. */
.task-uid {
  flex-shrink: 0;
  background: var(--cc-surface-2);
  padding: 0 0.2rem;
  border-radius: var(--cc-radius-xs);
}

.task-elapsed { font-family: var(--cc-mono); flex-shrink: 0; }

/* .icon-btn → cc-btn cc-btn-bare cc-btn-icon */
.icon-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.icon-btn.danger:hover { background: #7f1d1d55; color: #fca5a5; }

/* + cc-btn cc-btn-bare cc-btn-icon. The `.task-item:hover` rule that sat here was dead — it set
   `display: inline-flex` with no `display: none` base to reverse. */
.jump-btn { color: #4ade80; -webkit-text-stroke: 0.4px #4ade80; }

.task-log {
  font-family: var(--cc-mono);
  font-size: var(--cc-fs-xs);
  color: var(--cc-text-dim);
  background: var(--cc-console-bg);
  padding: 0.5rem 0.6rem;
  margin: 0;
  max-height: 160px;
  overflow-y: auto;
  white-space: pre-wrap;
  word-break: break-all;
  border-top: 1px solid var(--cc-border);
}

</style>
