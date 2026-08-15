<!--
  Shows tasks for a given module, with live status, log expand, cancel, dismiss.

  The HEADING and its two list-wide actions (cancel everything in flight, clear everything finished)
  live here rather than in the host. They were hand-rolled inside `TaskRunner`, so `BatchMoviesPanel`
  — the other place that embeds this list — had a list you could neither tidy nor cancel wholesale
  (Dominik, 2026-08-10). A list-wide action belongs to the list, not to whoever placed it.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { useTaskStore, type TaskEntry, type TaskStatus } from '../stores/tasks'
import { TASK_STATUS } from '../lib/taskStatus'
import { useCopyFlash } from '../composables/useCopyFlash'
import { useWsStore } from '../stores/ws'
import { useProjectMetaStore } from '../stores/projectMeta'
import { fetchLogBackfill } from '../utils/taskLogBackfill'
import { useNowTick } from '../composables/useNowTick'
import { taskElapsed } from '../utils/taskElapsed'
import { canRerunTask } from '../utils/taskRerun'
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

// shared formatter + shared 1s clock, so a running task's elapsed advances without a frame arriving
const now = useNowTick()
const elapsed = (t: TaskEntry) => taskElapsed(t.startedAt, t.finishedAt, now.value)
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
    <div v-if="items.length === 0" class="task-empty cc-muted">
      No tasks yet — select images and click Run.
    </div>

    <div
      v-for="t in items"
      :key="t.id"
      class="task-item"
      :class="'st-' + t.status"
    >
      <div class="task-header">
        <i :class="['pi', TASK_STATUS[t.status].icon, 'task-icon']"
          :style="{ color: TASK_STATUS[t.status].color }"
          v-tooltip.left="TIP[t.status]" />

        <div class="task-info">
          <!-- the full-label tip (for a truncated label) sits on the TEXT, not the row: the row also
               holds the jump button and the chain badge, whose own tips it fired over -->
          <span class="task-label">
            <button class="jump-btn cc-btn cc-btn-bare cc-btn-icon cc-btn-lg" @click.stop="jumpToTask(t)" v-tooltip.right="'Open in task manager'">
              <i class="pi pi-arrow-left" />
            </button>
            <span class="task-seq cc-muted cc-fs-2xs">#{{ t.seq }}</span>
            <i v-if="t.chainRunId" class="pi pi-sitemap chain-badge"
               v-tooltip.right="`Chain: ${t.chainName ?? t.chainRunId} / ${t.chainRunId}`" />
            <span v-tooltip.right="t.label">{{ t.label }}</span>
          </span>
          <span class="task-image cc-muted cc-fs-xs" v-tooltip.right="`UID: ${t.imageUid}`">
            <span class="task-uid">{{ t.imageUid }}</span>
            {{ t.imageName }}
          </span>
        </div>

        <span v-if="elapsed(t)" class="task-elapsed cc-muted cc-fs-xs"
          v-tooltip.left="t.startedAt ? `Started ${t.startedAt.toLocaleTimeString()}` : ''">
          {{ elapsed(t) }}
        </span>

        <div class="task-actions">
          <button
            v-if="t.log.length || t.adopted"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon"
            @click="toggleLog(t)"
            v-tooltip.left="expanded.has(t.id) ? 'Hide log' : 'Show task log'"
          >
            <i :class="['pi', expanded.has(t.id) ? 'pi-chevron-up' : 'pi-chevron-down']" />
          </button>

          <button
            v-if="t.status === 'running' || t.status === 'queued'"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon danger"
            @click="cancelTask(t)"
            v-tooltip.left="t.chainRunId ? 'Stop chain run' : 'Cancel this task'"
          >
            <i class="pi pi-times" />
          </button>

          <button
            v-if="canRerunTask(t)"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon"
            @click="rerun(t)"
            v-tooltip.left="'Rerun this task with the same parameters'"
          >
            <i class="pi pi-replay" />
          </button>

          <button
            v-if="t.log.length"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon"
            @click="copyLog(t)"
            v-tooltip.left="isCopied(t.id) ? 'Copied!' : 'Copy log to clipboard'"
          >
            <i :class="['pi', isCopied(t.id) ? 'pi-check' : 'pi-copy']" />
          </button>

          <button
            v-if="t.status === 'done' || t.status === 'failed' || t.status === 'cancelled'"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon"
            @click="tasks.remove(t.id)"
            v-tooltip.left="'Dismiss this task from the list'"
          >
            <i class="pi pi-trash" />
          </button>
        </div>
      </div>

      <!-- progress bar — flush inside the card, hence `thin` (the default) -->
      <CcProgressBar v-if="t.status === 'running' && t.progress !== undefined"
        :value="t.progress" :aria-label="`${t.label} progress`" />

      <!-- log -->
      <pre v-if="expanded.has(t.id) && t.log.length" class="task-log">{{ t.log.join('\n') }}</pre>
      <div v-else-if="expanded.has(t.id)" class="task-log">No log output yet.</div>
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

.task-empty { padding: 1.5rem 0.5rem; }

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

.task-item {
  border-radius: var(--cc-radius-md);
  border: 1px solid var(--cc-border);
  overflow: hidden;
  background: var(--cc-surface-1);
}
.task-item.st-running   { border-color: #1e3a5f; background: #1e3a5f18; }
.task-item.st-failed    { border-color: #7f1d1d; background: #7f1d1d18; }
.task-item.st-done      { border-color: #14532d55; }
.task-item.st-cancelled { border-color: #3f3f4666; opacity: 0.6; }

.task-header {
  display: flex;
  align-items: center;
  gap: 0.4rem;
  padding: 0.4rem 0.6rem;
}

.task-icon { font-size: var(--cc-fs-md); flex-shrink: 0; }
/* status icon colour is inline from TASK_STATUS (lib/taskStatus.ts) */

.task-info { display: flex; flex-direction: column; flex: 1; min-width: 0; }
.task-label {
  font-size: var(--cc-fs-sm);
  font-weight: 600;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  display: flex;
  align-items: center;
  gap: 0.3rem;
}
.task-seq { font-family: var(--cc-mono); flex-shrink: 0; }
.chain-badge {
  font-size: var(--cc-fs-2xs);
  color: var(--cc-accent);
  flex-shrink: 0;
}
.task-image { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; display: flex; align-items: center; gap: 0.3rem; }
.task-uid {
  font-family: var(--cc-mono);
  font-size: var(--cc-fs-2xs);
  color: var(--cc-text-dim);
  opacity: 0.6;
  flex-shrink: 0;
  background: var(--cc-surface-2);
  padding: 0 0.25rem;
  border-radius: var(--cc-radius-xs);
}

.task-elapsed { font-family: var(--cc-mono); flex-shrink: 0; }

.task-actions { display: flex; gap: 0.15rem; flex-shrink: 0; }

/* .icon-btn → cc-btn cc-btn-bare cc-btn-icon */
.icon-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.icon-btn.danger:hover { background: #7f1d1d55; color: #fca5a5; }

.jump-btn { color: #4ade80; -webkit-text-stroke: 0.4px #4ade80; }   /* + cc-btn cc-btn-bare cc-btn-icon cc-btn-lg */
.task-item:hover .jump-btn { display: inline-flex; }

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
