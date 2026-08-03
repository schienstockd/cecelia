<!--
  Shows tasks for a given module, with live status, log expand, cancel, dismiss.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { useTaskStore, type TaskEntry, type TaskStatus } from '../stores/tasks'
import { TASK_STATUS } from '../lib/taskStatus'
import { useCopyFlash } from '../composables/useCopyFlash'
import { useWsStore } from '../stores/ws'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useNowTick } from '../composables/useNowTick'
import { taskElapsed } from '../utils/taskElapsed'

const props       = defineProps<{ module: string }>()
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


function toggleLog(id: string) {
  if (expanded.value.has(id)) expanded.value.delete(id)
  else expanded.value.add(id)
  expanded.value = new Set(expanded.value)
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
          <span class="task-label" v-tooltip.right="t.label">
            <button class="jump-btn cc-btn cc-btn-bare cc-btn-icon cc-btn-lg" @click.stop="jumpToTask(t)" v-tooltip.right="'Open in task manager'">
              <i class="pi pi-arrow-left" />
            </button>
            <span class="task-seq cc-muted cc-fs-2xs">#{{ t.seq }}</span>
            <i v-if="t.chainRunId" class="pi pi-sitemap chain-badge"
               v-tooltip.right="`Chain: ${t.chainName ?? t.chainRunId} / ${t.chainRunId}`" />
            {{ t.label }}
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
            v-if="t.log.length"
            class="icon-btn cc-btn cc-btn-bare cc-btn-icon"
            @click="toggleLog(t.id)"
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
            v-if="!t.chainRunId && (t.status === 'done' || t.status === 'failed' || t.status === 'cancelled')"
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

      <!-- progress bar -->
      <div v-if="t.status === 'running' && t.progress !== undefined" class="task-progress">
        <div class="task-progress-fill" :style="{ width: `${(t.progress * 100).toFixed(1)}%` }" />
      </div>

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
}

.task-empty { padding: 1.5rem 0.5rem; }

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

.task-progress {
  height: 3px;
  background: var(--cc-surface-2);
  overflow: hidden;
}
.task-progress-fill {
  height: 100%;
  background: var(--cc-accent);
  transition: width 0.25s ease;
  min-width: 2px;
}

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
