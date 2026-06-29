<!--
  Shows tasks for a given module, with live status, log expand, cancel, dismiss.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { useTaskStore, type TaskEntry, type TaskStatus } from '../stores/tasks'
import { useWsStore } from '../stores/ws'

const props  = defineProps<{ module: string }>()
const tasks  = useTaskStore()
const ws     = useWsStore()
const router = useRouter()

const expanded  = ref<Set<string>>(new Set())
const copied    = ref<Set<string>>(new Set())

const items = computed(() => tasks.forModule(props.module))


function toggleLog(id: string) {
  if (expanded.value.has(id)) expanded.value.delete(id)
  else expanded.value.add(id)
  expanded.value = new Set(expanded.value)
}

const cfg: Record<TaskStatus, { cls: string; icon: string; tip: string }> = {
  queued:    { cls: 'st-queued',    icon: 'pi-clock',         tip: 'Waiting to run.' },
  running:   { cls: 'st-running',   icon: 'pi-spin pi-cog',   tip: 'Task is running.' },
  done:      { cls: 'st-done',      icon: 'pi-check-circle',  tip: 'Completed successfully.' },
  failed:    { cls: 'st-failed',    icon: 'pi-times-circle',  tip: 'Task failed. Expand to see log.' },
  cancelled: { cls: 'st-cancelled', icon: 'pi-ban',           tip: 'Cancelled.' },
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
  await navigator.clipboard.writeText(t.log.join('\n'))
  copied.value = new Set([...copied.value, t.id])
  setTimeout(() => {
    copied.value = new Set([...copied.value].filter(id => id !== t.id))
  }, 1500)
}

function elapsed(t: TaskEntry) {
  if (!t.startedAt) return null
  const end = t.finishedAt ?? new Date()
  const s = Math.round((end.getTime() - t.startedAt.getTime()) / 1000)
  return s < 60 ? `${s}s` : `${Math.floor(s / 60)}m ${s % 60}s`
}
</script>

<template>
  <div class="task-list">
    <div v-if="items.length === 0" class="task-empty">
      No tasks yet — select images and click Run.
    </div>

    <div
      v-for="t in items"
      :key="t.id"
      class="task-item"
      :class="cfg[t.status].cls"
    >
      <div class="task-header">
        <i :class="['pi', cfg[t.status].icon, 'task-icon']"
          v-tooltip.left="cfg[t.status].tip" />

        <div class="task-info">
          <span class="task-label" v-tooltip.right="t.label">
            <button class="jump-btn" @click.stop="jumpToTask(t)" v-tooltip.right="'Open in task manager'">
              <i class="pi pi-arrow-left" />
            </button>
            <span class="task-seq">#{{ t.seq }}</span>
            <i v-if="t.chainRunId" class="pi pi-sitemap chain-badge"
               v-tooltip.right="`Chain: ${t.chainName ?? t.chainRunId} / ${t.chainRunId}`" />
            {{ t.label }}
          </span>
          <span class="task-image" v-tooltip.right="`UID: ${t.imageUid}`">
            <span class="task-uid">{{ t.imageUid }}</span>
            {{ t.imageName }}
          </span>
        </div>

        <span v-if="elapsed(t)" class="task-elapsed"
          v-tooltip.left="t.startedAt ? `Started ${t.startedAt.toLocaleTimeString()}` : ''">
          {{ elapsed(t) }}
        </span>

        <div class="task-actions">
          <button
            v-if="t.log.length"
            class="icon-btn"
            @click="toggleLog(t.id)"
            v-tooltip.left="expanded.has(t.id) ? 'Hide log' : 'Show task log'"
          >
            <i :class="['pi', expanded.has(t.id) ? 'pi-chevron-up' : 'pi-chevron-down']" />
          </button>

          <button
            v-if="t.status === 'running' || t.status === 'queued'"
            class="icon-btn danger"
            @click="cancelTask(t)"
            v-tooltip.left="t.chainRunId ? 'Stop chain run' : 'Cancel this task'"
          >
            <i class="pi pi-times" />
          </button>

          <button
            v-if="!t.chainRunId && (t.status === 'done' || t.status === 'failed' || t.status === 'cancelled')"
            class="icon-btn"
            @click="rerun(t)"
            v-tooltip.left="'Rerun this task with the same parameters.'"
          >
            <i class="pi pi-replay" />
          </button>

          <button
            v-if="t.log.length"
            class="icon-btn"
            @click="copyLog(t)"
            v-tooltip.left="copied.has(t.id) ? 'Copied!' : 'Copy log to clipboard'"
          >
            <i :class="['pi', copied.has(t.id) ? 'pi-check' : 'pi-copy']" />
          </button>

          <button
            v-if="t.status === 'done' || t.status === 'failed' || t.status === 'cancelled'"
            class="icon-btn"
            @click="tasks.remove(t.id)"
            v-tooltip.left="'Dismiss this task from the list.'"
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
      <div v-else-if="expanded.has(t.id)" class="task-log dim">No log output yet.</div>
    </div>
  </div>
</template>

<style scoped>
.task-list {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
}

.task-empty {
  font-size: 0.75rem;
  color: var(--cc-text-dim);
  text-align: center;
  padding: 1.5rem 0.5rem;
}

.task-item {
  border-radius: 0.4rem;
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

.task-icon { font-size: 0.85rem; flex-shrink: 0; }
.st-running .task-icon { color: #93c5fd; }
.st-done    .task-icon { color: #86efac; }
.st-failed  .task-icon { color: #fca5a5; }
.st-queued  .task-icon { color: #71717a; }
.st-cancelled .task-icon { color: #52525b; }

.task-info { display: flex; flex-direction: column; flex: 1; min-width: 0; }
.task-label {
  font-size: 0.78rem;
  font-weight: 600;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  display: flex;
  align-items: center;
  gap: 0.3rem;
}
.task-seq {
  font-size: 0.65rem;
  font-family: var(--cc-mono);
  font-weight: 700;
  color: var(--cc-text-dim);
  flex-shrink: 0;
}
.chain-badge {
  font-size: 0.65rem;
  color: #a78bfa;
  flex-shrink: 0;
}
.task-image {
  font-size: 0.7rem;
  color: var(--cc-text-dim);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  display: flex;
  align-items: center;
  gap: 0.3rem;
}
.task-uid {
  font-family: var(--cc-mono);
  font-size: 0.65rem;
  color: var(--cc-text-dim);
  opacity: 0.6;
  flex-shrink: 0;
  background: var(--cc-surface-2);
  padding: 0 0.25rem;
  border-radius: 0.2rem;
}

.task-elapsed {
  font-size: 0.68rem;
  font-family: var(--cc-mono);
  color: var(--cc-text-dim);
  flex-shrink: 0;
}

.task-actions { display: flex; gap: 0.15rem; flex-shrink: 0; }

.icon-btn {
  background: none; border: none; cursor: pointer;
  color: var(--cc-text-dim); font-size: 0.72rem;
  padding: 0.2rem 0.3rem; border-radius: 0.2rem;
}
.icon-btn:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.icon-btn.danger:hover { background: #7f1d1d55; color: #fca5a5; }

.jump-btn {
  display: none;
  background: none;
  border: none;
  cursor: pointer;
  color: #4ade80;
  font-size: 0.8rem;
  padding: 0 0.15rem 0 0;
  flex-shrink: 0;
  line-height: 1;
  -webkit-text-stroke: 0.4px #4ade80;
}
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
  font-size: 0.68rem;
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
.dim { color: var(--cc-text-dim); font-size: 0.72rem; }
</style>
