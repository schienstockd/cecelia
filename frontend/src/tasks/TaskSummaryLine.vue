<!--
  One line under a panel's action button (Run / Render / Generate) standing in for the module's task
  list, which the two-half panels (`utils/paneExpand.ts`) never show beside the config: what is running,
  or how the last run went, and a "Tasks" link that switches the panel to the list. Renders nothing for
  a module that has never run anything.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useTaskStore } from '../stores/tasks'
import { useProjectMetaStore } from '../stores/projectMeta'
import { lastRunSummary } from '../utils/taskRows'
import { formatAgo } from '../utils/formatAgo'
import { TASK_STATUS } from '../lib/taskStatus'

const props = defineProps<{ module: string }>()
const emit = defineEmits<{ show: [] }>()

const taskStore = useTaskStore()
const projectMeta = useProjectMetaStore()
const lastRun = computed(() => lastRunSummary(taskStore.forModule(props.module, projectMeta.current?.uid ?? '')))
const lastRunAgo = computed(() => lastRun.value.lastFinished ? formatAgo(lastRun.value.lastFinished.toISOString()) : '')
</script>

<template>
  <div v-if="lastRun.live || lastRun.lastFinished" class="last-run cc-muted cc-fs-xs">
    <span v-if="lastRun.live"><i :class="['pi', TASK_STATUS.running.icon]" /> {{ lastRun.live }} running or queued</span>
    <span v-else>Last run{{ lastRunAgo ? ` ${lastRunAgo} ago` : '' }}
      <template v-if="lastRun.done"> · {{ lastRun.done }} done</template>
      <template v-if="lastRun.failed"> · <span class="cc-muted-error">{{ lastRun.failed }} failed</span></template>
    </span>
    <button class="cc-btn cc-btn-bare cc-btn-dense cc-fs-xs last-run-tasks" data-guide="task.lastRun"
            v-tooltip.left="'Show this page’s tasks — watch, cancel, read logs'" @click="emit('show')">Tasks</button>
  </div>
</template>

<style scoped>
.last-run { display: flex; align-items: center; gap: 0.4rem; margin-top: 0.5rem; }
.last-run-tasks { margin-left: auto; color: var(--cc-accent-soft); }
</style>
