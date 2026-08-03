<script setup lang="ts">
import { computed } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { TaskStatus } from '../stores/tasks'
import { TASK_STATUS } from '../lib/taskStatus'
import { useNowTick } from '../composables/useNowTick'
import { taskElapsed } from '../utils/taskElapsed'

const props = defineProps<{
  id: string
  data: {
    fn: string
    label?: string
    variant?: string      // distinguishing value_name (e.g. fan-out output "T" vs "default")
    imageUid: string
    status: TaskStatus
    startedAt?: number    // epoch ms
    finishedAt?: number   // epoch ms
    nodeId?: string       // chain template node id (for "resume from here")
    restart?: 'start' | 'rerun'   // 'start' = chosen resume node; 'rerun' = downstream (will re-run)
  }
}>()

// shared 1s clock + shared formatter (utils/nowTick.ts, utils/taskElapsed.ts) — this component used to
// own its own interval, which is how three copies of the same counter drifted apart
const now = useNowTick()

const elapsed = computed(() => taskElapsed(
  props.data.startedAt  === undefined ? undefined : new Date(props.data.startedAt),
  props.data.finishedAt === undefined ? undefined : new Date(props.data.finishedAt),
  now.value,
))

const STATUS_COLORS: Record<TaskStatus, string> = {
  queued:    '#3f3f46',
  running:   '#1e40af',
  done:      '#14532d',
  failed:    '#7f1d1d',
  cancelled: '#3f3f46',
}

// icon + text colour come from the canonical map (lib/taskStatus.ts); STATUS_COLORS above is this
// node's own dark background tint (component chrome), kept separate.
</script>

<template>
  <div class="live-node" :class="{ 'restart-start': data.restart === 'start', 'restart-rerun': data.restart === 'rerun' }"
       :style="{ borderColor: data.restart ? undefined : STATUS_COLORS[data.status] }">
    <!-- anchor points for the execution-order edges (not user-connectable) -->
    <Handle type="target" :position="Position.Left" class="live-handle" :connectable="false" />
    <Handle type="source" :position="Position.Right" class="live-handle" :connectable="false" />
    <span v-if="data.restart === 'start'" class="restart-badge">resume from</span>
    <div class="live-status-bar" :style="{ background: STATUS_COLORS[data.status] }">
      <i :class="['pi', TASK_STATUS[data.status].icon]"
         :style="{ color: TASK_STATUS[data.status].color }" />
      <span class="live-status-label" :style="{ color: TASK_STATUS[data.status].color }">
        {{ data.status }}
      </span>
      <span v-if="elapsed" class="live-elapsed">{{ elapsed }}</span>
    </div>
    <div class="live-fn">{{ data.label ?? data.fn.split('.').pop() }}</div>
    <!-- output value_name on its own line so a long label doesn't clip it -->
    <div v-if="data.variant" class="live-variant-row">
      <span class="live-variant">{{ data.variant }}</span>
    </div>
  </div>
</template>

<style scoped>
.live-node {
  background: var(--cc-surface-1);
  border: 1.5px solid #3f3f46;
  border-radius: var(--cc-radius-sm);
  padding: 5px 9px;
  font-size: var(--cc-fs-xs);
  min-width: 110px;
  cursor: pointer;              /* clickable: pick as the resume-from node */
  position: relative;
}

/* resume-from highlight: the chosen start node (solid accent) + everything downstream that will
   re-run (dashed accent). Overrides the status border while a start node is picked. */
.live-node.restart-start {
  border-color: var(--cc-accent) !important;
  box-shadow: 0 0 0 2px color-mix(in srgb, var(--cc-accent) 40%, transparent);
}
.live-node.restart-rerun {
  border-style: dashed;
  border-color: var(--cc-accent) !important;
}
.restart-badge {
  position: absolute;
  top: -8px; left: 6px;
  font-size: var(--cc-fs-3xs);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--cc-surface-1);
  background: var(--cc-accent);
  border-radius: var(--cc-radius-xs);
  padding: 1px 4px;
  z-index: 1;
}

/* handles are pure edge anchors here — keep them subtle and non-interactive */
.live-handle {
  width: 6px;
  height: 6px;
  background: #52525b;
  border: none;
  opacity: 0.5;
}

.live-status-bar {
  display: flex;
  align-items: center;
  gap: 0.25rem;
  margin: -5px -9px 5px;
  padding: 3px 9px;
  border-radius: var(--cc-radius-xs) 3px 0 0;
  font-size: var(--cc-fs-3xs);
}

.live-status-label {
  font-size: var(--cc-fs-3xs);
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
}

.live-elapsed {
  margin-left: auto;
  font-size: var(--cc-fs-3xs);
  font-family: var(--cc-mono);
  opacity: 0.8;
}

.live-fn {
  font-size: var(--cc-fs-xs);
  font-weight: 600;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 160px;
}

.live-variant-row {
  margin-top: 3px;
}
.live-variant {
  font-size: var(--cc-fs-3xs);
  font-family: var(--cc-mono, monospace);
  font-weight: 600;
  color: var(--cc-accent);
  background: color-mix(in srgb, var(--cc-accent) 18%, transparent);
  border-radius: var(--cc-radius-xs);
  padding: 1px 4px;
}
</style>
