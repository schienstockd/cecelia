<script setup lang="ts">
import { ref, computed, onMounted, onUnmounted } from 'vue'
import { Handle, Position } from '@vue-flow/core'
import type { TaskStatus } from '../stores/tasks'

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

const now = ref(Date.now())
let timer: ReturnType<typeof setInterval> | null = null

onMounted(() => {
  timer = setInterval(() => {
    if (props.data.status === 'running') now.value = Date.now()
  }, 1000)
})
onUnmounted(() => { if (timer) clearInterval(timer) })

const elapsed = computed(() => {
  if (!props.data.startedAt) return undefined
  const end = props.data.finishedAt ?? now.value
  const s = Math.round((end - props.data.startedAt) / 1000)
  return s < 60 ? `${s}s` : `${Math.floor(s / 60)}m ${s % 60}s`
})

const STATUS_COLORS: Record<TaskStatus, string> = {
  queued:    '#3f3f46',
  running:   '#1e40af',
  done:      '#14532d',
  failed:    '#7f1d1d',
  cancelled: '#3f3f46',
}

const STATUS_TEXT: Record<TaskStatus, string> = {
  queued:    '#a1a1aa',
  running:   '#93c5fd',
  done:      '#86efac',
  failed:    '#fca5a5',
  cancelled: '#71717a',
}

const STATUS_ICONS: Record<TaskStatus, string> = {
  queued:    'pi-clock',
  running:   'pi-spin pi-cog',
  done:      'pi-check-circle',
  failed:    'pi-times-circle',
  cancelled: 'pi-ban',
}
</script>

<template>
  <div class="live-node" :class="{ 'restart-start': data.restart === 'start', 'restart-rerun': data.restart === 'rerun' }"
       :style="{ borderColor: data.restart ? undefined : STATUS_COLORS[data.status] }">
    <!-- anchor points for the execution-order edges (not user-connectable) -->
    <Handle type="target" :position="Position.Left" class="live-handle" :connectable="false" />
    <Handle type="source" :position="Position.Right" class="live-handle" :connectable="false" />
    <span v-if="data.restart === 'start'" class="restart-badge">resume from</span>
    <div class="live-status-bar" :style="{ background: STATUS_COLORS[data.status] }">
      <i :class="['pi', STATUS_ICONS[data.status]]"
         :style="{ color: STATUS_TEXT[data.status] }" />
      <span class="live-status-label" :style="{ color: STATUS_TEXT[data.status] }">
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
  background: var(--cc-surface-1, #1e1b2e);
  border: 1.5px solid #3f3f46;
  border-radius: 5px;
  padding: 5px 9px;
  font-size: 11px;
  min-width: 110px;
  cursor: pointer;              /* clickable: pick as the resume-from node */
  position: relative;
}

/* resume-from highlight: the chosen start node (solid accent) + everything downstream that will
   re-run (dashed accent). Overrides the status border while a start node is picked. */
.live-node.restart-start {
  border-color: var(--cc-accent, #a78bfa) !important;
  box-shadow: 0 0 0 2px color-mix(in srgb, var(--cc-accent, #a78bfa) 40%, transparent);
}
.live-node.restart-rerun {
  border-style: dashed;
  border-color: var(--cc-accent, #a78bfa) !important;
}
.restart-badge {
  position: absolute;
  top: -8px; left: 6px;
  font-size: 8px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--cc-surface-1, #1e1b2e);
  background: var(--cc-accent, #a78bfa);
  border-radius: 3px;
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
  border-radius: 3px 3px 0 0;
  font-size: 9px;
}

.live-status-label {
  font-size: 9px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
}

.live-elapsed {
  margin-left: auto;
  font-size: 9px;
  font-family: monospace;
  opacity: 0.8;
}

.live-fn {
  font-size: 11px;
  font-weight: 600;
  color: var(--cc-text, #e2e2f0);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 160px;
}

.live-variant-row {
  margin-top: 3px;
}
.live-variant {
  font-size: 9px;
  font-family: var(--cc-mono, monospace);
  font-weight: 600;
  color: var(--cc-accent, #a78bfa);
  background: color-mix(in srgb, var(--cc-accent, #a78bfa) 18%, transparent);
  border-radius: 3px;
  padding: 1px 4px;
}
</style>
