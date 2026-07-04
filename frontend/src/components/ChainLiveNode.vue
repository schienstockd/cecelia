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
  <div class="live-node" :style="{ borderColor: STATUS_COLORS[data.status] }">
    <!-- anchor points for the execution-order edges (not user-connectable) -->
    <Handle type="target" :position="Position.Left" class="live-handle" :connectable="false" />
    <Handle type="source" :position="Position.Right" class="live-handle" :connectable="false" />
    <div class="live-status-bar" :style="{ background: STATUS_COLORS[data.status] }">
      <i :class="['pi', STATUS_ICONS[data.status]]"
         :style="{ color: STATUS_TEXT[data.status] }" />
      <span class="live-status-label" :style="{ color: STATUS_TEXT[data.status] }">
        {{ data.status }}
      </span>
      <span v-if="elapsed" class="live-elapsed">{{ elapsed }}</span>
    </div>
    <div class="live-fn">
      {{ data.label ?? data.fn.split('.').pop() }}
      <span v-if="data.variant" class="live-variant">{{ data.variant }}</span>
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
  cursor: default;
  position: relative;
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
  display: flex;
  align-items: center;
  gap: 5px;
  font-size: 11px;
  font-weight: 600;
  color: var(--cc-text, #e2e2f0);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 150px;
}

.live-variant {
  font-size: 9px;
  font-family: var(--cc-mono, monospace);
  font-weight: 600;
  color: var(--cc-accent, #a78bfa);
  background: color-mix(in srgb, var(--cc-accent, #a78bfa) 18%, transparent);
  border-radius: 3px;
  padding: 0 4px;
  flex-shrink: 0;
}
</style>
