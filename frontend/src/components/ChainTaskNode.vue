<script setup lang="ts">
import { Handle, Position } from '@vue-flow/core'

defineProps<{
  id: string
  data: {
    fn: string
    scope: string
    params: Record<string, unknown>
    barrier_policy: string
    resource_pool: string
    label: string
  }
  selected: boolean
}>()
</script>

<template>
  <div class="chain-task-node" :class="{ selected, incremental: data.scope === 'incremental' }">
    <Handle type="target" :position="Position.Left" class="node-handle" />

    <div class="node-scope" :title="`scope: ${data.scope}`">
      <span class="scope-dot" />
      <span class="scope-label">{{ data.scope }}</span>
    </div>
    <div class="node-label">{{ data.label || data.fn }}</div>
    <div class="node-fn">{{ data.fn }}</div>
    <div v-if="data.resource_pool" class="node-pool">
      <i class="pi pi-server" style="font-size:0.6rem" />
      {{ data.resource_pool }}
    </div>

    <Handle type="source" :position="Position.Right" class="node-handle" />
  </div>
</template>

<style scoped>
.chain-task-node {
  background: var(--cc-surface-1, #1e1b2e);
  border: 1.5px solid var(--cc-accent, #a78bfa);
  border-radius: 6px;
  padding: 7px 11px;
  font-size: 12px;
  min-width: 140px;
  cursor: pointer;
  position: relative;
  transition: box-shadow 0.12s;
}
.chain-task-node.selected {
  box-shadow: 0 0 0 2px var(--cc-accent, #a78bfa);
}
.chain-task-node.incremental {
  border-style: dashed;
  opacity: 0.85;
}

.node-scope {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  margin-bottom: 0.25rem;
}
.scope-dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: var(--cc-accent, #a78bfa);
  flex-shrink: 0;
}
.incremental .scope-dot { background: #60a5fa; }
.scope-label {
  font-size: 9px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: var(--cc-text-dim, #8b8ca7);
}

.node-label {
  font-size: 12px;
  font-weight: 600;
  color: var(--cc-text, #e2e2f0);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 160px;
}
.node-fn {
  font-size: 10px;
  color: var(--cc-text-dim, #8b8ca7);
  font-family: monospace;
  margin-top: 2px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 160px;
}
.node-pool {
  display: flex;
  align-items: center;
  gap: 3px;
  font-size: 9px;
  color: #f97316;
  margin-top: 4px;
}
.node-handle {
  width: 10px;
  height: 10px;
}
</style>
