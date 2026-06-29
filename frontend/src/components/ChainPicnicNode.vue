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
  <!-- Picnic = set-scope synchronisation node. Visually distinct: amber border,
       diamond accent, barrier policy shown. Image threads barrier here. -->
  <div class="chain-picnic-node" :class="{ selected }">
    <Handle type="target" :position="Position.Left" class="node-handle" />

    <div class="picnic-header">
      <span class="picnic-diamond">◆</span>
      <span class="picnic-badge">set</span>
    </div>
    <div class="node-label">{{ data.label || data.fn }}</div>
    <div class="node-fn">{{ data.fn }}</div>
    <div class="policy-row">
      <span class="policy-label">barrier:</span>
      <span class="policy-val">{{ data.barrier_policy }}</span>
    </div>
    <div v-if="data.resource_pool" class="node-pool">
      <i class="pi pi-server" style="font-size:0.6rem" />
      {{ data.resource_pool }}
    </div>

    <Handle type="source" :position="Position.Right" class="node-handle" />
  </div>
</template>

<style scoped>
.chain-picnic-node {
  background: #1c1505;
  border: 2px solid #f59e0b;
  border-radius: 6px;
  padding: 7px 11px;
  font-size: 12px;
  min-width: 140px;
  cursor: pointer;
  position: relative;
  transition: box-shadow 0.12s;
}
.chain-picnic-node.selected {
  box-shadow: 0 0 0 2px #f59e0b;
}

.picnic-header {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  margin-bottom: 0.25rem;
}
.picnic-diamond {
  font-size: 0.65rem;
  color: #f59e0b;
  line-height: 1;
}
.picnic-badge {
  font-size: 9px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: #f59e0b;
}

.node-label {
  font-size: 12px;
  font-weight: 600;
  color: #fef3c7;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 160px;
}
.node-fn {
  font-size: 10px;
  color: #d97706;
  font-family: monospace;
  margin-top: 2px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 160px;
}
.policy-row {
  display: flex;
  align-items: center;
  gap: 3px;
  margin-top: 4px;
}
.policy-label {
  font-size: 9px;
  color: #92400e;
  font-weight: 600;
}
.policy-val {
  font-size: 9px;
  color: #f59e0b;
  font-family: monospace;
}
.node-pool {
  display: flex;
  align-items: center;
  gap: 3px;
  font-size: 9px;
  color: #f97316;
  margin-top: 3px;
}
.node-handle {
  width: 10px;
  height: 10px;
}
</style>
