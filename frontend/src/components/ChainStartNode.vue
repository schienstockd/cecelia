<!--
  UML "start" (initial) node for the chain whiteboard: a filled dot you link to the task(s) where a
  run should BEGIN. A run executes only the nodes reachable from here (backend `_prune_to_start`) —
  so you can drop it mid-chain to run just the later tasks (earlier ones kept as drafts), or link it
  to one branch and leave another disconnected as a draft. Source-only (an initial node has no input);
  draggable like any node. There is at most one per chain (id `__start__`).
-->
<script setup lang="ts">
import { Handle, Position } from '@vue-flow/core'

defineProps<{ id: string; selected: boolean }>()
</script>

<template>
  <div class="chain-start-node" :class="{ selected }" title="Start — link to the task(s) a run begins from">
    <span class="start-dot" />
    <span class="cc-eyebrow cc-eyebrow-3xs">start</span>
    <!-- initial node: source only (no target handle) -->
    <Handle type="source" :position="Position.Right" class="node-handle" />
  </div>
</template>

<style scoped>
.chain-start-node {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 3px;
  cursor: pointer;
  position: relative;
}
.start-dot {
  width: 34px;
  height: 34px;
  border-radius: var(--cc-radius-pill);
  background: var(--cc-text);
  border: 3px solid var(--cc-accent);
  box-shadow: 0 0 0 2px transparent;
  transition: box-shadow 0.12s;
}
.chain-start-node.selected .start-dot {
  box-shadow: 0 0 0 3px var(--cc-accent);
}
.node-handle {
  width: 10px;
  height: 10px;
}
</style>
