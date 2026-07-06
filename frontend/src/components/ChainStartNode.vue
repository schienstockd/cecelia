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
    <span class="start-label">start</span>
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
  border-radius: 50%;
  background: var(--cc-text, #e2e2f0);
  border: 3px solid var(--cc-accent, #a78bfa);
  box-shadow: 0 0 0 2px transparent;
  transition: box-shadow 0.12s;
}
.chain-start-node.selected .start-dot {
  box-shadow: 0 0 0 3px var(--cc-accent, #a78bfa);
}
.start-label {
  font-size: 9px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: var(--cc-text-dim, #8b8ca7);
}
.node-handle {
  width: 10px;
  height: 10px;
}
</style>
