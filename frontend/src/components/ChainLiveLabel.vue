<!--
  Non-interactive header label for the Live-run grid: a column header (task name, top of each
  execution-order column) or a row header (image name, left of each image row). Not a task node —
  it carries no status and no handles.
-->
<script setup lang="ts">
defineProps<{
  id: string
  data: {
    text: string
    sub?: string
    kind: 'row' | 'col'
  }
}>()
</script>

<template>
  <div class="live-label" :class="data.kind">
    <span class="live-label-text">{{ data.text }}</span>
    <span v-if="data.sub" class="live-label-sub">{{ data.sub }}</span>
  </div>
</template>

<style scoped>
.live-label {
  font-size: 10px;
  color: var(--cc-text-dim, #8b8ca7);
  white-space: nowrap;
  pointer-events: none;
  display: flex;
  flex-direction: column;
  gap: 1px;
}
.live-label.col {
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--cc-text, #e2e2f0);
  max-width: 170px;
}
.live-label.row {
  justify-content: center;
  align-items: flex-end;
  text-align: right;
  width: 120px;
  height: 100%;
}
.live-label.row .live-label-text {
  font-weight: 600;
  color: var(--cc-text, #e2e2f0);
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 120px;
}
.live-label-text {
  overflow: hidden;
  text-overflow: ellipsis;
}
.live-label-sub {
  font-size: 9px;
  font-family: monospace;
  opacity: 0.7;
}
</style>
