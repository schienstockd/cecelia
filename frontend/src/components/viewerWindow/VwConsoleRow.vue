<!--
  The viewer window's console row (docs/todo/GUI_LAYOUT_PLAN.md, V2; mock frame 4): a one-line strip
  at the bottom of the window saying what the viewer is doing — loading, the level shown, the
  renderer, an integrated-GPU warning — and the cache budget on the right. Live viewer state, not log
  entries, which is why it is not the main window's ErrorConsole. Errors that break the view stay
  on the image.
-->
<script setup lang="ts">
defineProps<{
  /** what is loading, '' when settled */
  loading: string
  /** the pyramid level on screen ("L1"), '' for a single-level image */
  level: string
  renderer: string
  integratedGpu: boolean
  /** the cache budget in use */
  cache: string
}>()
</script>

<template>
  <div class="vw-console cc-fs-xs" role="status">
    <span v-if="loading" class="vw-console-item">
      <span class="cc-dot cc-dot-warn vw-console-dot" aria-hidden="true" />{{ loading }}
    </span>
    <span v-if="level" class="vw-console-item cc-muted"
          v-tooltip.top="'The pyramid level on screen — set it in the playback bar'">showing {{ level }}</span>
    <span v-if="renderer" class="vw-console-item cc-muted"
          v-tooltip.top="'Change it under the gear in the panel'">Renderer: {{ renderer }}</span>
    <span v-if="integratedGpu" class="vw-console-item cc-muted-warn"
          v-tooltip.top="'The browser picked the integrated GPU — expect much slower frames'">Integrated GPU</span>
    <span class="vw-console-grow" />
    <span v-if="cache" class="vw-console-item cc-muted"
          v-tooltip.top="'VRAM the viewer may hold — change it under the gear'">{{ cache }}</span>
  </div>
</template>

<style scoped>
.vw-console {
  flex-shrink: 0; display: flex; align-items: center; gap: 1rem;
  height: 1.6rem; padding: 0 0.75rem;
  background: var(--cc-surface-1); border-top: 1px solid var(--cc-border);
  white-space: nowrap; overflow: hidden;
}
.vw-console-item { display: inline-flex; align-items: center; gap: 0.35rem; }
.vw-console-grow { flex: 1; }
</style>
