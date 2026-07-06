<!--
  Shared loading-spinner overlay for heavy plots. Drive it with `useDelayedLoading` (composables/) so
  it only appears once a load runs past the threshold — never on small/fast plots (docs/UI.md →
  "Plot loading state"). Place inside a `position: relative` container; it fills and centres.

    <div class="plot-host" style="position: relative">
      <PlotChart :data="..." :opts="..." />
      <PlotSpinner v-if="showSpinner" label="Loading…" />
    </div>
-->
<script setup lang="ts">
defineProps<{ label?: string }>()
</script>

<template>
  <div class="plot-spinner" role="status" aria-live="polite">
    <div class="plot-spinner__wheel" aria-hidden="true" />
    <span v-if="label" class="plot-spinner__label">{{ label }}</span>
  </div>
</template>

<style scoped>
.plot-spinner {
  position: absolute;
  inset: 0;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 8px;
  /* translucent scrim so it reads as "working" over whatever is (or isn't) already drawn */
  background: color-mix(in srgb, var(--cc-surface-1) 45%, transparent);
  pointer-events: none;   /* never block interaction with the plot underneath */
  z-index: 5;
}
.plot-spinner__wheel {
  width: 26px;
  height: 26px;
  border-radius: 50%;
  border: 2.5px solid var(--cc-border);
  border-top-color: var(--cc-accent);
  animation: plot-spinner-rot 0.7s linear infinite;
}
.plot-spinner__label {
  font-size: 11px;
  color: var(--cc-text-dim);
}
@keyframes plot-spinner-rot { to { transform: rotate(360deg); } }
@media (prefers-reduced-motion: reduce) {
  .plot-spinner__wheel { animation-duration: 2.4s; }
}
</style>
