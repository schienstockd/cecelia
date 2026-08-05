<script setup lang="ts">
// Frame rate for a rendered movie. ONE implementation for the three places that produce one — the
// napari ViewerPanel recorder, BatchMoviesPanel and AnimationModule.
//
// It also carried a `res` supersample (napari-animation's `scale_factor`), which is gone. It was never
// a decision: the viewer and batch panels had it and Animation did not, and unifying these controls
// spread it to all three rather than asking whether it earned its place. It did not — the recorder
// screenshots the canvas, so the only thing res changed was how much the canvas-sized frame was scaled
// up on the way out, at 4x the per-frame buffer and encode cost for 2x. Resizing the napari window is
// the honest way to get a bigger movie.
//
// A named v-model rather than a config object, because the three sites store fps differently: the
// viewer and batch panels share a per-set movie config, Animation keeps per-project refs.

defineProps<{ fps: number }>()
defineEmits<{ (e: 'update:fps', v: number): void }>()
</script>

<template>
  <div class="mo">
    <span class="mo-lbl cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Frames per second'">fps</span>
    <input type="range" min="1" max="60" step="1" class="mo-range" :value="fps" v-tooltip.bottom="'Frames per second'"
           @input="$emit('update:fps', ($event.target as HTMLInputElement).valueAsNumber)" />
    <span class="mo-val cc-readout">{{ fps }}</span>
  </div>
</template>

<style scoped>
.mo { display: flex; align-items: center; gap: 0.4rem; flex-wrap: wrap; }
.mo-lbl { flex-shrink: 0; }
.mo-range { width: 4.5rem; flex-shrink: 0; }
.mo-val { min-width: 1.6rem; }
</style>
