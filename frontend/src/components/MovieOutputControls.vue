<script setup lang="ts">
// Frame rate + resolution for a rendered movie. ONE implementation for the three places that produce
// one — the napari ViewerPanel recorder, BatchMoviesPanel and AnimationModule.
//
// The three had drifted in what they even OFFERED, not just how it looked: the viewer and batch panels
// had fps 1-60 plus a res supersample, while Animation had fps 1-40 and no res at all — not a decision,
// just the control nobody added. Its render path could always take one (`napari-animation`'s
// `animate()` has `scale_factor`, which the timelapse recorder was already passing); the parameter is
// now plumbed through `record_keyframes` so this control means the same thing everywhere.
//
// Two named v-models rather than one config object, because the three sites store these differently:
// the viewer and batch panels share a per-set movie config, Animation keeps per-project refs.

defineProps<{ fps: number, scale: number }>()
defineEmits<{ (e: 'update:fps', v: number): void, (e: 'update:scale', v: number): void }>()
</script>

<template>
  <div class="mo">
    <span class="mo-lbl cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Frames per second'">fps</span>
    <input type="range" min="1" max="60" step="1" class="mo-range" :value="fps" v-tooltip.bottom="'Frames per second'"
           @input="$emit('update:fps', ($event.target as HTMLInputElement).valueAsNumber)" />
    <span class="mo-val cc-readout">{{ fps }}</span>

    <span class="mo-lbl cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Resolution supersample (2× = double resolution)'">res</span>
    <input type="range" min="1" max="3" step="1" class="mo-range" :value="scale" v-tooltip.bottom="'Resolution supersample (2× = double resolution)'"
           @input="$emit('update:scale', ($event.target as HTMLInputElement).valueAsNumber)" />
    <span class="mo-val cc-readout">{{ scale }}×</span>
  </div>
</template>

<style scoped>
.mo { display: flex; align-items: center; gap: 0.4rem; flex-wrap: wrap; }
.mo-lbl { flex-shrink: 0; }
.mo-range { width: 4.5rem; flex-shrink: 0; }
.mo-val { min-width: 1.6rem; }
</style>
