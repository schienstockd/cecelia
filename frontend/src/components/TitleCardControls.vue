<script setup lang="ts">
// The title-card control row: on/off, duration, and an optional note. ONE implementation for the three
// places that record a movie — the napari ViewerPanel recorder, BatchMoviesPanel, and AnimationModule.
//
// All three already shared the MODEL: the same `TitleCardCfg`, the same 1-10s clamp, and the same
// `buildTitleCard` payload builder. Only the widgets were hand-rolled, three times, and they had
// drifted — the toggle read "title" in the viewer and "Title card" in the other two, with three
// different tooltip wordings and three sets of class names (`.movie-*` / `.bm-*` / `.anim-*`). This is
// the PoolThrottle pattern: the component owns the label, the tooltip, the clamp and the layout, so
// there is nothing left to drift.
//
// v-model takes the WHOLE config and emits a whole new object, which suits all three call sites: the
// viewer and batch panels persist via a merge-patch into their per-set config, and the animation store
// holds a ref with a deep watch that autosaves either way.
import CcToggle from './CcToggle.vue'
import type { TitleCardCfg } from '../utils/batchMovie'

const props = defineProps<{ modelValue: TitleCardCfg }>()
const emit = defineEmits<{ (e: 'update:modelValue', v: TitleCardCfg): void }>()

const patch = (p: Partial<TitleCardCfg>) => emit('update:modelValue', { ...props.modelValue, ...p })

// The clamp lives here rather than at each call site — it is a property of the control, and two of the
// three sites were each re-implementing the same Math.min/Math.max.
const setDuration = (v: number) => patch({ durationSec: Math.min(10, Math.max(1, Math.round(v) || 1)) })
</script>

<template>
  <div class="tc">
    <CcToggle
      :model-value="modelValue.enabled"
      label="Title card"
      v-tooltip.bottom="'Prepend a slide with the image name, attributes, channels and their colours'"
      @update:model-value="v => patch({ enabled: v })" />
    <template v-if="modelValue.enabled">
      <input type="range" min="1" max="10" step="1" class="tc-range"
             :value="modelValue.durationSec"
             v-tooltip.bottom="'How long the title card shows'"
             @input="setDuration(($event.target as HTMLInputElement).valueAsNumber)" />
      <span class="tc-dur cc-readout">{{ modelValue.durationSec }}s</span>
      <input type="text" class="tc-note cc-input-xs" placeholder="note (optional)"
             :value="modelValue.note" v-tooltip.bottom="'Second line on the title card'"
             @input="patch({ note: ($event.target as HTMLInputElement).value })" />
    </template>
  </div>
</template>

<style scoped>
.tc { display: flex; align-items: center; gap: 0.4rem; flex-wrap: wrap; }
.tc-range { width: 4.5rem; flex-shrink: 0; }
.tc-dur { min-width: 1.6rem; }
/* the note takes the leftover width where there is any, and wraps to its own line where there is not */
.tc-note { flex: 1; min-width: 6rem; }
</style>
