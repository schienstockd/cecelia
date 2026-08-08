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
//
// LABEL TIER: an eyebrow to the left of the switch, matching the `fps`/`px`/`name`/`show` rows this
// control always sits under (MovieOutputControls). It used to use CcToggle's own `label`, which is a
// tier and a half larger (--cc-fs-md vs --cc-fs-2xs) and full-brightness — so "Title card" read as a
// section heading rather than as the fourth row of the same block, loudest in the batch panel where
// the real section headings are that size too.
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
  <div class="tc cc-row">
    <span class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs"
            v-tooltip.bottom="'Prepend a slide with the image name, attributes, channels and their colours'">title</span>
      <!-- the tooltip rides the LABEL, which is this row's heading; repeating it on the switch beside
           it is what `duplicateTooltips` (uiCopy.test.ts) exists to catch -->
      <CcToggle
        :model-value="modelValue.enabled"
        aria-label="Title card"
        @update:model-value="v => patch({ enabled: v })" />
    </span>
    <template v-if="modelValue.enabled">
      <!-- slider + its readout are ONE wrap unit (`.cc-row-group`): the panel is narrow enough that
           they would otherwise split across lines and read as an orphan number -->
      <span class="cc-row-group">
        <input type="range" min="1" max="10" step="1" class="tc-range"
               :value="modelValue.durationSec"
               v-tooltip.bottom="'How long the title card shows'"
               @input="setDuration(($event.target as HTMLInputElement).valueAsNumber)" />
        <span class="tc-dur cc-readout">{{ modelValue.durationSec }}s</span>
      </span>
      <input type="text" class="tc-note cc-input-xs" placeholder="note (optional)"
             :value="modelValue.note" v-tooltip.bottom="'Second line on the title card'"
             @input="patch({ note: ($event.target as HTMLInputElement).value })" />
    </template>
  </div>
</template>

<style scoped>
.tc { min-width: 0; }
/* the label column is the shared `.cc-lbl-col` (style.css) — that IS what makes the rows align */
.tc-range { width: 4.5rem; flex: 1 1 3rem; min-width: 2.5rem; }
.tc-dur { min-width: 1.6rem; }
/* The note takes a WHOLE line of its own, everywhere — the same rule (and the same `flex-basis: 100%`)
   as MovieOutputControls' `name` and `show` groups.

   It used to absorb the leftover width, so where it landed depended on the container: its own row in
   the batch panel, squeezed onto the title row in the viewer's 22rem popover. That is one component
   rendering as two layouts, which is exactly what having one component is supposed to prevent —
   Dominik was cross-checking the two surfaces by eye after every change (2026-08-08).

   The rule for this block, stated once so it stops being decided by accident: a group holding a TEXT
   FIELD or a chip row takes its own line; the short numeric groups (fps, px, z) may share one. */
.tc-note { flex: 1 1 100%; min-width: 6rem; max-width: 22rem; }   /* capped like `.mo-txt` — see there */
</style>
