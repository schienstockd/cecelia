<script setup lang="ts">
// Which stretch of a timelapse a movie records — the shared control for the two surfaces that sweep T
// (the napari recorder and the Batch page). The Animation page deliberately does NOT get one: its
// keyframes carry their own dims, so the timeline already IS the range.
//
// FRAME INDICES, not a percentage. The 3D crop's z/t ranges are percentages because a crop is one
// image's own geometry; a recording range is the recorders' contract all the way down (`_t_range` →
// `_t_sweep_frames` → the bridge → `napari_utils.record_timelapse`), and every one of them CLAMPS to
// the image's length. So one range across a batch of unequal timelapses records to the end of each.
//
// `tEnd === null` means "the last frame", and is what a full-range selection WRITES — so the config
// stays clean, an image longer than the one it was authored on still records in full, and a movie
// recorded before this control existed reads identically. The read/write pair lives in
// `utils/batchMovie.ts` so both surfaces cannot disagree about it.
//
// LAYOUT: a SIBLING of `MovieOutputControls` and `TitleCardControls` in the same options popover, so it
// is built the same way they are — a `.cc-row` root capped at `--cc-movie-block`, one `cc-row-group`,
// and the shared `.cc-lbl-col` eyebrow for the label. That reserved column is the whole reason fps /
// px / name / z / show / title start their controls on one x; a hand-rolled label row starts on
// another, which is what this first shipped as.
import { computed } from 'vue'
import RangeSlider from './RangeSlider.vue'
import { resolveFrameRange, storeFrameEnd } from '../utils/batchMovie'

const props = defineProps<{
  tStart?: number
  tEnd?: number | null
  /** Frames available — the LONGEST timelapse in play, since the backend clamps the short ones. */
  frames: number
}>()
const emit = defineEmits<{
  (e: 'update:tStart', v: number): void
  (e: 'update:tEnd', v: number | null): void
}>()

const last = computed(() => Math.max(0, props.frames - 1))
const range = computed(() => resolveFrameRange(props.tStart, props.tEnd, props.frames))
const setHi = (v: number) => emit('update:tEnd', storeFrameEnd(v, props.frames))
</script>

<template>
  <div class="mtr cc-row">
    <span class="cc-row-group mtr-row">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs"
            v-tooltip.bottom="'Which frames to record'">frames</span>
      <RangeSlider class="mtr-slider" :lo="range.lo" :hi="range.hi" :min="0" :max="last"
                   @update:lo="emit('update:tStart', $event)" @update:hi="setHi"
                   v-tooltip.bottom="'First and last frame to record'" />
      <span class="mtr-val cc-readout">{{ range.full ? 'all' : `${range.lo}–${range.hi}` }}</span>
      <button v-if="!range.full" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              @click="emit('update:tStart', 0); emit('update:tEnd', null)"
              v-tooltip.bottom="'Record the whole timelapse'">
        <i class="pi pi-times" />
      </button>
    </span>
  </div>
</template>

<style scoped>
/* the same block cap as `.mo` / `.tc`, so the three stack as one column of controls */
.mtr { min-width: 0; max-width: var(--cc-movie-block); }
/* Its own line, like `name` and `show` — a dual-thumb track shares a row badly, and the readout
   swings between "all" and "120–480" as you drag, which would shuffle whatever sat beside it. */
.mtr-row { flex: 1 1 100%; }
/* matches `.mo-range` — the same track width as the fps / z sliders it stacks with */
.mtr-slider { flex: 1 1 3rem; min-width: 2.5rem; }
/* room for "120–480" without the drag reflowing the row */
.mtr-val { min-width: 3.4rem; text-align: right; }
</style>
