<script setup lang="ts">
// Frame rate + output size for a rendered movie. ONE implementation for the three places that produce
// one — the napari ViewerPanel recorder, BatchMoviesPanel and AnimationModule.
//
// The size is two explicit pixel fields, not a multiplier. A 1-3x `res` slider used to live here and was
// removed: napari-animation screenshots the canvas and then `ndi.zoom`s the frame, so it bought 4x the
// pixels and no detail. A multiplier is also the wrong shape even done right — its base is the live
// canvas, so the same "2x" gives a different movie on a laptop and a desktop, while a journal asks for
// absolute dimensions. Blank = the napari canvas size (the default), shown as the placeholder so the
// honest default is visible. See docs/NAPARI.md.
//
// Named v-models rather than one config object, because the three sites store these differently: the
// viewer and batch panels share a per-set movie config, Animation keeps per-project refs.
import { computed } from 'vue'
import { movieAxisPlaceholder, parseMovieAxis } from '../utils/movieSize'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'

const props = defineProps<{
  fps: number
  sizeX: number | null
  sizeY: number | null
  // appended to the filename; a movie is named after the IMAGE, so this is what keeps a recording of
  // the corrected version from overwriting one of the raw import
  suffix: string
  // what napari would record at right now (GET /api/napari/status), for the placeholder
  canvasX?: number | null
  canvasY?: number | null
  // napari's BAKED overlays — drawn into the canvas, so they are burnt into every frame and can only
  // be left out by hiding them for the render. Optional: pass them and the chips appear, omit them
  // (the Animation page) and the row is exactly what it was.
  timestamp?: boolean
  scaleBar?: boolean
  // How much of the z stack to record. Optional in the same way: pass `sizeZ` and the row appears (and
  // only for a real stack), omit it and the control is exactly what it was.
  sizeZ?: number | null
  show3D?: boolean
  zSlice?: number | null
}>()
const emit = defineEmits<{
  (e: 'update:fps', v: number): void
  (e: 'update:sizeX', v: number | null): void
  (e: 'update:sizeY', v: number | null): void
  (e: 'update:suffix', v: string): void
  (e: 'update:timestamp', v: boolean): void
  (e: 'update:scaleBar', v: boolean): void
  (e: 'update:show3D', v: boolean): void
  (e: 'update:zSlice', v: number): void
}>()

const hasOverlays = computed(() => props.timestamp !== undefined || props.scaleBar !== undefined)
// Whole stack (3D) or one slice (2D). A segmented pair rather than a toggle: neither is the "off"
// state of the other, and "3D"/"slice" says what you get where an on/off switch would need a label
// saying which way is which.
const Z_OPTIONS: ChipOption[] = [
  { value: '3d', label: '3D' },
  { value: 'slice', label: 'slice' },
]
// one multi-select row, ON = burnt into the movie. No per-option tips: the row carries one tooltip
// for the whole control, and a second one on the chip renders on top of it.
const OVERLAY_OPTIONS: ChipOption[] = [
  { value: 'timestamp', label: 'timestamp' },
  { value: 'scaleBar', label: 'scale bar' },
]
const overlays = computed<string[]>({
  get: () => [props.timestamp !== false && 'timestamp', props.scaleBar !== false && 'scaleBar']
    .filter(Boolean) as string[],
  set: v => {
    emit('update:timestamp', v.includes('timestamp'))
    emit('update:scaleBar', v.includes('scaleBar'))
  },
})

const onAxis = (axis: 'sizeX' | 'sizeY', ev: Event) =>
  emit(`update:${axis}` as 'update:sizeX', parseMovieAxis((ev.target as HTMLInputElement).value))
</script>

<template>
  <!-- `.cc-row` of `.cc-row-group`s (style.css): the row wraps between GROUPS, so it never orphans a
       label or splits `724 × 722`. The viewer sidebar is narrow enough that this lands as two lines;
       the Animation page's header keeps them on one — no breakpoint, no per-surface variant. -->
  <div class="mo cc-row">
    <span class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Frames per second'">fps</span>
      <input type="range" min="1" max="60" step="1" class="mo-range" :value="fps" v-tooltip.bottom="'Frames per second'"
             @input="$emit('update:fps', ($event.target as HTMLInputElement).valueAsNumber)" />
      <span class="mo-val cc-readout">{{ fps }}</span>
    </span>

    <span class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Output size in pixels; blank = canvas size'">px</span>
      <input type="number" min="2" max="4096" step="2" class="cc-input-2xs mo-num" :value="sizeX ?? ''"
             :placeholder="movieAxisPlaceholder(canvasX)" v-tooltip.bottom="'Width; blank = canvas width'"
             @change="onAxis('sizeX', $event)" />
      <span class="cc-muted cc-fs-2xs">×</span>
      <input type="number" min="2" max="4096" step="2" class="cc-input-2xs mo-num" :value="sizeY ?? ''"
             :placeholder="movieAxisPlaceholder(canvasY)" v-tooltip.bottom="'Height; blank = canvas height'"
             @change="onAxis('sizeY', $event)" />
    </span>

    <span class="cc-row-group mo-grow">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Added to the file name'">name</span>
      <input type="text" class="cc-input-2xs mo-txt" :value="suffix" placeholder="suffix"
             v-tooltip.bottom="'Added to the file name; keeps versions apart'"
             @change="$emit('update:suffix', ($event.target as HTMLInputElement).value)" />
    </span>

    <!-- How much of the z stack the movie shows. ONE switch for both the image and the mask layers:
         napari cannot project a Labels layer at all, so "the whole stack" for a mask can only mean the
         volumetric render. Hidden entirely for an image with no z depth. -->
    <span v-if="(sizeZ ?? 0) > 1" class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">z</span>
      <ChipSelect variant="segmented" :options="Z_OPTIONS" :model-value="show3D ? '3d' : 'slice'"
                  aria-label="How much of the z stack to record"
                  v-tooltip.bottom="'Record the whole stack in 3D, or one z slice'"
                  @update:model-value="$emit('update:show3D', $event === '3d')" />
      <template v-if="!show3D">
        <input type="range" min="0" :max="(sizeZ ?? 1) - 1" step="1" class="mo-range"
               :value="zSlice ?? 0" v-tooltip.bottom="'Which z slice to record'"
               @input="$emit('update:zSlice', ($event.target as HTMLInputElement).valueAsNumber)" />
        <span class="mo-val cc-readout">{{ zSlice ?? 0 }}</span>
      </template>
    </span>

    <span v-if="hasOverlays" class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Drawn into the recorded frames'">show</span>
      <ChipSelect multiple :options="OVERLAY_OPTIONS" v-model="overlays"
                  aria-label="Overlays burnt into the movie"
                  v-tooltip.bottom="'Napari overlays burnt into every frame'" />
    </span>
  </div>
</template>

<style scoped>
.mo { min-width: 0; }
/* The name group takes a WHOLE line of its own (`flex-basis: 100%`), everywhere. It used to absorb
   leftover width (`1 1 8rem`), which in the viewer's 22rem popover left it sharing a line with the
   two size fields and squeezed to a few characters — and a filename is free text, the one field here
   that genuinely wants the width (Dominik, 2026-08-08). Full-width on the wider surfaces too, rather
   than a breakpoint: the same control reading differently per surface is what this component exists
   to prevent. */
.mo-grow { flex: 1 1 100%; }
/* .mo-lbl → .cc-lbl-col (style.css): one reserved label column shared with the title-card and
   compare rows, so the whole Movie block starts its controls on the same x. */
.mo-range { width: 4.5rem; flex: 1 1 3rem; min-width: 2.5rem; }
.mo-val { min-width: 1.6rem; }
/* wide enough for 4 digits PLUS the number spinner — 4096 was clipping at 3.6rem */
.mo-num { width: 4.2rem; flex-shrink: 0; }
.mo-txt { flex: 1 1 auto; min-width: 4rem; }
</style>
