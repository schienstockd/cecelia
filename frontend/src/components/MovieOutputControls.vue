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
}>()
const emit = defineEmits<{
  (e: 'update:fps', v: number): void
  (e: 'update:sizeX', v: number | null): void
  (e: 'update:sizeY', v: number | null): void
  (e: 'update:suffix', v: string): void
  (e: 'update:timestamp', v: boolean): void
  (e: 'update:scaleBar', v: boolean): void
}>()

const hasOverlays = computed(() => props.timestamp !== undefined || props.scaleBar !== undefined)
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
      <span class="mo-lbl cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Frames per second'">fps</span>
      <input type="range" min="1" max="60" step="1" class="mo-range" :value="fps" v-tooltip.bottom="'Frames per second'"
             @input="$emit('update:fps', ($event.target as HTMLInputElement).valueAsNumber)" />
      <span class="mo-val cc-readout">{{ fps }}</span>
    </span>

    <span class="cc-row-group">
      <span class="mo-lbl cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Output size in pixels; blank = canvas size'">px</span>
      <input type="number" min="2" max="4096" step="2" class="cc-input-2xs mo-num" :value="sizeX ?? ''"
             :placeholder="movieAxisPlaceholder(canvasX)" v-tooltip.bottom="'Width; blank = canvas width'"
             @change="onAxis('sizeX', $event)" />
      <span class="cc-muted cc-fs-2xs">×</span>
      <input type="number" min="2" max="4096" step="2" class="cc-input-2xs mo-num" :value="sizeY ?? ''"
             :placeholder="movieAxisPlaceholder(canvasY)" v-tooltip.bottom="'Height; blank = canvas height'"
             @change="onAxis('sizeY', $event)" />
    </span>

    <span class="cc-row-group mo-grow">
      <span class="mo-lbl cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Added to the file name'">name</span>
      <input type="text" class="cc-input-2xs mo-txt" :value="suffix" placeholder="suffix"
             v-tooltip.bottom="'Added to the file name; keeps versions apart'"
             @change="$emit('update:suffix', ($event.target as HTMLInputElement).value)" />
    </span>

    <span v-if="hasOverlays" class="cc-row-group">
      <span class="mo-lbl cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Drawn into the recorded frames'">show</span>
      <ChipSelect multiple :options="OVERLAY_OPTIONS" v-model="overlays"
                  aria-label="Overlays burnt into the movie"
                  v-tooltip.bottom="'Napari overlays burnt into every frame'" />
    </span>
  </div>
</template>

<style scoped>
.mo { min-width: 0; }
/* only the name group absorbs leftover width; the numeric ones keep their intrinsic size */
.mo-grow { flex: 1 1 8rem; }
.mo-lbl { flex-shrink: 0; }
.mo-range { width: 4.5rem; flex: 1 1 3rem; min-width: 2.5rem; }
.mo-val { min-width: 1.6rem; }
/* wide enough for 4 digits PLUS the number spinner — 4096 was clipping at 3.6rem */
.mo-num { width: 4.2rem; flex-shrink: 0; }
.mo-txt { flex: 1 1 auto; min-width: 4rem; }
</style>
