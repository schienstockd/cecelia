<script setup lang="ts">
// Frame rate + output size for a rendered movie. ONE implementation for the three places that produce
// one — the ViewerPanel recorder, BatchMoviesPanel and AnimationModule.
//
// The size is two explicit pixel fields, not a multiplier. A 1-3x `res` slider used to live here and
// was removed: an earlier recorder screenshotted the canvas and then upscaled the frame, so it bought
// 4x the pixels and no detail. A multiplier is also the wrong shape even done right — its base is the
// live canvas, so the same "2x" gives a different movie on a laptop and a desktop, while a journal
// asks for absolute dimensions. Blank = the viewer's canvas size (the default), shown as the
// placeholder so the honest default is visible.
//
// Named v-models rather than one config object, because the three sites store these differently: the
// viewer and batch panels share a per-set movie config, Animation keeps per-project refs.
import { computed } from 'vue'
import { movieAxisPlaceholder, parseMovieAxis } from '../utils/movieSize'
import { useFieldDraft } from '../composables/useFieldDraft'
import SuggestInput from './SuggestInput.vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'

const props = withDefaults(defineProps<{
  fps: number
  sizeX: number | null
  sizeY: number | null
  // appended to the filename; a movie is named after the IMAGE, so this is what keeps a recording of
  // the corrected version from overwriting one of the raw import
  suffix: string
  // suffixes already used in this project, offered as you type (`useMovieSuffixes`). Optional: omit
  // it and the field is exactly the plain input it was.
  suffixOptions?: string[]
  // the browser viewer's canvas size (via useViewerMovieDefaults), used as the placeholder
  canvasX?: number | null
  canvasY?: number | null
  // BAKED overlays — drawn into the canvas, so they are burnt into every frame and can only be left
  // out by hiding them for the render. Optional: pass them and the chips appear, omit them (the
  // Animation page) and the row is exactly what it was.
  //
  // `boolean | null`, and NOT a bare `boolean`, because absence is the whole signal here. Vue casts an
  // optional Boolean prop with no default to `false` when the parent omits it, so `!== undefined` was
  // always true and the Animation page — the one caller that passes neither — rendered two chips that
  // could never turn on: they emitted to a parent not listening. A union plus an explicit default
  // suppresses the cast, so absent really is absent. `utils/booleanProps.ts` ratchets the class.
  timestamp?: boolean | null
  scaleBar?: boolean | null
  // How much of the z stack to record. Optional in the same way: pass `sizeZ` and the row appears (and
  // only for a real stack), omit it and the control is exactly what it was.
  sizeZ?: number | null
  show3D?: boolean
  zSlice?: number | null
  // multiscale levels the open image has. >1 makes the 3D detail control meaningful (and visible);
  // omit it, or pass 0/1, and the row is exactly what it was.
  levels?: number | null
  detail3d?: number | null
// `null` defaults for the two overlay props: a union alone does NOT suppress Vue's Boolean cast — the
// cast-to-false branch fires whenever the prop is absent and has no `default`, whatever the union says.
// Verified against Vue's own prop resolution rather than read off the docs.
}>(), { timestamp: null, scaleBar: null })
const emit = defineEmits<{
  (e: 'update:fps', v: number): void
  (e: 'update:sizeX', v: number | null): void
  (e: 'update:sizeY', v: number | null): void
  (e: 'update:suffix', v: string): void
  (e: 'update:timestamp', v: boolean): void
  (e: 'update:scaleBar', v: boolean): void
  (e: 'update:show3D', v: boolean): void
  (e: 'update:zSlice', v: number): void
  (e: 'update:detail3d', v: number): void
}>()

const hasOverlays = computed(() => props.timestamp !== null || props.scaleBar !== null)
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

// The three FREE-TEXT fields commit on `@change` (blur / Enter), not per keystroke — parsing a
// half-typed width would clamp "8" to the minimum before the user reaches "800". That makes them
// uncontrolled while focused, and Vue force-patches an input's `value` on every element patch, so a
// re-render mid-typing used to replace what was typed with the bound value: the reported "I enter a
// name and it jumps back to the prefilled one", on both this surface and the batch panel.
// `useFieldDraft` keeps the DOM and the binding in lockstep without changing when the value commits.
const suffixDraft = useFieldDraft(() => props.suffix)
const sizeXDraft  = useFieldDraft(() => props.sizeX)
const sizeYDraft  = useFieldDraft(() => props.sizeY)

// The 3D detail row: only when 3D is selected AND there is more than one level to choose between.
const hasDetail = computed(() => props.show3D === true && (props.levels ?? 0) > 1)
// what a level actually costs you, in the terms you can see: levels halve X and Y (never Z), so level
// n is 1/2^n of the image's width. Said as a fraction rather than an index, because "2" means nothing.
const detailLabel = (lv: number) => (lv <= 0 ? 'full' : `1/${2 ** lv}`)

const onAxis = (axis: 'sizeX' | 'sizeY', raw: string) =>
  emit(`update:${axis}` as 'update:sizeX', parseMovieAxis(raw))
</script>

<template>
  <!-- `.cc-row` of `.cc-row-group`s (style.css): the row wraps between GROUPS, so it never orphans a
       label or splits `724 × 722`.

       WHERE a group lands must not depend on the container, or one component renders as two layouts
       and the surfaces have to be cross-checked by eye after every change. The rule: a group holding a
       TEXT FIELD or a chip row takes its OWN line (`.mo-own-row`, and `.tc-note` in TitleCardControls);
       the short numeric groups — fps, px, z — may share one. Everything below this component is then
       the same block on all three surfaces. -->
  <div class="mo cc-row">
    <span class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Frames per second'">fps</span>
      <input type="range" min="1" max="60" step="1" class="mo-range" :value="fps" v-tooltip.bottom="'Frames per second'"
             @input="$emit('update:fps', ($event.target as HTMLInputElement).valueAsNumber)" />
      <span class="mo-val cc-readout">{{ fps }}</span>
    </span>

    <span class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Output size in pixels; blank = canvas size'">px</span>
      <input type="number" min="2" max="4096" step="2" class="cc-input-2xs mo-num" v-model="sizeXDraft"
             :placeholder="movieAxisPlaceholder(canvasX)" v-tooltip.bottom="'Width; blank = canvas width'"
             @change="onAxis('sizeX', sizeXDraft)" />
      <span class="cc-muted cc-fs-2xs">×</span>
      <input type="number" min="2" max="4096" step="2" class="cc-input-2xs mo-num" v-model="sizeYDraft"
             :placeholder="movieAxisPlaceholder(canvasY)" v-tooltip.bottom="'Height; blank = canvas height'"
             @change="onAxis('sizeY', sizeYDraft)" />
    </span>

    <span class="cc-row-group mo-own-row">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Added to the file name'">name</span>
      <!-- Offers the suffixes already used in this project (registry-banked, `useMovieSuffixes`), so
           the label that keeps two recordings of one image apart is a pick rather than a re-type. -->
      <SuggestInput v-model="suffixDraft" :options="suffixOptions ?? []"
             input-class="cc-input-2xs mo-txt" placeholder="suffix"
             :tip="'Added to the file name; keeps versions apart'"
             @change="$emit('update:suffix', suffixDraft)" />
    </span>

    <!-- How much of the z stack the movie shows. ONE switch for both the image and the mask layers:
         a Labels layer can't be projected, so "the whole stack" for a mask can only mean the
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

    <!-- How much detail the 3D render uses. The renderer's default in 3D is a coarse pyramid level,
         which erases a segmentation; full resolution costs memory on a big volume. Only the person
         looking at the image can weigh that, so it is a control. -->
    <span v-if="hasDetail" class="cc-row-group">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'3D render detail'">detail</span>
      <input type="range" min="0" :max="(levels ?? 1) - 1" step="1" class="mo-range"
             :value="detail3d ?? 0"
             v-tooltip.bottom="'Full resolution is sharpest and heaviest; coarser is faster'"
             @input="$emit('update:detail3d', ($event.target as HTMLInputElement).valueAsNumber)" />
      <span class="mo-val cc-readout">{{ detailLabel(detail3d ?? 0) }}</span>
    </span>

    <span v-if="hasOverlays" class="cc-row-group mo-own-row">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs" v-tooltip.bottom="'Drawn into the recorded frames'">show</span>
      <ChipSelect multiple :options="OVERLAY_OPTIONS" v-model="overlays"
                  aria-label="Overlays burnt into the movie"
                  v-tooltip.bottom="'Overlays burnt into every frame'" />
    </span>
  </div>
</template>

<style scoped>
/* The block stops at a reading width, everywhere — see `--cc-movie-block` (style.css) for why the
   cap is on the BLOCK rather than per field. */
.mo { min-width: 0; max-width: var(--cc-movie-block); }
/* Groups that take a WHOLE line of their own, everywhere (Dominik, 2026-08-08). Two different
   reasons, one rule:

   NAME — a filename is free text and is the one field here that wants the width. It used to absorb
   leftover width (`1 1 8rem`), which in the viewer's 22rem popover left it sharing a line with the
   two size fields and squeezed to a few characters.

   SHOW — because the Z group beside it CHANGES WIDTH: picking 3D hides the slice slider and its
   readout, which freed enough room for the overlay chips to reflow up onto the z row. A control
   jumping when you touch an unrelated one is worse than a row that is sometimes short, so the
   overlays are pinned to the layout the 2D state already had.

   Full-width on the wider surfaces too, rather than a breakpoint: the same control reading
   differently per surface is what this component exists to prevent. */
.mo-own-row { flex: 1 1 100%; }
/* .mo-lbl → .cc-lbl-col (style.css): one reserved label column shared with the title-card and
   compare rows, so the whole Movie block starts its controls on the same x. */
.mo-range { width: 4.5rem; flex: 1 1 3rem; min-width: 2.5rem; }
.mo-val { min-width: 1.6rem; }
/* wide enough for 4 digits PLUS the number spinner — 4096 was clipping at 3.6rem */
.mo-num { width: 4.2rem; flex-shrink: 0; }
/* …and the field then fills its own line, up to the block cap above (which is what stops it eating a
   1200px page — a filename suffix is a handful of characters). */
.mo-txt { flex: 1 1 auto; min-width: 4rem; }
</style>
