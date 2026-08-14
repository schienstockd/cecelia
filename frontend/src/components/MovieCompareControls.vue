<!--
  What a movie records, and what it puts side by side — the one control for both movie surfaces (the
  viewer's recorder and the batch panel), replacing the batch's old single-version `<select>`.

  TWO lists: the image VERSIONS to record, and the SEGMENTATION masks to draw. The selection is still
  the mode (D8): nothing = the ordinary movie, one = that one, two or more = a comparison in chip order
  (drag to reorder). The two together give the LAYOUT with nothing to choose — versions across, masks
  down: two of both is a grid, two of one is a single row side by side (`compareShape`). The outline
  slider appears with the masks it applies to — 0 fills them, N draws an N-px contour so the channel
  signal underneath stays readable. Contrast
  appears once there is something to compare; the arrangement toggle (across / stacked / wrapped into
  a grid) only when ONE list is doing the comparing, since picking from both already fixes both
  directions.

  Renders nothing for an image with one version and no segmentations — the common case gets smaller,
  not bigger. Pure logic lives in utils/movieCompare.ts. See docs/todo/MOVIE_COMPARE_PLAN.md.
-->
<script setup lang="ts">
import { computed } from 'vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import { isComparison, compareShape,
         type CompareLayout, type CompareContrast } from '../utils/movieCompare'
import { LABEL_CONTOUR_MAX, clampContour } from '../utils/batchMovie'

const props = defineProps<{
  versions: string[]              // the picked versions, in column order
  available: string[]             // the versions this image has
  segmentations?: string[]        // the picked segmentation masks, in column order
  availableSegmentations?: string[]  // the segmentations this image has
  contour?: number                // mask outline width in px (0 = filled)
  layout: CompareLayout
  contrast: CompareContrast
}>()
const emit = defineEmits<{
  (e: 'update:versions', v: string[]): void
  (e: 'update:segmentations', v: string[]): void
  (e: 'update:contour', v: number): void
  (e: 'update:layout', v: CompareLayout): void
  (e: 'update:contrast', v: CompareContrast): void
}>()

// No per-option `tip` on any row here. A chip row already carries ONE tooltip for the whole control,
// and a second one anchored on the individual chip renders on top of it — two overlapping boxes over
// the thing you were about to click (Dominik, 2026-08-07). So the group tooltip has to say what the
// per-option tips would have, and the icon rows below get labels a tooltip can name.
const options = computed<ChipOption[]>(() =>
  props.available.map(v => ({ value: v, label: v })))
const segOptions = computed<ChipOption[]>(() =>
  (props.availableSegmentations ?? []).map(v => ({ value: v, label: v })))

const segs = computed(() => props.segmentations ?? [])
// The outline slider only makes sense once a mask is actually drawn, so it rides the masks row rather
// than sitting in the options popover with fps and size — it is a property OF the thing above it.
const contour = computed(() => clampContour(props.contour))
const shape = computed(() => compareShape(props.versions, segs.value, props.layout))
const comparing = computed(() => isComparison(shape.value))
const LAYOUT_OPTIONS: ChipOption[] = [
  { value: 'row', label: '', icon: 'pi pi-pause' },
  { value: 'column', label: '', icon: 'pi pi-equals' },
  { value: 'grid', label: '', icon: 'pi pi-th-large' },
]
const CONTRAST_OPTIONS: ChipOption[] = [
  { value: 'reference', label: 'matched' },
  { value: 'version', label: 'own' },
]
</script>

<template>
  <div v-if="available.length > 1 || segOptions.length" class="mc">
    <span v-if="available.length > 1" class="cc-row-group cc-row-group-top mc-line">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">version</span>
      <ChipSelect :options="options" :model-value="versions" multiple reorderable
                  aria-label="Image versions to record"
                  v-tooltip.bottom="'Select versions to record; two or more compare them'"
                  @update:model-value="emit('update:versions', $event as string[])" />
    </span>
    <span v-if="segOptions.length" class="cc-row-group cc-row-group-top mc-line">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">masks</span>
      <ChipSelect :options="segOptions" :model-value="segs" multiple reorderable
                  aria-label="Segmentation masks to draw"
                  v-tooltip.bottom="'Segmentations drawn into the movie; two or more compare them'"
                  @update:model-value="emit('update:segmentations', $event as string[])" />
    </span>
    <span v-if="segOptions.length && segs.length" class="cc-row-group mc-line">
      <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">outline</span>
      <input type="range" min="0" :max="LABEL_CONTOUR_MAX" step="1" class="mc-range" :value="contour"
             v-tooltip.bottom="'Mask outline width; 0 fills the mask'"
             @input="emit('update:contour', clampContour(($event.target as HTMLInputElement).valueAsNumber))" />
      <span class="mc-val cc-readout">{{ contour || 'fill' }}</span>
    </span>
    <div v-if="comparing" class="mc-row cc-row">
      <!-- picking from BOTH lists already fixes both directions (versions across, masks down), so the
           arrangement is only offered when one list is doing the comparing -->
      <ChipSelect v-if="!shape.fixed" variant="segmented" :options="LAYOUT_OPTIONS" :model-value="layout"
                  aria-label="Comparison layout"
                  v-tooltip.bottom="'Cells side by side, stacked, or wrapped into a grid'"
                  @update:model-value="emit('update:layout', $event as CompareLayout)" />
      <ChipSelect variant="segmented" :options="CONTRAST_OPTIONS" :model-value="contrast"
                  aria-label="Comparison contrast"
                  v-tooltip.bottom="'Contrast matched to the first cell, or each version\'s own'"
                  @update:model-value="emit('update:contrast', $event as CompareContrast)" />
    </div>
  </div>
</template>

<style scoped>
.mc { display: flex; flex-direction: column; gap: 0.3rem; min-width: 0; }
/* The two chip rows carry an eyebrow label each — with two lists, unlabelled chips are a guess. The
   label column + top alignment are the shared `.cc-lbl-col` / `.cc-row-group-top` (style.css), so
   these rows line up with the recorder's fps/px/name/show/title rows rather than near them. */
.mc-line { min-width: 0; }
/* + .cc-row (wrap/align/gap). Nothing else shares this row: a hint line here pushed the second
   control onto its own line in the viewer panel, and the cost it stated now rides the action
   button's tooltip, where the user is actually committing to it. */
.mc-row { gap: 0.35rem; }
/* same proportions as MovieOutputControls' fps slider, so the two rows read as one block */
.mc-range { width: 4.5rem; flex: 1 1 3rem; min-width: 2.5rem; }
.mc-val { min-width: 1.6rem; }
</style>
