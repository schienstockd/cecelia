<!--
  The viewer window's Segmentation section (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer): what
  mask is on screen, its visibility, opacity, outline and colour mode. No picker — which segmentation
  is shown is picked under "On the image" (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, Decision 2).
  The window owns the mask state; this renders it and asks for a redraw.
-->
<script setup lang="ts">
import CollapsibleSection from '../CollapsibleSection.vue'
import CcToggle from '../CcToggle.vue'
import ChipSelect from '../ChipSelect.vue'
import { useSettingsStore } from '../../stores/settings'
import { computed } from 'vue'
import type { MaskColour } from '../../utils/maskPopTable'

const props = defineProps<{
  /** the mask on screen ('' = none) */
  maskName: string
  /** a task preview or a live run's mask, not a stored one */
  maskState: '' | 'preview' | 'live'
  hidden: boolean
  /** stored segmentations ticked in the Viewer panel — only the first renders */
  shownLabelCount: number
  labelName: string
  /** why Colour draws the palette or all grey; '' when it draws what was asked */
  colourNote: string
  /** the last cell id the colour table could hold, when it was truncated */
  truncatedPast: number | null
  singlePlane: boolean
  mode: 'plane' | 'volume'
  /** more than one timepoint; a still image has no track clusters to colour by */
  timecourse: boolean
}>()
const emit = defineEmits<{ 'update:hidden': [hidden: boolean]; redraw: [] }>()
const settings = useSettingsStore()

// Track clusters only on a timecourse — unless already picked, so the stored choice stays visible and switchable.
const colourOptions = computed<{ value: MaskColour; label: string; tip: string }[]>(() => [
  { value: 'id', label: 'Cell id', tip: 'A colour per cell' },
  { value: 'pops', label: 'Pops', tip: 'Each cell by its deepest shown population' },
  ...(props.timecourse || settings.viewerMaskColour === 'trackclust'
    ? [{ value: 'trackclust' as const, label: 'Track clusters', tip: 'Each tracked cell by its track cluster' }] : []),
])
</script>

<template>
  <CollapsibleSection label="Segmentation" tip="Draw a segmentation mask over the image"
                      accordion="cc.vw.section" section="seg"
                      fill>
    <!-- No picker: WHICH segmentation is shown is picked in "On the image" (VwOnImageOverlay,
         docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md). The row below just SHOWS what's on and offers
         opacity, contour and colour for it.
         `labelNames` is the server's directory check, not the label registry, so an imported
         track set with a table and no mask does not offer a phantom row.
         Empty state: same shape as Populations and Tracks — one-liner "No X shown — action in
         the viewer panel" using .cc-empty-inline, so the three sections read coherently
. -->
    <template v-if="maskName">
      <!-- The task preview's mask counts as shown too: it gets the same opacity + outline, and
           the toggle flips whichever mask is on screen so it can be checked against the signal. -->
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Mask</span>
        <span class="cc-fs-2xs vw-grow" :title="maskName">
          {{ maskName }}<span v-if="maskState" class="cc-muted">
            · {{ maskState }}</span>
        </span>
        <CcToggle :model-value="!hidden" @update:model-value="v => { emit('update:hidden', !v); emit('redraw') }"
                  aria-label="Show mask"
                  v-tooltip.bottom="hidden ? 'Show the mask' : 'Hide the mask'" />
      </div>
      <!-- More than one ticked: only the first renders because the compositor's bind group has
           one label slot. Multi-mask rendering is a later phase; naming the limit here is the
           alternative to silently dropping the others. -->
      <div v-if="!maskState && shownLabelCount > 1" class="cc-muted-warn cc-fs-3xs">
        {{ shownLabelCount }} segmentations ticked — showing {{ labelName }} only
      </div>
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Opacity</span>
        <input
          type="range" class="vw-grow" :min="0" :max="1" :step="0.05"
          v-model.number="settings.viewerLabelOpacity" @input="emit('redraw')"
          v-tooltip.bottom="'How strongly the mask covers the signal'"
        >
        <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerLabelOpacity.toFixed(2) }}</span>
      </div>
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Outline</span>
        <input
          type="range" class="vw-grow" :min="0" :max="5" :step="1"
          v-model.number="settings.viewerLabelContour" @input="emit('redraw')"
          v-tooltip.bottom="'Outline width in voxels — 0 fills each cell'"
        >
        <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerLabelContour || 'fill' }}</span>
      </div>
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Colour</span>
        <ChipSelect
          :options="colourOptions" :model-value="settings.viewerMaskColour"
          variant="segmented" aria-label="Colour mask by"
          @update:model-value="v => (settings.viewerMaskColour = v as MaskColour)"
        />
      </div>
      <div v-if="colourNote" class="cc-muted cc-fs-3xs">{{ colourNote }}</div>
      <div v-else-if="truncatedPast !== null" class="cc-muted-warn cc-fs-3xs">
        Cells past id {{ truncatedPast }} not drawn
      </div>
      <div v-if="!singlePlane" class="cc-muted cc-fs-3xs">
        {{ mode === 'volume' ? '3D' : 'The ± window' }} shows the nearest mask surface
      </div>
    </template>
    <div v-else class="cc-empty-inline cc-fs-2xs">
      No mask shown — turn one on under On the image
    </div>

  </CollapsibleSection>
</template>

<style scoped>
.vw-grow { flex: 1; min-width: 0; }
/* the readout keeps a fixed box, so the slider beside it does not resize as the number changes */
.vw-num { flex: none; min-width: 4.5rem; text-align: right; }
</style>
