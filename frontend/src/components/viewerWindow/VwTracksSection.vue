<!--
  The viewer window's Tracks section (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer): one ribbon
  row per gated population with tracks, colour mode (track / speed / source / population) with its
  legend, z reach and tail. Which segmentations draw is ticked in the main window's Viewer panel; the
  window owns the ribbon state, this renders it and reports the user's changes.
-->
<script setup lang="ts">
import CollapsibleSection from '../CollapsibleSection.vue'
import CcToggle from '../CcToggle.vue'
import ColourPicker from '../ColourPicker.vue'
import { useSettingsStore } from '../../stores/settings'
import type { TrackColorMode } from '../../utils/viewerOverlays'

/** a gated population that can draw as a ribbon layer */
export interface TrackPopRow {
  key: string; vn: string; popType: string; path: string; show: boolean
  name: string; colour: string; count: number
  /** its ribbon layer is on */
  drawn: boolean
  /** track clusters are drawing its segmentation, so its own toggle is held */
  clusterHeld: boolean
}

defineProps<{
  /** ribbon segments drawn */
  segCount: number
  pops: TrackPopRow[]
  colorMode: TrackColorMode
  /** µm per frame, for the speed ramp; null when not colouring by speed */
  speedRange: [number, number] | null
  /** the speed ramp's CSS gradient — the same lookup the ribbons are shaded with */
  rampStyle: Record<string, string>
  /** one per ticked segmentation that drew, for the source legend */
  sources: { vn: string; hex: string; count: number }[]
  singlePlane: boolean
  hasZ: boolean
}>()
const emit = defineEmits<{
  togglePop: [pop: TrackPopRow]
  'update:colorMode': [mode: TrackColorMode]
  sourceColour: [vn: string, hex: string]
  redraw: []
}>()
const settings = useSettingsStore()
</script>

<template>
  <CollapsibleSection label="Tracks" tip="Track ribbons from each ticked segmentation"
                      accordion="cc.vw.section" section="tracks"
                      fill>
    <!-- Tracks. The per-segmentation "directions" eye in the ViewerPanel ticks vns on; this
         section colours + shapes the ribbons. Gated on a ribbon-drawable pop as well as on
         drawn segments: hiding every pop's ribbon drops segCount to 0, and the per-pop eyes
         that turn them back on must stay. -->
    <template v-if="segCount > 0 || pops.length > 0">
      <!-- Per-pop ribbon rows: one line per gated pop with tracks (isTrack || hasTracks),
           mirroring the Populations section above (swatch/name/count/toggle). The eye hides
           the ribbon layer WITHOUT touching the point layer's eye — `ribbonEyes` is a
           separate bag from `popEyes` (MULTI_POP_TRACKING_PLAN.md Decision 5). -->
      <div v-for="pop in pops" :key="pop.key" class="cc-row cc-row-tight">
        <span class="vw-swatch" :style="{ background: pop.colour }" />
        <span class="cc-fs-2xs vw-pop-name" :title="pop.vn + ' ' + pop.path">{{ pop.name }}</span>
        <span class="cc-readout cc-fs-3xs">{{ pop.count }}</span>
        <CcToggle
          :model-value="pop.drawn" :disabled="pop.clusterHeld"
          v-tooltip.bottom="pop.clusterHeld ? 'Track clusters are drawing this segmentation' : 'Draw this population as a track ribbon layer'"
          :aria-label="'Show tracks for ' + pop.name" @update:modelValue="emit('togglePop', pop)"
        />
      </div>
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Colour by</span>
        <select :value="colorMode" @change="emit('update:colorMode', ($event.target as HTMLSelectElement).value as TrackColorMode)" class="cc-fs-2xs vw-grow"
                v-tooltip.bottom="'How ribbons are coloured'">
          <option value="track">track</option>
          <option value="speed">speed</option>
          <option value="solid">source</option>
          <option value="pop">population</option>
        </select>
      </div>
      <!-- Numeric ramp for speed mode — same shape as the point colour-by legend, so the reading
           stays consistent across overlay kinds. µm per frame (Δt = 1 hop). -->
      <div v-if="colorMode === 'speed' && speedRange"
           class="cc-row cc-row-tight cc-fs-3xs">
        <span class="cc-muted">{{ speedRange[0].toPrecision(3) }}</span>
        <span class="vw-ramp" :style="rampStyle" />
        <span class="cc-muted">{{ speedRange[1].toPrecision(3) }} µm/frame</span>
      </div>
      <!-- Per-source legend for solid mode — one row per ticked vn, showing a clickable
           swatch (the shared ColourPicker) + count. Same picker + palette as the population
           manager, so a source's colour authored here matches the visual language elsewhere.
           Doubles as a diagnostic: a vn with count 0 would be ticked but have no tracked
           cells; buildMultiTrackBuffer filters those out already, so the legend only lists
           sources that actually drew. -->
      <template v-if="colorMode === 'solid' && sources.length">
        <div v-for="src in sources" :key="src.vn" class="cc-row cc-row-tight cc-fs-3xs">
          <ColourPicker :model-value="src.hex"
                        @update:model-value="hex => emit('sourceColour', src.vn, hex)"
                        :tip="'Colour for ' + src.vn" />
          <span class="cc-fs-2xs vw-pop-name" :title="src.vn">{{ src.vn }}</span>
          <span class="cc-readout cc-fs-3xs">{{ src.count }}</span>
        </div>
      </template>
      <div v-if="singlePlane && hasZ" class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Z reach</span>
        <input
          type="range" class="vw-grow" :min="0" :max="10" :step="1"
          v-model.number="settings.viewerTrackZTol" @input="emit('redraw')"
          v-tooltip.bottom="'Planes either side that still show a tail'"
        >
        <span class="cc-readout cc-fs-2xs vw-num">±{{ settings.viewerTrackZTol }}</span>
      </div>
      <div class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Tail</span>
        <input
          type="range" class="vw-grow" :min="0" :max="60" :step="1"
          v-model.number="settings.viewerTailLength" @input="emit('redraw')"
          v-tooltip.bottom="'Track history in frames — 0 hides the tails'"
        >
        <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerTailLength }}</span>
      </div>
      <div v-if="settings.viewerTailLength > 0" class="cc-row cc-row-tight">
        <span class="cc-muted cc-fs-2xs cc-lbl-col">Tail width</span>
        <input
          type="range" class="vw-grow" :min="1" :max="12" :step="1"
          v-model.number="settings.viewerTailWidth" @input="emit('redraw')"
          v-tooltip.bottom="'Tail thickness in image pixels — scales with zoom'"
        >
        <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerTailWidth }}</span>
      </div>
      <div class="cc-muted cc-fs-3xs">
        {{ segCount }} segments · {{ sources.length }} source{{ sources.length === 1 ? '' : 's' }}
      </div>
    </template>
    <div v-else class="cc-empty-inline cc-fs-2xs">
      No tracks shown — tick a segmentation in the viewer panel
    </div>

  </CollapsibleSection>
</template>

<style scoped>
.vw-grow { flex: 1; min-width: 0; }
.vw-num { flex: none; min-width: 4.5rem; text-align: right; }
/* A population row: swatch, name that can shrink, count, toggle — as in the Populations section. */
.vw-swatch { flex: none; width: 0.7rem; height: 0.7rem; border-radius: var(--cc-radius-xs);
  border: 1px solid var(--cc-border); }
.vw-pop-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vw-ramp { flex: 1; min-width: 2rem; height: 0.5rem; border-radius: var(--cc-radius-xs); }
</style>
