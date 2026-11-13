<!--
  The viewer window's Populations section (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer): one
  row per gated population drawn as points, the section-wide eyes, the colour-by in use and its
  legend, z reach, point size and border. No colour-by picker: the column is
  picked under "On the image" (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, Decision 2). The window owns the layers and the eyes;
  this renders them and reports the user's changes.
-->
<script setup lang="ts">
import CollapsibleSection from '../CollapsibleSection.vue'
import CcToggle from '../CcToggle.vue'
import { useSettingsStore } from '../../stores/settings'
import type { ColourScale, OverlayPayload, OverlayPop } from '../../utils/viewerOverlays'

/** one segmentation × pop type's populations, as the window draws them */
export interface VwPopLayer { vn: string; popType: string; payload: OverlayPayload }

defineProps<{
  /** "On the image" has a pop type on — off reads as inactive, not as an empty summary */
  panelOn: boolean
  summary: { cells: number; pops: number; dropped: number }
  /** why the overlays failed to load, '' when they did */
  error: string
  layers: VwPopLayer[]
  /** distinct segmentations / pop types listed — a group is named only when there is more than one */
  segs: number
  types: number
  isDrawn: (l: VwPopLayer, path: string) => boolean
  allVisible: boolean
  /** one at a time — showing a population hides the others */
  solo: boolean
  colourBy: string
  legend: ColourScale | null
  rampStyle: Record<string, string>
  singlePlane: boolean
  hasZ: boolean
  pointSize: number
  pointBorder: number
  pointCount: number
  mode: 'plane' | 'volume'
  zLoaded: [number, number]
}>()
const emit = defineEmits<{
  setAll: [visible: boolean]
  'update:solo': [solo: boolean]
  popEye: [l: VwPopLayer, pop: OverlayPop, visible: boolean]
  'update:pointSize': [px: number]
  'update:pointBorder': [px: number]
  redraw: []
}>()
const settings = useSettingsStore()

</script>

<template>
  <CollapsibleSection label="Populations" tip="Gated cell populations drawn as coloured points"
                      accordion="cc.vw.section" section="pops"
                      fill>
    <!-- Populations. Only when there is something to say: an unsegmented image has no cell table
         and no populations, and an empty group would read as a broken feature rather than as an
         image that has not been through segmentation yet.
         Panel gate off = the whole section reads as inactive rather than as a cell-count summary
         ("how you can show a pops stats when the pops toggle in the viewer
         controls is off").
         Empty-state phrasing lines up with Segmentation and Tracks. -->
    <template v-if="!panelOn">
      <div class="cc-empty-inline cc-fs-2xs">
        No populations shown — turn a type on under On the image
      </div>
    </template>
    <template v-else-if="summary.cells > 0 || error">
      <div v-if="error" class="cc-muted-warn cc-fs-2xs">{{ error }}</div>
      <!-- "cells but no populations" is a DIFFERENT state from "no cells", and they look identical
           on the canvas — so the panel names which one it is. -->
      <div v-else-if="summary.pops === 0" class="cc-muted cc-fs-2xs">
        {{ summary.cells }} cells, no populations gated
      </div>
      <template v-else>
        <!-- Section-wide eyes, same icons as the Channels row. They act on EVERY listed pop —
             all segmentations × pop types the panel has on — so "one at a time" means one pop
             on screen, whichever segmentation it is from. -->
        <div class="cc-row cc-row-tight">
          <div class="cc-row-group">
          <button :class="['cc-btn cc-btn-ghost cc-btn-icon', { 'cc-btn-on cc-btn-on-tint': allVisible }]"
                  @click="emit('setAll', !allVisible)" :aria-pressed="allVisible"
                  v-tooltip.right="allVisible ? 'Hide every population' : 'Show every population'"
                  aria-label="Toggle every population">
            <i :class="['pi', allVisible ? 'pi-eye' : 'pi-eye-slash']" />
          </button>
          <button :class="['cc-btn cc-btn-ghost cc-btn-icon', { 'cc-btn-on cc-btn-on-tint': solo }]"
                  @click="emit('update:solo', !solo)" :aria-pressed="solo"
                  v-tooltip.right="'One at a time — showing a population hides the others'"
                  aria-label="Show one population at a time">
            <i class="pi pi-dot-circle" />
          </button>
          </div>
        </div>
        <template v-for="l in layers" :key="l.vn + '::' + l.popType">
          <!-- one group per (segmentation, pop type); named only when there is more than one -->
          <div v-if="segs > 1 || types > 1" class="cc-muted cc-fs-3xs">
            {{ segs > 1 ? l.vn : '' }}{{ segs > 1 && types > 1 ? ' · ' : '' }}{{ types > 1 ? l.popType : '' }}
          </div>
          <div v-for="pop in l.payload.pops" :key="pop.path" class="cc-row cc-row-tight">
            <span class="vw-swatch" :style="{ background: pop.colour }" />
            <span class="cc-fs-2xs vw-pop-name" :title="pop.path">{{ pop.name }}</span>
            <span class="cc-readout cc-fs-3xs">{{ pop.labels.length }}</span>
            <CcToggle
              :model-value="isDrawn(l, pop.path)"
              v-tooltip.bottom="'Draw this population over the image'"
              :aria-label="'Show ' + pop.name" @update:modelValue="v => emit('popEye', l, pop, v)"
            />
          </div>
        </template>
        <!-- No colour-by picker: the column is a pick, so it is under "On the image", keyed per set.
             This row shows what it resolved to; the legend below shows its scale. -->
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Colour by</span>
          <span class="cc-fs-2xs vw-grow" :title="colourBy || 'population'">
            {{ colourBy || 'population' }}
          </span>
        </div>
        <!-- The legend says which SCALE is in use, because that is the server's decision (the same
             rule the plots use) and the two kinds look nothing alike. -->
        <div v-if="legend?.kind === 'numeric'"
             class="cc-row cc-row-tight cc-fs-3xs">
          <span class="cc-muted">{{ (legend.range?.[0] ?? 0).toPrecision(3) }}</span>
          <span class="vw-ramp" :style="rampStyle" />
          <span class="cc-muted">{{ (legend.range?.[1] ?? 1).toPrecision(3) }}</span>
        </div>
        <div v-else-if="legend?.kind === 'categorical'"
             class="cc-muted cc-fs-3xs">
          {{ legend.levels.length }} levels
        </div>

        <div v-if="singlePlane && hasZ" class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Z reach</span>
          <input
            type="range" class="vw-grow" :min="0" :max="10" :step="1"
            v-model.number="settings.viewerPointZTol" @input="emit('redraw')"
            v-tooltip.bottom="'Planes either side that still show a cell — 0 is the exact plane'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">±{{ settings.viewerPointZTol }}</span>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Point size</span>
          <input
            type="range" class="vw-grow" :min="2" :max="24" :step="1"
            :value="pointSize" @input="emit('update:pointSize', Number(($event.target as HTMLInputElement).value)); emit('redraw')"
            v-tooltip.bottom="'Marker size in image pixels — scales with zoom'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">{{ pointSize }}</span>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Border</span>
          <input
            type="range" class="vw-grow" :min="0" :max="6" :step="1"
            :value="pointBorder" @input="emit('update:pointBorder', Number(($event.target as HTMLInputElement).value)); emit('redraw')"
            v-tooltip.bottom="'Black outline around every point (0 = none)'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">{{ pointBorder }}</span>
        </div>
        <div class="cc-muted cc-fs-3xs">
          <template v-if="segs === 1">{{ layers[0]?.vn }} · </template>
          {{ pointCount }} drawn · {{ summary.cells }} cells
          <template v-if="summary.dropped">· {{ summary.dropped }} without a centroid</template>
          <template v-if="singlePlane">· this plane only</template>
          <template v-else-if="mode === 'plane'">· planes {{ zLoaded[0] }}–{{ zLoaded[1] }}</template>
        </div>
      </template>
    </template>

  </CollapsibleSection>
</template>

<style scoped>
.vw-grow { flex: 1; min-width: 0; }
.vw-num { flex: none; min-width: 4.5rem; text-align: right; }
/* A population row: swatch, name that can shrink, count, toggle. The name is the only flexible part —
   letting the count or the toggle shrink is what made the channel rows overlap. */
.vw-swatch { flex: none; width: 0.7rem; height: 0.7rem; border-radius: var(--cc-radius-xs);
  border: 1px solid var(--cc-border); }
.vw-pop-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vw-ramp { flex: 1; min-width: 2rem; height: 0.5rem; border-radius: var(--cc-radius-xs); }
</style>
