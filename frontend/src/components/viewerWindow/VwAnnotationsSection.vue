<!--
  The viewer window's Annotations section (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer): scale
  bar, timestamp, region grid and landscape heatmap. Overlays on the view, never baked into the frame
  or a Share capture, so they sit with the viewport controls rather than the layer sections
  (docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md, P1). Settings live in the settings store; the window only
  hands in the computed landscape for the legend.
-->
<script setup lang="ts">
import CollapsibleSection from '../CollapsibleSection.vue'
import CcToggle from '../CcToggle.vue'
import { useSettingsStore } from '../../stores/settings'
import type { LandscapeResult } from '../../utils/landscape'

defineProps<{ landscape: LandscapeResult | null }>()
const settings = useSettingsStore()
</script>

<template>
  <CollapsibleSection label="Annotations" tip="Overlays on the view — not in captures"
                      accordion="cc.vw.section" section="ann"
                      fill>
    <!-- Toggle and text size share a row: the size is only ever adjusted with the thing it sizes
         in front of you, and a separate row for each would double the group's height. -->
    <div class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs cc-lbl-col"
            v-tooltip.right="'Physical scale of the current zoom'">Scale bar</span>
      <CcToggle v-model="settings.viewerScaleBar" aria-label="Show the scale bar" />
      <!-- The size slider appears WITH the thing it sizes. A control you cannot move is noise in
           a panel this dense — it says "there is something here" and then
           refuses. Nothing is lost: the toggle beside it is how you get the slider back. -->
      <template v-if="settings.viewerScaleBar">
        <input
          type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
          v-model.number="settings.viewerScaleBarPx"
          v-tooltip.bottom="'Scale-bar text size'" aria-label="Scale bar text size"
        >
        <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerScaleBarPx }}</span>
      </template>
    </div>
    <div class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs cc-lbl-col"
            v-tooltip.right="'Elapsed time, or the frame index if uncalibrated'">Timestamp</span>
      <CcToggle v-model="settings.viewerTimestamp" aria-label="Show the timestamp" />
      <template v-if="settings.viewerTimestamp">
        <input
          type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
          v-model.number="settings.viewerTimestampPx"
          v-tooltip.bottom="'Timestamp text size'" aria-label="Timestamp text size"
        >
        <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerTimestampPx }}</span>
      </template>
    </div>
    <!-- Set-of-Mark grid: named cells (A1..H8 at 8×8) over the current viewport, so a share-in
         / point-out session can cite a region by name. The density slider shows with the
         toggle OFF too: every capture carries a landscape at this density, so it is a capture
         setting, not only a display one. -->
    <div class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs cc-lbl-col"
            v-tooltip.right="'Named cells (A1..H8) over the viewport'">Grid</span>
      <CcToggle v-model="settings.viewerGrid" aria-label="Show the region grid" />
      <input
        type="range" class="vw-grow vw-px" :min="4" :max="32" :step="1"
        v-model.number="settings.viewerGridDensity"
        v-tooltip.bottom="'Cells per side — also the capture landscape grid'" aria-label="Grid density"
      >
      <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerGridDensity }}</span>
    </div>
    <!-- Landscape heatmap: cheap categorical fill per grid tile so a share-in / MCP reader
         has a rough semantic prior BEFORE looking at raw pixels. NOT a segmentation; sits at
         the same tile resolution as the grid above. BIDIR PR #6 (Decision 14 reframe). -->
    <div class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs cc-lbl-col"
            v-tooltip.right="'Show the tile heatmap — captures always carry it'">Landscape</span>
      <CcToggle v-model="settings.viewerLandscape" aria-label="Show the landscape heatmap" />
      <template v-if="settings.viewerLandscape">
        <span class="cc-muted cc-fs-3xs vw-landscape-sub-lbl"
              v-tooltip.bottom="'Print each tile category on the fill — eyeball whether the labelling reads right'">Labels</span>
        <CcToggle v-model="settings.viewerLandscapeLabels" aria-label="Show landscape tile labels" />
      </template>
    </div>
    <ul v-if="settings.viewerLandscape && landscape" class="vw-landscape-legend"
        v-tooltip.bottom="'Tile category legend — cell count in parentheses'">
      <li v-for="l in landscape.legend" :key="l.category" class="cc-row cc-row-tight">
        <span class="vw-lg-swatch" :style="{ background: l.swatch }" />
        <span class="cc-fs-3xs">{{ l.category }}<span class="cc-muted">&nbsp;({{ l.nTiles }})</span></span>
      </li>
    </ul>
  </CollapsibleSection>
</template>

<style scoped>
.vw-grow { flex: 1; min-width: 0; }
.vw-px { min-width: 3.5rem; }
.vw-px-val { flex: none; min-width: 1.4rem; text-align: right; }
.vw-landscape-legend { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 0.15rem; }
.vw-landscape-sub-lbl { margin-left: 0.4rem; }
.vw-lg-swatch { display: inline-block; width: 10px; height: 10px; border-radius: var(--cc-radius-xs); border: 1px solid var(--cc-border); }
</style>
