<!--
  The viewer window's Annotations section (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer): the scale
  bar's and timestamp's text sizes and the lens radius (their switches are `VwOverlayToggles`), then the region grid and
  landscape heatmap, which captures carry. Overlays on the view, never baked into the frame
  or a Share capture, so they sit with the viewport controls rather than the layer sections
  (docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md, P1). Settings live in the settings store; the window only
  hands in the computed landscape for the legend.
-->
<script setup lang="ts">
import CollapsibleSection from '../CollapsibleSection.vue'
import CcToggle from '../CcToggle.vue'
import { useSettingsStore } from '../../stores/settings'
import type { LandscapeResult } from '../../utils/landscape'
import { LENS_RADIUS_MIN, LENS_RADIUS_MAX } from '../../utils/viewer/channelLens'

/** `timed`: more than one frame — the timestamp's size only matters when one is drawn */
defineProps<{ landscape: LandscapeResult | null; timed: boolean }>()
const settings = useSettingsStore()
</script>

<template>
  <CollapsibleSection label="Annotations" tip="Overlay text sizes, and the grid for captures"
                      accordion="cc.vw.section" section="ann"
                      fill="content">
    <!-- The switches are the row of buttons under the panel's top controls (`VwOverlayToggles`); a
         size slider shows here only while the thing it sizes is on. -->
    <div v-if="settings.viewerScaleBar" class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs cc-lbl-col">Scale bar</span>
      <input
        type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
        v-model.number="settings.viewerScaleBarPx"
        v-tooltip.bottom="'Scale-bar text size'" aria-label="Scale bar text size"
      >
      <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerScaleBarPx }}</span>
    </div>
    <div v-if="timed && settings.viewerTimestamp" class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs cc-lbl-col">Time</span>
      <input
        type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
        v-model.number="settings.viewerTimestampPx"
        v-tooltip.bottom="'Timestamp text size'" aria-label="Timestamp text size"
      >
      <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerTimestampPx }}</span>
    </div>
    <div v-if="settings.viewerLens" class="cc-row cc-row-tight">
      <span class="cc-muted cc-fs-2xs cc-lbl-col">Lens</span>
      <input
        type="range" class="vw-grow vw-px" :min="LENS_RADIUS_MIN" :max="LENS_RADIUS_MAX" :step="10"
        v-model.number="settings.viewerLensRadius"
        v-tooltip.bottom="'Lens radius'" aria-label="Lens radius"
      >
      <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerLensRadius }}</span>
    </div>
    <div class="cc-eyebrow cc-fs-3xs vw-ann-group">For captures</div>
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
.vw-ann-group { margin-top: 0.2rem; }
.vw-lg-swatch { display: inline-block; width: 10px; height: 10px; border-radius: var(--cc-radius-xs); border: 1px solid var(--cc-border); }
</style>
