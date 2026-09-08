<!--
  Shared "one cell in a strip/grid" primitive — image + optional ViewLegend + StillOverlay
  (scale bar / timestamp) + click target + slotted actions. Consumed by ImageStripView (the
  Analysis-board filmstrip) and CellCardsView (per-cluster cards). Extracted per
  docs/todo/CELL_CARDS_PLAN.md → "make a common helper" so the two views share cell chrome by
  construction instead of drifting.

  Deliberately owns no capture/recapture/remove logic — that's specific to ImageStripView, which
  supplies its own buttons via <slot name="actions">. The cell chrome (padding/positioning of the
  overlays and legend, click target) is what's shared.
-->
<script setup lang="ts">
import ViewLegend from '../ViewLegend.vue'
import StillOverlay from '../StillOverlay.vue'

interface ExtentUm { x?: number; y?: number; unit?: string | null }
interface LegendSection { title: string; items: { label: string; colour: string }[] }

const props = defineProps<{
  src?: string
  alt?: string
  extentUm?: ExtentUm | null
  timeLabel?: string
  showScaleBar?: boolean
  showTimestamp?: boolean
  showLegend?: boolean
  legendSections?: LegendSection[]
  /** Optional 2px inset ring around the image (e.g. a card's pop colour). */
  ringColour?: string
}>()
const emit = defineEmits<{ (e: 'click'): void }>()
</script>

<template>
  <div class="strip-cell" :style="ringColour ? { boxShadow: `inset 0 0 0 2px ${ringColour}` } : {}"
       @click="emit('click')">
    <img v-if="src" :src="src" :alt="alt || ''" class="sc-img" />
    <div v-else class="cc-empty cc-empty-overlay"><slot name="empty" /></div>
    <StillOverlay v-if="src && (showScaleBar || showTimestamp)"
                  :extent-um="extentUm" :time-label="timeLabel || ''"
                  :show-scale-bar="!!showScaleBar" :show-timestamp="!!showTimestamp" />
    <ViewLegend v-if="src && showLegend && legendSections && legendSections.length"
                :sections="legendSections" :swatch="9" vertical class="sc-legend" />
    <div class="sc-actions"><slot name="actions" /></div>
  </div>
</template>

<style scoped>
.strip-cell { position: relative; flex: 1; min-width: 0; min-height: 120px; display: flex;
  flex-direction: column; overflow: hidden; background: var(--cc-bg); }
/* contain (not cover) so the whole frame shows — see ImageStripView's original comment for why.
   The card grid inherits the same trade-off (letterbox rather than cropped medoid). */
.sc-img { flex: 1; width: 100%; object-fit: contain; min-height: 0; }
/* legend chip anchored bottom-left, on-image styling. Copied verbatim from ImageStripView so the
   two views draw the legend identically. */
.sc-legend { position: absolute; bottom: 6px; left: 6px; padding: 3px 5px; border-radius: var(--cc-radius-xs);
  background: rgba(0,0,0,0.45); pointer-events: none; z-index: 7;
  color: #fff; font-size: var(--cc-fs-2xs); font-weight: 600; text-shadow: 0 1px 2px rgba(0,0,0,0.85); }
/* Action slot: bottom-left, auto-hide on hover — same policy ImageStripView's `.is-actions` had. */
.sc-actions { position: absolute; bottom: 4px; left: 4px; display: flex; gap: 4px; z-index: 8;
  opacity: 0; transition: opacity 0.12s ease; }
.strip-cell:hover .sc-actions { opacity: 1; }
</style>
