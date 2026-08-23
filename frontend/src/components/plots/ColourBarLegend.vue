<!--
  The colour-by legend for a MONTAGE: one bar for the whole grid, not one per tile. Every tile of a
  montage shares the same ramp (the range is whole-dataset — docs/POPULATION.md → Colour by a third
  measure), and a 9px bar with its labels inside a 200px pairs tile would cost more of the tile than it
  explains — so the host draws this once beside the grid and passes `:value-legend="false"` to the tiles.

  It renders the SAME SVG body the plot's own bar and the figure export use (plots/valueColour
  `colourBarSvg`), so a legend cannot describe the ramp differently on screen than in a PDF. `ink` is
  `currentColor` here, so the themed colour (and the light-theme flip the export applies) comes for free;
  a caller stitching this into a standalone figure asks for the body again with a concrete colour.
-->
<script setup lang="ts">
import { computed, useTemplateRef } from 'vue'
import { colourBarSvg, type BarBox } from '../../plots/valueColour'

const props = withDefaults(defineProps<{
  extent: [number, number]
  ticks: { pos: number; label: string }[]
  label?: string
  width?: number                       // bar length (px) — the strip is this wide plus its frame
  orient?: 'v' | 'h'
}>(), { width: 132, orient: 'h' })

const FS = 9, BAR = 8, CAP = FS + 3    // caption line above the bar, tick labels below it
const box = computed<BarBox>(() => props.orient === 'h'
  ? { x: 0.5, y: CAP, w: props.width - 1, h: BAR }
  : { x: props.width - BAR - 0.5, y: CAP, w: BAR, h: props.width })
const size = computed(() => props.orient === 'h'
  ? { w: props.width, h: CAP + BAR + FS + 2 }
  : { w: props.width, h: CAP + props.width + 2 })

const body = (ink: string) => colourBarSvg(box.value, {
  extent: props.extent, ticks: props.ticks, label: props.label, ink, fontSize: FS, orient: props.orient })

const hostEl = useTemplateRef<HTMLElement>('hostEl')
// the host stitches this into a standalone figure: the same body with a concrete ink, plus where the
// strip sits on screen so it can be translated into the capture
defineExpose({ svgBody: (ink: string) => body(ink), getEl: () => hostEl.value })
</script>

<template>
  <div ref="hostEl" class="cb-legend" :style="{ width: `${size.w}px`, height: `${size.h}px` }">
    <svg :width="size.w" :height="size.h" :viewBox="`0 0 ${size.w} ${size.h}`" v-html="body('currentColor')" />
  </div>
</template>

<style scoped>
/* currentColor drives the bar frame + labels, so the themed dim ink (and the light-theme flip the PDF
   export puts on an ancestor) applies with no per-mode branch here */
.cb-legend { flex: none; color: var(--cc-text-dim); line-height: 0; }
</style>
