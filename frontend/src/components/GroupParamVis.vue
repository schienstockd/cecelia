<script setup lang="ts">
/**
 * One column per entry of a repeatable group, each parameter drawn to scale — the picture beside
 * coastal's model group.
 *
 * Ten numbers per pass in microns do not answer the question that decides whether a two-pass
 * segmentation works: are these two passes looking for different objects? They have to be, because
 * entries are applied in order and each labels only what an earlier one left. Two circles of visibly
 * different size and two markers at opposite ends of a track answer it at a glance — and two
 * identical columns answer it just as clearly, which is the case worth catching before a 500-second
 * run rather than after.
 *
 * All geometry is in `paramVis.ts` so it can be tested without mounting anything. This file is shapes.
 */
import { computed } from 'vue'
import type { ParamDef, ParamValues } from '../tasks/types'
import { paramVisColumns, uniformWarning, MAX_R, TRACK } from '../tasks/paramVis'
import InlineNote from './InlineNote.vue'

const props = defineProps<{
  param: ParamDef
  values: Record<string, ParamValues>
  /** entry keys in RUN order */
  order: string[]
  /** µm per pixel of the image this will run on, when known */
  pxSize?: number | null
  /** entry heading text, one per column — the form's own numbering or model names */
  headings?: string[]
}>()

const vis = computed(() => paramVisColumns(props.param, props.values, props.order, props.pxSize))
const warn = computed(() => uniformWarning(vis.value))

/** Column width and the row height, in the SVG's units. */
const COL = 78
const ROW = MAX_R * 2 + 4

const heading = (i: number) => props.headings?.[i] ?? String(i + 1)
</script>

<template>
  <div v-if="vis.rows.length" class="param-vis">
    <div class="vis-grid" :style="{ gridTemplateColumns: `7.5rem repeat(${vis.columns.length}, 1fr)` }">
      <!-- corner -->
      <div class="vis-corner cc-eyebrow cc-fs-2xs">
        {{ vis.pxSize ? 'in image pixels' : 'in form units' }}
      </div>
      <div v-for="(c, i) in vis.columns" :key="`h-${c}`" class="vis-head cc-eyebrow cc-fs-2xs">
        {{ heading(i) }}
      </div>

      <template v-for="row in vis.rows" :key="row.key">
        <div class="vis-label cc-muted cc-fs-2xs" :class="{ 'is-uniform': row.uniform }">
          {{ row.label }}
        </div>
        <div v-for="(cell, i) in row.cells" :key="`${row.key}-${i}`" class="vis-cell">
          <svg :viewBox="`0 0 ${COL} ${ROW}`" class="vis-svg" role="img"
            :aria-label="`${row.label}: ${cell.text}`">
            <!-- a circle whose radius IS the value, relative to the widest column in this row -->
            <circle v-if="row.role === 'diameter'"
              :cx="COL / 2" :cy="MAX_R + 2" :r="cell.r" class="sh-diameter" />

            <!-- blur as a soft edge: the sigma is a fuzziness, not an outline -->
            <template v-else-if="row.role === 'blur'">
              <circle v-if="cell.r > 0" :cx="COL / 2" :cy="MAX_R + 2" :r="cell.r"
                class="sh-blur" :style="{ filter: `blur(${Math.max(1, cell.r / 2.5)}px)` }" />
              <line v-else :x1="COL / 2 - 6" :y1="MAX_R + 2" :x2="COL / 2 + 6" :y2="MAX_R + 2"
                class="sh-off" />
            </template>

            <!-- a span, drawn as a span: this one is a search radius, not an object -->
            <template v-else-if="row.role === 'distance'">
              <line :x1="COL / 2 - cell.r" :y1="MAX_R + 2" :x2="COL / 2 + cell.r" :y2="MAX_R + 2"
                class="sh-distance" />
              <circle :cx="COL / 2 - cell.r" :cy="MAX_R + 2" r="1.6" class="sh-cap" />
              <circle :cx="COL / 2 + cell.r" :cy="MAX_R + 2" r="1.6" class="sh-cap" />
            </template>

            <!-- AREA, so the disc's area carries the value (radius is its sqrt — see paramVis) -->
            <circle v-else-if="row.role === 'area'"
              :cx="COL / 2" :cy="MAX_R + 2" :r="cell.r" class="sh-area" />

            <!-- a threshold on its own 0–1 track, so two passes at opposite ends look opposite -->
            <template v-else>
              <line :x1="(COL - TRACK) / 2" :y1="MAX_R + 2" :x2="(COL + TRACK) / 2" :y2="MAX_R + 2"
                class="sh-track" />
              <circle :cx="(COL - TRACK) / 2 + cell.at * TRACK" :cy="MAX_R + 2" r="4"
                class="sh-marker" />
            </template>

          </svg>
          <!-- The number as TEXT, not an SVG <text>: it reflows, respects the user's font size, and
               needs no hardcoded px inside a viewBox. -->
          <div class="vis-cap cc-muted cc-fs-2xs">{{ cell.text }}</div>
        </div>
      </template>
    </div>

    <!-- The one state this picture exists to catch, said in words as well as shape. `InlineNote` is
         the canonical short-line-plus-reasoning primitive (docs/ui/PRIMITIVES.md) — an icon and a
         span with a tooltip by hand is the variant it exists to delete. -->
    <InlineNote v-if="warn" class="vis-warn cc-fs-2xs" severity="warn" placement="bottom"
      :short="warn"
      detail="Entries are applied in order and each labels only what an earlier one left, so a pass
              configured like the one before it grows to the same regions and is then clipped along
              their boundaries." />
  </div>
</template>

<style scoped>
.param-vis { margin: 0.25rem 0 0.6rem; }

.vis-grid {
  display: grid;
  align-items: center;
  column-gap: 0.25rem;
  row-gap: 0.1rem;
}

.vis-corner, .vis-head { padding-bottom: 0.2rem; }
.vis-head { text-align: center; }

.vis-label { text-align: right; padding-right: 0.4rem; line-height: 1.2; }
/* A row that is identical across passes is the thing to notice, so it is the thing that is marked. */
.vis-label.is-uniform { color: var(--cc-warn); }

.vis-cell { display: flex; flex-direction: column; align-items: center; gap: 0.1rem; }
.vis-cap { text-align: center; }
.vis-svg { width: 100%; max-width: 5rem; height: auto; overflow: visible; }

/* Shapes read as "the object" (filled), "the reach" (a line), "the setting" (a marker). */
.sh-diameter { fill: var(--cc-accent-tint); stroke: var(--cc-accent); stroke-width: 1; }
.sh-blur     { fill: var(--cc-accent); opacity: 0.35; }
.sh-area     { fill: var(--cc-accent); opacity: 0.55; }
.sh-distance { stroke: var(--cc-accent); stroke-width: 1.5; }
.sh-cap      { fill: var(--cc-accent); }
.sh-off      { stroke: var(--cc-text-dim); stroke-width: 1; stroke-dasharray: 2 2; }
.sh-track    { stroke: var(--cc-border); stroke-width: 1.5; stroke-linecap: round; }
.sh-marker   { fill: var(--cc-accent); }

.vis-warn { margin-top: 0.35rem; }
</style>
