<script setup lang="ts">
/**
 * A figure that draws NUMBERS as shapes, in a labelled grid — one row per quantity, one column per
 * thing being compared.
 *
 * Deliberately not about segmentation, or about task params. It takes a computed `VisColumns` and
 * draws it; whatever produced that is the caller's business (`tasks/paramVis.ts` builds one from a
 * repeatable param group, but a plot legend, a QC summary or a model manifest could build one too).
 * The reason to have it at all: a column of ten numbers does not answer "are these two things
 * different", and two shapes of visibly different size do.
 *
 * Five shapes, because five kinds of quantity read differently:
 *   diameter - a circle of that size          (something's extent)
 *   blur     - a soft ring                    (a fuzziness, not an outline)
 *   distance - a span with end caps           (a reach, not an object)
 *   area     - a disc whose AREA is the value (so the radius carries the square root)
 *   fraction - a marker on a 0-1 track        (a setting, not a size)
 *
 * Shapes only. Every number, scale and comparison decision is in the producer, so it can be tested
 * without mounting anything.
 */
import { computed } from 'vue'
import { type VisColumns, MAX_R, TRACK } from '../tasks/paramVis'
import InlineNote from './InlineNote.vue'
import type { Severity } from '../lib/severity'

const props = defineProps<{
  /** the figure, already computed — see `tasks/paramVis.ts` for one producer */
  vis: VisColumns
  /** column headings; falls back to 1-based numbering */
  headings?: string[]
  /** a line under the figure, when the caller has something to say about it */
  note?: string
  /** severity of that line — the canonical union, not a second spelling of it */
  noteSeverity?: Severity
}>()

const vis = computed(() => props.vis)

/** Column width and row height, in the SVG's own units. */
const COL = 78
const ROW = MAX_R * 2 + 4

const heading = (i: number) => props.headings?.[i] ?? String(i + 1)
</script>

<template>
  <div v-if="vis.rows.length" class="visual-aid">
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

    <!-- Whatever the caller wants said about the figure. `InlineNote` is the canonical
         short-line-plus-reasoning primitive (docs/ui/PRIMITIVES.md). -->
    <InlineNote v-if="props.note" class="vis-warn cc-fs-2xs" :severity="props.noteSeverity"
      placement="bottom" :short="props.note" />
  </div>
</template>

<style scoped>
.visual-aid { margin: 0.25rem 0 0.6rem; }

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
