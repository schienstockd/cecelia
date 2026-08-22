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
        {{ vis.pxSize ? `${Number(vis.pxSize.toFixed(3))} µm/px` : '' }}
      </div>
      <div v-for="(c, i) in vis.columns" :key="`h-${c}`" class="vis-head cc-eyebrow cc-fs-2xs">
        {{ heading(i) }}
      </div>

      <!-- `ri` only to drop the rule under the LAST row: a grid row is not an element, so the divider
           has to be a border on each of its cells, and `:last-child` cannot see row boundaries. -->
      <template v-for="(row, ri) in vis.rows" :key="row.key">
        <div class="vis-label cc-muted cc-fs-2xs"
          :class="{ 'is-uniform': row.uniform, 'is-last': ri === vis.rows.length - 1 }">
          {{ row.label }}
        </div>
        <div v-for="(cell, i) in row.cells" :key="`${row.key}-${i}`" class="vis-cell"
          :class="{ 'is-last': ri === vis.rows.length - 1 }">
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

            <!-- A threshold as a FILLED GAUGE on its own 0-1 rail, so two passes at opposite ends
                 look opposite. Deliberately not a rail plus a round handle: that is exactly what the
                 real sliders in the form look like, and a control you cannot move is worse than a
                 picture — you try to drag it. A filled bar reads as a reading. -->
            <template v-else>
              <rect :x="(COL - TRACK) / 2" :y="MAX_R" :width="TRACK" height="4" rx="2"
                class="sh-rail" />
              <rect :x="(COL - TRACK) / 2" :y="MAX_R" :width="Math.max(1, cell.at * TRACK)"
                height="4" rx="2" class="sh-fill" />
            </template>

          </svg>
          <!-- The number as TEXT, not an SVG <text>: it reflows, respects the user's font size, and
               needs no hardcoded px inside a viewBox. In the FORM's units, matching the row label and
               the control being edited; pixels are the dimmer second line, because that is what the
               engine receives and what a pixel-tuned reference is checked against. -->
          <div class="vis-cap cc-muted cc-fs-2xs">{{ cell.text }}</div>
          <div v-if="cell.pxText" class="vis-cap-px cc-fs-2xs">{{ cell.pxText }}</div>
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
  row-gap: 0;
}

.vis-corner, .vis-head { padding-bottom: 0.2rem; }
.vis-head { text-align: center; }

/* One rule per row, so eleven rows read as a list rather than as floating shapes. On the CELLS as
   well as the label, because a grid row is not an element and a single border would stop at the
   label's edge. `row-gap: 0` so the rule sits between rows instead of inside a gap. */
.vis-label, .vis-cell { border-bottom: 1px solid var(--cc-border); padding: 0.3rem 0; }
.vis-label.is-last, .vis-cell.is-last { border-bottom: 0; }
.vis-label { text-align: right; padding-right: 0.4rem; line-height: 1.2; }
/* A row that is identical across passes is the thing to notice, so it is the thing that is marked. */
.vis-label.is-uniform { color: var(--cc-warn); }

.vis-cell { display: flex; flex-direction: column; align-items: center; gap: 0.05rem; }
.vis-cap { text-align: center; }
/* The engine-facing number, subordinate to the one that matches the label. */
.vis-cap-px { text-align: center; color: var(--cc-text-dim); opacity: 0.75; }
.vis-svg { width: 100%; max-width: 5rem; height: auto; overflow: visible; }

/* Shapes read as "the object" (filled), "the reach" (a line), "the setting" (a marker). */
.sh-diameter { fill: var(--cc-accent-tint); stroke: var(--cc-accent); stroke-width: 1; }
.sh-blur     { fill: var(--cc-accent); opacity: 0.35; }
.sh-area     { fill: var(--cc-accent); opacity: 0.55; }
.sh-distance { stroke: var(--cc-accent); stroke-width: 1.5; }
.sh-cap      { fill: var(--cc-accent); }
.sh-off      { stroke: var(--cc-text-dim); stroke-width: 1; stroke-dasharray: 2 2; }
.sh-rail     { fill: var(--cc-border); }
.sh-fill     { fill: var(--cc-accent); }

.vis-warn { margin-top: 0.35rem; }
</style>
