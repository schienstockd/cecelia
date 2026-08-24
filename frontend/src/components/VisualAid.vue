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
 * Five shapes. Four are kinds of SIZE, which read differently:
 *   diameter - a circle of that size          (something's extent)
 *   blur     - a soft ring                    (a fuzziness, not an outline)
 *   distance - a span with end caps           (a reach, not an object)
 *   area     - a disc whose AREA is the value (so the radius carries the square root)
 *
 * …and `grid` is not a size at all: a small field of values, optionally several frames of one, cycled
 * on a timer. It exists because some choices are not distinguishable by magnitude — median vs gated
 * smoothing have the SAME window, the same everything a circle could draw, and differ only in what
 * they do to a moving spot. A still picture of that comparison is a picture of nothing.
 *
 * The grid is the ONE row that breaks *one line per row* below, and the exception is the whole reason
 * the rule holds elsewhere: every other row is a shape ALONGSIDE the number it duplicates, so it can
 * afford to be 16px. A grid IS the content — there is no number beside it saying the same thing — so
 * it gets a band.
 *
 * ONE index for every grid row in the figure, not one timer each. Two independently-scheduled rows
 * drift apart within seconds, and a comparison of two methods at two different timepoints is a
 * comparison of nothing.
 *
 * A `text` row (which model, matched as what, on which channels) gets no shape either — the value IS
 * the content, and it comes first, because it is what you check before any number. An empty channel
 * list reads `none` rather than blank: it resolves to channel 0 downstream and segments something
 * nobody picked, so a blank cell would hide a real mistake.
 *
 * A `fraction` gets NO shape — just its number. It was a rail with a filled bar, and before that a
 * rail with a handle; both cost a row of height to say what "0.2" beside "0.8" already says, and the
 * handled version looked like the real sliders in the form. A picture is worth having when it shows a
 * ratio the number does not. For a plain 0-1 setting it does not.
 *
 * ONE LINE PER ROW. The shapes sit inline in the text, at 16px, not stacked above a caption in their
 * own 44px band: eleven of those made a panel taller than the screen, and a 16px circle shows a ratio
 * exactly as well.
 *
 * Shapes only. Every number, scale and comparison decision is in the producer, so it can be tested
 * without mounting anything.
 */
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { type VisColumns, type VisFrame, MAX_R } from '../tasks/paramVis'
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

/** The inline shape's box, in CSS px. `MAX_R` is its radius, so the box is a shade wider. */
const SZ = MAX_R * 2 + 2

/** The `grid` band, in CSS px — see the header on why this one row is not 16px like the rest. */
const GRID_SZ = 48

const heading = (i: number) => props.headings?.[i] ?? String(i + 1)

/**
 * ── the sequence ──────────────────────────────────────────────────────────────────────────────
 * How fast is a judgement about legibility, not physics: the frames are a schematic of motion, not
 * the movie's own rate, and below ~120ms the eye reads a flicker rather than a direction.
 */
const FRAME_MS = 220

const frame = ref(0)
let timer: number | undefined

const hasSequence = computed(() =>
  vis.value.rows.some(r => r.role === 'grid' && r.cells.some(c => (c.frames?.length ?? 0) > 1)))

/**
 * Everything else in this float is a still image, so this is the first thing in it that moves on its
 * own — and something that moves without being asked is exactly what `prefers-reduced-motion` is for.
 * Reduced motion still gets the FIRST frame of every sequence: the comparison is legible standing
 * still, just less vivid, which is better than an empty row.
 *
 * Nothing else is needed to stop the timer when the panel closes: `ParamFigure` renders the float
 * with `v-if`, so closing it unmounts this component and `onBeforeUnmount` fires. A `v-show` there
 * would leave the interval running for a panel nobody has open — which is why it must stay `v-if`.
 */
onMounted(() => {
  if (!hasSequence.value) return
  if (window.matchMedia?.('(prefers-reduced-motion: reduce)').matches) return
  timer = window.setInterval(() => { frame.value++ }, FRAME_MS)
})
onBeforeUnmount(() => { if (timer !== undefined) window.clearInterval(timer) })

/**
 * The frame to show for one cell. Modulo per cell rather than a shared counter reset: two sequences of
 * different length still both loop, and the shared `frame` keeps them phase-locked at index 0.
 */
function frameOf(frames: VisFrame[] | undefined): VisFrame | null {
  if (!frames?.length) return null
  return frames[frame.value % frames.length]
}

/** Cell geometry for a grid frame — square cells, sized to fit the band. */
function cellSize(f: VisFrame): number {
  const cols = Math.max(1, ...f.map(r => r.length))
  return GRID_SZ / Math.max(cols, f.length)
}
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
        <!-- A `grid` row draws its FRAMES, one per column, like every other role. It briefly had a
             `span` mode for a row that was the shared INPUT to the columns beside it; that figure now
             gives the input a COLUMN instead, which compares better, and a spanning row with no
             consumer is machinery waiting to grow a second way of doing this. -->
        <template v-if="row.role === 'grid'">
          <div v-for="(cell, i) in row.cells"
            :key="`${row.key}-${i}`" class="vis-cell vis-cell-grid cc-fs-2xs"
            :class="{ 'is-last': ri === vis.rows.length - 1 }">
            <svg class="vis-grid-svg" :width="GRID_SZ" :height="GRID_SZ"
              :viewBox="`0 0 ${GRID_SZ} ${GRID_SZ}`" role="img"
              :aria-label="`${row.label}${cell.text ? `: ${cell.text}` : ''}`">
              <template v-if="frameOf(cell.frames)">
                <rect v-for="(v, k) in frameOf(cell.frames)!.flat()" :key="k"
                  :x="(k % frameOf(cell.frames)![0].length) * cellSize(frameOf(cell.frames)!)"
                  :y="Math.floor(k / frameOf(cell.frames)![0].length) * cellSize(frameOf(cell.frames)!)"
                  :width="cellSize(frameOf(cell.frames)!)" :height="cellSize(frameOf(cell.frames)!)"
                  class="sh-grid" :fill-opacity="v" />
              </template>
            </svg>
            <span v-if="cell.text" class="vis-num">{{ cell.text }}</span>
          </div>
        </template>

        <div v-else v-for="(cell, i) in row.cells" :key="`${row.key}-${i}`" class="vis-cell cc-fs-2xs"
          :class="{ 'is-last': ri === vis.rows.length - 1 }">
          <!-- The shape INLINE with its number, so a row is one line of text tall. A `fraction` has
               no shape at all — see the header. -->
          <svg v-if="row.role !== 'fraction' && row.role !== 'text'" class="vis-svg"
            :width="SZ" :height="SZ"
            :viewBox="`0 0 ${SZ} ${SZ}`" role="img" :aria-label="`${row.label}: ${cell.text}`">
            <circle v-if="row.role === 'diameter'" :cx="SZ / 2" :cy="SZ / 2" :r="cell.r"
              class="sh-diameter" />
            <circle v-else-if="row.role === 'area'" :cx="SZ / 2" :cy="SZ / 2" :r="cell.r"
              class="sh-area" />
            <circle v-else-if="row.role === 'blur' && cell.r > 0" :cx="SZ / 2" :cy="SZ / 2"
              :r="cell.r" class="sh-blur"
              :style="{ filter: `blur(${Math.max(0.5, cell.r / 2.5)}px)` }" />
            <template v-else-if="row.role === 'distance'">
              <line :x1="SZ / 2 - cell.r" :y1="SZ / 2" :x2="SZ / 2 + cell.r" :y2="SZ / 2"
                class="sh-distance" />
            </template>
            <!-- blur at zero: a dim dot, not a dashed rule that reads like a control -->
            <circle v-else :cx="SZ / 2" :cy="SZ / 2" r="1" class="sh-off" />
          </svg>
          <span class="vis-num">{{ cell.text }}</span>
          <span v-if="cell.pxText" class="vis-px">{{ cell.pxText }}</span>
        </div>
      </template>
    </div>

    <!-- Whatever the caller wants said about the figure. `InlineNote` is the canonical
         short-line-plus-reasoning primitive (docs/ui/PRIMITIVES.md). -->
    <InlineNote v-if="props.note" class="vis-warn cc-fs-2xs" :severity="props.noteSeverity"
      :short="props.note" />
  </div>
</template>

<style scoped>
/* Pads ITSELF. `FloatingPanel`'s `.fp-body` has no padding on purpose — `LabLogPanel` needs its
   section dividers to run full-bleed to the panel edge — so a consumer that wants inset content pads
   its own root, as `ViewerPanel` does. */
.visual-aid { padding: 0.35rem 0.6rem 0.45rem; }

/* One line per row. `align-items: stretch` + the SAME symmetric padding on the label and the cells is
   what makes the label line up with its row — `center` cannot align two boxes whose padding differs,
   which is why the titles sat high through three attempts. */
.vis-grid {
  display: grid;
  align-items: stretch;
  column-gap: 0.4rem;
  row-gap: 0;
}

.vis-corner, .vis-head { padding: 0 0 0.25rem; }
.vis-head { text-align: center; }

/* The rule per row, on the cells as well as the label: a grid row is not an element, so one border
   would stop at the label's edge. */
.vis-label, .vis-cell {
  border-bottom: 1px solid var(--cc-border);
  padding: 0.22rem 0;
  display: flex;
  align-items: center;
  line-height: 1.25;
}
.vis-label.is-last, .vis-cell.is-last { border-bottom: 0; }

.vis-label { justify-content: flex-end; text-align: right; }
/* A row identical across every column is the thing to notice, so it is the thing that is marked. */
.vis-label.is-uniform { color: var(--cc-warn); }

.vis-cell { gap: 0.3rem; }
/* The one row that is a band rather than a line — see the header. Centred, because a grid has no
   baseline to sit on the way an inline shape does. */
.vis-cell-grid { justify-content: center; gap: 0.35rem; padding: 0.3rem 0; }
.vis-grid-svg { flex: 0 0 auto; }
.vis-svg { flex: 0 0 auto; overflow: visible; }
.vis-num { font-variant-numeric: tabular-nums; min-width: 0; overflow-wrap: anywhere; }
/* The engine-facing number, subordinate to the one that matches the label. */
.vis-px { color: var(--cc-text-dim); opacity: 0.75; font-variant-numeric: tabular-nums; }

/* Shapes read as "the object" (filled), "the reach" (a line), "off" (a dim dot). */
.sh-diameter { fill: var(--cc-accent-tint); stroke: var(--cc-accent); stroke-width: 1; }
.sh-blur     { fill: var(--cc-accent); opacity: 0.35; }
.sh-area     { fill: var(--cc-accent); opacity: 0.55; }
.sh-distance { stroke: var(--cc-accent); stroke-width: 1.5; }
.sh-off      { fill: var(--cc-text-dim); opacity: 0.5; }
/* Value as OPACITY over the panel's own background, so a grid is legible in both themes without a
   second colour scale to keep in step with the tokens. */
.sh-grid     { fill: var(--cc-accent); shape-rendering: crispEdges; }

.vis-warn { margin-top: 0.3rem; }
</style>
