<!--
  Custom plate builder for the Analysis board (docs/ANALYSIS.md). Set the grid size, then DRAG across
  cells to MERGE them into one panel (a rectangular span); click a merged block to split it back to 1×1s.
  Emits the resulting LayoutTemplate on Apply — the board then applyTemplate()s it, preserving slot
  contents by index. All the span math is the pure, unit-tested utils/plateBuilder.ts; this is just the
  drag UI. Seeded from the current plate so you can tweak rather than start over.
-->
<script setup lang="ts">
import { ref, computed, watch, onBeforeUnmount } from 'vue'
import { normSpan, applyDrag, clampSpans, slotsToSpans, buildPlate, type PlateSpan, type Cell } from '../../utils/plateBuilder'
import type { LayoutTemplate } from '../../plots/layoutTemplates'

const props = defineProps<{ cols: number; rows: number; slotAreas: string[] }>()
const emit = defineEmits<{ apply: [LayoutTemplate]; cancel: [] }>()

const MIN = 1, MAX = 6
const clamp = (v: number) => Math.max(MIN, Math.min(MAX, Math.round(v || MIN)))
const cols = ref(clamp(props.cols))
const rows = ref(clamp(props.rows))
const spans = ref<PlateSpan[]>(slotsToSpans(props.slotAreas))
// resizing the grid must not leave spans hanging outside it
watch([cols, rows], () => { spans.value = clampSpans(spans.value, cols.value, rows.value) })

const gridCells = computed<Cell[]>(() => {
  const out: Cell[] = []
  for (let r = 0; r < rows.value; r++) for (let c = 0; c < cols.value; c++) out.push({ r, c })
  return out
})
const gridTmpl = computed(() => ({
  gridTemplateColumns: `repeat(${cols.value}, 1fr)`, gridTemplateRows: `repeat(${rows.value}, 1fr)` }))
const areaStyle = (s: PlateSpan) => ({ gridArea: `${s.r0 + 1} / ${s.c0 + 1} / ${s.r1 + 2} / ${s.c1 + 2}` })

// ── drag to merge / click to split ────────────────────────────────────────────
const dragging = ref(false)
const start = ref<Cell | null>(null)
const hover = ref<Cell | null>(null)
const dragRect = computed(() => (dragging.value && start.value && hover.value) ? normSpan(start.value, hover.value) : null)
function onDown(cell: Cell) { start.value = hover.value = cell; dragging.value = true; window.addEventListener('pointerup', onUp) }
function onEnter(cell: Cell) { if (dragging.value) hover.value = cell }
function onUp() {
  window.removeEventListener('pointerup', onUp)
  if (dragging.value && start.value && hover.value) spans.value = applyDrag(spans.value, normSpan(start.value, hover.value))
  dragging.value = false; start.value = hover.value = null
}
onBeforeUnmount(() => window.removeEventListener('pointerup', onUp))

const slotCount = computed(() => buildPlate(cols.value, rows.value, spans.value).slots.length)
function apply() { emit('apply', buildPlate(cols.value, rows.value, spans.value)) }
</script>

<template>
  <div class="pb">
    <div class="pb-head">
      <label class="pb-num"><span>cols</span>
        <input type="number" :min="MIN" :max="MAX" v-model.number="cols" @change="cols = clamp(cols)" /></label>
      <label class="pb-num"><span>rows</span>
        <input type="number" :min="MIN" :max="MAX" v-model.number="rows" @change="rows = clamp(rows)" /></label>
      <span class="pb-hint">drag to merge · click a merge to split</span>
    </div>

    <div class="pb-grid" :style="gridTmpl">
      <div v-for="cell in gridCells" :key="`${cell.r}-${cell.c}`" class="pb-cell"
           @pointerdown.prevent="onDown(cell)" @pointerenter="onEnter(cell)" />
      <div v-for="(s, i) in spans" :key="`s${i}`" class="pb-span" :style="areaStyle(s)" />
      <div v-if="dragRect" class="pb-drag" :style="areaStyle(dragRect)" />
    </div>

    <div class="pb-foot">
      <span class="pb-count">{{ slotCount }} panel{{ slotCount === 1 ? '' : 's' }}</span>
      <span class="pb-spacer" />
      <button class="cc-btn cc-btn-ghost" type="button" @click="emit('cancel')">Cancel</button>
      <button class="cc-btn cc-btn-primary" type="button" @click="apply">Apply</button>
    </div>
  </div>
</template>

<style scoped>
.pb { display: flex; flex-direction: column; gap: 8px; width: 15rem; }
.pb-head { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
.pb-num { display: inline-flex; align-items: center; gap: 4px; font-size: 11px; color: var(--cc-text-dim); }
.pb-num input { width: 3rem; font-size: 12px; padding: 2px 4px; }
.pb-hint { flex-basis: 100%; font-size: 10px; color: var(--cc-text-dim); opacity: 0.75; }
/* the plate canvas: an A4-portrait-ish aspect so the preview reads like a page */
.pb-grid { position: relative; display: grid; gap: 3px; aspect-ratio: 0.78; width: 100%;
  padding: 3px; background: var(--cc-border); border-radius: 5px; user-select: none; touch-action: none; }
.pb-cell { background: var(--cc-surface-2); border-radius: 3px; cursor: crosshair; }
.pb-cell:hover { background: var(--cc-surface-1); }
/* a merged span: one contiguous block over the cells it covers (spanning a grid-area covers the
   internal gaps automatically); pointer-events none so a click passes through to the cell to split */
.pb-span { pointer-events: none; z-index: 2; border-radius: 3px;
  background: color-mix(in srgb, var(--cc-accent) 30%, var(--cc-surface-2));
  border: 1px solid var(--cc-accent); }
/* live drag highlight */
.pb-drag { pointer-events: none; z-index: 3; border-radius: 3px;
  background: color-mix(in srgb, var(--cc-accent) 22%, transparent); border: 1px dashed var(--cc-accent); }
.pb-foot { display: flex; align-items: center; gap: 6px; }
.pb-count { font-size: 11px; color: var(--cc-text-dim); }
.pb-spacer { flex: 1; }
</style>
