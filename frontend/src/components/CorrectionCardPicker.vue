<!--
  CorrectionCardPicker — the acquisition-card selector for CorrectionPlanPanel. The card IS the
  vis: an immunologist recognizes their data by its appearance rather than by the scanner name
  ("was this resonance or galvo?"). The recognition surface is the point — a text-only pill row
  shipped that up-front decision to a phrase most users cannot answer without asking.

  **Bespoke by convention.** `ChipSelect`'s docstring explicitly says colour-swatch grids stay
  bespoke; `VisualAid` is a table-of-comparisons, not a single-image face. Neither is the right
  primitive here, so the grid layout and per-card selected state live in this component's scoped
  CSS.

  **The card face is an N x N intensity grid — same pixel-rendering primitive `VisualAid` uses
  for its `grid` role.** Each cell is a `<rect>` with `fill-opacity` = intensity. Reads as
  microscopy because it IS microscopy-shaped; line-art dots on a black field looked like a legend
  key, not the thing itself.

  **Frame cycling** for the animated cards (`resonance`, `spinning_disk`) uses the same pattern as
  `VisualAid.vue`: one `setInterval` while at least one animated figure is mounted, a
  reduced-motion escape hatch, `onBeforeUnmount` to clear. If a third site needs the same, extract.
-->
<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { N, cardFigure, isAnimated } from '../tasks/cardVis'
import type { AcquisitionPresetSummary } from '../types/correctionPlan'

const props = defineProps<{
  presets: AcquisitionPresetSummary[]
  modelValue: string | null
  disabled?: boolean
}>()

const emit = defineEmits<{ 'update:modelValue': [id: string] }>()

/**
 * Custom stays last as the "none of these" escape row — separated visually below the 2 x 2 grid
 * so it does not read as one of the opinionated four. The other four render in the order the
 * backend ships them (`fetchCorrectionPresets` yields the display order).
 */
const opinionated = computed(() =>
  props.presets.filter(p => p.id !== 'custom'))
const customPreset = computed(() =>
  props.presets.find(p => p.id === 'custom') ?? null)

function pick(id: string): void {
  if (props.disabled || id === props.modelValue) return
  emit('update:modelValue', id)
}

// ── figures + frame cycling ────────────────────────────────────────────────────────────────────
// Same as `VisualAid`: one shared counter so every animated card advances on the same tick
// (independent timers drift out of phase within seconds). 220 ms because below ~120 ms the eye
// reads flicker rather than direction; `VisualAid.FRAME_MS` is 220 for the same reason.
const FRAME_MS = 220
const frame = ref(0)
let timer: number | undefined

const figures = computed(() =>
  props.presets.map(p => ({ id: p.id, fig: cardFigure(p.id), animated: isAnimated(p.id) })))

const hasAnimated = computed(() => figures.value.some(f => f.animated))

onMounted(() => {
  if (!hasAnimated.value) return
  if (window.matchMedia?.('(prefers-reduced-motion: reduce)').matches) return
  timer = window.setInterval(() => { frame.value++ }, FRAME_MS)
})
onBeforeUnmount(() => { if (timer !== undefined) window.clearInterval(timer) })

/** Cell size in viewBox units — one rect per grid cell, tiling the 100 x 100 face. */
const CELL = 100 / N

function figureFor(id: string): number[][] {
  const entry = figures.value.find(f => f.id === id)
  if (!entry) return []
  const frames = entry.fig.frames
  return frames[frame.value % frames.length]
}
</script>

<template>
  <div class="card-picker" :class="{ 'is-disabled': disabled }">
    <div class="card-grid">
      <button v-for="p in opinionated" :key="p.id"
        type="button" class="card-tile"
        :class="{ 'is-picked': modelValue === p.id }"
        :disabled="disabled"
        :aria-pressed="modelValue === p.id"
        @click="pick(p.id)"
        v-tooltip.top="p.description">
        <div class="card-fig-wrap">
          <svg viewBox="0 0 100 100" preserveAspectRatio="xMidYMid meet"
            class="card-fig" role="img" :aria-label="p.name"
            shape-rendering="crispEdges">
            <!-- Dark microscopy field. Kept in the template so a card can never accidentally ship
                 without one and read as a floating diagram. -->
            <rect x="0" y="0" width="100" height="100" class="card-bg" />
            <!-- One rect per intensity cell. `fill-opacity` = intensity — the same rendering
                 primitive VisualAid uses for its `grid` role. -->
            <template v-for="(row, y) in figureFor(p.id)" :key="`r${y}`">
              <rect v-for="(v, x) in row" :key="`c${y}-${x}`"
                :x="x * CELL" :y="y * CELL" :width="CELL" :height="CELL"
                class="fg-cell" :fill-opacity="v" />
            </template>
          </svg>
        </div>
        <div class="card-name cc-fs-xs">{{ p.name.split(' /')[0] }}</div>
      </button>
    </div>

    <!-- Custom sits below as the "none of these" escape — small, one-line row so it does not
         visually compete with the opinionated four. -->
    <button v-if="customPreset"
      type="button" class="card-escape"
      :class="{ 'is-picked': modelValue === customPreset.id }"
      :disabled="disabled"
      :aria-pressed="modelValue === customPreset.id"
      @click="pick(customPreset.id)"
      v-tooltip.top="customPreset.description">
      <svg viewBox="0 0 100 100" class="card-escape-glyph" aria-hidden="true">
        <rect x="14" y="14" width="72" height="72" rx="4" fill="none" class="fg-dash" />
        <text x="50" y="66" text-anchor="middle" class="fg-qmark">?</text>
      </svg>
      <span class="card-escape-label cc-fs-xs">{{ customPreset.name.split(' /')[0] }}</span>
      <span class="card-escape-hint cc-muted cc-fs-2xs">None of these</span>
    </button>
  </div>
</template>

<style scoped>
.card-picker {
  display: flex;
  flex-direction: column;
  gap: 8px;
}
.card-picker.is-disabled {
  opacity: 0.6;
  pointer-events: none;
}
.card-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 6px;
}
.card-tile {
  display: flex;
  flex-direction: column;
  align-items: stretch;
  gap: 4px;
  padding: 6px;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm);
  cursor: pointer;
  color: var(--cc-text);
  font: inherit;
  text-align: center;
  transition: border-color 120ms ease, background 120ms ease;
}
.card-tile:hover:not(:disabled) {
  border-color: var(--cc-accent);
}
.card-tile.is-picked {
  border-color: var(--cc-accent);
  background: color-mix(in srgb, var(--cc-accent) 10%, var(--cc-surface-2));
}
.card-tile:disabled {
  cursor: not-allowed;
}
.card-fig-wrap {
  aspect-ratio: 1 / 1;
  width: 100%;
  border-radius: var(--cc-radius-xs);
  overflow: hidden;
}
.card-fig {
  display: block;
  width: 100%;
  height: 100%;
}
.card-bg { fill: #0a0d14; }
.fg-cell { fill: #eaf1ff; }
.fg-dash {
  stroke: var(--cc-border);
  stroke-width: 2;
  stroke-dasharray: 5 4;
}
.card-name {
  color: var(--cc-text);
  line-height: 1.2;
}
.card-escape {
  display: grid;
  grid-template-columns: 32px 1fr auto;
  align-items: center;
  gap: 8px;
  padding: 4px 8px;
  background: transparent;
  border: 1px dashed var(--cc-border);
  border-radius: var(--cc-radius-sm);
  cursor: pointer;
  color: var(--cc-text);
  font: inherit;
  text-align: left;
}
.card-escape:hover:not(:disabled) {
  border-color: var(--cc-accent);
  border-style: solid;
}
.card-escape.is-picked {
  border-color: var(--cc-accent);
  border-style: solid;
  background: color-mix(in srgb, var(--cc-accent) 10%, var(--cc-surface-2));
}
.card-escape:disabled {
  cursor: not-allowed;
}
.card-escape-glyph {
  width: 32px;
  height: 32px;
}
.fg-qmark {
  fill: var(--cc-text-dim);
  font-size: 44px;
  font-weight: 500;
}
.card-escape-label {
  color: var(--cc-text);
}
</style>
