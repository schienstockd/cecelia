<script setup lang="ts">
/**
 * The BUTTON that offers a `VisualAid`, and the float it opens. One component so that "a figure of
 * these settings" is one thing to mount, wherever the settings live.
 *
 * It was born inside `ParamRenderer`'s repeatable-group branch, because the first figure compared two
 * segmentation passes and a group is what has passes. Nothing about the toggle, the float or the
 * remembered position was ever about groups though — and the second consumer (smoothing's median vs
 * gated) has no group at all: it compares two values of ONE select. Left where it was, that consumer
 * would have had to copy the button, the `ref` and the `FloatingPanel` into a second branch, which is
 * how one pattern becomes two that drift.
 *
 * The caller owns the FIGURE (a `VisColumns` from whatever producer suits it — `tasks/paramVis.ts`
 * for a group, `tasks/smoothVis.ts` for smoothing); this owns only whether it is on screen. Keeping
 * the producer outside is the same rule the figure itself follows: every number and comparison
 * decision is testable without mounting anything.
 *
 * FLOATING, not inline. Eleven rows above the entry list pushed the whole form down and was the first
 * thing Dominik said about it — a reference you consult while tuning wants to sit beside the
 * controls, not between them. `FloatingPanel` remembers where you put it.
 */
import { ref, watch } from 'vue'
import FloatingPanel from './FloatingPanel.vue'
import VisualAid from './VisualAid.vue'
import type { VisColumns } from '../tasks/paramVis'
import type { Severity } from '../lib/severity'

const props = withDefaults(defineProps<{
  /** the figure, already computed */
  vis: VisColumns
  /** the float's title — normally `<what these settings are> — at a glance` */
  title: string
  /** column headings; `VisualAid` falls back to 1-based numbering */
  headings?: string[]
  /** a line under the figure, when the caller has something to say about it */
  note?: string
  /**
   * Severity of that line, or none. Undefined is a real value here, not a missing one: `InlineNote`
   * with no severity draws a neutral info icon, which is what a VERDICT wants ("median is enough at
   * this window"). Borrowing `warn` for it would put a warning triangle on a recommendation, and
   * `ok` would draw a green check claiming a verdict about the user's data that nobody reached.
   */
  noteSeverity?: Severity
  /** what the button's tooltip says — the figure is not always "these settings" */
  tip?: string
  /**
   * `FloatingPanel`'s localStorage namespace. Per CONSUMER, not per component: two figures that
   * remembered one position would fight over it the moment a form offered both.
   */
  storageKey?: string
  /** the panel's initial size — a comparison of two whole methods is wider than a column strip */
  defaultW?: number
  defaultH?: number
}>(), {
  headings: undefined, note: '', noteSeverity: undefined, tip: 'Show these settings as a figure',
  storageKey: 'param-figure', defaultW: 330, defaultH: 420,
})

const open = ref(false)

/**
 * A figure with no rows has nothing to draw, so the button hides — and if it vanishes while the float
 * is OPEN (every drawable param cleared, a group emptied), the float goes with it. Otherwise the panel
 * outlives its own toggle and the only way to shut it is its close button.
 */
watch(() => props.vis.rows.length, n => { if (!n) open.value = false })
</script>

<template>
  <button v-if="vis.rows.length" class="param-fig-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-micro"
    type="button" :class="{ 'cc-btn-on': open }" @click="open = !open"
    v-tooltip.left="tip">
    <i class="pi pi-chart-bar" />
  </button>

  <FloatingPanel v-if="open" :title="title" :storage-key="storageKey" icon="pi-chart-bar"
    :default-w="defaultW" :default-h="defaultH" @close="open = false">
    <VisualAid :vis="vis" :headings="headings" :note="note" :note-severity="noteSeverity" />
  </FloatingPanel>
</template>
