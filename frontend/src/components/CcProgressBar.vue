<script setup lang="ts">
// THE determinate progress bar for the app (docs/UI.md → UX-primitive catalog). A track with a fill
// whose width is a 0–1 fraction: a task's `[PROGRESS] n/total`, a data patch, an export/import job.
//
// Use it whenever you know HOW FAR ALONG something is. When you don't — "working, no number" — this is
// the wrong control and there is nothing to render: in a task row the running status icon already says
// it (`lib/taskStatus.ts`, `--cc-active`), and a plot area has `plots/PlotSpinner.vue`. Do not fake an
// indeterminate bar by animating this one.
//
// Extracted from FOUR hand-rolled copies (the per-module task list, the task manager's log pane, the
// Settings data-patch bar, the project panel's export/import row). They agreed on the two colours and
// on nothing else — two heights (3/4px), two radii (none / `--cc-radius-xs`), two transitions
// (0.25s / 0.2s), `min-width` on half of them, a `<div>` fill in three and a bare `<span>` in the
// fourth, and three different fraction→width sums (see `utils/progress.ts`). Rationale + the measured
// table: `docs/todo/TASK_LIST_UNIFICATION_PLAN.md` → Decision 7a.
//
// `size` is the one axis that survived that merge, because both shapes are real:
//   thin  3px, square    — flush inside a row or card, where the bar is an edge-to-edge rule
//   bar   4px, rounded   — a standalone element with space around it
//
// GEOMETRY STAYS WITH THE CALLER. The root is a plain block that fills its container, so a caller's
// `flex: 0 0 90px`, `margin-bottom` or `flex-shrink: 0` goes on a class at the call site — it is the
// layout's business how wide the bar is and what sits next to it, not this component's.
import { computed } from 'vue'
import { progressWidth } from '../utils/progress'

const props = withDefaults(defineProps<{
  /** how far along, 0–1. Missing/NaN reads as no progress; out of range is clamped. */
  value: number | null | undefined
  size?: 'thin' | 'bar'
  /** accessible name — a bar with no nearby label needs one (e.g. a row of identical job rows). */
  ariaLabel?: string
}>(), { size: 'thin' })

const width = computed(() => progressWidth(props.value))
// `aria-valuenow` wants the same clamped number the fill is drawn from, so a screen reader and the
// pixels can't disagree — read it back off the width rather than re-deriving the clamp here.
const pct = computed(() => parseFloat(width.value))
</script>

<template>
  <div class="cc-pb" :class="`cc-pb-${size}`"
       role="progressbar" :aria-label="ariaLabel"
       aria-valuemin="0" aria-valuemax="100" :aria-valuenow="pct">
    <div class="cc-pb-fill" :style="{ width }" />
  </div>
</template>

<style scoped>
.cc-pb {
  background: var(--cc-surface-2);
  overflow: hidden;
}
.cc-pb-fill {
  height: 100%;
  background: var(--cc-accent);
  transition: width 0.2s ease;
  /* A fill rounding to under a pixel renders as nothing, so a job that has genuinely started looks
     identical to one that hasn't. Two of the four copies had this and two didn't. */
  min-width: 2px;
}

/* flush inside a row or card — the bar is an edge-to-edge rule, so no radius */
.cc-pb-thin { height: 3px; }

/* standalone, with space around it */
.cc-pb-bar { height: 4px; border-radius: var(--cc-radius-xs); }
</style>
