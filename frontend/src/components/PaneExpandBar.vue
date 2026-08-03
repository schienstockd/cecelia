<!--
  The control for a two-half side panel: one toggle per half, each expanding its half to the whole panel
  (and un-expanding it on a second click). Pair with `usePaneExpand` + `v-show` — see
  `utils/paneExpand.ts` for the scenario, `docs/MODULES.md` for the consumer recipe.

  Deliberately thin and title-less: vertical space is the thing this exists to reclaim, so the bar spends
  as little of it as possible. Icons are the consumer's, because they name that panel's halves; the
  tooltips are built here so every panel phrases the action the same way.
-->
<script setup lang="ts">
import type { PaneExpand, PaneHalf } from '../utils/paneExpand'

const props = defineProps<{
  pane: PaneExpand
  /** what each half holds, lower-case and short — goes straight into the tooltip ("the task list") */
  topLabel: string
  bottomLabel: string
  topIcon: string      // PrimeIcons class, e.g. 'pi-cog'
  bottomIcon: string
}>()

const emit = defineEmits<{ toggle: [half: PaneHalf] }>()

// Expanded → the action is to bring the OTHER half back, so say that instead of repeating "expand".
const tip = (half: PaneHalf) => props.pane === half
  ? `Show the ${half === 'top' ? props.bottomLabel : props.topLabel} again`
  : `Expand the ${half === 'top' ? props.topLabel : props.bottomLabel}`
</script>

<template>
  <div class="pane-bar">
    <button
      class="pane-btn cc-btn cc-btn-bare cc-btn-icon"
      :class="{ 'cc-btn-on': pane === 'top' }"
      :aria-pressed="pane === 'top'"
      :aria-label="tip('top')"
      @click="emit('toggle', 'top')"
      v-tooltip.left="tip('top')"
    >
      <i :class="['pi', topIcon]" />
    </button>
    <button
      class="pane-btn cc-btn cc-btn-bare cc-btn-icon"
      :class="{ 'cc-btn-on': pane === 'bottom' }"
      :aria-pressed="pane === 'bottom'"
      :aria-label="tip('bottom')"
      @click="emit('toggle', 'bottom')"
      v-tooltip.left="tip('bottom')"
    >
      <i :class="['pi', bottomIcon]" />
    </button>
  </div>
</template>

<style scoped>
.pane-bar {
  display: flex;
  justify-content: flex-end;
  gap: 0.15rem;
  flex-shrink: 0;
}
.pane-btn { font-size: var(--cc-fs-xs); }   /* + cc-btn cc-btn-bare cc-btn-icon */
</style>
