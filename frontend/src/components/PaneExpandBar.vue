<!--
  The control for a two-half side panel: one toggle per half, each expanding its half to the whole panel
  (and un-expanding it on a second click). Pair with `usePaneExpand` + a `pane-<mode>` class on the panel root — see
  `utils/paneExpand.ts` for the scenario, `docs/MODULES.md` for the consumer recipe.

  Deliberately thin and title-less: vertical space is the thing this exists to reclaim, so the bar spends
  as little of it as possible. Icons are the consumer's, because they name that panel's halves; the
  tooltips are built here so every panel phrases the action the same way.

  The default slot is a readout for the half that is currently HIDDEN — the row is already paid for, so a
  panel can keep a one-line summary of what you can no longer see (TaskRunner puts its running/queued
  counts there while the task list is collapsed). Left-aligned, opposite the toggles.
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
  <div class="pane-bar" data-guide="layout.paneBar">
    <span class="pane-note"><slot /></span>
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
  align-items: center;
  gap: 0.15rem;
  flex-shrink: 0;
}
/* the readout takes the slack, so the toggles stay pinned right whether or not there's a note */
.pane-note { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.pane-btn { font-size: var(--cc-fs-xs); }   /* + cc-btn cc-btn-bare cc-btn-icon */
</style>
