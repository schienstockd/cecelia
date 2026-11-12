<!--
  The viewer window's playback bar (docs/todo/GUI_LAYOUT_PLAN.md, V2): a floating box at the bottom
  centre of the image for time, z and level. A shell: the window fills it (the controls stay bound to
  the window's state), so this owns only the placement, the grid, the look and the fold. It floats
  above the bottom strip of the image, where the scale bar (bottom right, part of what Save image
  composites) and the status chips (bottom left) sit, so it never covers either.

  Open, it is a grid of fixed columns: two buttons, the slider, the readout, a chip, the fold. The
  window puts each control in its column (time row, then z row), so the rows line up and nothing moves
  while it plays or when ± turns on. `rows` says how many rows there are; the fold sits at the end of
  the last one.

  Folded (per profile, `cc.vw.playbackBar`) it is a pill holding the `collapsed` slot — play and the
  positions — with `line` as a read-only hairline along its bottom edge (where t is and what is
  cached, or where the planes sit in the stack). Playback runs on while folded; the state is the window's.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useStoredSectionOpen } from '../../utils/sectionOpen'
import { cacheGradient, type CacheRun } from '../../utils/volumeCache'

const props = withDefaults(defineProps<{
  /** rows in the open grid (time, z) */
  rows: number
  /** something to fold to: time or z. A Level-only bar has no fold */
  foldable?: boolean
  /** the pill's hairline: cache runs, and the span marking where you are (fractions, 0–1) */
  line?: { runs: CacheRun[]; at: [number, number] } | null
}>(), { foldable: true, line: null })

const open = useStoredSectionOpen('cc.vw.playbackBar', true)
const collapsed = computed(() => props.foldable && !open.value)
const lineBg = computed(() => props.line ? cacheGradient(props.line.runs) : '')
const atStyle = computed(() => {
  const at = props.line?.at
  if (!at) return {}
  // at least 3 px, so one plane of many still shows
  return { left: `${at[0] * 100}%`, width: `max(3px, ${(at[1] - at[0]) * 100}%)` }
})
</script>

<template>
  <div class="vw-pbar" :class="collapsed ? 'vw-pbar-collapsed' : 'vw-pbar-grid'" role="toolbar" aria-label="Playback">
    <slot v-if="!collapsed" />
    <slot v-else name="collapsed" />
    <button v-if="foldable" class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense vw-pbar-fold"
            :style="collapsed ? undefined : { gridColumn: 6, gridRow: rows }" :aria-expanded="!collapsed"
            :aria-label="collapsed ? 'Show the playback bar' : 'Collapse the playback bar'"
            v-tooltip.top="collapsed ? 'Show the playback bar' : 'Collapse to play and position'"
            @click="open = !open">
      <i class="pi" :class="collapsed ? 'pi-chevron-up' : 'pi-chevron-down'" />
    </button>
    <span v-if="collapsed && line" class="vw-pbar-line" :style="{ background: lineBg }" aria-hidden="true">
      <span class="vw-pbar-at" :style="atStyle" />
    </span>
  </div>
</template>

<style scoped>
.vw-pbar {
  position: absolute; left: 50%; bottom: 3rem; transform: translateX(-50%);
  max-width: calc(100% - 1.5rem);
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm); box-shadow: 0 2px 10px var(--cc-shadow);
  white-space: nowrap; z-index: 2;
}
/* buttons · buttons · slider · readout · chip · fold — fixed, so the rows line up and nothing jumps */
.vw-pbar-grid {
  display: grid; align-items: center; column-gap: 0.4rem; row-gap: 0.2rem; padding: 0.3rem 0.5rem;
  grid-template-columns: 1.6rem 1.6rem minmax(8rem, 15rem) 5.6rem 5.4rem 1.6rem;
}
.vw-pbar-collapsed {
  display: flex; align-items: center; gap: 0.25rem; padding: 0.15rem 0.4rem 0.35rem;
  border-radius: var(--cc-radius-pill);
}
.vw-pbar-line {
  position: absolute; left: 0.9rem; right: 0.9rem; bottom: 0.15rem; height: 2px;
  border-radius: var(--cc-radius-xs);
}
.vw-pbar-at { position: absolute; top: -1px; height: 4px; border-radius: var(--cc-radius-xs); background: var(--cc-text); }
</style>
