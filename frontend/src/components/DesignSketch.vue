<script setup lang="ts">
/*
  What the design's roles mean, drawn from the set's own images: one row per compared group, one
  circle per replicate (the n), and a dot inside it per thing within that replicate. Updates as the
  roles change in the design dialog's layout step. Data: `designSketch` (utils/experimentDesign.ts).
*/
import type { DesignSketch } from '../utils/experimentDesign'

defineProps<{ sketch: DesignSketch | null }>()
const MAX_DOTS = 4
</script>

<template>
  <div class="ds cc-card">
    <template v-if="sketch">
      <div v-for="g in sketch.groups" :key="g.label" class="ds-group">
        <span class="ds-label">{{ g.label }}</span>
        <span class="cc-row cc-row-tight">
          <span v-for="(r, i) in g.reps" :key="i" class="ds-rep" v-tooltip.top="r.label || sketch.repNoun.one">
            <span v-for="k in Math.min(r.inside, MAX_DOTS)" :key="k" class="ds-dot" />
          </span>
          <span v-if="g.more" class="cc-muted cc-fs-2xs">+{{ g.more }}</span>
        </span>
      </div>
      <div class="cc-row cc-row-loose cc-muted cc-fs-2xs">
        <span><span class="ds-rep ds-key" /> one {{ sketch.repNoun.one }} — the n</span>
        <span v-if="sketch.insideNoun"><span class="ds-dot ds-key" /> {{ sketch.insideNoun }} inside it</span>
      </div>
      <div class="cc-fs-xs">{{ sketch.caption }}</div>
    </template>
    <div v-else class="cc-muted cc-fs-xs">Pick what you compare to see the layout</div>
  </div>
</template>

<style scoped>
.ds { display: flex; flex-direction: column; gap: 6px; padding: 8px 10px; min-width: 0; }
.ds-group { display: flex; align-items: center; gap: 8px; min-width: 0; }
.ds-label { min-width: 2.5rem; max-width: 6rem; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
            font-weight: 600; font-size: var(--cc-fs-xs); }
.ds-rep { display: inline-flex; flex-wrap: wrap; align-content: center; justify-content: center; gap: 1px;
          width: 16px; height: 16px; border-radius: 50%; border: 1.5px solid var(--cc-accent); }
.ds-dot { width: 4px; height: 4px; border-radius: 50%; background: var(--cc-accent); display: inline-block; }
.ds-key { vertical-align: middle; }
</style>
