<!--
  What was measured about the selected image that bears on Cleanup — per channel the zero and clipped
  voxels, per drift run how far it drifted — and nothing else (docs/todo/CLEANUP_FACTS_PLAN.md D1). No
  cutoff, no "do this": each task's own text says what its method needs, and the reader matches the two.
  Reads `cleanupFacts` off the image payload (`img_cleanup_facts`, app/src/qc.jl), the same field the
  MCP's get_image_info returns (D5). One image at a time; with none or several selected it renders nothing.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useProjectStore } from '../stores/project'

const props = defineProps<{ selectedUids: string[] }>()

const project = useProjectStore()
const facts = computed(() =>
  props.selectedUids.length === 1 ? project.imageByUid(props.selectedUids[0])?.cleanupFacts ?? null : null)
const shown = computed(() => !!facts.value && (facts.value.channels.length > 0 || facts.value.drift.length > 0))

// Clipped is a fraction of SIGNAL voxels and is tiny when it is there at all — keep enough digits that
// a real pile-up does not round to 0, without printing noise for a clean channel.
function pct(v: number | null, digits: number): string {
  if (v === null || v === undefined) return '—'
  if (v === 0) return '0%'
  return `${v < 10 ** -digits ? `<${10 ** -digits}` : v.toFixed(digits)}%`
}
function driftText(d: { maxDriftPx: number; maxDriftUm: number | null }): string {
  return d.maxDriftUm === null ? `${d.maxDriftPx} px` : `${d.maxDriftUm} µm (${d.maxDriftPx} px)`
}
</script>

<template>
  <div v-if="shown && facts" class="cleanup-facts">
    <div class="cc-eyebrow">Measured</div>
    <div v-if="facts.channels.length" class="facts-grid cc-fs-xs">
      <span class="cc-muted">Channel</span>
      <span class="cc-muted" v-tooltip.left="'Voxels exactly 0, as % of all voxels'">Zero</span>
      <span class="cc-muted" v-tooltip.left="'Voxels piled up at the top value, as % of signal voxels'">Clipped</span>
      <template v-for="c in facts.channels" :key="c.index">
        <span class="facts-name">{{ c.name }}</span>
        <span class="cc-readout">{{ pct(c.zeroPct, 1) }}</span>
        <span class="cc-readout">{{ pct(c.clippedPct, 3) }}</span>
      </template>
    </div>
    <div v-for="d in facts.drift" :key="d.valueName" class="cc-muted cc-fs-xs">
      Drift ({{ d.valueName }}): {{ driftText(d) }}
    </div>
  </div>
</template>

<style scoped>
.cleanup-facts {
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 0 8px;
}
.facts-grid {
  display: grid;
  grid-template-columns: minmax(0, 1fr) auto auto;
  column-gap: 12px;
  row-gap: 2px;
}
.facts-name {
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
</style>
