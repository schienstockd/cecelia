<!--
  Per-card detail floater — opened by clicking a card in CellCardsView. Reuses <StripCell> for the
  full-length filmstrip and the same card colour ring; the stats footer shows every measure the
  server returned (not just the top five the grid squeezes in). One floater open at a time; the
  storage key is shared so opening a new card reuses the same window position.

  Uses <FloatingPanel> (top-level fixed window, drag + resize + collapse + persist) rather than a
  canvas panel — comparison stays in the grid, individual arrangement happens on demand here.
-->
<script setup lang="ts">
import FloatingPanel from '../FloatingPanel.vue'
import StripCell from './StripCell.vue'
import type { Card } from './cellCards'

const props = defineProps<{ card: Card; projectUid: string }>()
defineEmits<{ (e: 'close'): void }>()

function frameSrc(assetId: string): string {
  return `/api/board-assets?projectUid=${encodeURIComponent(props.projectUid)}&assetId=${encodeURIComponent(assetId)}`
}

function fmt(v: number): string {
  return Math.abs(v) >= 100 ? v.toFixed(0) : Math.abs(v) >= 10 ? v.toFixed(1) : v.toFixed(2)
}
</script>

<template>
  <FloatingPanel :title="`Card · ${card.name}`" storage-key="cellCards-detail"
                 :accent="card.colour" :default-w="520" :default-h="620"
                 @close="$emit('close')">
    <div class="ccd">
      <div class="ccd-head">
        <span class="ccd-name" :style="{ color: card.colour }">{{ card.name }}</span>
        <span class="cc-muted cc-fs-xs">
          n={{ card.n }} · medoid track {{ card.medoid.track_id }}
          <span v-if="card.medoid.uid && card.medoid.value_name">
            ({{ card.medoid.uid }} / {{ card.medoid.value_name }})
          </span>
        </span>
      </div>
      <div v-if="card.filmstrip.length" class="ccd-strip">
        <StripCell v-for="f in card.filmstrip" :key="f.asset_id"
                   class="ccd-cell" :ring-colour="card.colour"
                   :src="frameSrc(f.asset_id)" :alt="`t=${f.t}`"
                   :show-timestamp="true" :time-label="`t=${f.t}`" />
      </div>
      <p v-else class="cc-muted">No filmstrip PNGs — the medoid's image has no OME-Zarr on disk.</p>
      <table class="ccd-stats">
        <thead>
          <tr><th>measure</th><th>median</th><th>q25</th><th>q75</th></tr>
        </thead>
        <tbody>
          <tr v-for="s in card.stats" :key="s.name">
            <td class="cc-muted">{{ s.name.replace(/^live\.track\./, '') }}</td>
            <td class="ccd-num">{{ fmt(s.median) }}</td>
            <td class="ccd-num cc-muted">{{ fmt(s.q25) }}</td>
            <td class="ccd-num cc-muted">{{ fmt(s.q75) }}</td>
          </tr>
        </tbody>
      </table>
    </div>
  </FloatingPanel>
</template>

<style scoped>
.ccd { padding: 8px 10px; display: flex; flex-direction: column; gap: 8px; height: 100%;
  overflow: auto; }
.ccd-head { display: flex; align-items: baseline; justify-content: space-between; gap: 8px; }
.ccd-name { font-weight: 700; font-size: var(--cc-fs-md); }
.ccd-strip { display: flex; gap: 4px; overflow-x: auto; min-height: 140px; }
.ccd-cell { flex: 1 1 0; min-width: 140px; min-height: 140px; border-radius: var(--cc-radius-xs); }
.ccd-stats { width: 100%; border-collapse: collapse; font-size: var(--cc-fs-xs); }
.ccd-stats th { text-align: left; color: var(--cc-text-dim); font-weight: 600;
  border-bottom: 1px solid var(--cc-border); padding: 2px 4px; }
.ccd-stats td { padding: 2px 4px; border-bottom: 1px solid var(--cc-border); }
.ccd-num { text-align: right; font-variant-numeric: tabular-nums; }
</style>
