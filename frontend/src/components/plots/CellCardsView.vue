<!--
  Cell cards — one card per trackclust pop the cluster manager ticks. Same contract as the other
  summary-family cluster plots (heatmap, HMM states/transitions): registered in `CLUSTER_PANELS`,
  wraps `CanvasPanel` itself, `trackOnly: true` (trackclust runs only).

  Server-rendered filmstrips: POST /api/cell_cards renders each card's medoid track through
  overlay_author (`track_color_mode="pop"`, `pops_filter=[pop_path]`) and saves the PNGs as
  board-assets. The trace on a card matches the trace on a recorded movie by construction.

  See docs/todo/CELL_CARDS_PLAN.md.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import CanvasPanel from '../canvas/CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import StripCell from './StripCell.vue'
import CellCardDetailPanel from './CellCardDetailPanel.vue'
import PlotSpinner from './PlotSpinner.vue'
import type { Card, CardsResponse } from './cellCards'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  projectUid: string; setUid: string | null; imageUids: string[]
  popType: 'clust' | 'trackclust' | 'region'; suffix: string
  shownPops?: { path: string; name: string; colour: string; clusterIds: number[] }[]
  state: { maxPx?: number; padPx?: number }
  docked?: boolean
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: [] }>()

const maxPx = computed(() => props.state.maxPx ?? 320)
const padPx = computed(() => props.state.padPx ?? 8)

const cards = ref<Card[]>([])
const pool  = ref<CardsResponse['pool']>([])
const loading = ref(false)
const err = ref('')

const rootUid = computed(() => props.imageUids[0] ?? '')

async function fetchCards() {
  cards.value = []
  pool.value = []
  err.value = ''
  const pops = props.shownPops ?? []
  if (!rootUid.value || !props.suffix || !pops.length) return
  loading.value = true
  try {
    const res = await fetch('/api/cell_cards', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: props.projectUid,
        rootUid: rootUid.value,
        // valueName omitted — server derives from co_clustered_value_names(suffix).
        suffix: props.suffix,
        pops: pops.map(p => ({ path: p.path, clusterIds: p.clusterIds })),
        maxPx: maxPx.value, padPx: padPx.value,
      }),
    })
    if (!res.ok) { err.value = ((await res.json().catch(() => ({}))) as { error?: string }).error ?? 'Cards fetch failed'; return }
    const data = (await res.json()) as CardsResponse
    pool.value  = data.pool ?? []
    cards.value = data.cards ?? []
  } catch (e) { err.value = e instanceof Error ? e.message : String(e) }
  finally { loading.value = false }
}

watch([rootUid, () => props.suffix,
       () => JSON.stringify((props.shownPops ?? []).map(p => [p.path, p.clusterIds]))],
      fetchCards, { immediate: true })

const cardSrc = (c: Card, i: number): string | undefined => {
  const aid = c.filmstrip?.[i]?.asset_id
  return aid ? `/api/board-assets?projectUid=${encodeURIComponent(props.projectUid)}&assetId=${encodeURIComponent(aid)}` : undefined
}
const primarySrc = (c: Card): string | undefined =>
  cardSrc(c, Math.min(1, (c.filmstrip?.length ?? 1) - 1)) ?? cardSrc(c, 0)

function fmtStat(median: number, q25: number, q75: number): { m: string; iqr: string } {
  const f = (v: number) => Math.abs(v) >= 100 ? v.toFixed(0) : Math.abs(v) >= 10 ? v.toFixed(1) : v.toFixed(2)
  return { m: f(median), iqr: `${f(q25)}–${f(q75)}` }
}

const detailCard = ref<Card | null>(null)

// InteractivePanel-style export contract (heatmap doesn't have this because it's a chart; cards
// export as an image sheet). Kept minimal until the multipage PDF export needs it wired.
defineExpose({})
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey"
               title="Cell cards" :docked="docked"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <div class="ccv">
      <p v-if="err" class="cc-muted-warn">{{ err }}</p>
      <p v-else-if="popType !== 'trackclust'" class="cc-muted">
        Cell cards need a track-clustering run. Switch popType to <b>trackclust</b> above.
      </p>
      <p v-else-if="!rootUid" class="cc-muted">Select an image.</p>
      <p v-else-if="!suffix" class="cc-muted">No clustering run in context.</p>
      <p v-else-if="!(shownPops && shownPops.length)" class="cc-muted">
        Tick one or more track clusters in the panel on the right.
      </p>
      <p v-else-if="!cards.length && !loading" class="cc-muted">No cards yet.</p>

      <div v-else class="ccv-grid">
        <div v-for="c in cards" :key="c.path" class="cc-card ccv-card">
          <StripCell class="ccv-cell" :ring-colour="c.colour"
                     :src="primarySrc(c)" :alt="c.name"
                     @click="detailCard = c" />
          <div class="ccv-foot">
            <div class="ccv-head">
              <span class="ccv-name" :style="{ color: c.colour }">{{ c.name }}</span>
              <span class="cc-muted cc-fs-2xs">n={{ c.n }}</span>
            </div>
            <ul class="ccv-stats">
              <li v-for="s in c.stats.slice(0, 5)" :key="s.name">
                <span class="ccv-stat-name cc-muted cc-fs-2xs">{{ s.name.replace(/^live\.track\./, '') }}</span>
                <span class="ccv-stat-val cc-fs-xs">{{ fmtStat(s.median, s.q25, s.q75).m }}</span>
                <span class="ccv-stat-iqr cc-muted cc-fs-2xs">{{ fmtStat(s.median, s.q25, s.q75).iqr }}</span>
              </li>
            </ul>
          </div>
        </div>
      </div>

      <PlotSpinner v-if="loading" label="Rendering cards…" />

      <CellCardDetailPanel v-if="detailCard" :card="detailCard" :project-uid="projectUid"
                           @close="detailCard = null" />
    </div>
    <template #footer>
      <button class="ccv-iconbtn cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" type="button"
              @click="emit('duplicate')" v-tooltip.top="'Duplicate this plot'">
        <i class="pi pi-copy" />
      </button>
    </template>
  </CanvasPanel>
</template>

<style scoped>
.ccv { position: relative; display: flex; flex-direction: column; height: 100%; overflow: auto;
  padding: 6px; }
.ccv-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
  gap: 0.5rem; }
/* layout-only extension of .cc-card (the global utility owns surface/border/radius) */
.ccv-card { display: flex; flex-direction: column; overflow: hidden; }
.ccv-cell { min-height: 180px; cursor: pointer; }
.ccv-foot { padding: 6px 8px; display: flex; flex-direction: column; gap: 4px; }
.ccv-head { display: flex; align-items: baseline; justify-content: space-between; gap: 6px; }
.ccv-name { font-weight: 700; font-size: var(--cc-fs-sm); }
.ccv-stats { list-style: none; padding: 0; margin: 0; display: grid; gap: 2px; }
.ccv-stats li { display: grid; grid-template-columns: 1fr auto auto; gap: 6px; align-items: baseline; }
.ccv-stat-name { text-align: left; }
.ccv-stat-val { text-align: right; font-variant-numeric: tabular-nums; font-weight: 600; }
.ccv-stat-iqr { text-align: right; font-variant-numeric: tabular-nums; }
</style>
