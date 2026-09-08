<!--
  Cell cards — one card per trackclust pop. Fetches POST /api/cell_cards for the pops the cluster
  rail has ticked, then draws a grid of cards. Each card is a small filmstrip (first / mid / last
  frame of the medoid track, with the track trace baked in server-side via overlay_author's
  `track_color_mode="pop"`) plus a stats footer of the pop's median motility measures.

  Ratio behind the shape:
  - the picture-taking + trace overlay + PNG storage all happen SERVER-side (`api/src/cell_cards_api.jl`),
    so the trace on a card matches the trace on a recorded movie by construction — no client-side
    canvas draw that could drift;
  - the strip cell primitive is shared with ImageStripView via `<StripCell>` — same on-image legend,
    same click target, same scale-bar/timestamp policy.

  See docs/todo/CELL_CARDS_PLAN.md.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import StripCell from './StripCell.vue'
import CellCardDetailPanel from './CellCardDetailPanel.vue'
import PlotSpinner from './PlotSpinner.vue'
import { gridColumns, imageGridPng, imageGridSvgFrom } from '../../plots/imageGrid'
import { downloadDataUrl, downloadText } from '../../plots/export'
import type { Card, CardsResponse } from './cellCards'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  popType?: 'clust' | 'trackclust' | 'region'
  /** The clustering-run suffix, e.g. "movement". Comes from the cluster panel context. */
  suffix?: string
  /** Populations ticked in the cluster manager — the rail's own selection. */
  shownPops?: { path: string; name: string; colour: string; clusterIds: number[] }[]
  state: { maxPx?: number; padPx?: number }
}>()
// The floater is owned locally (one per view instance): clicking a second card just swaps content.
const detailCard = ref<Card | null>(null)
const openCard = (c: Card) => { detailCard.value = c }

const maxPx = computed({ get: () => props.state.maxPx ?? 320, set: v => (props.state.maxPx = v) })
const padPx = computed({ get: () => props.state.padPx ?? 8,   set: v => (props.state.padPx = v) })

const cards = ref<Card[]>([])
const pool  = ref<CardsResponse['pool']>([])
const loading = ref(false)
const error = ref('')

const rootUid = computed(() => props.imageUids[0] ?? '')

// Fetch when the cluster rail's selection changes, the root image changes, or the suffix changes.
// A trackclust pop's clusterIds come from the manager's own filter parsing — the rail computes them
// so we don't re-derive from the pop's filter here.
async function fetchCards() {
  cards.value = []
  pool.value = []
  error.value = ''
  const pops = props.shownPops ?? []
  if (!rootUid.value || !props.suffix || !pops.length) return
  loading.value = true
  try {
    const res = await fetch('/api/cell_cards', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: props.projectUid,
        rootUid: rootUid.value,
        // valueName omitted — the server derives it from co_clustered_value_names(suffix).
        suffix: props.suffix,
        pops: pops.map(p => ({ path: p.path, clusterIds: p.clusterIds })),
        maxPx: maxPx.value, padPx: padPx.value,
      }),
    })
    if (!res.ok) { error.value = ((await res.json().catch(() => ({}))) as { error?: string }).error ?? 'Cards fetch failed'; return }
    const data = (await res.json()) as CardsResponse
    pool.value  = data.pool ?? []
    cards.value = data.cards ?? []
  } catch (e) { error.value = e instanceof Error ? e.message : String(e) }
  finally { loading.value = false }
}

watch([rootUid, () => props.suffix, () => props.shownPops], fetchCards, { immediate: true })

const cardSrc = (c: Card, i: number): string | undefined => {
  const aid = c.filmstrip?.[i]?.asset_id
  return aid ? `/api/board-assets?projectUid=${encodeURIComponent(props.projectUid)}&assetId=${encodeURIComponent(aid)}` : undefined
}
const primarySrc = (c: Card): string | undefined => cardSrc(c, Math.min(1, (c.filmstrip?.length ?? 1) - 1)) ?? cardSrc(c, 0)

function fmtStat(median: number, q25: number, q75: number): { m: string; iqr: string } {
  const f = (v: number) => Math.abs(v) >= 100 ? v.toFixed(0) : Math.abs(v) >= 10 ? v.toFixed(1) : v.toFixed(2)
  return { m: f(median), iqr: `${f(q25)}–${f(q75)}` }
}

// ── export (native-size tiles via plots/imageGrid — same helper FlowMetricsView uses) ─────────
const gridRef = ref<HTMLElement | null>(null)
const exportFormats = ['png', 'svg']
const stem = computed(() => ['cell_cards', rootUid.value, props.suffix].filter(Boolean).join('_').replace(/[^\w.-]+/g, '_'))
const tiles = () => cards.value
  .map(c => { const s = primarySrc(c); return s ? { name: c.name, dataUrl: s } : null })
  .filter((x): x is { name: string; dataUrl: string } => !!x)
function exportAs(kind: string) {
  const cols = gridColumns(gridRef.value)
  if (kind === 'png')  imageGridPng(tiles(), cols).then(url => url && downloadDataUrl(`${stem.value}.png`, url))
  else if (kind === 'svg') imageGridSvgFrom(tiles(), cols).then(svg => svg && downloadText(`${stem.value}.svg`, svg, 'image/svg+xml'))
}
const exportImage = () => imageGridPng(tiles(), gridColumns(gridRef.value))
const exportSvg   = () => imageGridSvgFrom(tiles(), gridColumns(gridRef.value))
defineExpose({ exportFormats, exportAs, exportImage, exportSvg })
</script>

<template>
  <div class="cc-view">
    <p v-if="error" class="cc-muted-warn">{{ error }}</p>
    <p v-else-if="!rootUid" class="cc-muted">Select an image.</p>
    <p v-else-if="!suffix" class="cc-muted">No clustering run in context.</p>
    <p v-else-if="!(shownPops && shownPops.length)" class="cc-muted">
      Tick one or more track clusters in the panel on the right.
    </p>
    <p v-else-if="!cards.length && !loading" class="cc-muted">No cards yet.</p>

    <div v-else ref="gridRef" class="cc-grid">
      <!-- .cc-card provides the surface/border/radius; .ccv-card is the LAYOUT-only extension
           (per docs/ui/PRIMITIVES.md — compose the utility, don't override it) -->
      <div v-for="c in cards" :key="c.path" class="cc-card ccv-card">
        <StripCell class="ccv-cell" :ring-colour="c.colour"
                   :src="primarySrc(c)" :alt="c.name"
                   @click="openCard(c)" />
        <div class="ccv-foot">
          <div class="ccv-head">
            <span class="ccv-name" :style="{ color: c.colour }">{{ c.name }}</span>
            <span class="cc-card-n cc-muted cc-fs-2xs">n={{ c.n }}</span>
          </div>
          <ul class="cc-stats">
            <li v-for="s in c.stats.slice(0, 5)" :key="s.name">
              <span class="cc-stat-name cc-muted cc-fs-2xs">{{ s.name.replace(/^live\.track\./, '') }}</span>
              <span class="cc-stat-val cc-fs-xs">{{ fmtStat(s.median, s.q25, s.q75).m }}</span>
              <span class="cc-stat-iqr cc-muted cc-fs-2xs">{{ fmtStat(s.median, s.q25, s.q75).iqr }}</span>
            </li>
          </ul>
        </div>
      </div>
    </div>

    <PlotSpinner v-if="loading" label="Rendering cards…" />

    <CellCardDetailPanel v-if="detailCard" :card="detailCard" :project-uid="projectUid"
                         @close="detailCard = null" />
  </div>
</template>

<style scoped>
.cc-view { position: relative; display: flex; flex-direction: column; height: 100%; overflow: auto;
  padding: 6px; }
.cc-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
  gap: 0.5rem; }
/* layout-only extension of .cc-card (the global utility owns surface/border/radius) */
.ccv-card { display: flex; flex-direction: column; overflow: hidden; }
.ccv-cell { min-height: 180px; cursor: pointer; }
.ccv-foot { padding: 6px 8px; display: flex; flex-direction: column; gap: 4px; }
.ccv-head { display: flex; align-items: baseline; justify-content: space-between; gap: 6px; }
.ccv-name { font-weight: 700; font-size: var(--cc-fs-sm); }
.cc-stats { list-style: none; padding: 0; margin: 0; display: grid; gap: 2px; }
.cc-stats li { display: grid; grid-template-columns: 1fr auto auto; gap: 6px; align-items: baseline; }
.cc-stat-name { text-align: left; }
.cc-stat-val { text-align: right; font-variant-numeric: tabular-nums; font-weight: 600; }
.cc-stat-iqr { text-align: right; font-variant-numeric: tabular-nums; }
</style>
