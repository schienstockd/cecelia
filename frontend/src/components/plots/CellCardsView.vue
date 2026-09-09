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
import { computed, ref, useTemplateRef, watch, nextTick } from 'vue'
import CanvasPanel from '../canvas/CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import StripCell from './StripCell.vue'
import CellCardDetailPanel from './CellCardDetailPanel.vue'
import PlotSpinner from './PlotSpinner.vue'
import StatBox from './StatBox.vue'
import { elementToImageURL } from '../../plots/export'
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

// max output resolution + track bbox pad, both in native pixels. The bigger max_px is, the crisper
// the PNG stays when CSS stretches it into a card cell; the backend's own min-crop floor (~3× bbox,
// clamped to max_px, floor 480) then decides how much context sits around the track.
const maxPx = computed(() => props.state.maxPx ?? 512)
// A modest halo around the medoid — every card takes the SAME crop side (max bbox across cards +
// 2×padPx, computed server-side), so pad is what carries context beyond raw motion. 24 leaves room
// for the cell to sit inside its environment without swallowing it in tissue.
const padPx = computed(() => props.state.padPx ?? 24)

const cards = ref<Card[]>([])
const pool  = ref<CardsResponse['pool']>([])
const statScales = ref<CardsResponse['statScales']>({})
const loading = ref(false)
const err = ref('')

// scale a card's stat is drawn against — the pool-wide range for that measure across every card in
// this response (see backend `statScales`). Falls back to the stat's own min/max so a solitary card
// still renders a sensible box.
function scaleFor(stat: { name: string; min: number; max: number }): [number, number] {
  const s = statScales.value[stat.name]
  return s && s.length === 2 ? [s[0], s[1]] : [stat.min, stat.max]
}

const rootUid = computed(() => props.imageUids[0] ?? '')

async function fetchCards() {
  cards.value = []
  pool.value = []
  statScales.value = {}
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
    statScales.value = data.statScales ?? {}
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
// Grid tile shows the LAST frame of the filmstrip — the medoid track's end (Dominik 2026-09-09).
// The detail panel iterates the whole filmstrip; the grid card is deliberately a single still, and
// that still is the end-of-track. Never the middle: an earlier fallback to index `min(1, len-1)`
// picked the middle of a 3-frame strip and disagreed with the header's timestamp.
const primarySrc = (c: Card): string | undefined =>
  cardSrc(c, (c.filmstrip?.length ?? 1) - 1) ?? cardSrc(c, 0)

// Time label under the card image. Prefer real seconds when the medoid image records a
// TimeIncrement — shown as `mm:ss` for anything above a minute, else `Ns`. Falls back to the frame
// index (`t=N`) when the image is uncalibrated (see backend: `t_s` is omitted, not defaulted).
function timeLabelFor(c: Card): string | undefined {
  const f = c.filmstrip?.[c.filmstrip.length - 1]
  if (!f) return undefined
  if (typeof f.t_s !== 'number' || !isFinite(f.t_s)) return `t=${f.t}`
  const s = Math.round(f.t_s)
  if (s < 60) return `${s}s`
  const m = Math.floor(s / 60); const rem = s - m * 60
  return `${m}:${String(rem).padStart(2, '0')}`
}

// Board slots must not scroll — same rule as the gating-strategy tile: pick a column count that
// fits N cards into the slot without wrapping past it, and let grid rows share the available height
// (`grid-auto-rows: 1fr` + `min-height: 0` on ancestors). `ceil(sqrt(N))` keeps cards near-square:
// 1→1×1, 2→2×1, 3–4→2×2, 5–9→3×3, up to 5×5=25. Beyond that the row height gets tight; the detail
// panel is the escape valve.
const gridStyle = computed(() => {
  const n = cards.value.length
  if (!n) return {}
  const cols = Math.max(1, Math.ceil(Math.sqrt(n)))
  return {
    gridTemplateColumns: `repeat(${cols}, minmax(0, 1fr))`,
    gridAutoRows: 'minmax(0, 1fr)',
  } as Record<string, string>
})

const detailCard = ref<Card | null>(null)

// ── PDF export: inline every card's medoid PNG as a data URL (a served /api/board-assets URL fails
// to render inside html-serialised foreignObject output — same fix ImageStripView uses), then flip
// the container into a `cc-light` scope (dark-token → light-token override, mirroring
// GateMontage/GatingStrategy) and rasterise via elementToImageURL with a #ffffff ground.
const hostRef = useTemplateRef<HTMLElement>('hostRef')
const exportSrcs = ref<Record<string, string>>({})
const capturing = ref(false)

async function assetToDataUrl(assetId: string): Promise<string | null> {
  try {
    const r = await fetch(`/api/board-assets?projectUid=${encodeURIComponent(props.projectUid)}&assetId=${encodeURIComponent(assetId)}`)
    if (!r.ok) return null
    const bytes = new Uint8Array(await r.arrayBuffer())
    let bin = ''
    for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i])
    return 'data:image/png;base64,' + btoa(bin)
  } catch { return null }
}

// prefer the inlined src while capturing; fall back to the live /api URL for interactive rendering.
function displaySrc(c: Card): string | undefined {
  const aid = c.filmstrip?.[c.filmstrip.length - 1]?.asset_id ?? c.filmstrip?.[0]?.asset_id
  if (!aid) return undefined
  return exportSrcs.value[aid] ?? primarySrc(c)
}

async function exportImage(): Promise<string | null> {
  const el = hostRef.value; if (!el) return null
  const map: Record<string, string> = {}
  for (const c of cards.value) {
    const aid = c.filmstrip?.[c.filmstrip.length - 1]?.asset_id ?? c.filmstrip?.[0]?.asset_id
    if (!aid) continue
    const d = await assetToDataUrl(aid); if (d) map[aid] = d
  }
  exportSrcs.value = map
  capturing.value = true
  await nextTick()
  try { return await elementToImageURL(el, 'png', '#ffffff') }
  finally { capturing.value = false; exportSrcs.value = {} }
}

defineExpose({ exportImage })
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey"
               title="Cell cards" :docked="docked"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <div ref="hostRef" class="ccv" :class="{ 'cc-light': capturing }">
      <p v-if="err" class="cc-muted-warn">{{ err }}</p>
      <p v-else-if="!rootUid" class="cc-muted">Select an image.</p>
      <p v-else-if="!suffix" class="cc-muted">No clustering run in context.</p>
      <p v-else-if="!(shownPops && shownPops.length)" class="cc-muted">
        Tick one or more track clusters in the panel on the right.
      </p>
      <p v-else-if="!cards.length && !loading" class="cc-muted">No cards yet.</p>

      <div v-else class="ccv-grid" :style="gridStyle">
        <div v-for="c in cards" :key="c.path" class="cc-card ccv-card">
          <!-- Ring as an OUTER border on the wrapper, not StripCell's inset shadow — the inset ring
               sat on top of the image and read as "png overlaps the frame" once the crop cleared
               its own whitespace (Dominik 2026-09-09). Ring lives outside; image fills its box. -->
          <div class="ccv-frame" :style="{ borderColor: c.colour }">
            <StripCell class="ccv-cell"
                       :src="displaySrc(c)" :alt="c.name"
                       @click="detailCard = c" />
          </div>
          <div class="ccv-foot">
            <!-- Header row: pop name (never wraps — a two-line "Population 1" broke the export
                 layout by pushing "1" onto the first boxplot row) + a right-aligned time · n
                 summary. Timestamp moved OUT of the image overlay to here: `StillOverlay`'s top-
                 left position was clipped by the card's ring shadow at export scale. -->
            <div class="ccv-head">
              <span class="ccv-name" :style="{ color: c.colour }">{{ c.name }}</span>
              <span class="cc-muted cc-fs-2xs ccv-meta">
                <template v-if="timeLabelFor(c)">{{ timeLabelFor(c) }} ·&nbsp;</template>n={{ c.n }}
              </span>
            </div>
            <!-- One horizontal mini-boxplot per row (measure per row). Shared per-measure scale
                 across every card in the response; box fill is viridis(median rank on that scale),
                 so scanning the same row across cards reads as a heatmap of "high vs. low" for that
                 measure. Top 6 measures shown; the detail panel has the full set. -->
            <table class="ccv-stats">
              <tr v-for="s in c.stats.slice(0, 6)" :key="s.name">
                <td class="ccv-stat-name cc-muted cc-fs-2xs">{{ s.name.replace(/^live\.track\./, '') }}</td>
                <td class="ccv-stat-plot"><StatBox :stat="s" :scale="scaleFor(s)" :w="120" :h="14" /></td>
              </tr>
            </table>
          </div>
        </div>
      </div>

      <PlotSpinner v-if="loading" label="Rendering cards…" />

      <CellCardDetailPanel v-if="detailCard" :card="detailCard" :project-uid="projectUid"
                           :stat-scales="statScales"
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
/* Board rule: slots don't scroll. Flex column with min-height:0 so the grid child gets a bounded
   height, `overflow: hidden` clips rather than scrolling if we truly outrun the slot. */
.ccv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0;
  overflow: hidden; padding: 6px; }
/* grid is sized to fill the ccv (height 100% + min-height:0 chain). Column/row template is set
   inline from gridStyle (ceil(sqrt(N))) so cards stay near-square whatever N is. */
.ccv-grid { display: grid; gap: 0.5rem; flex: 1; min-height: 0; }
/* layout-only extension of .cc-card (the global utility owns surface/border/radius). Cell owns its
   own overflow so the image flex-fills and the footer stays visible. */
.ccv-card { display: flex; flex-direction: column; min-height: 0; overflow: hidden; }
/* frame wrapper carries the pop-colour ring; StripCell fills it edge-to-edge (no inset shadow) */
.ccv-frame { flex: 1; min-height: 0; display: flex; border: 2px solid transparent;
  border-radius: var(--cc-radius-sm); overflow: hidden; }
.ccv-cell { flex: 1; min-height: 0; cursor: pointer; }
/* StripCell's shared primitive sets `min-height: 120px` (right for the ImageStripView filmstrip
   scroller). In the board grid we own the vertical budget — rows share the slot with the footer —
   so the min-height would force a row to be taller than its 1fr share and reintroduce a scrollbar.
   Override via :deep so the primitive stays generic. */
.ccv-frame :deep(.strip-cell) { min-height: 0; }
.ccv-foot { flex: none; padding: 6px 8px; display: flex; flex-direction: column; gap: 4px;
  min-height: 0; }
.ccv-head { display: flex; align-items: baseline; justify-content: space-between; gap: 6px;
  min-width: 0; }
/* nowrap: the export path wrapped "Population 1" into two lines and pushed the "1" onto the first
   boxplot row. Ellipsis rather than wrap keeps the header height fixed. */
.ccv-name { font-weight: 700; font-size: var(--cc-fs-sm); flex: 1 1 auto; min-width: 0;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.ccv-meta { flex: none; white-space: nowrap; }
/* measure-per-row mini-boxplot table — label column right-sized, plot column fills the card */
.ccv-stats { width: 100%; border-collapse: collapse; }
.ccv-stats td { padding: 1px 4px; vertical-align: middle; }
.ccv-stat-name { text-align: left; white-space: nowrap; }
.ccv-stat-plot { width: 100%; }
.ccv-stat-plot :deep(svg) { display: block; width: 100%; }
/* PDF export mode: swap dark tokens for light so the captured card is ink-on-white. Mirrors
   GateMontage/GatingStrategy's `.cc-light` override — one class per capture host, no per-element
   colour edits. The viridis fill on `StatBox` stays as-is (perceptually uniform in either theme). */
.ccv.cc-light { --cc-text: #111; --cc-text-dim: #555; --cc-border: #c9ccd1; --cc-bg: #fff;
  --cc-surface-1: #fff; --cc-surface-2: #f0f0f3; --cc-surface-3: #e5e7eb; background: #fff; }
.ccv.cc-light :deep(.cc-card) { background: #fff; border-color: var(--cc-border); }
</style>
