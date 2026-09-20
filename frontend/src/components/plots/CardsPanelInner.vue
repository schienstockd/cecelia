<!--
  Cards panel inner — content only. Renders the fetch → grid → statScales → detail-slot pipeline
  used by every card family, WITHOUT a CanvasPanel wrap. Consumed two ways:

  - `CardsPanelBase.vue` — wraps this in a CanvasPanel; the mount used by `cellCards` (which is
    registered in `CLUSTER_PANELS` and mounted bare from LayoutCanvas, so the wrap has to come
    from inside).
  - `MotifCardsView.vue` (and, future, `HmmStateCardsView.vue`) — mounts this directly, because
    those views are registered in `INTERACTIVE_VIEWS` and mounted via `InteractivePanel`, which
    already draws its own CanvasPanel.

  Design source: docs/todo/BEHAVIOUR_CARDS_PLAN.md — Decisions 1, 2, 5, 8.
-->
<script setup lang="ts">
import { computed, ref, useTemplateRef, watch, nextTick } from 'vue'
import StripCell from './StripCell.vue'
import PlotSpinner from './PlotSpinner.vue'
import StatBox from './StatBox.vue'
import { elementToImageURL } from '../../plots/export'
import type { Card, CardsResponse, CardFamily, ShownPop } from './cardsPanel'

const props = defineProps<{
  projectUid: string; imageUids: string[]
  suffix?: string
  shownPops?: ShownPop[]
  state: { maxPx?: number; padPx?: number }
  family: CardFamily
}>()
const emit = defineEmits<{ cardSelect: [Card] }>()

// max output resolution + track bbox pad, both in native pixels. The bigger max_px is, the crisper
// the PNG stays when CSS stretches it into a card cell; the backend's own min-crop floor (~3× bbox,
// clamped to max_px, floor 480) then decides how much context sits around the track.
const maxPx = computed(() => props.state.maxPx ?? 512)
const padPx = computed(() => props.state.padPx ?? 24)

const cards = ref<Card[]>([])
const pool  = ref<CardsResponse['pool']>([])
const statScales = ref<CardsResponse['statScales']>({})
const loading = ref(false)
const err = ref('')

function scaleFor(stat: { name: string; min: number; max: number }): [number, number] {
  const s = statScales.value[stat.name]
  return s && s.length === 2 ? [s[0], s[1]] : [stat.min, stat.max]
}

const rootUid = computed(() => props.imageUids[0] ?? '')

// Family-scoped gates: cellCards requires both a suffix (clustering run in scope) AND a non-empty
// shownPops (user ticked pops on the rail). motifCards/hmmCards discover their content server-side,
// so both gates are OFF and the fetch fires as soon as we have an image. Defaults match cellCards.
const needSuffix    = computed(() => props.family.requireSuffix    !== false)
const needShownPops = computed(() => props.family.requireShownPops !== false)

async function fetchCards() {
  cards.value = []
  pool.value = []
  statScales.value = {}
  err.value = ''
  const shownPops = props.shownPops ?? []
  if (!rootUid.value) return
  if (needSuffix.value && !props.suffix) return
  if (needShownPops.value && !shownPops.length) return
  loading.value = true
  try {
    const body = props.family.buildRequestBody({
      projectUid: props.projectUid,
      rootUid: rootUid.value,
      suffix: props.suffix ?? '',
      shownPops,
      maxPx: maxPx.value,
      padPx: padPx.value,
    })
    const res = await fetch(props.family.endpoint, {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
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
const primarySrc = (c: Card): string | undefined =>
  cardSrc(c, (c.filmstrip?.length ?? 1) - 1) ?? cardSrc(c, 0)

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
// fits N cards into the slot without wrapping past it, and let grid rows share the available height.
const gridStyle = computed(() => {
  const n = cards.value.length
  if (!n) return {}
  const cols = Math.max(1, Math.ceil(Math.sqrt(n)))
  return {
    gridTemplateColumns: `repeat(${cols}, minmax(0, 1fr))`,
    gridAutoRows: 'minmax(0, 1fr)',
  } as Record<string, string>
})

const statLabelFor = (name: string): string =>
  props.family.footerStatLabel ? props.family.footerStatLabel(name) : name

// ── PDF export: inline every card's medoid PNG as a data URL (a served /api/board-assets URL fails
// to render inside html-serialised foreignObject output — same fix ImageStripView uses), then flip
// the container into a `cc-light` scope and rasterise via elementToImageURL with a #ffffff ground.
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
  <div ref="hostRef" class="ccv" :class="{ 'cc-light': capturing }">
    <p v-if="err" class="cc-muted-warn">{{ err }}</p>
    <p v-else-if="!rootUid" class="cc-muted">{{ family.emptyNoRoot }}</p>
    <p v-else-if="needSuffix && !suffix" class="cc-muted">{{ family.emptyNoSuffix }}</p>
    <p v-else-if="needShownPops && !(shownPops && shownPops.length)" class="cc-muted">{{ family.emptyNoShownPops }}</p>
    <p v-else-if="!cards.length && !loading" class="cc-muted">No cards yet.</p>

    <div v-else class="ccv-grid" :style="gridStyle">
      <div v-for="c in cards" :key="c.path" class="cc-card ccv-card">
        <div class="ccv-frame" :style="{ borderColor: c.colour }">
          <StripCell class="ccv-cell"
                     :src="displaySrc(c)" :alt="c.name"
                     @click="emit('cardSelect', c)" />
        </div>
        <div class="ccv-foot">
          <div class="ccv-head">
            <span class="ccv-name" :style="{ color: c.colour }">{{ c.name }}</span>
            <span class="cc-muted cc-fs-2xs ccv-meta">
              <template v-if="timeLabelFor(c)">{{ timeLabelFor(c) }} ·&nbsp;</template>n={{ c.n }}
            </span>
          </div>
          <table class="ccv-stats">
            <tr v-for="s in c.stats.slice(0, 6)" :key="s.name">
              <td class="ccv-stat-name cc-muted cc-fs-2xs">{{ statLabelFor(s.name) }}</td>
              <td class="ccv-stat-plot"><StatBox :stat="s" :scale="scaleFor(s)" :w="120" :h="14" /></td>
            </tr>
          </table>
        </div>
      </div>
    </div>

    <PlotSpinner v-if="loading" label="Rendering cards…" />

    <!-- Family-specific detail panel goes here. Consumer decides what to render for its family. -->
    <slot name="detail" :cards="cards" :stat-scales="statScales" />
  </div>
</template>

<style scoped>
/* Board rule: slots don't scroll. */
.ccv { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0;
  overflow: hidden; padding: 6px; }
.ccv-grid { display: grid; gap: 0.5rem; flex: 1; min-height: 0; }
.ccv-card { display: flex; flex-direction: column; min-height: 0; overflow: hidden; }
.ccv-frame { flex: 1; min-height: 0; display: flex; border: 2px solid transparent;
  border-radius: var(--cc-radius-sm); overflow: hidden; }
.ccv-cell { flex: 1; min-height: 0; cursor: pointer; }
.ccv-frame :deep(.strip-cell) { min-height: 0; }
.ccv-foot { flex: none; padding: 6px 8px; display: flex; flex-direction: column; gap: 4px;
  min-height: 0; }
.ccv-head { display: flex; align-items: baseline; justify-content: space-between; gap: 6px;
  min-width: 0; }
.ccv-name { font-weight: 700; font-size: var(--cc-fs-sm); flex: 1 1 auto; min-width: 0;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.ccv-meta { flex: none; white-space: nowrap; }
.ccv-stats { width: 100%; border-collapse: collapse; }
.ccv-stats td { padding: 1px 4px; vertical-align: middle; }
.ccv-stat-name { text-align: left; white-space: nowrap; }
.ccv-stat-plot { width: 100%; }
.ccv-stat-plot :deep(svg) { display: block; width: 100%; }
.ccv.cc-light { --cc-text: #111; --cc-text-dim: #555; --cc-border: #c9ccd1; --cc-bg: #fff;
  --cc-surface-1: #fff; --cc-surface-2: #f0f0f3; --cc-surface-3: #e5e7eb; background: #fff; }
.ccv.cc-light :deep(.cc-card) { background: #fff; border-color: var(--cc-border); }
</style>
