<!--
  Population positions: where each population's cells sit on the image, frame by frame. One tile per
  selected image (x/y in µm when calibrated, y downward like the image), every cell of the frame as the
  gate plot's density dots (dimmed under populations), each picked population's cells in its colour.
  Any population type: gates, cell clusters, regions, and the track families, whose cells are coloured
  in every frame they were tracked. The populations decide the segmentations drawn (server-side).

  Populations come from the host: the population rail (`series`, one family — the board and the summary
  module pages) or, on a Cluster / Region page, the populations its manager has switched on
  (`shownPops`). Data: `GET /api/plots/pop-positions` (`meta=1` per image for the axes, then the
  cells — both with the populations); the layer split is `plots/popPositions.ts` (tested). Drawn by the shared `GateScatterCell`,
  read-only. A movie plays through its frames (`PlayPauseButton` + `usePlaybackTimer`, the viewer's).
  Colour by: the populations over the all-cells density (default), the populations over every
  cell in plain grey (`GREY_BASE` — the picked populations are then the only colour), or a cell column —
  a Find similar lens score above all (`colourByGroups`) — shading every cell on ONE scale across the tiles (`sharedValueScale`; a lens score's
  is fixed at 0–1), so the images compare side by side.
-->
<script setup lang="ts">
import { ref, computed, watch, onBeforeUnmount } from 'vue'
import { useDelayedLoading } from '../../composables/useDelayedLoading'
import { usePopFamily } from '../../composables/usePopFamily'
import { useProjectStore } from '../../stores/project'
import { getBuffer, getJson, tryApi } from '../../utils/api'
import GateScatterCell from './GateScatterCell.vue'
import PopFamilySelect from './PopFamilySelect.vue'
import PlotSpinner from './PlotSpinner.vue'
import CcToggle from '../CcToggle.vue'
import PlayPauseButton from '../PlayPauseButton.vue'
import { usePlaybackTimer } from '../../composables/usePlaybackTimer'
import { playbackAdvance } from '../../utils/volumeCache'
import SearchSelect from '../SearchSelect.vue'
import ColourBarLegend from './ColourBarLegend.vue'
import { colourByGroups, fetchColourByColumns, NO_COLOUR_BY, type ColourByColumns } from '../../utils/viewer/colourByColumns'
import { useLensNames } from '../../composables/useLensNames'
import { lensLabel } from '../../utils/viewer/lensQuery'
import type { PopLayer } from './PlotLayers.vue'
import type { PopTypeOption } from '../../plots/popTypes'
import { filterSeriesToPopType } from '../../plots/popTypes'
import type { SeriesTarget } from '../../plots/types'
import { fetchSegmentationPops } from '../../plots/populations'
import type { CompareMode } from '../../plots/trackGroups'
import type { VisProps } from '../../plots/plot'
import { dotRadiusFor } from '../../plots/density'
import { GATE_CELL_PAD, GREY_BASE, colourColumn, framesIn, tileBox, positionLayers, positionPops, resolveFrame,
         sharedValueScale } from '../../plots/popPositions'
import { fitGridCols } from '../../plots/montage'
import { useBoxSize } from '../../composables/usePlotResize'
import { debouncedLatest } from '../../utils/debouncedLatest'
import { useDataRefresh } from '../../composables/useDataRefresh'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  // the population rail (board, summary module pages)
  series?: SeriesTarget[]
  popTypes?: PopTypeOption[]
  compareMode?: CompareMode
  // a Cluster / Region page: its family and the populations its manager shows
  popType?: string
  shownPops?: { path: string; name: string; colour: string }[]
  vis?: VisProps
  state: { popType?: string; frame?: number | null; colourBy?: string }
}>()

type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }
type Tick = { pos: number; label: string }
interface Meta { n: number; unit: string; extents: Ext; xTicks: Tick[]; yTicks: Tick[]; frames: [number, number]
                valueNames?: string[]; valueRange?: [number, number] | null; valueTicks?: Tick[] }
interface Tile { uid: string; meta: Meta; data: Float32Array }

const project = useProjectStore()

// the family: the rail's (persisted per panel) on a rail host, the page's on a cluster/region page
const { options: familyOptions, popType: railFamily } =
  usePopFamily(() => props.popTypes, () => props.state.popType, v => (props.state.popType = v))
const onRail = computed(() => !!props.popTypes?.length)
// colour by: '' = the populations, GREY_BASE = the populations on grey, else a cell column (persisted per panel)
const colourBy = computed({ get: () => props.state.colourBy ?? '', set: v => (props.state.colourBy = v) })
const greyBase = computed(() => colourBy.value === GREY_BASE)
const column = computed(() => colourColumn(colourBy.value))
const family = computed(() => onRail.value ? railFamily.value : (props.popType ?? 'flow'))
const ownSeries = computed(() => filterSeriesToPopType(props.series ?? [], family.value))

// "this image" compare mode focuses on the first selected image; otherwise one tile per image
const MAX_TILES = 12
const uids = computed(() => (props.compareMode === 'image' ? props.imageUids.slice(0, 1) : props.imageUids)
  .slice(0, MAX_TILES))

// a rail population keeps its gating-map colour (the manager's swatch)
const popColours = ref<Record<string, string>>({})
async function loadColours() {
  if (!onRail.value || !props.projectUid || !uids.value.length) { popColours.value = {}; return }
  const groups = await fetchSegmentationPops({ projectUid: props.projectUid, imageUids: uids.value.slice(0, 1),
                                               popType: family.value, granularity: 'cell' })
  const out: Record<string, string> = {}
  for (const g of groups) for (const pp of g.populations) if (pp.colour) out[`${g.valueName}${pp.path}`] = pp.colour
  popColours.value = out
}
const pops = computed(() => positionPops({
  series: onRail.value ? ownSeries.value : undefined,
  shownPops: onRail.value ? undefined : props.shownPops,
  popType: family.value,
  colourOf: t => popColours.value[`${t.valueName}${t.pop}`],
}))

const tiles = ref<Tile[]>([])
// the columns to offer: the first image's cell table the tiles draw (a lens score is on every image of its run)
const columns = ref<ColourByColumns>(NO_COLOUR_BY)
const columnsVn = computed(() => tiles.value[0]?.meta.valueNames?.[0] ?? '')
watch([() => uids.value[0], columnsVn], async ([uid, vn]) => {
  columns.value = uid && vn ? await fetchColourByColumns(props.projectUid, uid, vn) : NO_COLOUR_BY
}, { immediate: true })
// a lens is named as it was renamed (Movies & figures / the viewer's Lenses), not by its column
const lensNames = useLensNames(() => props.projectUid)
// the two population colourings lead the list: on the density (default) or on plain grey
const colourGroups = computed(() => {
  const [lead, ...rest] = colourByGroups(columns.value, lensNames.value, 'Populations on density')
  return [{ options: [...lead!.options, { value: GREY_BASE, label: 'Populations on grey' }] }, ...rest]
})
const valueScale = computed(() => (column.value ? sharedValueScale(tiles.value.map(t => t.meta)) : null))
// the legend switch (the rail's Legend): the population key and the colour bar, both in the controls
// strip above the tiles — one for the grid (`ColourBarLegend`, the montage's rule), never on a tile
const showLegend = computed(() => props.vis?.legend !== false)
const valueLabel = computed(() => column.value.startsWith('spatial.lens.')
  ? `like ${lensLabel(column.value, lensNames.value)}` : column.value)
const loading = ref(false)
useDelayedLoading(loading)
const error = ref('')
async function fetchTiles(isCurrent: () => boolean) {
  if (!props.projectUid || !uids.value.length) { tiles.value = []; return }
  error.value = ''
  try {
    const out: Tile[] = []
    for (const uid of uids.value) {
      const q = new URLSearchParams({ projectUid: props.projectUid, imageUid: uid, popType: family.value })
      if (pops.value.length) q.set('pops', pops.value.map(p => p.token).join(','))
      if (column.value) q.set('colourBy', column.value)
      const mr = await tryApi(getJson<Meta>(`/api/plots/pop-positions?${q}&meta=1`))
      if (!mr.ok) continue
      const meta = mr.data
      if (!meta?.n) continue
      const dr = await tryApi(getBuffer(`/api/plots/pop-positions?${q}`))
      if (!dr.ok) continue
      out.push({ uid, meta, data: new Float32Array(dr.data) })
    }
    if (!isCurrent()) return
    tiles.value = out
    if (!out.length) error.value = 'No cells to show'
  } catch (e) {
    if (isCurrent()) error.value = String(e)
  }
}
// one coalesced request per change burst (ticking several populations is several updates); `loading`
// follows the scheduler, so a queued request already reads as busy (an export waits for it)
const run = debouncedLatest<void>((_a, isCurrent) => fetchTiles(isCurrent),
  { wait: 150, onState: st => { loading.value = st !== 'idle' },
    onError: e => { error.value = e instanceof Error ? e.message : String(e) } })
watch([() => uids.value.join(','), family, () => pops.value.map(p => p.token).join(','), () => column.value],
      () => run.schedule(), { immediate: true })
onBeforeUnmount(() => run.cancel())
// a task finishing on one of these images (a new region run, a re-gate) redraws the plot
useDataRefresh(() => uids.value, () => { loadColours(); run.schedule() })
watch([() => uids.value.join(','), family], loadColours, { immediate: true })

// ── time: one frame (the slider), or every frame at once ──
const frames = computed(() => [...new Set(tiles.value.flatMap(t => framesIn(t.data)))].sort((a, b) => a - b))
const isMovie = computed(() => frames.value.length > 1)
const frame = computed<number | null>({
  get: () => resolveFrame(props.state.frame, frames.value),
  set: v => (props.state.frame = v),
})
const allFrames = computed({ get: () => frame.value === null, set: on => (frame.value = on ? null : (frames.value[0] ?? 0)) })
const frameIdx = computed({
  get: () => Math.max(0, frames.value.indexOf(frame.value ?? frames.value[0]!)),
  set: i => (frame.value = frames.value[i] ?? null),
})

// play: steps the slider through the frames in a loop. A frame is drawn from the cells already loaded,
// so the only wait is a refetch in flight (a population ticked mid-play) — the step holds until it lands.
const PLAY_FPS = 5
const playing = ref(false)
const canPlay = () => isMovie.value && !allFrames.value
const { stopPlay, togglePlay } = usePlaybackTimer({
  playing, fps: () => PLAY_FPS, canPlay,
  step: () => {
    const s = playbackAdvance(frameIdx.value, frames.value.length, true, () => !loading.value)
    if (s.ended) return 'ended'
    if (!s.stalled) frameIdx.value = s.t
  },
})
watch([isMovie, allFrames], () => { if (playing.value && !canPlay()) stopPlay() })

const imageName = (uid: string) =>
  project.sets.flatMap(s => s.images).find(i => i.uid === uid)?.name ?? uid
// each tile's layers for the shown frame — computed once per frame/population change, not per binding
const layers = computed(() => Object.fromEntries(tiles.value.map(t => {
  const l = positionLayers(t.data, frame.value, pops.value.length)
  const popLayers: PopLayer[] = l.pops.map((pts, k) =>
    ({ path: pops.value[k]!.token, colour: pops.value[k]!.colour, points: pts }))
  return [t.uid, { base: l.base, values: l.values, popLayers }]
})))
// THE TILE SIZE, from the measured panel: as many columns as give the biggest tiles (the gating
// montage's rule), then each map as large as its cell allows at its own aspect. A CSS auto-fill grid
// kept empty tracks, so a wider panel gave a SMALLER map.
const gridEl = ref<HTMLElement | null>(null)
const gridBox = useBoxSize(gridEl)
const CAPTION_H = 16, GAP = 8
const cols = computed(() => fitGridCols(tiles.value.length, gridBox.value.w, gridBox.value.h))
// x and y are both image distance, so the DOTS take the image's shape (equal µm per pixel on both
// axes) and the cell's axis room is added around them — not fitted inside the image's shape.
const aspectOf = (e: Ext) => (e.xMax - e.xMin) / (e.yMax - e.yMin)
const tileStyle = (e: Ext) => {
  const n = tiles.value.length, c = cols.value, r = Math.max(1, Math.ceil(n / c))
  const cw = (gridBox.value.w - (c - 1) * GAP) / c
  const ch = (gridBox.value.h - (r - 1) * GAP) / r - (n > 1 ? CAPTION_H : 0)
  const b = tileBox(cw, ch, aspectOf(e), n > 1 ? GATE_CELL_PAD.compact : GATE_CELL_PAD.full)
  return { width: `${b.w}px`, height: `${b.h}px` }
}
</script>

<template>
  <div class="ppv">
    <div class="ppv-ctrl cc-panel-controls">
      <div class="cc-row">
        <PopFamilySelect v-if="onRail" :options="familyOptions" v-model="railFamily" />
        <span class="cc-row-group" v-tooltip.top="'Colour the cells by populations or a measure'">
          <span class="cc-fs-xs">Colour</span>
          <SearchSelect v-model="colourBy" :groups="colourGroups" aria-label="Colour by" />
        </span>
        <template v-if="isMovie">
          <span class="cc-row-group">
            <CcToggle v-model="allFrames" aria-label="All frames" v-tooltip.top="'Every frame at once'" />
            <span class="cc-fs-xs">All frames</span>
          </span>
          <span v-if="!allFrames" class="cc-row-group ppv-frame">
            <PlayPauseButton :playing="playing" @toggle="togglePlay" />
            <input v-model.number="frameIdx" type="range" min="0" :max="frames.length - 1" step="1"
                   aria-label="Frame" v-tooltip.top="'Frame'" @pointerdown="stopPlay" />
            <span class="cc-fs-xs">t {{ frame }}</span>
          </span>
        </template>
      </div>
      <div v-if="showLegend && valueScale" class="cc-row ppv-legend">
        <ColourBarLegend :extent="valueScale.extent" :ticks="valueScale.ticks" :label="valueLabel" />
      </div>
      <div v-else-if="showLegend && pops.length" class="cc-row ppv-legend">
        <span v-for="p in pops" :key="p.token" class="ppv-key cc-fs-xs">
          <i class="ppv-swatch" :style="{ background: p.colour }" />{{ p.label }}
        </span>
      </div>
    </div>

    <p v-if="error" class="cc-muted cc-fs-xs">{{ error }}</p>
    <div ref="gridEl" class="ppv-grid" :style="{ gridTemplateColumns: `repeat(${cols}, minmax(0, 1fr))` }">
      <figure v-for="t in tiles" :key="t.uid" class="ppv-tile">
        <!-- the uid, not the name: names run to two lines and push the maps down; the name is on hover -->
        <figcaption v-if="tiles.length > 1" v-tooltip.top="imageName(t.uid)" class="cc-fs-2xs cc-muted">{{ t.uid }}</figcaption>
        <div class="ppv-plot" :style="tileStyle(t.meta.extents)">
          <GateScatterCell :points="layers[t.uid]!.base" :extents="t.meta.extents" :view-extents="t.meta.extents"
                           :x-ticks="t.meta.xTicks" :y-ticks="t.meta.yTicks"
                           :x-label="`X (${t.meta.unit})`" :y-label="`Y (${t.meta.unit})`"
                           :pop-layers="layers[t.uid]!.popLayers" :show-pops="pops.length > 0 && !valueScale" :base-grey="greyBase"
                           :base-values="valueScale ? layers[t.uid]!.values : null"
                           :value-extent="valueScale?.extent ?? null" :value-ticks="valueScale?.ticks ?? []"
                           :value-label="valueLabel" :value-legend="false"
                           render-mode="points" mode="off" :readonly="true" :flip-y="true"
                           :compact="tiles.length > 1" :aspect="aspectOf(t.meta.extents)" :font-size="vis?.fontSize ?? 11"
                           :dot-size="dotRadiusFor(vis?.pointSize)" />
        </div>
      </figure>
    </div>
    <PlotSpinner v-if="loading" label="Reading cells" />
  </div>
</template>

<style scoped>
/* flex: 1 (not height: 100%, which the panel body does not resolve): the grid must measure the PANEL,
   not its own content, or the map keeps whatever size it first had (ClusterMapView .uv, same rule) */
.ppv { display: flex; flex-direction: column; flex: 1; min-height: 0; position: relative; }
.ppv-frame input { width: 10rem; }
.ppv-legend { flex-wrap: wrap; gap: 0.6rem; }
.ppv-key { display: inline-flex; align-items: center; gap: 0.3rem; }
.ppv-swatch { width: 0.7rem; height: 0.7rem; border-radius: 50%; display: inline-block; }
.ppv-grid { flex: 1; min-height: 0; overflow: hidden; display: grid; gap: 8px; align-content: center; }
.ppv-tile { margin: 0; display: flex; flex-direction: column; align-items: center; min-width: 0; min-height: 0; }
.ppv-plot { position: relative; }
</style>
