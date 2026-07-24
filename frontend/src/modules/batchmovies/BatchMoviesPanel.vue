<!--
  Batch-movie authoring + run (F1.3 "make a movie for all images", docs/todo/ANIMATION_PLAN.md → F1).
  Author ONE config — which channels + colormap, which overlays (tracks / track-clusters / populations),
  a colour-by measure, fps/scale, and which attributes name the output file — then Generate one attr-named
  mp4 per selected image. The batch drives the single shared napari viewer sequentially (see
  api/src/napari_api.jl → run_batch_movies): it TAKES OVER the viewer for a while, so we warn while it runs.

  Config is persisted per-set in the settings store (getBatchMovieConfig/setBatchMovieConfig); fps/scale
  reuse the same per-set movie config as the ViewerPanel recorder. Progress/cancel ride the normal task
  UI (a client task record + `movie:batch` WS message; the backend emits task:progress/log/status/result).
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useSettingsStore } from '../../stores/settings'
import { useTaskStore } from '../../stores/tasks'
import { useWsStore } from '../../stores/ws'
import { useLogStore } from '../../stores/log'
import { CHANNEL_COLORMAP_OPTIONS } from '../../utils/napariColormap'
import { buildBatchMovieConfig, movieFilename, seedConfigFromViewState, defaultChannelSeed, MOVIE_CHANNELS_TOKEN, TITLE_CARD_DEFAULT, type BatchMovieCfg, type TitleCardCfg } from '../../utils/batchMovie'
import SwatchSelect, { type SwatchOption } from '../../components/SwatchSelect.vue'
import ChipSelect, { type ChipOption } from '../../components/ChipSelect.vue'
import TaskList from '../../tasks/TaskList.vue'

const props = defineProps<{ selectedUids: string[]; selectedNames: string[] }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()
const settings    = useSettingsStore()
const tasks       = useTaskStore()
const ws          = useWsStore()
const log         = useLogStore()

const uniq = (xs: string[]) => [...new Set(xs)]
const setUid = computed(() => project.activeSetUid ?? '')

// selected image objects (from the project store — same lookup TaskRunner uses)
const imgs = computed(() => {
  const all = project.sets.flatMap(s => s.images)
  return props.selectedUids.map(u => all.find(i => i.uid === u)).filter((i): i is NonNullable<typeof i> => !!i)
})
const channelList  = computed(() => uniq(imgs.value.flatMap(i => i.channelNames ?? [])))
const attrKeys     = computed(() => uniq(imgs.value.flatMap(i => Object.keys(i.attr ?? {}))))
const versionNames = computed(() => uniq(imgs.value.flatMap(i => Object.keys(i.filepaths ?? {}))))
const segNames     = computed(() => uniq(imgs.value.flatMap(i => Object.keys(i.labels ?? {}))))

// ── persisted config (per set) ────────────────────────────────────────────────
const cfg = computed(() => setUid.value ? settings.getBatchMovieConfig(setUid.value) : {})
function patch(p: Record<string, unknown>) { if (setUid.value) settings.setBatchMovieConfig(setUid.value, p) }
// fps/scale reuse the ViewerPanel recorder's per-set config
const movie = computed(() => setUid.value ? settings.getMovieConfig(setUid.value) : { fps: 15, scale: 1 })
const fps   = computed<number>({ get: () => movie.value.fps,   set: v => setUid.value && settings.setMovieConfig(setUid.value, { fps: v }) })
const scale = computed<number>({ get: () => movie.value.scale, set: v => setUid.value && settings.setMovieConfig(setUid.value, { scale: v }) })

const valueName    = computed<string>({ get: () => cfg.value.valueName ?? '',        set: v => patch({ valueName: v }) })
const colourBy     = computed<string>({ get: () => cfg.value.colourBy ?? '',         set: v => patch({ colourBy: v }) })
const showTracks   = computed<boolean>({ get: () => !!cfg.value.showTracks,          set: v => patch({ showTracks: v }) })
const showTrackclust = computed<boolean>({ get: () => !!cfg.value.showTrackclust,    set: v => patch({ showTrackclust: v }) })
const showGated    = computed<boolean>({ get: () => !!cfg.value.showGatedTracks,     set: v => patch({ showGatedTracks: v }) })
const showPops     = computed<boolean>({ get: () => !!cfg.value.showPopulations,     set: v => patch({ showPopulations: v }) })
const colourLabels = computed<boolean>({ get: () => !!cfg.value.colourLabels,        set: v => patch({ colourLabels: v }) })
const popType      = computed<string>({ get: () => cfg.value.popType ?? 'flow',      set: v => patch({ popType: v }) })
const tailWidth    = computed<number>({ get: () => cfg.value.tailWidth ?? 4,         set: v => patch({ tailWidth: v }) })
const pointsSize   = computed<number>({ get: () => cfg.value.pointsSize ?? 6,        set: v => patch({ pointsSize: v }) })

// Title card (Phase H) — merge-patch so each control keeps the others' values.
function patchTitle(p: Partial<TitleCardCfg>) {
  patch({ titleCard: { ...TITLE_CARD_DEFAULT, ...(cfg.value.titleCard ?? {}), ...p } })
}
const titleCardOn = computed<boolean>({ get: () => cfg.value.titleCard?.enabled ?? TITLE_CARD_DEFAULT.enabled, set: v => patchTitle({ enabled: v }) })
const titleNote   = computed<string>({  get: () => cfg.value.titleCard?.note ?? '',                                set: v => patchTitle({ note: v }) })
const titleDur    = computed<number>({  get: () => cfg.value.titleCard?.durationSec ?? TITLE_CARD_DEFAULT.durationSec, set: v => patchTitle({ durationSec: Math.min(10, Math.max(1, v)) }) })

// channel-colormap picker options: a leading "hidden" (no colour) + the standard swatch palette
const colormapOpts: SwatchOption[] = [
  { value: '', label: '— hidden —', hex: null },
  ...CHANNEL_COLORMAP_OPTIONS,
]
const channels = computed<Record<string, string>>(() => cfg.value.channels ?? {})
function setChannel(ch: string, cmap: string) {
  const next = { ...channels.value }
  if (!cmap) delete next[ch]; else next[ch] = cmap
  patch({ channels: next })
}
// Ordered filename tokens (attribute keys and/or the channels sentinel). The order IS the filename
// order — the user drags the chips to reorder. Persisted per set.
const fileAttrs = computed<string[]>({
  get: () => cfg.value.fileAttrs ?? [],
  set: v => patch({ fileAttrs: v }),
})
// Chip options: one per attribute key, plus a "channels" token (only when the images have channels).
const attrOptions = computed<ChipOption[]>(() => {
  const opts: ChipOption[] = attrKeys.value.map(k => ({ value: k, label: k }))
  if (channelList.value.length)
    opts.push({ value: MOVIE_CHANNELS_TOKEN, label: 'channels', icon: 'pi pi-palette',
                tip: 'Insert the shown channel names (joined by -) into the filename' })
  return opts
})
// Channels actually shown in the movie (have a colormap), in channel order — what the token expands to.
const shownChannels = computed(() => channelList.value.filter(c => channels.value[c]))

// Overlay toggles — five independent feature flags surfaced as one multi-select chip row.
const OVERLAY_OPTIONS: ChipOption[] = [
  { value: 'tracks',     label: '', icon: 'pi pi-directions',     tip: 'Tracks — all tracked segmentations' },
  { value: 'trackclust', label: '', icon: 'pi pi-sitemap',        tip: 'Track-cluster populations' },
  { value: 'gated',      label: '', icon: 'pi pi-filter',         tip: 'Gated track populations' },
  { value: 'pops',       label: '', icon: 'pi pi-chart-scatter',  tip: 'Populations (points)' },
  { value: 'labels',     label: '', icon: 'pi pi-palette',        tip: 'Colour label masks by the colour-by measure' },
]
const overlaysModel = computed<string[]>({
  get: () => [showTracks.value && 'tracks', showTrackclust.value && 'trackclust', showGated.value && 'gated',
              showPops.value && 'pops', colourLabels.value && 'labels'].filter(Boolean) as string[],
  set: (v) => { showTracks.value = v.includes('tracks'); showTrackclust.value = v.includes('trackclust')
                showGated.value = v.includes('gated'); showPops.value = v.includes('pops'); colourLabels.value = v.includes('labels') },
})

// ── colour-by options (obs columns of a representative segmentation) ───────────
const obsCols = ref<string[]>([])
async function loadObs() {
  const uid = props.selectedUids[0]
  const projectUid = projectMeta.current?.uid
  const seg = segNames.value[0]
  if (!uid || !projectUid || !seg) { obsCols.value = []; return }
  try {
    const q = `projectUid=${projectUid}&imageUid=${uid}&valueName=${encodeURIComponent(seg)}`
    const res = await fetch(`/api/gating/channels?${q}`)
    if (res.ok) {
      const j = await res.json() as { obsColumns?: string[]; trackColourColumns?: string[] }
      obsCols.value = [...new Set([...(j.obsColumns ?? []), ...(j.trackColourColumns ?? [])])]
    } else obsCols.value = []
  } catch { obsCols.value = [] }
}
watch(() => [props.selectedUids[0], segNames.value[0]] as const, loadObs, { immediate: true })

// ── seed the config so it's not blank (colours + pops of the first selected image) ─────────────
// Prefer the first image's LIVE napari view (its actual channel colours + shown overlays) when that
// image is the one open; otherwise fall back to a default palette so the pickers are still populated.
// The set's last colour-by seeds `colourBy`. Only fills EMPTY fields — never clobbers the user's edits.
const seeding = ref(false)
async function fillFromView(force = false) {
  const projectUid = projectMeta.current?.uid
  const first = props.selectedUids[0]
  const rep = imgs.value[0]
  if (!projectUid || !first || !rep) return
  if (!force && Object.keys(channels.value).length) return   // already authored → leave alone
  seeding.value = true
  let seed: BatchMovieCfg = {}
  try {
    const res = await fetch('/api/napari/view-state', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ projectUid }),
    })
    if (res.ok) {
      const j = await res.json() as { viewState?: unknown; imageUid?: string }
      // auto: only trust the live view when the OPEN image is the first selected one
      if (force || j.imageUid === first) seed = seedConfigFromViewState(j.viewState as never, rep.channelNames ?? [])
    }
  } catch { /* fall through to the palette default */ }
  // no usable live channels → default palette so the picker isn't blank
  if (!Object.keys(seed.channels ?? {}).length) {
    seed = { channels: defaultChannelSeed(rep.channelNames ?? [], CHANNEL_COLORMAP_OPTIONS.map(o => o.value)) }
  }
  const cb = setUid.value ? settings.getColourBy(setUid.value) : ''
  if (cb && !cfg.value.colourBy) seed.colourBy = cb
  patch(seed as Record<string, unknown>)
  seeding.value = false
}
// auto-seed when the selection changes and nothing's been authored yet
watch(() => props.selectedUids[0], () => { if (props.selectedUids.length) fillFromView(false) }, { immediate: true })

// output filename preview (mirrors the backend _movie_basename)
const filenamePreview = computed(() =>
  movieFilename(fileAttrs.value, imgs.value[0]?.attr ?? {}, imgs.value[0]?.uid ?? 'uid', shownChannels.value))

// ── build request + run ───────────────────────────────────────────────────────
function buildConfig() {
  const overrides = setUid.value ? settings.getColourOverrides(setUid.value, colourBy.value) : {}
  return buildBatchMovieConfig(cfg.value, segNames.value, overrides)
}

const running = computed(() =>
  tasks.forModule('batchMovies', projectMeta.current?.uid).some(t => t.status === 'running' || t.status === 'queued'))
const canRun = computed(() => props.selectedUids.length > 0 && !!projectMeta.current?.uid && !running.value)

function generate() {
  const uids = props.selectedUids
  const projectUid = projectMeta.current?.uid
  if (!uids.length || !projectUid) return
  const rep = uids[0]
  const repName = imgs.value[0]?.name ?? rep
  const t = tasks.add({
    module: 'batchMovies', label: `Batch movies (${uids.length} image${uids.length > 1 ? 's' : ''})`,
    imageUid: rep, imageName: repName, status: 'queued',
    taskName: 'movie.batch', funName: 'movie.batch', params: {}, projectUid,
  })
  ws.send({
    type: 'movie:batch', taskId: t.id, projectUid, imageUids: uids,
    config: buildConfig(), fileAttrs: fileAttrs.value, fps: fps.value, scale: scale.value,
  })
  log.info(`Batch movies started for ${uids.length} image(s) — napari will be busy for a bit`, { source: 'napari' })
}

async function previewOpen() {
  const uid = project.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) { log.warn('Open an image in napari first to preview the look', { source: 'napari' }); return }
  try {
    const res = await fetch('/api/napari/apply-movie-config', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: uid, config: buildConfig() }),
    })
    if (!res.ok) log.warn(`Preview failed: ${(await res.json())?.error ?? res.status}`, { source: 'napari' })
  } catch (e) { log.warn(`Preview failed: ${e}`, { source: 'napari' }) }
}
</script>

<template>
  <div class="bm">
    <p v-if="!selectedUids.length" class="bm-hint">Select one or more images (left) to author a batch of movies.</p>

    <template v-else>
      <!-- BUSY banner: the batch takes over the single napari viewer -->
      <div v-if="running" class="bm-busy">
        <i class="pi pi-spin pi-spinner" />
        <span>Napari is busy generating movies…</span>
      </div>

      <!-- Channels -->
      <section class="bm-sec">
        <h4>
          Channels <span class="bm-sub">shown channels + colormap (others hidden)</span>
          <button class="bm-link" :disabled="seeding || !project.napariImageUid" @click="fillFromView(true)"
                  title="Copy the channel colours + overlays from the image currently open in napari">
            <i class="pi pi-sync" /> fill from view
          </button>
        </h4>
        <p v-if="!channelList.length" class="bm-hint">No channel names on the selected images.</p>
        <div v-for="ch in channelList" :key="ch" class="bm-row">
          <span class="bm-ch">{{ ch }}</span>
          <SwatchSelect :model-value="channels[ch] ?? ''" :options="colormapOpts"
                        @update:model-value="v => setChannel(ch, v)" />
        </div>
      </section>

      <!-- Overlays -->
      <section class="bm-sec">
        <h4>Overlays <span class="bm-sub">click to toggle</span></h4>
        <ChipSelect class="bm-toggles" multiple :options="OVERLAY_OPTIONS" v-model="overlaysModel"
                    aria-label="Movie overlays" />
        <div v-if="showTracks" class="bm-inset">
          <span class="bm-lbl">tail</span>
          <input type="range" min="1" max="20" step="1" v-model.number="tailWidth" />
          <span class="bm-val">{{ tailWidth }}</span>
        </div>
        <div v-if="showPops" class="bm-inset">
          <select v-model="popType" class="bm-mini">
            <option value="flow">gating</option>
            <option value="clust">clusters</option>
          </select>
          <span class="bm-lbl">size</span>
          <input type="range" min="1" max="20" step="1" v-model.number="pointsSize" />
          <span class="bm-val">{{ pointsSize }}</span>
        </div>
      </section>

      <!-- Colour by -->
      <section class="bm-sec">
        <h4>Colour by <span class="bm-sub">measure used to colour tracks / labels</span></h4>
        <select v-model="colourBy">
          <option value="">— none (population / default colour) —</option>
          <option v-for="c in obsCols" :key="c" :value="c">{{ c }}</option>
        </select>
      </section>

      <!-- Image version -->
      <section v-if="versionNames.length > 1" class="bm-sec">
        <h4>Image version</h4>
        <select v-model="valueName">
          <option value="">active</option>
          <option v-for="vn in versionNames" :key="vn" :value="vn">{{ vn }}</option>
        </select>
      </section>

      <!-- Output -->
      <section class="bm-sec">
        <h4>Output</h4>
        <div class="bm-inset">
          <span class="bm-lbl">fps</span>
          <input type="range" min="1" max="60" step="1" v-model.number="fps" />
          <span class="bm-val">{{ fps }}</span>
          <span class="bm-lbl">res</span>
          <input type="range" min="1" max="3" step="1" v-model.number="scale" />
          <span class="bm-val">{{ scale }}×</span>
        </div>
        <div class="bm-attrs">
          <span class="bm-lbl">filename attrs <span class="bm-sub">click to include · drag to reorder</span></span>
          <ChipSelect v-if="attrOptions.length" v-model="fileAttrs" :options="attrOptions" multiple reorderable
                      aria-label="Filename attributes" />
          <span v-else class="bm-hint">no attributes — files named by uid</span>
        </div>
        <p class="bm-preview">→ movies/<b>{{ filenamePreview }}</b></p>
      </section>

      <!-- Title card (Phase H) — auto description slide prepended to each movie -->
      <section class="bm-sec">
        <div class="bm-title-row">
          <label class="bm-title-toggle"
                 v-tooltip.bottom="'Name, attributes, channels &amp; colours — prepended to each movie'">
            <input type="checkbox" v-model="titleCardOn" /> Title card
          </label>
          <template v-if="titleCardOn">
            <input type="range" min="1" max="10" step="1" v-model.number="titleDur" class="bm-title-range"
                   v-tooltip.bottom="'Title-card duration (seconds)'" />
            <span class="bm-val">{{ titleDur }}s</span>
          </template>
        </div>
        <input v-if="titleCardOn" type="text" class="bm-note" v-model="titleNote"
               placeholder="note (optional)" />
      </section>

      <!-- Actions -->
      <div class="bm-actions">
        <button class="cc-btn cc-btn-ghost" :disabled="!project.napariImageUid" @click="previewOpen"
                title="Apply this config to the currently open napari image (no recording)">
          <i class="pi pi-eye" /> Preview on open image
        </button>
        <button class="cc-btn cc-btn-primary" :disabled="!canRun" @click="generate">
          <i class="pi pi-video" /> Generate movies ({{ selectedUids.length }})
        </button>
      </div>

      <TaskList module="batchMovies" />
    </template>
  </div>
</template>

<style scoped>
.bm { display: flex; flex-direction: column; gap: 7px; flex: 1; min-width: 0; padding: 2px; }
.bm-hint { color: var(--cc-text-dim); font-size: 0.78rem; margin: 2px 0; }
.bm-busy { display: flex; align-items: center; gap: 8px; padding: 6px 9px; border-radius: 6px;
  background: color-mix(in srgb, var(--cc-warn) 16%, transparent); border: 1px solid var(--cc-warn);
  color: var(--cc-text); font-size: 0.8rem; }
.bm-sec { border: 1px solid var(--cc-border); border-radius: 6px; padding: 6px 8px; background: var(--cc-surface-1); }
.bm-sec h4 { display: flex; align-items: baseline; margin: 0 0 4px; font-size: 0.8rem; font-weight: 700; }
.bm-mini { min-width: 0; padding: 0.2rem 1.4rem 0.2rem 0.4rem; font-size: 0.72rem; }
.bm-sub { font-weight: 400; color: var(--cc-text-dim); font-size: 0.72rem; margin-left: 6px; }
.bm-link { float: right; font-size: 0.7rem; color: var(--cc-accent); background: none; border: none;
  cursor: pointer; padding: 0; display: inline-flex; align-items: center; gap: 3px; }
.bm-link:hover:not(:disabled) { text-decoration: underline; }
.bm-link:disabled { opacity: 0.4; cursor: not-allowed; }
.bm-row { display: flex; align-items: center; gap: 8px; margin: 3px 0; }
.bm-ch { flex: 1; font-size: 0.8rem; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.bm-row select, .bm-sec > select { min-width: 150px; }
.bm-chk { display: flex; align-items: center; gap: 6px; font-size: 0.8rem; margin: 3px 0; cursor: pointer; }
.bm-chk.inline { display: inline-flex; margin-right: 10px; }
.bm-inset { display: flex; align-items: center; gap: 6px; margin: 5px 0 1px; }
/* range inputs default to a fixed intrinsic width (~129px) and don't shrink, so two on one row
   (fps + res) overflow the sidebar. Let them flex down to share the available width. */
.bm-inset input[type="range"] { flex: 1; min-width: 0; }
.bm-lbl { font-size: 0.72rem; color: var(--cc-text-dim); }
.bm-val { font-size: 0.72rem; min-width: 1.6rem; }
.bm-attrs { margin-top: 6px; display: flex; flex-direction: column; gap: 4px; }
.bm-preview { font-size: 0.76rem; color: var(--cc-text-dim); margin: 6px 0 0; word-break: break-all; }
.bm-preview b { color: var(--cc-text); }
.bm-actions { display: flex; gap: 8px; flex-wrap: wrap; }
.bm-title-row { display: flex; align-items: center; gap: 0.5rem; }
.bm-title-toggle { display: inline-flex; align-items: center; gap: 6px; cursor: pointer; flex-shrink: 0; font-weight: 600; }
.bm-title-range { flex: 1; min-width: 3rem; accent-color: var(--cc-accent); }
.bm-note { width: 100%; box-sizing: border-box; font: inherit; padding: 3px 6px; margin-top: 5px;
  border: 1px solid var(--cc-border); border-radius: 4px; background: var(--cc-surface-1); color: var(--cc-text); }
</style>
