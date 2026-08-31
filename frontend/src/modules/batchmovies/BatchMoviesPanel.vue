<!--
  Batch-movie authoring + run (F1.3 "make a movie for all images", docs/todo/ANIMATION_PLAN.md → F1).
  Author ONE config — which channels + colormap, which overlays (tracks / track-clusters / populations),
  a colour-by measure, fps, and which attributes name the output file — then Generate one attr-named
  mp4 per selected image. The batch runs through the offline renderer (see api/src/movie_rail.jl →
  run_batch_offline), one image at a time.

  Two stacked halves like the module pages' TaskRunner — the movie CONFIG as the top half, the batch's
  task list as the bottom — sharing the same `PaneExpandBar` primitive (`utils/paneExpand.ts`), so either
  can take the whole panel instead of scrolling past the other.

  Config is persisted per-set in the settings store (getBatchMovieConfig/setBatchMovieConfig); fps
  reuse the same per-set movie config as the ViewerPanel recorder. Progress/cancel ride the normal task
  UI (a client task record + `movie:batch` WS message; the backend emits task:progress/log/status/result).
-->
<script setup lang="ts">
import { computed, ref, watch, nextTick } from 'vue'
import { useProjectStore } from '../../stores/project'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useSettingsStore } from '../../stores/settings'
import { useTaskStore } from '../../stores/tasks'
import { useViewerStore } from '../../stores/viewer'
import { useWsStore } from '../../stores/ws'
import { useLogStore } from '../../stores/log'
import { CHANNEL_COLORMAP_OPTIONS } from '../../utils/napariColormap'
import { buildBatchMovieConfig, movieFilename, seedConfigFromViewState, defaultChannelSeed, MOVIE_CHANNELS_TOKEN, TITLE_CARD_DEFAULT, clampContour, type BatchMovieCfg, type TitleCardCfg, type ViewStateLike } from '../../utils/batchMovie'
import { versionsFromConfig, compareSuffix, compareActionTip,
         COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         segmentationsFromConfig, compareShape,
         type CompareLayout, type CompareContrast } from '../../utils/movieCompare'
import SwatchSelect, { type SwatchOption } from '../../components/SwatchSelect.vue'
import ChipSelect, { type ChipOption } from '../../components/ChipSelect.vue'
import CcToggle from '../../components/CcToggle.vue'
import MovieCompareControls from '../../components/MovieCompareControls.vue'
import TaskList from '../../tasks/TaskList.vue'
import PaneExpandBar from '../../components/PaneExpandBar.vue'
import { usePaneExpand } from '../../composables/usePaneExpand'
import TitleCardControls from '../../components/TitleCardControls.vue'
import MovieOutputControls from '../../components/MovieOutputControls.vue'
import MovieTimeRange from '../../components/MovieTimeRange.vue'
import { movieSizeParams } from '../../utils/movieSize'
import { useViewerMovieDefaults } from '../../composables/useViewerMovieDefaults'
import { lookRestore, missingRefs, restoreNote, restoreTargetSet, type MovieRegistryEntry } from '../../utils/movieRestore'
import { useMovieRestore } from '../../composables/useMovieRestore'
import RestoreNotice from '../../components/RestoreNotice.vue'
import { useMovieSuffixes } from '../../composables/useMovieSuffixes'

const props = defineProps<{ selectedUids: string[]; selectedNames: string[] }>()

const project     = useProjectStore()
const projectMeta = useProjectMetaStore()

// Suffixes already used in this project, offered in the recorder's "name" field. Lazily fetched and
// cached across the three recorder panels — see composables/useMovieSuffixes.ts.
const { suffixes: movieSuffixes, ensure: ensureMovieSuffixes } = useMovieSuffixes()
watch(() => projectMeta.current?.uid ?? '', (uid: string) => { void ensureMovieSuffixes(uid) }, { immediate: true })
const settings    = useSettingsStore()
const tasks       = useTaskStore()
const viewer      = useViewerStore()
const ws          = useWsStore()
// the browser viewer's canvas size, for the size fields' placeholder (see useViewerMovieDefaults)
const { canvasSizeX, canvasSizeY, multiscaleLevels } = useViewerMovieDefaults()
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
// fps + output size reuse the ViewerPanel recorder's per-set config (null size = the viewer's canvas size)
const movie = computed(() => setUid.value
  ? settings.getMovieConfig(setUid.value)
  // no set open yet — the store's own defaults, so every reader below sees the same shape
  : { fps: 15, sizeX: null, sizeY: null, suffix: null, showTimestamp: true, showScaleBar: true })
const fps   = computed<number>({ get: () => movie.value.fps,   set: v => setUid.value && settings.setMovieConfig(setUid.value, { fps: v }) })
const sizeX = computed<number | null>({ get: () => movie.value.sizeX, set: v => setUid.value && settings.setMovieConfig(setUid.value, { sizeX: v }) })
const sizeY = computed<number | null>({ get: () => movie.value.sizeY, set: v => setUid.value && settings.setMovieConfig(setUid.value, { sizeY: v }) })
// filename addition; defaults to the version this batch opens (blank = the active one), so a corrected
// run and a raw run don't write over each other. null = untouched, '' = deliberately cleared.
const suffix = computed<string>({
  get: () => movie.value.suffix ?? compareSuffix(compareVersions.value, compareSegmentations.value),
  set: v => { if (setUid.value) settings.setMovieConfig(setUid.value, { suffix: v }) } })

// Which versions each movie shows, in column order (docs/todo/MOVIE_COMPARE_PLAN.md). Reads a config
// saved before comparisons existed through `versionsFromConfig`, so a batch that was set to the
// corrected version doesn't quietly revert to the active one.
const compareVersions = computed<string[]>({
  get: () => versionsFromConfig(cfg.value, versionNames.value),
  set: v => patch({ valueNames: v }),
})
// Which segmentation masks each movie DRAWS, in column order — and on the segmentation axis, the
// columns themselves. Empty means no masks: an authored batch config always says what it wants, so the
// backend gets an explicit empty list rather than "leave the canvas alone".
const compareSegmentations = computed<string[]>({
  get: () => segmentationsFromConfig(cfg.value, segNames.value),
  set: v => patch({ labelValueNames: v }),
})
// Mask outline width (0 = filled). Persisted only — unlike the viewer's recorder this page drives no
// live layers of its own; the value is applied per image when the batch renders it.
const labelContour = computed<number>({
  get: () => clampContour(cfg.value.labelContour), set: v => patch({ labelContour: clampContour(v) }) })
// Whole z stack (3D) or one slice. Authored per SET here rather than read from the live viewer — the
// batch opens each image itself, so there is no "what is on screen" to inherit.
const show3D = computed<boolean>({ get: () => !!cfg.value.show3D, set: v => patch({ show3D: v }) })
// 3D detail (multiscale level, 0 = full resolution). Stored in the batch config and applied per image
// by the recorder; the RANGE comes from the image currently open in the viewer, so the control only
// offers itself when there is something on screen to judge it against.
const detail3d = computed<number>({
  get: () => (cfg.value.detail3d as number | undefined) ?? 0, set: v => patch({ detail3d: v }) })
const zSlice = computed<number | null>({
  get: () => cfg.value.zSlice ?? null, set: v => patch({ zSlice: v }) })
// the shallowest stack in the selection — a slice index deeper than that would not exist on every image
const zDepth = computed(() => {
  const zs = imgs.value.map(i => i.sizeZ ?? 1).filter(n => n > 1)
  return zs.length ? Math.min(...zs) : 1
})
// Which stretch of the timelapse each movie sweeps. Unlike `zDepth` the bound here is the LONGEST, not
// the shortest: a z INDEX outside an image simply does not exist, while a frame RANGE is clamped per
// image by the recorder — so bounding by the shortest would make the extra frames of a longer image
// unreachable for no benefit.
const tStart = computed<number>({
  get: () => cfg.value.tStart ?? 0, set: v => patch({ tStart: v }) })
const tEnd = computed<number | null>({
  get: () => cfg.value.tEnd ?? null, set: v => patch({ tEnd: v }) })
const tFrames = computed(() => Math.max(1, ...imgs.value.map(i => i.sizeT ?? 1)))
// baked overlays, burnt into every frame (per set, like fps/size)
const movieTimestamp = computed<boolean>({
  get: () => movie.value.showTimestamp,
  set: v => { if (setUid.value) settings.setMovieConfig(setUid.value, { showTimestamp: v }) } })
const movieScaleBar = computed<boolean>({
  get: () => movie.value.showScaleBar,
  set: v => { if (setUid.value) settings.setMovieConfig(setUid.value, { showScaleBar: v }) } })
const compareLayout = computed<CompareLayout>({
  get: () => cfg.value.compareLayout ?? COMPARE_LAYOUT_DEFAULT, set: v => patch({ compareLayout: v }) })
// Versions across, masks down — picking from BOTH lists fully determines the layout. One list leaves
// the arrangement (across / stacked / wrapped into a grid) to `compareLayout`, so the shape needs it.
const compareShapeNow = computed(() =>
  compareShape(compareVersions.value, compareSegmentations.value, compareLayout.value))
const compareContrast = computed<CompareContrast>({
  get: () => cfg.value.compareContrast ?? COMPARE_CONTRAST_DEFAULT, set: v => patch({ compareContrast: v }) })
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
// TitleCardControls owns the clamp and emits a whole config; this just persists it.
const titleCardModel = computed<TitleCardCfg>({
  get: () => ({ ...TITLE_CARD_DEFAULT, ...(cfg.value.titleCard ?? {}) }),
  set: v => patchTitle(v),
})

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
  { value: 'labels',     label: '', icon: 'pi pi-palette',        tip: 'Colour the drawn masks by the colour-by measure' },
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
// Prefer the first image's LIVE viewer view (its actual channel colours + shown overlays) when that
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
  // Read the browser volume viewer's published viewState (`useViewerStore.viewState`). The popup
  // writes it on every camera / channel change. Auto-seed only trusts the source when its OPEN image
  // is the first selected one; forced (button click) reads whatever the browser viewer currently
  // has. If the browser viewer isn't open yet, `seed` stays empty and the palette default below
  // kicks in.
  const browserOpenUid = viewer.openImage?.imageUid
  if (viewer.viewState && (force || browserOpenUid === first)) {
    seed = seedConfigFromViewState(viewer.viewState as unknown as ViewStateLike, rep.channelNames ?? [])
  } else if (force) {
    // User pressed the button expecting a fill — tell them why nothing changed rather than
    // silently defaulting.
    log.info('Open the image in the viewer first — fill-from-view reads the live view.',
             { source: 'movies' })
  }
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

// Terminate each filename with the image NAME rather than its uid — what a single viewer recording
// does. Persisted with the config, so a restored viewer look regenerates the same file name.
const nameByImage = computed<boolean>({
  get: () => !!cfg.value.nameByImage, set: v => patch({ nameByImage: v }) })
// output filename preview (mirrors the backend _movie_basename)
const filenamePreview = computed(() =>
  movieFilename(fileAttrs.value, imgs.value[0]?.attr ?? {}, imgs.value[0]?.uid ?? 'uid',
                shownChannels.value, nameByImage.value ? (imgs.value[0]?.name ?? '') : ''))

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
    config: { ...buildConfig(), showTimestamp: movieTimestamp.value, showScaleBar: movieScaleBar.value },
    fileAttrs: fileAttrs.value, fps: fps.value, suffix: suffix.value,
    ...movieSizeParams(sizeX.value, sizeY.value),
  })
  log.info(`Batch movies started for ${uids.length} image(s)`, { source: 'movies' })
}

// ── Editing a movie's saved config (Phase 6, docs/todo/MOVIE_MANAGEMENT_PLAN.md) ───────────────
// Arriving from the Movies page with `?fromMovie=…`. Both `look` producers land here — a viewer
// recording and a batch are the same KIND, so they edit in the same place (Decision 7).
//
// Everything this touches is REPLACED rather than patched, and snapshotted first: the config is being
// swapped as a whole, and a merge could not undo it — it has no way to remove a key, so undoing would
// leave behind every option the restored config set and the previous one did not.
// Takes the set explicitly rather than reading the active one: a restore can SWITCH sets, and what has
// to be snapshotted is the config of the set about to be overwritten.
function outputSnapshot(uid: string) {
  const m = settings.getMovieConfig(uid)
  return { fps: m.fps, sizeX: m.sizeX, sizeY: m.sizeY, suffix: m.suffix,
           showTimestamp: m.showTimestamp, showScaleBar: m.showScaleBar }
}

// What the RESTORED images offer, read straight from the project store rather than from `imgs` — the
// selection is being set in the same breath, and the prop carrying it back down has not updated yet.
// `colourBy` is deliberately absent: `obsCols` is fetched per segmentation and is empty on arrival, so
// checking against it would report every colour-by as dead. A colour-by that no longer exists shows as
// a blank select instead, which is its own signal.
function availableFor(uids: string[]) {
  const all = project.sets.flatMap(s => s.images)
  const found = uids.map(u => all.find(i => i.uid === u)).filter((i): i is NonNullable<typeof i> => !!i)
  const pool = found.length ? found : imgs.value
  if (!pool.length) return {}
  return {
    versions: uniq(pool.flatMap(i => Object.keys(i.filepaths ?? {}))),
    segmentations: uniq(pool.flatMap(i => Object.keys(i.labels ?? {}))),
    channels: uniq(pool.flatMap(i => i.channelNames ?? [])),
  }
}

const { notice: restoreNotice, undo: undoRestore, dismiss: dismissRestore } = useMovieRestore({
  kind: 'look',
  projectUid: () => projectMeta.current?.uid ?? '',
  onError: m => log.error(m, { source: 'movies' }),
  apply: (entry: MovieRegistryEntry) => {
    const r = lookRestore(entry.config)
    if (!r) return null

    // WHICH SET, from the movie's own images rather than from whichever set happens to be active — and
    // switch to it. Config, output and selection are all stored per set, so this has to be settled
    // before anything is written. Checking the active set instead used to report "images from another
    // set" and leave the user to go and switch (Dominik, 2026-08-10); a restore is one click and should
    // repair what it can. Images spanning two sets have no single answer, so that keeps the active set
    // and says what it dropped.
    const known = new Set(project.sets.flatMap(s => s.images).map(i => i.uid))
    const set = restoreTargetSet(r.imageUids.map(u => project.setUidOfImage(u)), setUid.value)
    if (!set) return null
    const prevSetUid = project.activeSetUid
    if (prevSetUid !== set) project.activeSetUid = set

    // A batch reproduces its whole selection, not the one row that was clicked. Only the ones in this
    // set: a uid from a deleted image (or another set) would sit in the selection unselectable and
    // invisible.
    const inSet = new Set((project.sets.find(s => s.uid === set)?.images ?? []).map(i => i.uid))
    const wanted = r.imageUids.filter(u => inSet.has(u))
    const gone = r.imageUids.filter(u => !known.has(u))
    const elsewhere = r.imageUids.filter(u => known.has(u) && !inSet.has(u))

    const prevCfg = { ...settings.getBatchMovieConfig(set) }
    const prevOut = outputSnapshot(set)
    const prevSel = project.getImageSelection('batchMovies', set)

    settings.replaceBatchMovieConfig(set, r.cfg)
    // `titleCard` deliberately NOT patched into the per-set `movie` bag: this page reads its card from
    // its OWN config (`cfg.titleCard`), and that bag is the viewer recorder's card. Restoring a batch
    // would otherwise silently rewrite the title card of the ViewerPanel Record button.
    const { titleCard: _card, ...output } = r.output
    settings.setMovieConfig(set, output)
    // AFTER the current tick, because arriving here is a NAVIGATION: `ImageTable` seeds its checkboxes
    // from this same store slot `onMounted`, and on the first visit that mount can land after this
    // callback — reading the old (empty) selection and committing it straight back over ours. On a
    // second click the page is already mounted, which is exactly why it worked the second time
    // (Dominik, 2026-08-10). One tick puts us unambiguously after the seed either way.
    if (wanted.length) nextTick(() => project.setImageSelection('batchMovies', set, wanted))

    const dropped = [...r.dropped]
    if (gone.length) dropped.push(`${gone.length} image(s) that no longer exist`)
    if (elsewhere.length) dropped.push(`${elsewhere.length} image(s) from another set`)
    // Nothing banked WHICH image — every movie recorded before that was banked. Worth saying, because
    // the config lands and the selection does not, which otherwise looks like the restore half-failed.
    if (!r.imageUids.length) dropped.push('which image(s) it was recorded on — select them yourself')
    return {
      undo: () => {
        settings.replaceBatchMovieConfig(set, prevCfg)
        settings.setMovieConfig(set, prevOut)
        if (wanted.length) project.setImageSelection('batchMovies', set, prevSel)
        project.activeSetUid = prevSetUid
      },
      note: restoreNote(missingRefs(r.cfg, availableFor(wanted)), dropped),
    }
  },
})

// ── Which half is expanded — the shared two-half panel primitive (utils/paneExpand.ts) ──
// Same arrangement as the module pages' TaskRunner: config on top, task list below. Its own storage key,
// so this panel remembers its arrangement separately from the runner's.
const { pane, toggle: togglePane } = usePaneExpand('cc-batchmovies-pane')
</script>

<template>
  <div class="bm" :class="'pane-' + pane">
    <!-- Above the hint as well as the config: arriving here with nothing selected still means the page
         was filled in, and the Undo has to be reachable either way. -->
    <RestoreNotice v-if="restoreNotice" class="bm-restored" :source="restoreNotice.movie"
                   :note="restoreNotice.note" @undo="undoRestore" @dismiss="dismissRestore" />
    <p v-if="!selectedUids.length" class="bm-hint cc-muted">Select one or more images (left) to author a batch of movies.</p>

    <template v-else>
      <PaneExpandBar
        :pane="pane"
        top-label="movie config" bottom-label="task list"
        top-icon="pi-cog" bottom-icon="pi-bars"
        @toggle="togglePane"
      />

      <!-- BUSY banner: the batch is running -->
      <div v-if="running" class="bm-busy">
        <i class="pi pi-spin pi-spinner" />
        <span>Generating batch movies…</span>
      </div>

      <!-- ── The CONFIG half ── every `.bm-sec` below, plus the actions row: hidden as a group by the
           `pane-bottom` rule in this file's CSS rather than a guard on each one, so a section added
           later is covered without remembering to guard it. -->

      <!-- Channels -->
      <section class="bm-sec">
        <h4>
          Channels <span class="bm-sub cc-muted">shown channels + colormap (others hidden)</span>
          <button class="bm-link" :disabled="seeding || !viewer.openImage?.imageUid"
                  @click="fillFromView(true)"
                  title="Copy the channel colours + overlays from the image currently open in the viewer">
            <i class="pi pi-sync" /> fill from view
          </button>
        </h4>
        <p v-if="!channelList.length" class="bm-hint cc-muted">No channel names on the selected images.</p>
        <div v-for="ch in channelList" :key="ch" class="bm-row">
          <span class="bm-ch">{{ ch }}</span>
          <SwatchSelect :model-value="channels[ch] ?? ''" :options="colormapOpts"
                        @update:model-value="v => setChannel(ch, v)"
                        v-tooltip.bottom="'Colourmap this channel renders in'" />
        </div>
      </section>

      <!-- Overlays -->
      <section class="bm-sec">
        <h4>Overlays <span class="bm-sub cc-muted">click to toggle</span></h4>
        <ChipSelect class="bm-toggles" multiple :options="OVERLAY_OPTIONS" v-model="overlaysModel"
                    aria-label="Movie overlays" />
        <div v-if="showTracks" class="bm-inset">
          <span class="bm-lbl cc-muted">tail</span>
          <input type="range" min="1" max="20" step="1" v-model.number="tailWidth"
                 v-tooltip.bottom="'Thickness of the track tail'" />
          <span class="bm-val">{{ tailWidth }}</span>
        </div>
        <div v-if="showPops" class="bm-inset">
          <select v-model="popType" class="bm-mini" v-tooltip.bottom="'Which population type to draw as points'">
            <option value="flow">gating</option>
            <option value="clust">clusters</option>
          </select>
          <span class="bm-lbl cc-muted">size</span>
          <input type="range" min="1" max="20" step="1" v-model.number="pointsSize"
                 v-tooltip.bottom="'Diameter of the population points'" />
          <span class="bm-val">{{ pointsSize }}</span>
        </div>
      </section>

      <!-- Colour by -->
      <section class="bm-sec">
        <h4>Colour by <span class="bm-sub cc-muted">measure used to colour tracks / labels</span></h4>
        <select v-model="colourBy" v-tooltip.bottom="'Cell measure mapped to track and label colour'">
          <option value="">— none (population / default colour) —</option>
          <option v-for="c in obsCols" :key="c" :value="c">{{ c }}</option>
        </select>
      </section>

      <!-- Image versions — one chip = that version, two or more = a side-by-side comparison -->
      <section v-if="versionNames.length > 1" class="bm-sec">
        <h4>Image versions <span class="bm-sub cc-muted">click to include · drag to order</span></h4>
        <MovieCompareControls :available="versionNames"
                              :available-segmentations="segNames"
                              v-model:versions="compareVersions"
                              v-model:segmentations="compareSegmentations"
                              v-model:contour="labelContour"
                              v-model:layout="compareLayout"
                              v-model:contrast="compareContrast" />
      </section>

      <!-- Movie — the same controls as the viewer recorder and the animation page -->
      <section class="bm-sec">
        <h4>Movie</h4>
        <MovieOutputControls :suffix-options="movieSuffixes" v-model:fps="fps" v-model:sizeX="sizeX" v-model:sizeY="sizeY"
                             v-model:suffix="suffix" :canvas-x="canvasSizeX" :canvas-y="canvasSizeY"
                             v-model:timestamp="movieTimestamp" v-model:scale-bar="movieScaleBar"
                             :size-z="zDepth" v-model:show3D="show3D" v-model:zSlice="zSlice"
                             :levels="multiscaleLevels" v-model:detail3d="detail3d" />
        <!-- Only when there is a timelapse to trim. The bound is the LONGEST in the selection — the
             backend clamps each image to its own length, so a shorter one records to its end. -->
        <MovieTimeRange v-if="tFrames > 1" v-model:tStart="tStart" v-model:tEnd="tEnd" :frames="tFrames" />
        <TitleCardControls v-model="titleCardModel" />
      </section>

      <!-- Output — what is batch-specific: how each file gets named -->
      <section class="bm-sec">
        <h4>Output</h4>
        <div class="bm-attrs">
          <span class="bm-lbl cc-muted" v-tooltip.left="'Attributes joined to build each movie filename'">filename attrs <span class="bm-sub cc-muted">click to include · drag to reorder</span></span>
          <ChipSelect v-if="attrOptions.length" v-model="fileAttrs" :options="attrOptions" multiple reorderable
                      aria-label="Filename attributes" />
          <span v-else class="bm-hint cc-muted">no attributes — files named by {{ nameByImage ? 'image name' : 'uid' }}</span>
        </div>
        <!-- What ENDS the filename. The two recorders had chosen differently — a single viewer
             recording is named after the image, a batch after the uid — so a restored viewer config
             wrote a uid-named twin beside the original. Restoring one turns this on. -->
        <div class="bm-inset">
          <!-- the caveat, not a restatement of the label — two images CAN share a name, uids cannot -->
          <CcToggle v-model="nameByImage" label="name files after the image"
                    v-tooltip.bottom="'Images sharing a name overwrite each other'" />
        </div>
        <p class="bm-preview cc-muted">→ movies/<b>{{ filenamePreview }}</b></p>
      </section>

      <!-- Actions -->
      <div class="bm-actions cc-row">
        <button class="cc-btn cc-btn-primary" data-guide="batchMovies.generate" :disabled="!canRun" @click="generate"
                v-tooltip.top="compareActionTip(compareShapeNow, 'Record one movie per selected image')">
          <i class="pi pi-video" /> Generate movies ({{ selectedUids.length }})
        </button>
      </div>

      <!-- ── The TASKS half ── wrapped so the same `pane-<mode>` CSS hides it: TaskList's root belongs to
           that component, and reaching into it from here would be a scoped-CSS trick waiting to break. -->
      <div class="bm-tasks">
        <TaskList module="batchMovies" />
      </div>
    </template>
  </div>
</template>

<style scoped>
.bm { display: flex; flex-direction: column; gap: 7px; flex: 1; min-width: 0; padding: 2px; }
.bm-hint { margin: 2px 0; }
.bm-restored { margin-bottom: 0.5rem; }
/* Which half is showing, declared once per half — the same mechanism TaskRunner uses. Every config
   member is a direct `.bm-sec` child plus the actions row, so one rule covers the group AND a section
   added later, which a per-element guard would miss. The busy banner is in neither half on purpose:
   the running state matters MOST while you are watching the task list. */
.bm.pane-bottom > .bm-sec,
.bm.pane-bottom > .bm-actions { display: none; }
.bm.pane-top    > .bm-tasks   { display: none; }
/* The tasks half OWNS its overflow, the way TaskRunner's `.tasks-section`/`.tasks-scroll` pair does.
   Without a scroll container here a wide task row (a long image name, the five row buttons) made the
   whole PANEL wider and put a horizontal scrollbar under the config — every section shifted left
   while a task ran (Dominik, 2026-08-10). `min-height: 0` is what lets it scroll instead of growing
   the column; `min-width: 0` the same for the cross axis. */
.bm-tasks {
  display: flex; flex-direction: column;
  flex: 1 1 auto; min-width: 0;
  /* A FLOOR, not `min-height: 0`. The config half above has no cap (TaskRunner's `.params-section` is
     capped at 45vh), so `flex: 1` handed the list whatever was left — which on a long config was a
     clipped sliver (Dominik, 2026-08-10). It scrolls itself from here rather than shrinking away. */
  min-height: 14rem;
  overflow: auto;
}
/* …unless the pane bar has given it the WHOLE panel, where a floor would be a ceiling on nothing */
.bm.pane-bottom > .bm-tasks { min-height: 0; }
/* A "batch running" advisory — NOT the job's progress (the scheduler reports that in TasksModule). It
   states the condition of a resource, so it is a severity and takes the CVD-safe amber. */
.bm-busy { display: flex; align-items: center; gap: 8px; padding: 6px 9px; border-radius: var(--cc-radius-md);
  background: color-mix(in srgb, var(--cc-sev-warn) 16%, transparent); border: 1px solid var(--cc-sev-warn);
  color: var(--cc-text); font-size: var(--cc-fs-md); }
.bm-sec { border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md); padding: 6px 8px; background: var(--cc-surface-1); }
.bm-sec h4 { display: flex; align-items: baseline; margin: 0 0 4px; font-size: var(--cc-fs-md); font-weight: 700; }
.bm-mini { min-width: 0; padding: 0.2rem 1.4rem 0.2rem 0.4rem; }
.bm-sub { margin-left: 6px; }
.bm-link { float: right; font-size: var(--cc-fs-xs); color: var(--cc-accent); background: none; border: none;
  cursor: pointer; padding: 0; display: inline-flex; align-items: center; gap: 3px; }
.bm-link:hover:not(:disabled) { text-decoration: underline; }
.bm-link:disabled { opacity: 0.4; cursor: not-allowed; }
.bm-row { display: flex; align-items: center; gap: 8px; margin: 3px 0; }
.bm-ch { flex: 1; font-size: var(--cc-fs-md); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.bm-row select, .bm-sec > select { min-width: 150px; }
.bm-chk { display: flex; align-items: center; gap: 6px; font-size: var(--cc-fs-md); margin: 3px 0; cursor: pointer; }
.bm-chk.inline { display: inline-flex; margin-right: 10px; }
.bm-inset { display: flex; align-items: center; gap: 6px; margin: 5px 0 1px; }
/* range inputs default to a fixed intrinsic width (~129px) and don't shrink, so a slider sharing a row
   with the size fields overflows the sidebar. Let it flex down to share the available width. */
.bm-inset input[type="range"] { flex: 1; min-width: 0; }

.bm-val { font-size: var(--cc-fs-sm); min-width: 1.6rem; }
.bm-attrs { margin-top: 6px; display: flex; flex-direction: column; gap: 4px; }
.bm-preview { margin: 6px 0 0; word-break: break-all; }
.bm-preview b { color: var(--cc-text); }
</style>
