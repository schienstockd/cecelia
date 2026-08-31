<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import { useTaskStore } from '../stores/tasks'
import { useViewerStore } from '../stores/viewer'
import { openViewerWindow } from '../utils/viewerWindow'
import { buildTitleCard, type TitleCardPayload } from '../utils/titleCard'
import {
  colourLegend, colourLegendLabels, resetColourLegend,
  livePreviews, previewShown, togglePreview,
} from '../composables/useNapariAutoShow'
import { activeValueName, CELL_POP_TYPES, type CellPopType, trackableValueNames } from '../utils/napariAutoShow'
import type { TitleCardCfg } from '../utils/batchMovie'
import TitleCardControls from './TitleCardControls.vue'
import MovieOutputControls from './MovieOutputControls.vue'
import MovieTimeRange from './MovieTimeRange.vue'
import MovieOptionsButton from './MovieOptionsButton.vue'
import MovieCompareControls from './MovieCompareControls.vue'
import InlineNote from './InlineNote.vue'
import { movieSizeParams } from '../utils/movieSize'
import { clampContour, seedConfigFromViewState, type ViewStateLike } from '../utils/batchMovie'
import { normaliseItems, compareSuffix, compareActionTip, compareShape,
         COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         type CompareLayout, type CompareContrast } from '../utils/movieCompare'
import { useViewerMovieDefaults } from '../composables/useViewerMovieDefaults'
import { useMovieSuffixes } from '../composables/useMovieSuffixes'

const projectStore = useProjectStore()
const projectMeta  = useProjectMetaStore()

// Suffixes already used in this project, offered in the recorder's "name" field. Lazily fetched and
// cached across the three recorder panels — see composables/useMovieSuffixes.ts.
const { suffixes: movieSuffixes, ensure: ensureMovieSuffixes } = useMovieSuffixes()
watch(() => projectMeta.current?.uid ?? '', (uid: string) => { void ensureMovieSuffixes(uid) }, { immediate: true })
const settings     = useSettingsStore()
const ws           = useWsStore()
const log          = useLogStore()
const taskStore    = useTaskStore()
const viewerStore  = useViewerStore()

// Is a recording in flight? The napari viewer is UI-serial — one render at a time — so the Record
// button reflects the TASK, not a local flag: the render outlives this component's request and its
// progress/Cancel live in the task list. Covers the batch too, which drives the same viewer.
const recordingTask = computed(() => taskStore.tasks.some(t =>
  (t.funName ?? '').startsWith('movie.') && (t.status === 'queued' || t.status === 'running')
))

const selectedValueName = ref('')
const visibleLabels     = ref<Record<string, boolean>>({})
const gatedTracksShown  = ref(false)   // master "show gated track populations" toggle (TEST/SDGF)
const recording         = ref(false)   // a one-click timelapse recording is in progress

// per-pop-type population overlays as centroid POINTS. WHICH pop types these are is defined once, in
// utils/napariAutoShow (CELL_POP_TYPES) — the app-level autoshow restores exactly the same set, so a
// new pop type can't end up toggleable-but-never-restored. Only CELL-grained types are in that list:
// show-populations plots by cell label, whereas track/trackclust are track-grained (membership is
// track_ids) — their napari viz is ribbons (the Tracks-ribbon toggle below / per-segmentation
// directions), and trackclust ribbons are still to come. Layers are namespaced by pop type in the
// bridge, so flow + clust + region coexist. resolve_pops is generic over pop_type, so region (a filter
// on regions.{suffix}) resolves + colours its centroids like any other cell pop.
// Only the icon/label (presentation) lives here — icons MATCH the sidebar module nav (Gate =
// pi-chart-scatter, Cluster cells = pi-palette, Region = pi-map, Track = pi-share-alt, Cluster tracks
// = pi-sitemap) so a pop type reads the same.
const POP_TYPE_UI: Record<CellPopType, { icon: string; label: string }> = {
  flow:   { icon: 'pi-chart-scatter', label: 'gating populations' },
  clust:  { icon: 'pi-palette',       label: 'cell-cluster populations' },
  region: { icon: 'pi-map',           label: 'spatial-region populations' },
}
const POP_TYPES = CELL_POP_TYPES.map(key => ({ key, ...POP_TYPE_UI[key] }))
const trackVns          = ref<Record<string, boolean>>({})   // per-segmentation track-overlay visibility
const branchVns         = ref<Record<string, boolean>>({})   // per-segmentation branch-overlay visibility
const colourByCol       = ref('')      // obs column to shade tracks + labels by ('' = default)
const obsCols           = ref<string[]>([])   // obs columns of the open segmentation (colour-by options)

// The image the user is focused on — napari OR the browser viewer. This computed name is a legacy
// (P6 renames it). The read is now `openImageUid`: the panel gates its content on ANY viewer being
// open, not just napari. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P1.
const napariImage = computed(() => {
  const uid = projectStore.openImageUid
  if (!uid) return null
  for (const set of projectStore.sets) {
    const img = set.images.find(i => i.uid === uid)
    if (img) return img
  }
  return null
})

const valueNames = computed(() => Object.keys(napariImage.value?.filepaths ?? {}))
const labelNames  = computed(() => Object.keys(napariImage.value?.labels ?? {}))

// One row per label set, registered ones plus any store a task is writing RIGHT NOW (see
// `livePreviews`). A live-only set has no h5ad, no tracks and no branches yet — and deleting a store
// mid-write makes no sense — so its row offers the preview toggle and nothing else. A re-run to an
// EXISTING name is both at once: one row, with the preview watching the store the re-run recreated.
// Value names with a measurement table but NO mask — a track set imported directly from ImageJ or
// TrackMate for an image nothing has segmented. `labels` and `labelPropsNames` are two independent
// ccid.json registries, and such a set is only in the second, so before this it had no row at all:
// no tracks toggle, nothing, while gating and the observer listed it. It is a real analysis object
// with no pixels, which is a row that offers fewer toggles — not an absent row.
const pointsOnlyNames = computed(() =>
  (napariImage.value?.labelPropsNames ?? []).filter(vn => !labelNames.value.includes(vn)))

const labelRows = computed(() => {
  const live = new Set(livePreviews.value.map(p => p.valueName))
  const names = [...labelNames.value,
                 ...pointsOnlyNames.value,
                 ...[...live].filter(vn => !labelNames.value.includes(vn)
                                        && !pointsOnlyNames.value.includes(vn))]
  return names.map(valueName => ({
    valueName,
    registered: labelNames.value.includes(valueName) || pointsOnlyNames.value.includes(valueName),
    // `masked` gates everything that needs PIXELS: the show-labels eye and the branches toggle.
    // Tracks need only a `track_id` column, so they stay available on a points row — which is the
    // whole reason the row exists.
    masked: labelNames.value.includes(valueName),
    live: live.has(valueName),
  }))
})
const hasLabelRows = computed(() => labelRows.value.length > 0)
/**
 * Segmentations worth a row without being asked for: the ones DOING something — a mask, tracks or
 * branches on screen, or a run writing right now.
 *
 * Seven registered segmentations is an ordinary number on a real image (`fXgbTl` has seven), and a row
 * each made this section taller than everything under it put together — the panel became a scroll to
 * reach Populations (Dominik, 2026-08-25). The rest are one click away rather than gone, because which
 * segmentations EXIST is still the question this section answers.
 */
const activeLabelRows = computed(() => labelRows.value.filter(
  r => r.live || previewShown.value[r.valueName] || visibleLabels.value[r.valueName]
    || trackVns.value[r.valueName] || branchVns.value[r.valueName]))
const labelsExpanded = ref(false)
const shownLabelRows = computed(() =>
  labelsExpanded.value ? labelRows.value : activeLabelRows.value)
const foldedLabelCount = computed(() => labelRows.value.length - activeLabelRows.value.length)

// the set the open image belongs to — the key for per-set napari viewer prefs (colour-by, show-3D,
// point size, overlay toggles). These are experiment-level: set once, hold across the set's images.
const currentSetUid = computed(() =>
  projectStore.openImageUid ? projectStore.setUidOfImage(projectStore.openImageUid) : null)
// per-set option accessors bound to the open image's set (write-throughs persist to the settings store)
// Deliberately the SAME stored value the viewer's 3D button reads and the movie's z control writes —
// one setting with two entry points, not two settings that can disagree about what is on screen.
const show3D = computed<boolean>({
  get: () => currentSetUid.value ? settings.getShow3D(currentSetUid.value) : false,
  set: v => {
    if (currentSetUid.value) settings.setShow3D(currentSetUid.value, v)
  } })
// Which z slice a 2D recording pins. The LIVE viewer's current z wins over any stored value — a
// movie captures the plane the user is looking at, so scrubbing z in the viewer moves the movie
// form's slider too (reported: "the zslice is still not updating in the popover when i scrub in
// the viewer"). Stored value is the fallback for when no viewer is publishing (fresh session,
// closed viewer). The LIVE plane is chosen in the WebGPU viewer itself (its own `zPlane`), so
// setting this here doesn't need a mirror push.
const zSlice = computed<number | null>({
  get: () => {
    if (viewerZ.value != null) return viewerZ.value
    return currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).zSlice : null
  },
  set: v => {
    if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { zSlice: v })
  } })
// Which stretch of the timelapse the Record button sweeps (frame indices; null end = the last frame).
// Persisted per set like fps/size — the same pair the Batch page authors, read by the same `_t_range`.
const movieTStart = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).tStart : 0,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { tStart: v }) } })
const movieTEnd = computed<number | null>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).tEnd : null,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { tEnd: v }) } })

const popVisible = (popType: string): boolean =>
  currentSetUid.value ? settings.getPopVisible(currentSetUid.value, popType) : false
const setPopVisible = (popType: string, v: boolean) => {
  if (currentSetUid.value) settings.setPopVisible(currentSetUid.value, popType, v) }
// timelapse-recording params (per set): frame rate + output size (null size = the napari canvas size)
const movieFps = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).fps : 15,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { fps: v }) } })
const movieSizeX = computed<number | null>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).sizeX : null,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { sizeX: v }) } })
const movieSizeY = computed<number | null>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).sizeY : null,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { sizeY: v }) } })
// Filename addition. A movie is named after the IMAGE, so recording the AF-corrected version and then
// the raw import would overwrite the first — hence a suffix, prefilled with the version SHOWN in napari
// (`null` = never touched → use that default; `''` = the user cleared it, which must stick).
// A comparison names itself after the versions it shows, so it can't overwrite either single-version
// recording; a plain record still falls back to the version shown in napari.
const movieSuffixDefault = computed(() =>
  (compareVersions.value.length || compareSegmentations.value.length)
    ? compareSuffix(compareVersions.value, compareSegmentations.value)
    : (selectedValueName.value && selectedValueName.value !== 'default' ? selectedValueName.value : ''))
const movieSuffix = computed<string>({
  get: () => {
    const stored = currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).suffix : null
    return stored ?? movieSuffixDefault.value
  },
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { suffix: v }) } })
// How much detail the 3D render uses — a multiscale LEVEL index (0 = full resolution, higher =
// coarser). The WebGPU viewer picks its own detail level (`pickVolumeLevel`); persisted here as a
// per-set MOVIE parameter, not a live-viewer knob.
const detail3d = computed<number>({
  get: () => currentSetUid.value ? (settings.getMovieConfig(currentSetUid.value).detail3d ?? 0) : 0,
  set: v => {
    if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { detail3d: v })
  } })

// Side-by-side version comparison (docs/todo/MOVIE_COMPARE_PLAN.md). The selection IS the mode: none
// records what's on screen (unchanged), two or more record a column per version into one movie.
const compareVersions = computed<string[]>({
  get: () => currentSetUid.value
    ? normaliseItems(settings.getMovieConfig(currentSetUid.value).compareVersions, valueNames.value)
    : [],
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { compareVersions: v }) } })
// …and the segmentation axis's list. Empty means "whatever is on screen": the recorder then sends NO
// mask list at all, which is what keeps the plain "record what's shown" record untouched.
const compareSegmentations = computed<string[]>({
  get: () => currentSetUid.value
    ? normaliseItems(settings.getMovieConfig(currentSetUid.value).compareSegmentations, labelNames.value)
    : [],
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { compareSegmentations: v }) } })
// Mask outline width for the NEXT recording (persisted per-set). The live WebGPU viewer draws
// its outline from `settings.viewerLabelContour` (global), which is a separate control; two knobs
// that used to converge on the napari canvas and now don't. Kept as-is: unifying them is a
// panel-UX call, not a P9-scope change.
const labelContour = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).labelContour : 0,
  set: v => {
    const n = clampContour(v)
    if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { labelContour: n })
  } })
// What the recording draws as masks. An explicit pick wins; otherwise the label sets the user has
// toggled on in this panel, so a version comparison keeps the masks that are on screen instead of
// coming back bare in every column after the first (the re-open clears the canvas).
const movieLabelValueNames = computed<string[]>(() =>
  compareSegmentations.value.length
    ? compareSegmentations.value
    : labelNames.value.filter(vn => visibleLabels.value[vn]))
// Skeletons (`segment.branching`) are a separate registry with a separate toggle, and they had the
// same bug masks did — the re-open cleared them and nothing asked for them back. There is no movie
// picker for them (they are deliberately kept out of the generic labels picker), so the recorder
// takes what is ON SCREEN, which is what "record what's shown" means for an overlay with no config.
const movieBranchValueNames = computed<string[]>(() =>
  Object.keys(napariImage.value?.branchLabels ?? {}).filter(vn => branchVns.value[vn]))
const compareLayout = computed<CompareLayout>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).compareLayout : COMPARE_LAYOUT_DEFAULT,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { compareLayout: v }) } })
// Versions across, masks down — picking from BOTH lists fully determines the layout. One list leaves
// the arrangement (across / stacked / wrapped into a grid) to `compareLayout`, so the shape needs it.
const compareShapeNow = computed(() =>
  compareShape(compareVersions.value, compareSegmentations.value, compareLayout.value))
const compareContrast = computed<CompareContrast>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).compareContrast : COMPARE_CONTRAST_DEFAULT,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { compareContrast: v }) } })

// The movie OPTIONS (fps / size / name / title card) live in a popover off the gear — see
// MovieOptionsButton, which owns that chrome for both this panel and the Animation page.
// napari's baked overlays. They are drawn into the canvas, so a recording burns them in — hiding them
// is a record-time decision, and the batch RE-OPENS each image (which turns the scale bar back on),
// so toggling them in the napari window is not an alternative.
const movieTimestamp = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).showTimestamp : true,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { showTimestamp: v }) } })
const movieScaleBar = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).showScaleBar : true,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { showScaleBar: v }) } })

// Title card (Phase H, H3) — per-set, merge-patched so each control keeps the others' values.
const movieTitleCard = computed<TitleCardCfg>(() =>
  currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).titleCard : { enabled: true, note: '', durationSec: 3 })
function patchMovieTitle(p: Partial<TitleCardCfg>) {
  if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { titleCard: { ...movieTitleCard.value, ...p } }) }
// TitleCardControls owns the clamp and emits a whole config; this just persists it.
const movieTitleCardModel = computed<TitleCardCfg>({
  get: () => movieTitleCard.value,
  set: v => patchMovieTitle(v),
})


// These refs drive the TOGGLE UI only. The layers themselves are (re)pushed by the app-level
// useNapariAutoShow (on open) and onGatingChange — neither reads these refs, so this watcher's timing
// can no longer affect what actually reaches napari (it once did: see useNapariAutoShow's rules).
watch(napariImage, (img) => {
  // restore the remembered preference rather than always starting hidden
  gatedTracksShown.value = currentSetUid.value ? settings.getShowGatedTracks(currentSetUid.value) : false
  colourByCol.value = currentSetUid.value ? settings.getColourBy(currentSetUid.value) : ''   // per-set
  labelsExpanded.value = false                 // a different image is a different list
  if (!img) { selectedValueName.value = ''; visibleLabels.value = {}; trackVns.value = {}; branchVns.value = {}; obsCols.value = []; return }
  // Tracks are seeded over BOTH registries (labels ∪ branchLabels) — a points-only set is exactly
  // the one whose tracks you want. Label visibility below is NOT unioned — there are no pixels to
  // show for a set with no `labels` registry.
  trackVns.value = settings.getTrackVisibility(img.uid, trackableValueNames(img))
  branchVns.value = settings.getBranchVisibility(img.uid, Object.keys(img.branchLabels ?? {}))
  // Default to the active version (the `_active` key from the versioned filepath dict) — this is what
  // the server opens when no valueName is passed, so the dropdown must agree (shared resolver).
  selectedValueName.value = activeValueName(img)
  // Restore remembered label visibility for this image; unknown labels default to true.
  visibleLabels.value = settings.getLabelVisibility(img.uid, Object.keys(img.labels ?? {}))
  loadObsCols()                       // colour-by options for the selected segmentation
}, { immediate: true })

function openInViewer(valueName: string) {
  const uid        = projectStore.openImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return
  openViewerWindow({ projectUid, imageUid: uid, valueName: valueName || undefined })
}

// One-click timelapse recording: sweep the open image's T axis in the CURRENT view (whatever channels/
// populations/colour-by are shown) to an .mp4 under the project's movies/ folder.
//
// Sent over the WS task rail (`movie:record`), exactly like a batch: the recording appears in the task
// list with a live progress bar and a working Cancel. It used to be a blocking POST that resolved when
// the movie was finished — a frozen button, no progress, and no way out of a 4K render started by
// mistake. The button no longer owns the "in progress" state either; the task list does.
async function recordTimelapse() {
  // `openImageUid` is the ANY-viewer field — set by ImageTable's eye button whether the popup
  // browser viewer OR napari has the image. Was `napariImageUid`, which stayed null when only the
  // browser viewer was open, so the Record button silently early-returned (a regression the user
  // hit after napari was retired from the record path).
  const uid        = projectStore.openImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid || recording.value || recordingTask.value) return
  recording.value = true
  try {
    // The live view state, read ONCE and used for two things: the title card's non-channel sections,
    // and the `look` banked with the movie so it can be remade later
    // (docs/todo/MOVIE_MANAGEMENT_PLAN.md Decision 7). It used to be fetched only when the title card
    // was on — but the look has to be captured whether or not the movie carries a card, and this
    // recorder records what is ON SCREEN, so the view state is the only place that look exists.
    //
    // Read the browser viewer's published state (`useViewerStore.viewState`) — the popup writes it
    // on every camera / channel change. When no popup viewer is open on this image the look is
    // simply not banked; the card still renders (buildTitleCard tolerates a null snapshot) and the
    // recorder captures the current channel resolution at record time.
    let snapshot: ViewStateLike | null = null
    if (viewerStore.viewState && viewerStore.openImage?.imageUid === uid) {
      snapshot = viewerStore.viewState as unknown as ViewStateLike
    }

    // Title card (Phase H): built via the SHARED buildTitleCard — the same path the animation page
    // uses. Channels are added by the recorder from the live viewer, so the frontend supplies only
    // title + non-channel sections.
    const colourBy  = currentSetUid.value ? settings.getColourBy(currentSetUid.value) : ''
    const overrides = (currentSetUid.value && colourBy) ? settings.getColourOverrides(currentSetUid.value, colourBy) : {}
    let titleCard: TitleCardPayload | undefined
    if (movieTitleCard.value.enabled) {
      titleCard = await buildTitleCard(projectUid, uid, snapshot, napariImage.value,
        { note: movieTitleCard.value.note, durationSec: movieTitleCard.value.durationSec, colourBy, colourOverrides: overrides })
    }
    // The look, in the SAME shape the Batch page authors — one config kind, so a recorded look edits
    // on the page built to edit looks. `seedConfigFromViewState` is the existing live-view → config
    // reader ("fill from view" on that page); the colour-by is not in the layer names, so it rides
    // along from the per-set setting the overlays were actually drawn with.
    const look = { ...seedConfigFromViewState(snapshot, napariImage.value?.channelNames ?? []),
                   ...(colourBy ? { colourBy } : {}) }
    const versions = compareVersions.value
    const shape    = compareShapeNow.value
    const t = taskStore.add({
      module: 'viewer',
      label: shape.cells > 1
        ? (shape.grid ? `Compare ${shape.cols} x ${shape.rows}` : `Compare ${shape.cells}`)
          + ` — ${napariImage.value?.name ?? 'movie'}`
        : `Record ${napariImage.value?.name ?? 'movie'}`,
      imageUid: uid, imageName: napariImage.value?.name ?? '', status: 'queued',
      taskName: 'movie.record', funName: 'movie.record', params: {}, projectUid,
    })
    ws.send({
      type: 'movie:record', taskId: t.id, projectUid, imageUid: uid, fps: movieFps.value,
      suffix: movieSuffix.value, titleCard, apiUrl: window.location.origin,
      // More than one cell = a comparison; one is the plain record the backend already did.
      // `labelValueNames` is OMITTED when there is nothing to say — absent means "leave the masks
      // alone", which is what the plain record has always done.
      valueNames: versions, labelContour: labelContour.value,
      show3D: show3D.value, zSlice: show3D.value ? null : zSlice.value,
      // which stretch of the timelapse to sweep; null end = the last frame, clamped per image
      tStart: movieTStart.value, tEnd: movieTEnd.value,
      ...(movieLabelValueNames.value.length ? { labelValueNames: movieLabelValueNames.value } : {}),
      ...(movieBranchValueNames.value.length ? { branchValueNames: movieBranchValueNames.value } : {}),
      compareLayout: compareLayout.value, compareContrast: compareContrast.value,
      showTimestamp: movieTimestamp.value, showScaleBar: movieScaleBar.value,
      // banked with the movie, not acted on by the recorder — it already records this look by
      // recording the screen (MOVIE_MANAGEMENT_PLAN.md Phase 4)
      look,
      // The full napari-shape snapshot rides alongside `look`. `look` covers the channel picks +
      // overlay flags; the snapshot's `camera` + `canvas` are what the offline record needs to
      // reproduce the visible rectangle — a viewer zoomed into a corner would otherwise record
      // the whole image at native aspect (bug reported 2026-08-29, the movie/viewer side-by-side).
      // Absent when the snapshot fell through the napari fallback.
      ...(snapshot ? { viewState: snapshot } : {}),
      ...movieSizeParams(movieSizeX.value, movieSizeY.value),
    })
  } catch (e) {
    log.error(`Record timelapse failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally {
    // the RENDER is the task's business now; this flag only covers assembling the request
    recording.value = false
  }
}

// Push a pop type's populations to napari as centroid points. The server shows EVERY segmentation's
// pops at once (each as its own value_name-tagged layer), so `valueName` no longer selects which pops
// appear — the overlay is independent of which segmentation is "active" (opening the image shows all,
// not just the active/first one). The `valueName` used to be forwarded as the bridge's per-pop
// default; the WebGPU viewer reads pop visibility off the shared settings bag now (P5).

// Per-pop-type visibility toggle. Persist + ping the WebGPU viewer to re-derive its hidden-pop set.
// Was dual-write to napari; the mirror went with the P9 slice.
function togglePopType(popType: string) {
  setPopVisible(popType, !popVisible(popType))
  pingViewerOverlays()
}

// Tracks: the WebGPU viewer draws per-segmentation ribbons off `settings.getTrackVisibility` (P7).

// Per-segmentation toggle: flip this segmentation's track overlay, persist, ping the viewer.
function toggleTrack(vn: string) {
  // `openImageUid`, not `napariImageUid`: this write must land whether or not any legacy napari WS
  // event has fired. Before P6 the persist was gated on `napariImageUid=null`, which meant the
  // WebGPU viewer never saw the write (Dominik, 2026-08-26: "i can toggle. but nothing happens").
  const uid = projectStore.openImageUid
  trackVns.value = { ...trackVns.value, [vn]: !trackVns.value[vn] }
  if (uid) settings.setTrackVisibility(uid, trackVns.value)
  pingViewerOverlays()
}

// Per-segmentation toggle: flip this segmentation's branch (skeleton) label overlay. Persist +
// ping; the WebGPU viewer reads `settings.getBranchVisibility` (via P4-style store).
function toggleBranch(vn: string) {
  const uid   = projectStore.openImageUid
  const files = napariImage.value?.branchLabels?.[vn] ?? []
  if (!files.length) {
    log.error(`No branch label files registered for "${vn}"`, { source: 'viewer' })
    return
  }
  const wasVisible = branchVns.value[vn] ?? true
  branchVns.value = { ...branchVns.value, [vn]: !wasVisible }
  if (uid) settings.setBranchVisibility(uid, branchVns.value)
  pingViewerOverlays()
}

// Master toggle for the gated track populations (TEST/SDGF), like the Show populations toggle.
function toggleGatedTracks() {
  const next = !gatedTracksShown.value
  gatedTracksShown.value = next
  if (currentSetUid.value) settings.setShowGatedTracks(currentSetUid.value, next)
  pingViewerOverlays()
}

// Master toggle for the trackclust (track-cluster) populations as ribbons. Persisted per pop type
// (per-set popVis['trackclust']); ping so the viewer re-derives.
function toggleTrackclust() {
  setPopVisible('trackclust', !popVisible('trackclust'))
  pingViewerOverlays()
}

// ── Colour-by an obs column (tracks + labels) ──────────────────────────────────
// The WebGPU viewer reads `settings.getColourBy(setUid)` and re-derives colours on the overlay
// tick; the panel writes the setting and pings. Options are the open segmentation's obs columns.
async function loadObsCols() {
  const uid = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  const vn = selectedValueName.value
  if (!uid || !projectUid || !vn) { obsCols.value = []; return }
  try {
    const q = `projectUid=${projectUid}&imageUid=${uid}&valueName=${encodeURIComponent(vn)}`
    const res = await fetch(`/api/gating/channels?${q}`)
    if (res.ok) {
      const j = await res.json() as { obsColumns?: string[]; trackColourColumns?: string[] }
      // cell obs columns + track-level clusters.* (colour-by broadcasts a track column to its cells,
      // so you can colour tracks by their cluster/population). Track columns last; de-duplicated.
      obsCols.value = [...new Set([...(j.obsColumns ?? []), ...(j.trackColourColumns ?? [])])]
    } else obsCols.value = []
  } catch { obsCols.value = [] }
  // this image's segmentation may not have the SET's colour-by column (segmentations differ across a
  // set) — don't select/apply it here, but do NOT clear the persisted per-set value: another image in
  // the set may have it, and it's restored per image from the set on open.
  if (colourByCol.value && !obsCols.value.includes(colourByCol.value)) colourByCol.value = ''
}

// User picked a colour-by column: persist per-set; the WebGPU viewer picks it up via the overlay
// tick and re-derives label + track colours from the settings bag.
function onColourBy(e: Event) {
  const col = (e.target as HTMLSelectElement).value
  colourByCol.value = col
  if (currentSetUid.value) settings.setColourBy(currentSetUid.value, col)   // per-set
  resetColourLegend()
  pingViewerOverlays()
}

// Recolour a category value that has no population (its colour isn't defined anywhere) and ping so
// the viewer redraws; persisted per set + column.
function onRecolour(value: string, hex: string) {
  if (!currentSetUid.value || !colourByCol.value) return
  settings.setColourOverride(currentSetUid.value, colourByCol.value, value, hex)
  pingViewerOverlays()
}
// Clear this column's user recolours → back to population colours / the default palette.
function resetColours() {
  if (!currentSetUid.value || !colourByCol.value) return
  settings.clearColourOverrides(currentSetUid.value, colourByCol.value)
  pingViewerOverlays()
}

// Legend rows to render: pop-backed values are DEDUPED by population name — one population can be
// defined by several category values (e.g. a "Meandering" pop spanning two clusters), which share the
// pop's one colour, so they collapse to a single row. Values with no population stay one row each and
// are recolourable (their colour isn't defined anywhere else). `value` is the wire key for recolouring.
const legendItems = computed(() => {
  const seenPop = new Set<string>()
  const items: { key: string; label: string; hex: string; value: string; editable: boolean }[] = []
  for (const [value, hex] of Object.entries(colourLegend.value)) {
    const pop = colourLegendLabels.value[value]
    if (pop) {
      if (seenPop.has(pop)) continue            // same population, another cluster → one row only
      seenPop.add(pop)
      items.push({ key: `pop:${pop}`, label: pop, hex, value, editable: false })
    } else {
      items.push({ key: `val:${value}`, label: value, hex, value, editable: true })
    }
  }
  return items
})

/**
 * "Active version" advisory, same shape and severity split as tasks' `paramAdvisors`. Moved out of
 * the popup viewer window (VIEWER_CONTROLS_SPLIT_PLAN.md P3 extended, Dominik 2026-08-26): the panel
 * is the single control now, so the advisory sits next to the control that changes it.
 *
 * `null` when there is only one version, or `activeValueName` isn't reported — an absent answer
 * must read as no claim, not as a pass.
 */
const versionNote = computed(() => {
  const img = napariImage.value
  const active = img?.activeValueName
  const vn = selectedValueName.value
  if (!active || !vn || valueNames.value.length < 2) return null
  return vn === active
    ? { severity: 'ok' as const, short: 'Active version',
        detail: 'The version every task on this image reads.' }
    : { severity: 'warn' as const, short: 'Not the active version',
        detail: `Tasks on this image read "${active}". This view is of "${vn}".` }
})

function onValueNameChange(e: Event) {
  const name = (e.target as HTMLSelectElement).value
  selectedValueName.value = name
  loadObsCols()
  // Broadcast to the popup viewer — the panel is the single source of truth for the version now
  // (VIEWER_CONTROLS_SPLIT_PLAN.md P3 extended). The popup's storage-event bridge (P2) picks this
  // up via settings.setImageVersion → `cc.viewerImageVersion` and calls its `changeVersion`
  // internally, so the two windows never disagree about the version on screen. If no popup is up
  // yet, `openInViewer` opens one on the picked version.
  const openUid = projectStore.openImageUid
  if (openUid) settings.setImageVersion(openUid, name)
  else openInViewer(name)
}

// Fire the WebGPU-viewer overlays-refetch ping. Every panel toggle that changes what the viewer
// should draw calls this after persisting settings, so a popup viewer with its own store re-reads
// its inputs (label bag, pop-type bag, track bag) via the `storage` bridge. See P5 of
// VIEWER_CONTROLS_SPLIT_PLAN.md.
function pingViewerOverlays() {
  const openUid = projectStore.openImageUid
  if (!openUid || typeof localStorage === 'undefined') return
  localStorage.setItem('cc.viewerOverlaysTick', `${openUid}:${Date.now()}`)
}

function toggleLabel(valueName: string) {
  // Write the settings bag; the WebGPU viewer reads it via `storage` events.
  //
  // Radio-like: the WebGPU viewer draws one label mask at a time (r32uint, single-slot bind group;
  // multi-mask is deferred to PX). Ticking a segmentation UNticks the others so what you see in the
  // panel matches what you see in the viewer, instead of the viewer silently picking one of several
  // ticked (Dominik, 2026-08-25: "dont just show the last one clicked").
  const uid = projectStore.openImageUid
  const wasVisible = visibleLabels.value[valueName] ?? false
  const next = !wasVisible
  // Every label name gets an EXPLICIT boolean — `settings.getLabelVisibility` defaults unknown
  // names to true (so a fresh image shows all masks), which means omitting a name leaves it
  // reading as visible in the viewer. Radio-like: only `valueName` is true when enabling; all
  // false when disabling. Build the full name set from the panel + the store's current view so
  // no name gets orphaned.
  const allNames = new Set<string>([
    ...Object.keys(napariImage.value?.labels ?? {}),
    ...Object.keys(visibleLabels.value),
  ])
  const bag: Record<string, boolean> = {}
  for (const n of allNames) bag[n] = next && n === valueName
  visibleLabels.value = bag
  if (uid) settings.setLabelVisibility(uid, bag)
  pingViewerOverlays()
}

function onTaskStatus(data: Record<string, unknown>) {
  const status = String(data.status ?? '')
  if (!settings.viewerAutoUpdate) return
  if (status !== 'done') return
  const openUid = projectStore.openImageUid
  const taskUid = String(data.imageUid ?? '')
  if (!openUid || taskUid !== openUid) return
  reloadViewer()   // data-only unless the user ticked reset (task changed pixels → reopen)
  // Ping the WebGPU popup so it refetches overlays (pop counts / colours may have changed after a
  // seg or gating task). Slabs are NOT re-invalidated from here — a mask-writing task rewrites the
  // label store on disk, and the popup keeps its cached mask until `labelName` changes. That gap
  // is spelled out in VIEWER_CONTROLS_SPLIT_PLAN.md → Audit § refresh-labels.
  if (typeof localStorage !== 'undefined') {
    localStorage.setItem('cc.viewerOverlaysTick', `${openUid}:${Date.now()}`)
  }
}

// Refresh the SHOWN image. Data-only by default (ping the viewer to refetch overlays; the pyramid
// and camera stay); only reopen the whole image when the user ticked reset, or nothing is shown yet.
// This is what the eye (on the already-open image) and finished tasks call, so a plain reload no
// longer yanks the image out from under the user (mirrors viewerManager.R: reopen only on reset /
// uID change).
function reloadViewer() {
  if (settings.viewerResetOnReload || !projectStore.openImageUid) openInViewer(selectedValueName.value)
  else pingViewerOverlays()
}

function onTaskResult(data: Record<string, unknown>) {
  const imageUid = String(data.imageUid ?? '')
  if (!imageUid || imageUid !== projectStore.openImageUid) return
  const meta = (data.meta ?? {}) as Record<string, unknown>

  const addedValueName = meta.valueName as string | undefined
  if (addedValueName) {
    selectedValueName.value = addedValueName
    if (settings.viewerAutoUpdate) reloadViewer()   // data-only unless reset
  }

  const labelValueName = meta.labelValueName as string | undefined
  // A task that wrote a label store — the popup's cached mask pixels for THIS vn are stale. Ping
  // the popup so it invalidates its slabs. The listener on the other side matches on `imageUid`
  // AND `valueName`, so only the popup showing the affected mask reallocates.
  if (labelValueName && typeof localStorage !== 'undefined') {
    localStorage.setItem('cc.viewerSlabsTick',
                          `${imageUid}:${labelValueName}:${Date.now()}`)
  }
  if (labelValueName && settings.viewerAutoUpdate) {
    // Mark newly added label as visible; the WebGPU viewer picks it up via the overlay tick
    // fired by pingViewerOverlays. Radio-like — clear the others so only this new one is on.
    const uid = projectStore.openImageUid
    const bag: Record<string, boolean> = {}
    for (const n of Object.keys(napariImage.value?.labels ?? {})) bag[n] = n === labelValueName
    bag[labelValueName] = true
    visibleLabels.value = bag
    if (uid) settings.setLabelVisibility(uid, bag)
    nextTick(() => pingViewerOverlays())
  }
}

// the image-table eye, clicked on the ALREADY-open image, asks us to reload it (data-only unless reset)
watch(() => projectStore.viewerReloadTick, () => reloadViewer())

// Placeholder defaults for the movie size fields + level range for the 3D detail control. Sourced
// from the browser volume viewer's own published state (see useViewerMovieDefaults) — the canvas is
// what a movie records at when no size is asked for.
const { canvasSizeX, canvasSizeY, multiscaleLevels, viewerZ } = useViewerMovieDefaults()

onMounted(() => {
  ws.on('task:status', onTaskStatus)
  ws.on('task:result', onTaskResult)
})
onUnmounted(() => {
  ws.off('task:status', onTaskStatus)
  ws.off('task:result', onTaskResult)
})
</script>

<template>
  <div class="viewer-panel">
    <!-- ── View: viewer behaviour toggles (global prefs; apply on next open) ──
         Top of the panel — these are always available, even before an image is open. -->
    <!-- Convention: append new toggles at the END of the row. -->
    <div class="viewer-section first">
      <div class="viewer-section-title cc-eyebrow cc-fs-2xs">View</div>
      <div class="viewer-opts cc-row cc-row-tight">
        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.viewerAutoUpdate }"
          @click="settings.viewerAutoUpdate = !settings.viewerAutoUpdate"
          v-tooltip.bottom="'Auto-update: refresh the viewer whenever a task finishes on that image'"
        ><i class="pi pi-refresh" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.viewerResetOnReload }"
          @click="settings.viewerResetOnReload = !settings.viewerResetOnReload"
          v-tooltip.bottom="'Reopen the whole image, not just data — needed after pixels change'"
        ><i class="pi pi-image" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.viewerAutoSaveLayerProps }"
          @click="settings.viewerAutoSaveLayerProps = !settings.viewerAutoSaveLayerProps"
          v-tooltip.bottom="'Save contrast, colormap and T/Z as you change them'"
        ><i class="pi pi-bookmark" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': show3D }" :disabled="!currentSetUid"
          @click="show3D = !show3D"
          v-tooltip.bottom="'3D view: open images in 3D where they have a z-axis (per experiment/set)'"
        ><span class="opt-text">3D</span></button>

      </div>
    </div>

    <template v-if="napariImage">
      <!-- ── Current image: what's open + its versions + segmentation label sets ── -->
      <div class="viewer-section">
        <div class="viewer-section-title cc-eyebrow cc-fs-2xs">Current image</div>
        <div class="viewer-image">
          <i class="pi pi-eye viewer-eye" />
          <span class="viewer-name" :title="napariImage.name">{{ napariImage.name }}</span>
        </div>
        <select
          v-if="valueNames.length"
          class="viewer-select"
          :value="selectedValueName"
          @change="onValueNameChange"
          v-tooltip.bottom="`Which image version to show`"
        >
          <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
        </select>
        <span v-else class="viewer-hint cc-muted">No versions registered.</span>
        <!-- Whether what is on screen is the version every task runs against. Through `InlineNote`,
             the same shape a task param's advisory uses — same statement, we checked and here is
             what we found. Moved from the popup viewer (P3 extended). -->
        <InlineNote
          v-if="versionNote" :severity="versionNote.severity"
          :short="versionNote.short" :detail="versionNote.detail"
        />

        <!-- segmentation label sets: show labels / tracks, delete -->
        <div v-if="shownLabelRows.length" class="viewer-labels-list"
             :class="{ 'is-scrolled': labelsExpanded }">
          <div v-for="row in shownLabelRows" :key="row.valueName" class="viewer-label-row">
            <i :class="['pi', row.masked ? 'pi-th-large' : 'pi-circle-fill', 'viewer-label-icon']"
               v-tooltip.right="row.masked ? undefined : 'Points only — tracks, no mask'" />
            <span class="viewer-label-name cc-muted" :title="row.valueName">{{ row.valueName }}</span>
            <!-- action icons are hidden until row hover (keeps the narrow sidebar tidy); an ACTIVE
                 toggle stays visible so you can see what's shown without hovering -->
            <!-- The live-preview toggle is NOT hover-hidden: it exists only while the run does, so a
                 hidden affordance would be missed for good. -->
            <button
              v-if="row.live"
              class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': previewShown[row.valueName] }"
              @click="togglePreview(row.valueName)"
              v-tooltip.right="previewShown[row.valueName] ? 'Hide the live preview' : 'Preview this run while it writes'"
            ><i class="pi pi-bolt" /></button>
            <!-- Ordered DERIVED → BASIS, left to right: branches, tracks, then the segmentation itself
                 rightmost, because the segmentation is what the other two are computed from. -->
            <template v-if="row.registered">
              <button
                v-if="row.masked && (napariImage?.branchLabels?.[row.valueName]?.length ?? 0) > 0"
                class="opt-btn cc-btn cc-btn-ghost cc-btn-icon row-act" :class="{ 'cc-btn-on cc-btn-on-tint': branchVns[row.valueName] }"
                @click="toggleBranch(row.valueName)"
                v-tooltip.right="branchVns[row.valueName] ? 'Hide this segmentation\'s branches' : 'Show this segmentation\'s branches'"
              ><i class="pi pi-wave-pulse" /></button>
              <button
                class="opt-btn cc-btn cc-btn-ghost cc-btn-icon row-act" data-guide="viewer.toggleTracks"
                :class="{ 'cc-btn-on cc-btn-on-tint': trackVns[row.valueName] }"
                @click="toggleTrack(row.valueName)"
                v-tooltip.right="trackVns[row.valueName] ? 'Hide this segmentation\'s tracks' : 'Show this segmentation\'s tracks'"
              ><i class="pi pi-share-alt" /></button>
              <button
                v-if="row.masked"
                class="opt-btn cc-btn cc-btn-ghost cc-btn-icon row-act" data-guide="viewer.toggleLabels"
                :class="{ 'cc-btn-on cc-btn-on-tint': visibleLabels[row.valueName] }"
                @click="toggleLabel(row.valueName)"
                v-tooltip.right="visibleLabels[row.valueName] ? 'Hide labels in Napari' : 'Show labels in Napari'"
              ><i class="pi pi-eye" /></button>
              <!-- No delete here. Deleting a label set is one scope of the Import page's Delete modal
                   (docs/todo/IMAGE_DELETE_PLAN.md Decision 4) — the viewer shows and hides layers, it
                   does not curate what exists on disk. -->
            </template>
          </div>
        </div>
        <!-- The fold. Named by COUNT rather than "Show all": the number is the whole reason to click,
             and a section that silently shows a subset would read as segmentations having vanished. -->
        <button
          v-if="hasLabelRows && foldedLabelCount > 0"
          class="cc-btn cc-btn-ghost viewer-more cc-fs-2xs"
          @click="labelsExpanded = !labelsExpanded"
          v-tooltip.bottom="labelsExpanded ? 'Show only the segmentations in use'
                                           : 'Show every segmentation on this image'"
        >{{ labelsExpanded ? 'Show fewer' : `${foldedLabelCount} more` }}</button>
      </div>

    <!-- ── Populations & tracks: overlays on the open image ────────────────── -->
    <!-- pop toggles show coloured centroid POINTS (layers namespaced by pop type, so they coexist);
         the ribbon toggles show gated / cluster track populations as napari Tracks layers. -->
    <div class="viewer-section">
      <div class="viewer-section-title cc-eyebrow cc-fs-2xs">Populations &amp; tracks</div>
      <div class="viewer-opts cc-row cc-row-tight">
        <button
          v-for="pt in POP_TYPES" :key="pt.key"
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': popVisible(pt.key) }"
          @click="togglePopType(pt.key)"
          v-tooltip.bottom="`${popVisible(pt.key) ? 'Hide' : 'Show'} ${pt.label} (points)`"
        ><i :class="['pi', pt.icon]" /></button>
        <!-- Tracks as ribbons (TEST/SDGF gated track pops); per-segmentation _tracked toggles live
             in the Segmentations list above (directions icon per row) -->
        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': gatedTracksShown }"
          @click="toggleGatedTracks"
          v-tooltip.bottom="gatedTracksShown ? 'Hide track-pop ribbons' : 'Show track populations as ribbons (track-measure gates)'"
        ><i class="pi pi-share-alt" /></button>
        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': popVisible('trackclust') }"
          @click="toggleTrackclust"
          v-tooltip.bottom="popVisible('trackclust') ? 'Hide track-cluster ribbons' : 'Show track-cluster populations as ribbons'"
        ><i class="pi pi-sitemap" /></button>
      </div>
    </div>

      <!-- ── Colour by: shade tracks + labels by an obs column (e.g. HMM state); '' = default ── -->
      <div v-if="obsCols.length" class="viewer-section">
        <div class="viewer-section-title cc-eyebrow cc-fs-2xs">Colour by</div>
        <select class="opt-colourby" :value="colourByCol" @change="onColourBy"
                v-tooltip.bottom="'Colour tracks + labels by a cell property (e.g. HMM state)'">
          <option value="">default</option>
          <option v-for="c in obsCols" :key="c" :value="c">{{ c }}</option>
        </select>
        <!-- legend for a categorical colour-by: value → colour (a population's colour where one matches) -->
        <div v-if="legendItems.length" class="cby-legend cc-row">
          <span v-for="item in legendItems" :key="item.key" class="cby-item cc-muted cc-fs-xs"
                v-tooltip.right="item.editable
                  ? `${item.label} — click the swatch to recolour`
                  : `population: ${item.label} — colour set in the population manager`">
            <!-- pop-backed → static swatch (its colour is the population's, edit it there);
                 value with no population → editable colour input (it's not defined anywhere else) -->
            <span v-if="!item.editable" class="cby-swatch" :style="{ background: item.hex }" />
            <input v-else type="color" class="cby-swatch cby-swatch-edit" :value="item.hex"
                   @change="onRecolour(item.value, ($event.target as HTMLInputElement).value)" />
            {{ item.label }}
          </span>
          <button class="cby-reset" @click="resetColours"
                  v-tooltip.right="'Reset colours to population colours / the default palette'">Reset</button>
        </div>
      </div>

      <!-- ── Movie: record the CURRENT view over time → mp4 (project's movies/ folder) ──
           Records exactly what's shown (channels, populations, tracks, colour-by). fps + size + the
           filename suffix are per-set; the render runs as a task (progress + Cancel in the task list),
           and the fuller config (which channels/pops, T-range, batch) is F1.2/F1.3.
           Picking 2+ versions records them side by side instead (MOVIE_COMPARE_PLAN.md). The options
           sit in a popover: this panel is narrow, and they are set once and then left alone, while the
           version chips are the thing you change per movie. ONE row — an image with a single version
           (the common case) shows just the two buttons. -->
      <div class="viewer-section" data-guide="viewer.movieSection">
        <div class="viewer-section-title cc-eyebrow cc-fs-2xs">Movie</div>
        <div class="movie-row">
          <MovieCompareControls class="movie-versions" :available="valueNames"
                                :available-segmentations="labelNames"
                                v-model:versions="compareVersions"
                                v-model:segmentations="compareSegmentations"
                                v-model:contour="labelContour"
                                v-model:layout="compareLayout"
                                v-model:contrast="compareContrast" />
          <MovieOptionsButton class="opt-btn">
            <MovieOutputControls :suffix-options="movieSuffixes" v-model:fps="movieFps" v-model:sizeX="movieSizeX" v-model:sizeY="movieSizeY"
                                 v-model:suffix="movieSuffix" :canvas-x="canvasSizeX" :canvas-y="canvasSizeY"
                                 v-model:timestamp="movieTimestamp" v-model:scale-bar="movieScaleBar"
                                 :size-z="napariImage?.sizeZ" v-model:show3D="show3D"
                                 v-model:zSlice="zSlice" :default-z="viewerZ"
                                 :levels="multiscaleLevels" v-model:detail3d="detail3d" />
            <!-- Only for an actual timelapse — nothing to trim on a single frame -->
            <MovieTimeRange v-if="(napariImage?.sizeT ?? 1) > 1" v-model:tStart="movieTStart"
                            v-model:tEnd="movieTEnd" :frames="napariImage?.sizeT ?? 1" />
            <TitleCardControls v-model="movieTitleCardModel" />
          </MovieOptionsButton>
          <button class="opt-btn cc-btn cc-btn-ghost cc-btn-icon movie-rec" data-guide="viewer.record"
                  :class="{ 'cc-btn-on cc-btn-on-tint': recording || recordingTask }" :disabled="recording || recordingTask"
                  @click="recordTimelapse"
                  v-tooltip.bottom="compareActionTip(compareShapeNow,
                    'Record the current view over the time axis → mp4 in the project\'s movies/ folder')">
            <i :class="['pi', (recording || recordingTask) ? 'pi-spin pi-spinner' : 'pi-video']" />
          </button>
        </div>
      </div>
    </template>
    <div v-else class="viewer-section"><span class="viewer-hint cc-muted">No image open.</span></div>
  </div>
</template>

<style scoped>
.viewer-panel {
  padding: 0.35rem 0.6rem 0.4rem;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}

/* stale-bridge warning strip — amber, brief; the Restart button is the action */
.viewer-stale {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  padding: 0.28rem 0.4rem;
  border: 1px solid var(--cc-sev-warn);
  border-radius: var(--cc-radius-sm);
  background: color-mix(in srgb, var(--cc-sev-warn) 14%, transparent);
  color: var(--cc-sev-warn);
  font-size: var(--cc-fs-xs);
}
.viewer-stale-txt { flex: 1; min-width: 0; }
.viewer-stale-btn {
  flex-shrink: 0;
  font-size: var(--cc-fs-xs);
  font-weight: 600;
  padding: 0.15rem 0.45rem;
  border-radius: var(--cc-radius-xs);
  border: 1px solid var(--cc-sev-warn);
  background: none;
  color: var(--cc-sev-warn);
  cursor: pointer;
}
.viewer-stale-btn:hover { background: color-mix(in srgb, var(--cc-sev-warn) 22%, transparent); }

.viewer-image {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  min-width: 0;
}
.viewer-eye { font-size: var(--cc-fs-sm); color: #f97316; flex-shrink: 0; }
.viewer-name {
  font-size: var(--cc-fs-sm);
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

/* visual styling from the global form base (style.css) */
.viewer-select { width: 100%; }
/* colour-by dropdown: full width on its own line (the sidebar is narrow, so inline it clipped) */
.opt-colourby { font-size: var(--cc-fs-xs); width: 100%; min-width: 0; }
/* ONE row: the version chips take the width, the gear and Record are pinned to its right. The chips
   wrap inside their own flex child, so the buttons keep their place instead of drifting mid-wrap —
   which is why they don't just share the wrapping row directly. Top-aligned so they stay on the FIRST
   line when the chips (or the comparison's layout/contrast row) run to several. */
.movie-row { display: flex; align-items: flex-start; gap: 0.3rem; }
.movie-versions { flex: 1; min-width: 0; }
.movie-rec { margin-left: 0.1rem; }
/* the popover is free of the panel's width, so give the controls room to lay out on one line each */
/* .movie-lbl/-range/-val/-controls were left behind when MovieOutputControls was extracted — the
   component owns them now, so the dead rules are gone rather than re-orphaned here. */

/* colour-by legend: value → swatch (a population's colour where one matches, else default) */
.cby-legend { margin-top: 0.25rem; }
.cby-item { display: inline-flex; align-items: center; gap: 0.25rem; }
.cby-swatch { width: 0.7rem; height: 0.7rem; border-radius: var(--cc-radius-xs); flex-shrink: 0; border: 1px solid var(--cc-border); }
/* editable swatch: a native colour input squeezed to swatch size (categories with no population) */
.cby-swatch-edit { padding: 0; cursor: pointer; background: none; -webkit-appearance: none; appearance: none; }
.cby-swatch-edit::-webkit-color-swatch-wrapper { padding: 0; }
.cby-swatch-edit::-webkit-color-swatch { border: none; border-radius: var(--cc-radius-xs); }
.cby-swatch-edit::-moz-color-swatch { border: none; border-radius: var(--cc-radius-xs); }
.cby-reset {
  font-size: var(--cc-fs-2xs); color: var(--cc-text-dim); background: none; border: none; cursor: pointer;
  padding: 0 0.2rem; text-decoration: underline; align-self: center;
}
.cby-reset:hover { color: var(--cc-text); }

/* ── Sections ────────────────────────────────────────────────────────────
   Group the controls under short headings (Segmentations / View / Populations &
   tracks / Colour by) so the narrow sidebar reads as labelled blocks rather than
   a wall of icons. A hairline above each keeps the groups visually distinct. */
.viewer-section {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  padding-top: 0.35rem;
  border-top: 1px solid var(--cc-border);
}
/* the top section (View) sits flush against the panel top — no leading divider */
.viewer-section.first { padding-top: 0; border-top: none; }

.viewer-labels-list {
  display: flex;
  flex-direction: column;
  gap: 0.2rem;
}
/* Expanded, the list is capped and scrolls rather than pushing Populations off the panel — the whole
   point of the fold. Six rows: enough that most images never scroll at all. */
.viewer-labels-list.is-scrolled { max-height: 9rem; overflow-y: auto; }
/* Left-aligned with the rows above it, not a full-width bar: it is a disclosure, not an action. */
.viewer-more { align-self: flex-start; padding: 0.1rem 0.3rem; }
.viewer-label-row {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  min-width: 0;
}
.viewer-label-icon {
  font-size: var(--cc-fs-sm);
  color: var(--cc-accent);
  flex-shrink: 0;
}
.viewer-label-name { flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.opt-btn.danger:hover { border-color: var(--cc-danger); color: var(--cc-danger); }
/* row action icons (eye / directions): hidden until the row is hovered to keep the narrow sidebar
   uncluttered; an ACTIVE toggle (shown layer/tracks) stays visible so state is readable */
.row-act { opacity: 0; transition: opacity 0.12s; }
/* hover-reveal: the row's actions stay hidden until hover, but an ENGAGED one must always show
   (it is the only indication the layer/track is on). Keyed on .cc-btn-on, the state primitive. */
.viewer-label-row:hover .row-act, .row-act.cc-btn-on { opacity: 1; }
/* ── Option toggles ──────────────────────────────────────────────────── */



.opt-btn { transition: background 0.1s, color 0.1s, border-color 0.1s; }   /* + cc-btn cc-btn-ghost cc-btn-icon */
.opt-btn:hover        { color: var(--cc-text); border-color: #484f58; }

.opt-text {
  font-size: var(--cc-fs-2xs);
  font-weight: 700;
  letter-spacing: 0.03em;
  line-height: 1;
}

</style>
