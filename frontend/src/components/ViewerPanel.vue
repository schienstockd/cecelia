<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import { useTaskStore } from '../stores/tasks'
import { pushLabels as apiPushLabels, buildTitleCard, pushZView, pushLabelContour, pushDetail3d,
         type TitleCardPayload } from '../utils/napariOverlays'
import {
  pushAllOverlays, pushTracksNow, pushPopulationsNow, pushColourLabelsNow,
  colourLegend, colourLegendLabels, resetColourLegend,
  livePreviews, previewShown, togglePreview,
} from '../composables/useNapariAutoShow'
import { activeValueName, CELL_POP_TYPES, type CellPopType } from '../utils/napariAutoShow'
import type { TitleCardCfg } from '../utils/batchMovie'
import TitleCardControls from './TitleCardControls.vue'
import MovieOutputControls from './MovieOutputControls.vue'
import MovieTimeRange from './MovieTimeRange.vue'
import MovieOptionsButton from './MovieOptionsButton.vue'
import MovieCompareControls from './MovieCompareControls.vue'
import { movieSizeParams } from '../utils/movieSize'
import { clampContour, seedConfigFromViewState, type ViewStateLike } from '../utils/batchMovie'
import { normaliseItems, compareSuffix, compareActionTip, compareShape,
         COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         type CompareLayout, type CompareContrast } from '../utils/movieCompare'
import { useNapariStatus } from '../composables/useNapariStatus'
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

// Hint gate: cache-on serves stale labels when the user re-runs seg to the same output name
// (see settings.napariLabelsCache docstring). Fire only when the cache IS on AND a segment
// task is queued/running on the currently-open image — otherwise the hint is noise.
const segCacheWarn = computed(() => {
  if (!settings.napariLabelsCache) return false
  const uid = projectStore.napariImageUid
  if (!uid) return false
  return taskStore.tasks.some(t =>
    t.module === 'segment' && t.imageUid === uid && (t.status === 'queued' || t.status === 'running')
  )
})

// Is a recording in flight? The napari viewer is UI-serial — one render at a time — so the Record
// button reflects the TASK, not a local flag: the render outlives this component's request and its
// progress/Cancel live in the task list. Covers the batch too, which drives the same viewer.
const recordingTask = computed(() => taskStore.tasks.some(t =>
  (t.funName ?? '').startsWith('movie.') && (t.status === 'queued' || t.status === 'running')
))

// Pull the error message out of a non-ok response (the API sends { error: "..." }).
async function _resError(res: Response): Promise<string> {
  try { const j = await res.json(); return j?.error ?? `HTTP ${res.status}` }
  catch { return `HTTP ${res.status}` }
}

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

const napariImage = computed(() => {
  const uid = projectStore.napariImageUid
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
const labelRows = computed(() => {
  const live = new Set(livePreviews.value.map(p => p.valueName))
  const names = [...labelNames.value, ...[...live].filter(vn => !labelNames.value.includes(vn))]
  return names.map(valueName => ({
    valueName, registered: labelNames.value.includes(valueName), live: live.has(valueName),
  }))
})
const hasLabelRows = computed(() => labelRows.value.length > 0)

// the set the open image belongs to — the key for per-set napari viewer prefs (colour-by, show-3D,
// point size, overlay toggles). These are experiment-level: set once, hold across the set's images.
const currentSetUid = computed(() =>
  projectStore.napariImageUid ? projectStore.setUidOfImage(projectStore.napariImageUid) : null)
// per-set option accessors bound to the open image's set (write-throughs persist to the settings store)
// Deliberately the SAME stored value the viewer's 3D button reads and the movie's z control writes —
// one setting with two entry points, not two settings that can disagree about what is on screen.
const show3D = computed<boolean>({
  get: () => currentSetUid.value ? settings.getShow3D(currentSetUid.value) : false,
  set: v => {
    if (currentSetUid.value) settings.setShow3D(currentSetUid.value, v)
    pushZView(v, zSlice.value)
  } })
// Which z slice a 2D recording pins. null = whatever is showing, which is what every recording did
// before the setting existed.
const zSlice = computed<number | null>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).zSlice : null,
  set: v => {
    if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { zSlice: v })
    // Apply the z choice to the LIVE viewer, so it is chosen by looking at it rather than by watching a
    // render finish. Coalesced in `napariOverlays` — a drag is a burst, and each push costs a plane load.
    pushZView(show3D.value, v)
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
// coarser). Pushed live like the z choice: it is a display property you judge by looking at it.
const detail3d = computed<number>({
  get: () => currentSetUid.value ? (settings.getMovieConfig(currentSetUid.value).detail3d ?? 0) : 0,
  set: v => {
    if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { detail3d: v })
    pushDetail3d(v)
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
// Mask outline width. Pushed to the LIVE viewer as well as persisted: it is a display property of the
// layers already on screen, so seeing it is how you choose it — a value you can only judge by watching
// a render finish is not a setting, it's a guess.
const labelContour = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).labelContour : 0,
  set: v => {
    const n = clampContour(v)
    if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { labelContour: n })
    // Apply the outline to every mask layer currently on screen. Coalesced in `napariOverlays`.
    pushLabelContour(labelNames.value.filter(vn => visibleLabels.value[vn]), n)
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
  if (!img) { selectedValueName.value = ''; visibleLabels.value = {}; trackVns.value = {}; branchVns.value = {}; obsCols.value = []; return }
  trackVns.value = settings.getTrackVisibility(img.uid, Object.keys(img.labels ?? {}))
  branchVns.value = settings.getBranchVisibility(img.uid, Object.keys(img.branchLabels ?? {}))
  // Default to the active version (the `_active` key from the versioned filepath dict) — this is what
  // the server opens when no valueName is passed, so the dropdown must agree (shared resolver).
  selectedValueName.value = activeValueName(img)
  // Restore remembered label visibility for this image; unknown labels default to true.
  visibleLabels.value = settings.getLabelVisibility(img.uid, Object.keys(img.labels ?? {}))
  loadObsCols()                       // colour-by options for the selected segmentation
}, { immediate: true })

async function openInNapari(valueName: string) {
  const uid        = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return
  const autoProps = settings.napariAutoSaveLayerProps
  const body: Record<string, unknown> = {
    imageUid:      uid,
    projectUid,
    valueName:     valueName || undefined,
    autoSaveProps: autoProps,
    autoLoadProps: autoProps,
    show3D:        show3D.value,
    asDask:        settings.napariAsDask,
  }
  // Labels/branches/tracks/populations are deliberately NOT sent here. Every open broadcasts
  // `napari:opened`, and the app-level autoshow (composables/useNapariAutoShow) restores all overlay
  // kinds from the remembered toggles in one sequential pass. Sending labels in the open body too
  // would load the same label pyramid twice and put two overlay pushes in flight at once — which the
  // bridge's one-command-at-a-time layer reconciliation does not tolerate.
  body.labelsCache = settings.napariLabelsCache
  try {
    const res = await fetch('/api/napari/open', {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify(body),
    })
    if (!res.ok && res.status !== 202)
      log.error(`Open in Napari failed: ${await _resError(res)}`, { source: 'napari' })
  } catch (e) {
    log.error(`Open in Napari failed: ${e instanceof Error ? e.message : String(e)}`,
              { source: 'napari' })
  }
}

// Toggling auto-save while an image is already open should take effect immediately (not only on the
// next open), so tell the bridge to start/stop live-saving the current image. No-op if napari isn't
// running — the flag still applies on the next open via /api/napari/open.
watch(() => settings.napariAutoSaveLayerProps, async enabled => {
  try {
    await fetch('/api/napari/configure-autosave', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ enabled }),
    })
  } catch { /* napari not running */ }
})


// One-click timelapse recording: sweep the open image's T axis in the CURRENT view (whatever channels/
// populations/colour-by are shown) to an .mp4 under the project's movies/ folder.
//
// Sent over the WS task rail (`movie:record`), exactly like a batch: the recording appears in the task
// list with a live progress bar and a working Cancel. It used to be a blocking POST that resolved when
// the movie was finished — a frozen button, no progress, and no way out of a 4K render started by
// mistake. The button no longer owns the "in progress" state either; the task list does.
async function recordTimelapse() {
  const uid        = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid || recording.value || recordingTask.value) return
  recording.value = true
  try {
    // The live view state, read ONCE and used for two things: the title card's non-channel sections,
    // and the `look` banked with the movie so it can be remade later
    // (docs/todo/MOVIE_MANAGEMENT_PLAN.md Decision 7). It used to be fetched only when the title card
    // was on — but the look has to be captured whether or not the movie carries a card, and this
    // recorder records what is ON SCREEN, so the view state is the only place that look exists.
    let snapshot: ViewStateLike | null = null
    try {
      const vsr = await fetch('/api/napari/view-state', {
        method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ projectUid }) })
      if (vsr.ok) snapshot = ((await vsr.json()) as { viewState?: ViewStateLike }).viewState ?? null
    } catch { /* best-effort — the card still renders, the look is simply not banked */ }

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
// not just the active/first one). The bridge namespaces layers by `(popType) (valueName)`, so
// flow/clust overlays across segmentations coexist. `valueName` is still forwarded as the bridge's
// per-pop default for older senders; blank is fine.
const pushPopulations = pushPopulationsNow

// Per-pop-type visibility toggle; the choice is remembered (persisted) so it carries across opens.
async function togglePopType(popType: string) {
  const next = !popVisible(popType)
  if (await pushPopulations(popType, next)) setPopVisible(popType, next)
}

// Push the tracks for the currently-toggled-on segmentations (one Tracks layer per segmentation,
// named by its value_name). `valueNames` = the segmentations whose "directions" toggle is on; empty
// → the bridge clears all track layers. `colorBy` shades vertices by the chosen obs column.
const onTrackVns = computed(() => Object.keys(trackVns.value).filter(vn => trackVns.value[vn]))
// Delegates: every toggle below persists to `settings` BEFORE pushing, so the shared push (which reads
// settings) sees the new value. It also harvests the colour-by legend into the shared refs.
const pushTracks = pushTracksNow

// Per-segmentation toggle: flip this segmentation's track overlay, persist, re-push the on-set.
async function toggleTrack(vn: string) {
  const uid = projectStore.napariImageUid
  trackVns.value = { ...trackVns.value, [vn]: !trackVns.value[vn] }
  if (uid) settings.setTrackVisibility(uid, trackVns.value)
  await pushTracks()
}

// Per-segmentation toggle: flip this segmentation's branch (skeleton) label overlay. Uses
// `allBranchLabels` on show-labels so the bridge routes to `branchLabels/` + names the layer
// `({vn}) Branches` (kept out of the generic labels picker — BRANCHING_PLAN Decision 6).
async function toggleBranch(vn: string) {
  const uid   = projectStore.napariImageUid
  const files = napariImage.value?.branchLabels?.[vn] ?? []
  if (!files.length) {
    log.error(`No branch label files registered for "${vn}"`, { source: 'napari' })
    return
  }
  const wasVisible = branchVns.value[vn] ?? true
  try {
    const res = await apiPushLabels({ branchLabels: { [vn]: files }, show: !wasVisible,
                                      cache: settings.napariLabelsCache })
    if (res?.ok) {
      branchVns.value = { ...branchVns.value, [vn]: !wasVisible }
      if (uid) settings.setBranchVisibility(uid, branchVns.value)
    } else {
      log.error(`Show branches "${vn}" failed: ${res ? await _resError(res) : 'network error'}`,
                { source: 'napari' })
    }
  } catch (e) {
    log.error(`Show branches "${vn}" failed: ${e instanceof Error ? e.message : String(e)}`,
              { source: 'napari' })
  }
}

// Master toggle for the gated track populations (TEST/SDGF), like the Show populations toggle.
async function toggleGatedTracks() {
  const next = !gatedTracksShown.value
  gatedTracksShown.value = next
  if (currentSetUid.value) settings.setShowGatedTracks(currentSetUid.value, next)
  await pushTracks()
}

// Master toggle for the trackclust (track-cluster) populations as ribbons. Persisted per pop type
// (per-set popVis['trackclust']); re-pushes the track overlays (one call covers all ribbons).
async function toggleTrackclust() {
  setPopVisible('trackclust', !popVisible('trackclust'))
  await pushTracks()
}

// ── Colour-by an obs column (tracks + labels) ──────────────────────────────────
// Tracks: pushTracks already sends `colorBy`. Labels: recolour the Labels layer via a
// DirectLabelColormap (column='' resets). Options are the open segmentation's obs columns.
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

// Delegate, but against the panel's SELECTED version rather than the image's active one — the user may
// be viewing a different version in the dropdown than the one the server would open by default.
const pushColourLabels = (column: string): Promise<boolean> =>
  pushColourLabelsNow(column, selectedValueName.value)

// user picked a colour-by column: persist, recolour the tracks (if shown) and the labels layer
function onColourBy(e: Event) {
  const col = (e.target as HTMLSelectElement).value
  colourByCol.value = col
  if (currentSetUid.value) settings.setColourBy(currentSetUid.value, col)   // per-set
  resetColourLegend()                       // clear old column's legend; pushes below repopulate
  if (onTrackVns.value.length || gatedTracksShown.value) pushTracks()   // re-push tracks w/ new color_by
  pushColourLabels(col)                       // recolour labels (or reset when col === '')
}

// recolour a category value that has no population (its colour isn't defined anywhere) and re-push both
// layers so the new colour shows immediately; persisted per set + column.
function onRecolour(value: string, hex: string) {
  if (!currentSetUid.value || !colourByCol.value) return
  settings.setColourOverride(currentSetUid.value, colourByCol.value, value, hex)
  if (onTrackVns.value.length || gatedTracksShown.value) pushTracks()
  pushColourLabels(colourByCol.value)
}
// clear this column's user recolours → back to population colours / the default palette
function resetColours() {
  if (!currentSetUid.value || !colourByCol.value) return
  settings.clearColourOverrides(currentSetUid.value, colourByCol.value)
  if (onTrackVns.value.length || gatedTracksShown.value) pushTracks()
  pushColourLabels(colourByCol.value)
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

function onValueNameChange(e: Event) {
  const name = (e.target as HTMLSelectElement).value
  selectedValueName.value = name
  loadObsCols()
  openInNapari(name)
}

async function toggleLabel(valueName: string) {
  const uid = projectStore.napariImageUid
  const files = napariImage.value?.labels?.[valueName] ?? []
  const wasVisible = visibleLabels.value[valueName] ?? false
  if (!files.length) {
    log.error(`No label files registered for "${valueName}"`, { source: 'napari' })
    return
  }
  try {
    // the outline rides the push: this rebuilds the layer, so omitting it refills the mask
    const res = await apiPushLabels({ labels: { [valueName]: files }, show: !wasVisible,
                                      cache: settings.napariLabelsCache,
                                      labelContour: labelContour.value })
    if (res?.ok) {
      visibleLabels.value = { ...visibleLabels.value, [valueName]: !wasVisible }
      if (uid) settings.setLabelVisibility(uid, visibleLabels.value)
    } else {
      log.error(`Show labels "${valueName}" failed: ${res ? await _resError(res) : 'network error'}`,
                { source: 'napari' })
    }
  } catch (e) {
    log.error(`Show labels "${valueName}" failed: ${e instanceof Error ? e.message : String(e)}`,
              { source: 'napari' })
  }
}

function onTaskStatus(data: Record<string, unknown>) {
  const status = String(data.status ?? '')
  if (!settings.napariUpdateImage) return
  if (status !== 'done') return
  const napariUid = projectStore.napariImageUid
  if (!napariUid || String(data.imageUid ?? '') !== napariUid) return
  reloadViewer()   // data-only unless the user ticked reset (task changed pixels → reopen)
}

// Refresh the SHOWN image. Data-only by default (re-push overlays, re-read from disk — the pyramid and
// camera stay); only reopen the whole image when the user ticked reset, or nothing is shown yet. This
// is what the eye (on the already-open image) and finished tasks call, so a plain reload no longer
// yanks the image out from under the user (mirrors viewerManager.R: reopen only on reset / uID change).
//
// The overlay re-push itself is NOT this panel's job — it lives in composables/useNapariAutoShow,
// mounted app-level, because this panel is `v-if`'d in App.vue and so cannot be relied on to exist
// when an image opens. Read the rules in that file before adding another overlay kind here.
function reloadViewer() {
  if (settings.napariResetOnReload || !projectStore.napariImageUid) openInNapari(selectedValueName.value)
  else void pushAllOverlays()
}

function onTaskResult(data: Record<string, unknown>) {
  const imageUid = String(data.imageUid ?? '')
  if (!imageUid || imageUid !== projectStore.napariImageUid) return
  const meta = (data.meta ?? {}) as Record<string, unknown>

  const addedValueName = meta.valueName as string | undefined
  if (addedValueName) {
    selectedValueName.value = addedValueName
    if (settings.napariUpdateImage) reloadViewer()   // data-only unless reset
  }

  const labelValueName = meta.labelValueName as string | undefined
  if (labelValueName && settings.napariUpdateImage) {
    // Mark newly added label as visible and show it in napari
    visibleLabels.value = { ...visibleLabels.value, [labelValueName]: true }
    nextTick(() => {
      const files = napariImage.value?.labels?.[labelValueName] ?? []
      if (files.length) {
        void apiPushLabels({ labels: { [labelValueName]: files }, show: true,
                             cache: settings.napariLabelsCache })
      }
    })
  }
}

// the image-table eye, clicked on the ALREADY-open image, asks us to reload it (data-only unless reset)
watch(() => projectStore.napariReloadTick, () => reloadViewer())

// Bridge status (shared poll — see useNapariStatus): `bridgeStale` warns that napari is running older
// code than the checkout (it's a separate process that survives a backend restart), and the canvas size
// is what a movie records at when no size is asked for, shown as the size fields' placeholder.
const { bridgeStale, canvasSizeX, canvasSizeY, multiscaleLevels, poll: pollBridge } = useNapariStatus()
async function restartNapari() {
  try {
    const res = await fetch('/api/napari/restart', {
      method: 'POST', headers: { 'Content-Type': 'application/json' }, body: '{}' })
    if (res.ok) log.info('Napari restarting — reopen the image to reload it.', { source: 'napari' })
    else log.error(`Napari restart failed: ${await _resError(res)}`, { source: 'napari' })
  } catch (e) {
    log.error(`Napari restart failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  }
  setTimeout(pollBridge, 1500)
}

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
    <!-- stale-bridge warning: napari started before the latest napari-code changes (it survives a
         backend restart). Brief here; the action is the Restart button + the tooltip. -->
    <div v-if="bridgeStale" class="viewer-stale"
         v-tooltip.bottom="'Napari started before your latest changes — restart it, then reopen the image'">
      <i class="pi pi-exclamation-triangle" />
      <span class="viewer-stale-txt">Napari running old code</span>
      <button class="viewer-stale-btn" @click="restartNapari">Restart</button>
    </div>
    <!-- ── View: viewer behaviour toggles (global prefs; apply on next open) ──
         Top of the panel — these are always available, even before an image is open. -->
    <!-- Convention: append new toggles at the END of the row. -->
    <div class="viewer-section first">
      <div class="viewer-section-title cc-eyebrow cc-fs-2xs">View</div>
      <div class="viewer-opts cc-row cc-row-tight">
        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.napariUpdateImage }"
          @click="settings.napariUpdateImage = !settings.napariUpdateImage"
          v-tooltip.bottom="'Auto-update: refresh Napari whenever a task finishes on that image'"
        ><i class="pi pi-refresh" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.napariResetOnReload }"
          @click="settings.napariResetOnReload = !settings.napariResetOnReload"
          v-tooltip.bottom="'Reopen the whole image, not just data — needed after pixels change'"
        ><i class="pi pi-image" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.napariLabelsCache }"
          @click="settings.napariLabelsCache = !settings.napariLabelsCache"
          v-tooltip.bottom="'Cache label chunks — faster scrubbing, but stale after seg re-runs'"
        ><i class="pi pi-bolt" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.napariAutoSaveLayerProps }"
          @click="settings.napariAutoSaveLayerProps = !settings.napariAutoSaveLayerProps"
          v-tooltip.bottom="'Save contrast, colormap and T/Z as you change them'"
        ><i class="pi pi-bookmark" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': show3D }" :disabled="!currentSetUid"
          @click="show3D = !show3D"
          v-tooltip.bottom="'3D view: open images in 3D where they have a z-axis (per experiment/set)'"
        ><span class="opt-text">3D</span></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.napariAsDask }"
          @click="settings.napariAsDask = !settings.napariAsDask"
          v-tooltip.bottom="'Fast open, slices on demand; untick for smoother viewing'"
        ><i class="pi pi-database" /></button>
      </div>

      <!-- Segment-running warning: cache-on serves stale label bytes on re-run (dask task-name
           collision → napari's opportunistic cache HIT). Only surface when the cache IS on and
           a segment task is actually queued/running on the open image; one-click fix. Reuses
           the .viewer-stale amber strip so the two warnings share the same visual language. -->
      <div v-if="segCacheWarn" class="viewer-stale">
        <i class="pi pi-exclamation-triangle" />
        <span class="viewer-stale-txt">Segmentation running — cache may hide new labels</span>
        <button class="viewer-stale-btn" @click="settings.napariLabelsCache = false">Cache off</button>
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
          v-tooltip.right="`Which image version to show in Napari`"
        >
          <option v-for="vn in valueNames" :key="vn" :value="vn">{{ vn }}</option>
        </select>
        <span v-else class="viewer-hint cc-muted">No versions registered.</span>

        <!-- segmentation label sets: show labels / tracks, delete -->
        <div v-if="hasLabelRows" class="viewer-labels-list">
          <div v-for="row in labelRows" :key="row.valueName" class="viewer-label-row">
            <i class="pi pi-th-large viewer-label-icon" />
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
                v-if="(napariImage?.branchLabels?.[row.valueName]?.length ?? 0) > 0"
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
                v-tooltip.right="'Colour tracks + labels by a cell property (e.g. HMM state)'">
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
                                 v-model:zSlice="zSlice"
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
    <div v-else class="viewer-section"><span class="viewer-hint cc-muted">No image open in Napari.</span></div>
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
