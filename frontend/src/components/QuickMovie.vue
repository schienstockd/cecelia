<!--
  Quick movie: record the image as the viewer shows it (or each ROI, or a version comparison) to an mp4
  in the project's movies/ folder, or save it as a figure — without going to the Movies page. Used in the
  viewer window (Share → Record movie…). Every option is per set (`settings.getMovieConfig`), shared with
  the Batch movies page's fps and size. The render runs as a task: progress and Cancel are in the task list.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import { useTaskStore } from '../stores/tasks'
import { useViewerStore } from '../stores/viewer'
import type { CciaImage } from '../stores/project'
import { buildTitleCard, type TitleCardPayload } from '../utils/titleCard'
import type { TitleCardCfg } from '../utils/batchMovie'
import TitleCardControls from './TitleCardControls.vue'
import MovieOutputControls from './MovieOutputControls.vue'
import MovieTimeRange from './MovieTimeRange.vue'
import MovieOptionsButton from './MovieOptionsButton.vue'
import MovieCompareControls from './MovieCompareControls.vue'
import InlineNote from './InlineNote.vue'
import CcToggle from './CcToggle.vue'
import ChipSelect from './ChipSelect.vue'
import { fetchRois, type RoiList, type RoiStatus } from '../utils/rois'
import { roiRecordSelection, roiRecordBlock, roiFigureSpec, roiPadFrames, ROI_OUTPUT_DEFAULT, ROI_PAD_FRAMES, ROI_PAD_MAX,
         type RecordWhat, type RoiOutput } from '../utils/roiMovie'
import { readViewerLook, timelapseKeyframes, volumeViewState, hexViewState, lookForRender } from '../utils/viewer/viewerLook'
import type { ViewerViewState } from '../utils/viewer/viewState'
import { movieSizeParams } from '../utils/movieSize'
import { clampContour, seedConfigFromViewState, RENDER_QUALITY_DEFAULT, resolveFrameRange,
         type ViewStateLike, type RenderQuality } from '../utils/batchMovie'
import { normaliseItems, compareSuffix, compareActionTip, compareShape,
         COMPARE_LAYOUT_DEFAULT, COMPARE_CONTRAST_DEFAULT,
         type CompareLayout, type CompareContrast } from '../utils/movieCompare'
import { useViewerMovieDefaults } from '../composables/useViewerMovieDefaults'
import { useMovieSuffixes } from '../composables/useMovieSuffixes'
import { figureSpecFromView, niceFigureBarUm, figureFrames, figureName, figureTipText, type FigureSpec } from '../utils/figureSpec'
import { pixelSizeUm } from '../utils/physicalSize'
import type { Severity } from '../lib/severity'

const props = withDefaults(defineProps<{
  /** the image recorded — the store's image object (`/api/images/meta` in the viewer window) */
  img: CciaImage | null
  projectUid: string
  /** its set: every movie option is per set */
  setUid: string
  /** the image version on screen — a plain record's version and name suffix */
  shownVersion: string
  /** show the "Movie" heading (the panel's section); the viewer's popover has its own */
  heading?: boolean
  /** the options below the buttons, not behind a gear — when this form is already in a popover */
  inlineOptions?: boolean
}>(), { heading: false, inlineOptions: false })

const settings    = useSettingsStore()
const ws          = useWsStore()
const log         = useLogStore()
const taskStore   = useTaskStore()
const viewerStore = useViewerStore()

const openedImage = computed(() => props.img)
const currentSetUid = computed(() => props.setUid || null)
const selectedValueName = computed(() => props.shownVersion)
const valueNames = computed(() => Object.keys(props.img?.filepaths ?? {}))
const labelNames = computed(() => Object.keys(props.img?.labels ?? {}))
const visibleLabels = computed<Record<string, boolean>>(() => props.img
  ? settings.getLabelVisibility(props.img.uid, labelNames.value) : {})
// Deliberately the SAME stored value the viewer's 2D / 3D switch writes — one setting.
const show3D = computed<boolean>({
  get: () => currentSetUid.value ? settings.getShow3D(currentSetUid.value) : false,
  set: v => { if (currentSetUid.value) settings.setShow3D(currentSetUid.value, v) } })

// Suffixes already used in this project, offered in the "name" field (composables/useMovieSuffixes.ts).
const { suffixes: movieSuffixes, ensure: ensureMovieSuffixes } = useMovieSuffixes()
watch(() => props.projectUid, (uid: string) => { void ensureMovieSuffixes(uid) }, { immediate: true })
// Is a recording in flight? The render is a task that outlives this request, so the button follows it.
const recordingTask = computed(() => taskStore.tasks.some(t =>
  (t.funName ?? '').startsWith('movie.') && (t.status === 'queued' || t.status === 'running')))
const recording = ref(false)   // assembling the request
const { canvasSizeX, canvasSizeY, viewerZ } = useViewerMovieDefaults()

// Which z slice a 2D recording pins. The LIVE viewer's current z wins over any stored value — a
// movie captures the plane the user is looking at, so scrubbing z in the viewer must also move
// the movie form's slider. Stored value is the fallback for when no viewer is publishing (fresh
// session, closed viewer). The LIVE plane is chosen in the WebGPU viewer itself (its own
// `zPlane`), so setting this here doesn't need a mirror push.
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
// Save as figure's frame interval over that range; null = auto (`figureFrames`)
const figureEvery = computed<number | null>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).figureEvery : null,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { figureEvery: v }) } })
const figureSplit = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).figureSplit : false,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { figureSplit: v }) } })
const figureGrid = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).figureGrid : false,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { figureGrid: v }) } })
// A taken file name (movie or figure): off = overwrite, on = add _001, _002, …
const numberIfTaken = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).numberIfTaken : false,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { numberIfTaken: v }) } })
const figureRange = computed(() => {
  const r = resolveFrameRange(movieTStart.value, movieTEnd.value, openedImage.value?.sizeT ?? 1)
  return figureFrames(r.lo, r.hi, figureEvery.value)
})
const figureTip = computed(() => figureTipText((openedImage.value?.sizeT ?? 1) > 1 ? figureRange.value : null))
const movieTEnd = computed<number | null>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).tEnd : null,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { tEnd: v }) } })

// timelapse-recording params (per set): frame rate + output size (null size = the viewer canvas size)
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
// the raw import would overwrite the first — hence a suffix, prefilled with the version SHOWN in the
// viewer (`null` = never touched → use that default; `''` = the user cleared it, which must stick).
// A comparison names itself after the versions it shows, so it can't overwrite either single-version
// recording; a plain record still falls back to the version shown in the viewer.
const movieSuffixDefault = computed(() =>
  (!matchViewer.value && (compareVersions.value.length || compareSegmentations.value.length))
    ? compareSuffix(compareVersions.value, compareSegmentations.value)
    : (selectedValueName.value && selectedValueName.value !== 'default' ? selectedValueName.value : ''))
const movieSuffix = computed<string>({
  get: () => {
    const stored = currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).suffix : null
    return stored ?? movieSuffixDefault.value
  },
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { suffix: v }) } })
// 3D ray-cast quality for a 3D recording (the keyframe renderer's `renderQuality`). Per set.
const renderQuality = computed<RenderQuality>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).renderQuality : RENDER_QUALITY_DEFAULT,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { renderQuality: v }) } })

// "Match viewer": Record takes the version, mask, overlays, z / 3D and camera from the live viewer
// (`readViewerLook`) and ignores the version / segmentation picks and the z / 3D options below — the
// popover keeps only the OUTPUT options (fps, size, name, frame range, title card, baked overlays).
// Off = the comparison recorder those picks drive. Per set, default on.
const matchViewer = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).matchViewer : true,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { matchViewer: v }) } })
const recordNote = ref<{ severity: Severity; short: string; detail?: string } | null>(null)

// What Record / Save as figure take: the view, or each ROI of the viewer's ROI set — the set and status
// filter the viewer window's ROI list shows (`settings.viewerRoiSuffix` / `viewerRoiStatus`), so the
// movies are of the ROIs you are reviewing (docs/todo/ROI_TRIAGE_PLAN.md Decision 12).
const recordWhat = computed<RecordWhat>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).recordWhat : 'view',
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { recordWhat: v }) } })
const roiMax = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).roiMax : 12,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { roiMax: v }) } })
const roiOutput = computed<RoiOutput>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).roiOutput : ROI_OUTPUT_DEFAULT,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { roiOutput: v }) } })
const roiOutputOptions = [
  { value: 'movies', label: 'movies', tip: 'One movie per ROI' },
  { value: 'both', label: '+ sheet', tip: 'One movie per ROI and a contact sheet of them all' },
  { value: 'sheet', label: 'sheet', tip: 'Only the contact sheet' },
]
// frames of context before and after each ROI's window, for the movies and the figures
const roiPad = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).roiPad : ROI_PAD_FRAMES,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { roiPad: roiPadFrames(v) }) } })
const roiList = ref<RoiList | null>(null)
const hasRois = computed(() => (roiList.value?.rois.length ?? 0) > 0)
const recordingRois = computed(() => hasRois.value && recordWhat.value === 'rois')
const recordWhatOptions = [{ value: 'view', label: 'View' }, { value: 'rois', label: 'ROIs' }]
async function loadRoiList() {
  const uid = props.img?.uid, pu = props.projectUid
  roiList.value = uid && pu ? await fetchRois(pu, uid, settings.viewerRoiSuffix) : null
}
watch(() => [props.img?.uid, props.projectUid, settings.viewerRoiSuffix], loadRoiList,
      { immediate: true })
// a review elsewhere (the ROI list, Claude) — the same refresh the ROI list does
function onRoisChanged(data: Record<string, unknown>) {
  if (String(data.imageUid ?? '') === props.img?.uid) loadRoiList()
}
onMounted(() => ws.on('rois:changed', onRoisChanged))
onUnmounted(() => ws.off('rois:changed', onRoisChanged))
// how many of the ticked ROIs the list's filter shows — 0 = none ticked, the "most" count applies
const roiTicked = computed(() => {
  const list = roiList.value
  const ids = list ? settings.getRoiChecked(props.img?.uid ?? '', list.suffix) : []
  return ids.length ? roiRecordSelection(list!.rois, settings.viewerRoiStatus as RoiStatus | 'all', 0, ids).length : 0
})
// the ROIs a record takes right now — re-read first, so a review made since counts
async function currentRoiPick() {
  await loadRoiList()
  const list = roiList.value
  return list ? { suffix: list.suffix,
                  rows: roiRecordSelection(list.rois, settings.viewerRoiStatus as RoiStatus | 'all', roiMax.value,
                                           settings.getRoiChecked(props.img?.uid ?? '', list.suffix)) }
              : null
}

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
// that used to converge on a single viewer canvas and now don't. Kept as-is: unifying them is a
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
// Baked overlays. They are drawn into the canvas, so a recording burns them in — hiding them
// is a record-time decision, and the batch RE-OPENS each image (which turns the scale bar back on),
// so toggling them in the viewer window is not an alternative.
const movieTimestamp = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).showTimestamp : true,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { showTimestamp: v }) } })
const movieScaleBar = computed<boolean>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).showScaleBar : true,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { showScaleBar: v }) } })

// Title card — per-set, merge-patched so each control keeps the others' values.
const movieTitleCard = computed<TitleCardCfg>(() =>
  currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).titleCard : { enabled: true, note: '', durationSec: 3 })
function patchMovieTitle(p: Partial<TitleCardCfg>) {
  if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { titleCard: { ...movieTitleCard.value, ...p } }) }
// TitleCardControls owns the clamp and emits a whole config; this just persists it.
const movieTitleCardModel = computed<TitleCardCfg>({
  get: () => movieTitleCard.value,
  set: v => patchMovieTitle(v),
})


// One-click timelapse recording: sweep the open image's T axis in the CURRENT view (whatever channels/
// populations/colour-by are shown) to an .mp4 under the project's movies/ folder.
//
// Sent over the WS task rail (`movie:record`), exactly like a batch: the recording appears in the task
// list with a live progress bar and a working Cancel. It used to be a blocking POST that resolved when
// the movie was finished — a frozen button, no progress, and no way out of a 4K render started by
// mistake. The button no longer owns the "in progress" state either; the task list does.
// "Save as figure" (docs/todo/FIGURES_PLAN.md): this view — crop, z window, frame, visible channels at
// their on-screen contrast — as a figure spec, rendered from the raw image at native resolution in the
// house style and listed on the Movies page. The spec is kept, so the figure can be re-made with more
// frames or other names there.
const savingFigure = ref(false)
async function saveFigure() {
  const img = openedImage.value
  const projectUid = props.projectUid
  if (!img || !projectUid) return
  const vs = viewerStore.openImage?.imageUid === img.uid ? viewerStore.viewState as unknown as ViewerViewState | null : null
  const valueName = viewerStore.openImage?.valueName || undefined
  // the Record range, every Nth frame — a one-frame range is a single still
  const frames = (img.sizeT ?? 1) > 1 ? figureRange.value.frames : undefined
  const r = figureSpecFromView(vs, img.channelNames ?? [], { valueName, frames, split: figureSplit.value,
                                                                           grid: figureGrid.value,
                                                                           ownCrop: recordingRois.value })
  if ('error' in r) { recordNote.value = { severity: 'warn', short: r.error }; return }
  if (recordingRois.value) { await saveRoiFigures(projectUid, img, r.spec); return }
  r.spec.scaleBarUm = niceFigureBarUm(r.spec.sizePx, pixelSizeUm(img.physicalSizeX, img.physicalSizeUnit))
  savingFigure.value = true
  try {
    const res = await fetch('/api/figures/render', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      // named like a movie: the image, the frames, then the `name` field; a taken name is
      // overwritten unless "_001" is on
      body: JSON.stringify({ projectUid, imageUid: img.uid, spec: r.spec,
                             name: figureName(img.name ?? img.uid, r.spec.frames, movieSuffix.value),
                             replace: !numberIfTaken.value }),
    })
    const body = await res.json().catch(() => ({}))
    recordNote.value = res.ok
      ? { severity: 'ok', short: `Saved figure ${body.name}`, detail: 'Open it on the Movies page.' }
      : { severity: 'fail', short: 'Could not save the figure', detail: body.error ?? `HTTP ${res.status}` }
  } catch (e) {
    recordNote.value = { severity: 'fail', short: 'Could not save the figure', detail: String(e) }
  } finally {
    savingFigure.value = false
  }
}

// Save as figure, one per ROI: each the view's figure (channels, contrast, split, grid) framed on the
// ROI's box over its frames, named after the set and the ROI's rank.
async function saveRoiFigures(projectUid: string, img: NonNullable<typeof openedImage.value>, base: FigureSpec) {
  const pick = await currentRoiPick()
  if (!pick?.rows.length) { recordNote.value = { severity: 'warn', short: 'No ROIs under the ROI list filter' }; return }
  savingFigure.value = true
  let saved = 0
  try {
    for (const roi of pick.rows) {
      const spec = roiFigureSpec(base, roi, figureEvery.value, roiPad.value, img.sizeT ?? Infinity)
      spec.scaleBarUm = niceFigureBarUm(spec.sizePx, pixelSizeUm(img.physicalSizeX, img.physicalSizeUnit))
      const res = await fetch('/api/figures/render', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid, imageUid: img.uid, spec, replace: !numberIfTaken.value, roiSuffix: pick.suffix,
                               name: figureName(img.name ?? img.uid, spec.frames,
                                                [pick.suffix, `r${roi.rank}`, movieSuffix.value].filter(Boolean).join('_')) }),
      })
      if (res.ok) saved++
    }
    recordNote.value = saved === pick.rows.length
      ? { severity: 'ok', short: `Saved ${saved} ROI figure(s)`, detail: 'Open them on the Movies page.' }
      : { severity: 'fail', short: `Saved ${saved} of ${pick.rows.length} ROI figures` }
  } catch (e) {
    recordNote.value = { severity: 'fail', short: 'Could not save the ROI figures', detail: String(e) }
  } finally {
    savingFigure.value = false
  }
}

// Record, one movie per ROI (+ a sheet when "sheet" is on): the viewer's look, as Match viewer records,
// with each ROI's box, planes and frames (`api/src/movie_rois.jl`).
async function recordRois(uid: string, projectUid: string) {
  const img = openedImage.value
  const snapshot = viewerStore.viewState as unknown as ViewerViewState | null
  const look = img ? readViewerLook(img, currentSetUid.value ?? '') : null
  if (!img || !snapshot || !look) {
    recordNote.value = { severity: 'warn', short: 'Open the image in the viewer first' }
    return
  }
  const pick = await currentRoiPick()
  if (!pick?.rows.length) { recordNote.value = { severity: 'warn', short: 'No ROIs under the ROI list filter' }; return }
  const t = taskStore.add({
    module: 'viewer', label: `Record ${pick.rows.length} ROI(s) of ${img.name ?? 'image'}`,
    imageUid: uid, imageName: img.name ?? '', status: 'queued',
    taskName: 'movie.record', funName: 'movie.record', params: {}, projectUid,
  })
  ws.send({ type: 'movie:record', taskId: t.id, projectUid, imageUid: uid, fps: movieFps.value,
            suffix: movieSuffix.value, numbered: numberIfTaken.value, matchViewer: true,
            valueNames: look.valueNames ?? [], look: lookForRender(look),
            labelValueNames: look.labelValueNames ?? [], labelContour: look.labelContour ?? 0,
            showTimestamp: movieTimestamp.value, showScaleBar: movieScaleBar.value,
            viewState: hexViewState(snapshot),
            rois: roiRecordBlock(pick.suffix, pick.rows, roiOutput.value, roiPad.value),
            ...movieSizeParams(movieSizeX.value, movieSizeY.value) })
}

async function recordTimelapse() {
  // `openImageUid` is the ANY-viewer field — set by ImageTable's eye button whenever any viewer
  // has the image. Was `viewerImageUid`, which stayed null when only the popup viewer was open,
  // so the Record button silently early-returned.
  const uid        = props.img?.uid
  const projectUid = props.projectUid
  if (!uid || !projectUid || recording.value || recordingTask.value) return
  recordNote.value = null
  if (recordingRois.value) { await recordRois(uid, projectUid); return }
  if (matchViewer.value) { await recordViewerMatch(uid, projectUid); return }
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

    // Title card: built via the SHARED buildTitleCard — the same path the animation page
    // uses. Channels are added by the recorder from the live viewer, so the frontend supplies only
    // title + non-channel sections.
    const colourBy  = currentSetUid.value ? settings.getColourBy(currentSetUid.value) : ''
    const overrides = (currentSetUid.value && colourBy) ? settings.getColourOverrides(currentSetUid.value, colourBy) : {}
    let titleCard: TitleCardPayload | undefined
    if (movieTitleCard.value.enabled) {
      titleCard = await buildTitleCard(projectUid, uid, snapshot, openedImage.value,
        { note: movieTitleCard.value.note, durationSec: movieTitleCard.value.durationSec, colourBy, colourOverrides: overrides })
    }
    // The look, in the SAME shape the Batch page authors — one config kind, so a recorded look edits
    // on the page built to edit looks. `seedConfigFromViewState` is the existing live-view → config
    // reader ("fill from view" on that page); the colour-by is not in the layer names, so it rides
    // along from the per-set setting the overlays were actually drawn with.
    //
    // The overlay half (pops, tracks, their segmentation, sizes) is the viewer's, read by the shared
    // `readViewerLook` — this recorder has no picker for them, so "what is shown" is the only answer.
    const live = openedImage.value
      ? readViewerLook(openedImage.value, currentSetUid.value ?? '', { requireOpen: false }) : null
    const look = { ...(live ?? {}),
                   ...seedConfigFromViewState(snapshot, openedImage.value?.channelNames ?? []),
                   ...(colourBy ? { colourBy } : {}) }
    // No version picked = the version SHOWN (the panel's picker), not the image's active one.
    const versions = compareVersions.value.length ? compareVersions.value
      : (selectedValueName.value ? [selectedValueName.value] : [])
    const shape    = compareShapeNow.value
    // 3D and not a comparison → a volume render through the keyframe renderer (the compare grid
    // has no 3D renderer and stays an all-Z projection).
    if (show3D.value && shape.cells <= 1 && openedImage.value) {
      const vs3 = volumeViewState(snapshot as unknown as ViewerViewState | null,
                                  settings.viewerVolumeProjection === 'persp')
      const lastT = Math.max(0, (openedImage.value.sizeT ?? 1) - 1)
      const t = taskStore.add({
        module: 'viewer', label: `Record ${openedImage.value.name ?? 'movie'}`,
        imageUid: uid, imageName: openedImage.value.name ?? '', status: 'queued',
        taskName: 'movie.record', funName: 'movie.record', params: {}, projectUid,
      })
      ws.send({
        type: 'movie:record', taskId: t.id, projectUid, imageUid: uid, fps: movieFps.value,
        suffix: movieSuffix.value, numbered: numberIfTaken.value, titleCard, source: 'viewer',
        valueNames: versions, look: lookForRender(look), renderQuality: renderQuality.value,
        showTimestamp: movieTimestamp.value, showScaleBar: movieScaleBar.value,
        keyframes: timelapseKeyframes(hexViewState(vs3), movieTStart.value, Math.min(movieTEnd.value ?? lastT, lastT)),
        ...movieSizeParams(movieSizeX.value ?? (vs3.canvas.width || null),
                           movieSizeY.value ?? (vs3.canvas.height || null)),
      })
      return
    }
    const t = taskStore.add({
      module: 'viewer',
      label: shape.cells > 1
        ? (shape.grid ? `Compare ${shape.cols} x ${shape.rows}` : `Compare ${shape.cells}`)
          + ` — ${openedImage.value?.name ?? 'movie'}`
        : `Record ${openedImage.value?.name ?? 'movie'}`,
      imageUid: uid, imageName: openedImage.value?.name ?? '', status: 'queued',
      taskName: 'movie.record', funName: 'movie.record', params: {}, projectUid,
    })
    ws.send({
      type: 'movie:record', taskId: t.id, projectUid, imageUid: uid, fps: movieFps.value,
      suffix: movieSuffix.value, numbered: numberIfTaken.value, titleCard,
      // More than one cell = a comparison; one is the plain record the backend already did.
      // `labelValueNames` is OMITTED when there is nothing to say — absent means "leave the masks
      // alone", which is what the plain record has always done.
      valueNames: versions, labelContour: labelContour.value,
      show3D: show3D.value, zSlice: show3D.value ? null : zSlice.value,
      // which stretch of the timelapse to sweep; null end = the last frame, clamped per image
      tStart: movieTStart.value, tEnd: movieTEnd.value,
      ...(movieLabelValueNames.value.length ? { labelValueNames: movieLabelValueNames.value } : {}),
      compareLayout: compareLayout.value, compareContrast: compareContrast.value,
      showTimestamp: movieTimestamp.value, showScaleBar: movieScaleBar.value,
      // banked with the movie, not acted on by the recorder — it already records this look by
      // recording the screen (MOVIE_MANAGEMENT_PLAN.md Phase 4)
      look: lookForRender(look),
      // The full viewer-shape snapshot rides alongside `look`. `look` covers the channel picks +
      // overlay flags; the snapshot's `camera` + `canvas` are what the offline record needs to
      // reproduce the visible rectangle — a viewer zoomed into a corner would otherwise record
      // the whole image at native aspect (bug reported 2026-08-29, the movie/viewer side-by-side).
      // Absent when no snapshot was published.
      ...(snapshot ? { viewState: hexViewState(snapshot) } : {}),
      // one version, the one on screen → its live contrast too, not the autosaved props
      liveSpecs: shape.cells <= 1 && versions.length === 1 && versions[0] === viewerStore.openImage?.valueName,
      ...movieSizeParams(movieSizeX.value, movieSizeY.value),
    })
  } catch (e) {
    log.error(`Record timelapse failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'viewer' })
  } finally {
    // the RENDER is the task's business now; this flag only covers assembling the request
    recording.value = false
  }
}

// Record exactly what the browser viewer shows. The look (version, mask, overlays, channels, z / 3D)
// is read off the viewer by `readViewerLook`, the SAME reader Batch's Fill from view uses. A 2D view
// goes down the plain recorder with `matchViewer` (live contrast, crop and z from the snapshot); a 3D
// view goes down the keyframe renderer as a one-view timelapse (`timelapseKeyframes`), because that
// is the only one that renders a volume with the viewer's camera.
async function recordViewerMatch(uid: string, projectUid: string) {
  const img = openedImage.value
  const snapshot = viewerStore.viewState as unknown as ViewerViewState | null
  const look = img ? readViewerLook(img, currentSetUid.value ?? '') : null
  if (!img || !snapshot || !look) {
    recordNote.value = { severity: 'warn', short: 'Open the image in the viewer first',
      detail: 'Match viewer records what the viewer shows. Turn it off to record without one.' }
    return
  }
  recording.value = true
  try {
    const colourBy = look.colourBy ?? ''
    const titleCard = movieTitleCard.value.enabled
      ? await buildTitleCard(projectUid, uid, snapshot as unknown as ViewStateLike, img,
          { note: movieTitleCard.value.note, durationSec: movieTitleCard.value.durationSec, colourBy,
            colourOverrides: look.colourOverrides ?? {} })
      : undefined
    const lastT = Math.max(0, (img.sizeT ?? 1) - 1)
    const tEnd = Math.min(movieTEnd.value ?? lastT, lastT)
    const t = taskStore.add({
      module: 'viewer', label: `Record ${img.name ?? 'movie'}`,
      imageUid: uid, imageName: img.name ?? '', status: 'queued',
      taskName: 'movie.record', funName: 'movie.record', params: {}, projectUid,
    })
    const common = {
      type: 'movie:record', taskId: t.id, projectUid, imageUid: uid, fps: movieFps.value,
      suffix: movieSuffix.value, numbered: numberIfTaken.value, titleCard, matchViewer: true,
      valueNames: look.valueNames ?? [], look: lookForRender(look),
      showTimestamp: movieTimestamp.value, showScaleBar: movieScaleBar.value,
    }
    if (look.show3D) {
      // A blank size is the viewer's canvas — the 3D renderer otherwise falls back to 512 square.
      ws.send({ ...common, renderQuality: renderQuality.value,
        keyframes: timelapseKeyframes(hexViewState(snapshot), movieTStart.value, tEnd),
        ...movieSizeParams(movieSizeX.value ?? snapshot.canvas?.width ?? null,
                           movieSizeY.value ?? snapshot.canvas?.height ?? null) })
    } else {
      ws.send({ ...common,
        // the mask the viewer draws (or none) — more than one would turn the record into a comparison
        // grid; skeletons ride in the look (`skeletons`)
        labelValueNames: look.labelValueNames ?? [],
        labelContour: look.labelContour ?? 0,
        // the plane, and with the ±n window on its planes (a range wins: the max over them)
        show3D: false, zSlice: look.zSlice ?? null, zRange: look.zRange ?? null,
        tStart: movieTStart.value, tEnd: movieTEnd.value,
        viewState: hexViewState(snapshot),
        ...movieSizeParams(movieSizeX.value, movieSizeY.value) })
    }
  } catch (e) {
    log.error(`Record failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'viewer' })
  } finally {
    recording.value = false
  }
}

</script>

<template>
      <!-- ── Movie: record the CURRENT view over time → mp4 (project's movies/ folder) ──
           Records exactly what's shown (channels, populations, tracks, colour-by). fps + size + the
           filename suffix are per-set; the render runs as a task (progress + Cancel in the task list),
           and the fuller config (which channels/pops, T-range, batch) is F1.2/F1.3.
           Picking 2+ versions records them side by side instead (MOVIE_COMPARE_PLAN.md). The options
           sit in a popover: this panel is narrow, and they are set once and then left alone, while the
           version chips are the thing you change per movie. ONE row — an image with a single version
           (the common case) shows just the two buttons. -->
      <div class="quick-movie" data-guide="viewer.movieSection">
        <div v-if="heading" class="cc-eyebrow cc-fs-2xs">Movie</div>
        <!-- the view, or each ROI the viewer's ROI list shows (its set + filter) — only with an ROI set -->
        <ChipSelect v-if="hasRois" v-model="recordWhat" :options="recordWhatOptions" variant="segmented"
                    aria-label="What to record" class="cc-fs-xs"
                    v-tooltip.bottom="'ROIs: one movie per ticked ROI in the ROI list'" />
        <div class="movie-row">
          <div class="movie-versions">
            <CcToggle v-if="!recordingRois" v-model="matchViewer" label="Match viewer" class="cc-fs-xs"
                      v-tooltip.bottom="'Record exactly what the viewer shows'" />
            <MovieCompareControls v-if="!matchViewer && !recordingRois" :available="valueNames"
                                  :available-segmentations="labelNames"
                                  v-model:versions="compareVersions"
                                  v-model:segmentations="compareSegmentations"
                                  v-model:contour="labelContour"
                                  v-model:layout="compareLayout"
                                  v-model:contrast="compareContrast" />
          </div>
          <MovieOptionsButton class="opt-btn" :inline="inlineOptions">
            <div class="mob-head cc-eyebrow cc-fs-2xs">Output</div>
            <MovieOutputControls :suffix-options="movieSuffixes" v-model:fps="movieFps" v-model:sizeX="movieSizeX" v-model:sizeY="movieSizeY"
                                 v-model:suffix="movieSuffix" v-model:numbered="numberIfTaken" :canvas-x="canvasSizeX" :canvas-y="canvasSizeY"
                                 v-model:timestamp="movieTimestamp" v-model:scale-bar="movieScaleBar"
                                 :size-z="matchViewer ? 1 : openedImage?.sizeZ" v-model:show3D="show3D"
                                 v-model:zSlice="zSlice" :default-z="viewerZ"
                                 v-model:renderQuality="renderQuality" />
            <!-- Only for an actual timelapse — nothing to trim on a single frame -->
            <template v-if="(openedImage?.sizeT ?? 1) > 1">
              <div class="mob-head cc-eyebrow cc-fs-2xs">Frames</div>
              <MovieTimeRange v-model:tStart="movieTStart"
                              v-model:tEnd="movieTEnd" :frames="openedImage?.sizeT ?? 1"
                              show-every v-model:every="figureEvery" :every-auto="figureRange.every" />
            </template>
            <template v-if="recordingRois">
              <div class="mob-head cc-eyebrow cc-fs-2xs">ROIs</div>
              <div v-if="roiTicked" class="cc-muted cc-fs-2xs">{{ roiTicked }} ticked in the ROI list</div>
              <div v-else class="cc-row">
                <span class="cc-row-group">
                  <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">most</span>
                  <input type="number" min="1" max="100" step="1" class="cc-input-2xs" v-model.number="roiMax"
                         aria-label="Most ROIs to record"
                         v-tooltip.bottom="'None ticked: the first this many in the ROI list'" />
                </span>
              </div>
              <div class="cc-row">
                <span class="cc-row-group">
                  <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">pad</span>
                  <input type="number" min="0" :max="ROI_PAD_MAX" step="1" class="cc-input-2xs" v-model.number="roiPad"
                         aria-label="Frames of context before and after"
                         v-tooltip.bottom="'Frames of context before and after each ROI'" />
                </span>
              </div>
              <div class="cc-row">
                <span class="cc-row-group">
                  <span class="cc-lbl-col cc-eyebrow cc-fs-2xs">make</span>
                  <ChipSelect v-model="roiOutput" :options="roiOutputOptions" variant="segmented" class="cc-fs-xs"
                              aria-label="What an ROI record writes" />
                </span>
              </div>
            </template>
            <div class="mob-head cc-eyebrow cc-fs-2xs">Figure</div>
            <div class="cc-row">
              <span class="cc-row-group">
                <span class="cc-lbl-col cc-eyebrow cc-fs-2xs"
                      v-tooltip.bottom="'Save as figure: one grey row per channel under the merge'">split</span>
                <CcToggle v-model="figureSplit" aria-label="Split channels in the figure" />
              </span>
            </div>
            <div class="cc-row">
              <span class="cc-row-group">
                <span class="cc-lbl-col cc-eyebrow cc-fs-2xs"
                      v-tooltip.bottom="'Save as figure: wrap the frames into a square grid'">grid</span>
                <CcToggle v-model="figureGrid" aria-label="Frames as a grid in the figure" />
              </span>
            </div>
            <div class="mob-head cc-eyebrow cc-fs-2xs">Title card</div>
            <TitleCardControls v-model="movieTitleCardModel" />
          </MovieOptionsButton>
          <button class="opt-btn cc-btn cc-btn-ghost cc-btn-icon movie-rec" data-guide="viewer.record"
                  :class="{ 'cc-btn-on cc-btn-on-tint': recording || recordingTask }" :disabled="recording || recordingTask"
                  @click="recordTimelapse"
                  v-tooltip.bottom="recordingRois ? 'Record each ROI → mp4s in movies/' : matchViewer ? 'Record the viewer over time → mp4 in movies/'
                    : compareActionTip(compareShapeNow,
                    'Record the current view over the time axis → mp4 in the project\'s movies/ folder')">
            <i :class="['pi', (recording || recordingTask) ? 'pi-spin pi-spinner' : 'pi-video']" />
          </button>
          <button class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :disabled="savingFigure"
                  @click="saveFigure" v-tooltip.bottom="figureTip">
            <i :class="['pi', savingFigure ? 'pi-spin pi-spinner' : 'pi-print']" />
          </button>
        </div>
        <InlineNote v-if="recordNote" :severity="recordNote.severity"
                    :short="recordNote.short" :detail="recordNote.detail" />
      </div>
</template>

<style scoped>
.quick-movie { display: flex; flex-direction: column; gap: 0.35rem; }
/* ONE row: the version chips take the width, the gear and Record are pinned to its right, top-aligned
   so they stay on the first line when the chips run to several. */
.movie-row { display: flex; align-items: flex-start; gap: 0.3rem; flex-wrap: wrap; }
/* inline options (`inlineOptions`) sit in the row's markup but wrap to a full line under the buttons */
.movie-row :deep(.mob-inline) { order: 9; flex-basis: 100%; }
.movie-versions { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 0.3rem; }
.movie-rec { margin-left: 0.1rem; }
.opt-btn { transition: background 0.1s, color 0.1s, border-color 0.1s; }
.opt-btn:hover { color: var(--cc-text); border-color: var(--cc-border-hover); }
</style>
