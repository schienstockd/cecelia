<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import { pushTracks as apiPushTracks, pushPopulations as apiPushPopulations, pushColourLabels as apiPushColourLabels, buildTitleCard, type TitleCardPayload } from '../utils/napariOverlays'
import type { TitleCardCfg } from '../utils/batchMovie'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'

const projectStore = useProjectStore()
const projectMeta  = useProjectMetaStore()
const settings     = useSettingsStore()
const ws           = useWsStore()
const log          = useLogStore()

// Pull the error message out of a non-ok response (the API sends { error: "..." }).
async function _resError(res: Response): Promise<string> {
  try { const j = await res.json(); return j?.error ?? `HTTP ${res.status}` }
  catch { return `HTTP ${res.status}` }
}

const selectedValueName = ref('')
const visibleLabels     = ref<Record<string, boolean>>({})
const gatedTracksShown  = ref(false)   // master "show gated track populations" toggle (TEST/SDGF)
const recording         = ref(false)   // a one-click timelapse recording is in progress

// per-pop-type population overlays as centroid POINTS. Only the CELL-grained pop types (flow, clust,
// region) belong here: show-populations plots by cell label, whereas track/trackclust are track-grained
// (membership is track_ids) — their napari viz is ribbons (the Tracks-ribbon toggle below /
// per-segmentation directions), and trackclust ribbons are still to come. Layers are namespaced by
// pop type in the bridge, so flow + clust + region coexist. resolve_pops is generic over pop_type, so
// region (a filter on regions.{suffix}) resolves + colours its centroids like any other cell pop.
// icons MATCH the sidebar module nav (Gate = pi-chart-scatter, Cluster cells = pi-palette,
// Region = pi-map, Track = pi-share-alt, Cluster tracks = pi-sitemap) so a pop type reads the same.
const POP_TYPES: { key: string; icon: string; label: string }[] = [
  { key: 'flow',   icon: 'pi-chart-scatter', label: 'gating populations' },
  { key: 'clust',  icon: 'pi-palette',       label: 'cell-cluster populations' },
  { key: 'region', icon: 'pi-map',           label: 'spatial-region populations' },
]
const trackVns          = ref<Record<string, boolean>>({})   // per-segmentation track-overlay visibility
const colourByCol       = ref('')      // obs column to shade tracks + labels by ('' = default)
const colourLegend      = ref<Record<string, string>>({})   // {category value → hex} for the colour-by legend
const colourLegendLabels = ref<Record<string, string>>({})  // {category value → population name} where a pop defines it
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
const hasLabels   = computed(() => labelNames.value.length > 0)

// the set the open image belongs to — the key for per-set napari viewer prefs (colour-by, show-3D,
// point size, overlay toggles). These are experiment-level: set once, hold across the set's images.
const currentSetUid = computed(() =>
  projectStore.napariImageUid ? projectStore.setUidOfImage(projectStore.napariImageUid) : null)
// per-set option accessors bound to the open image's set (write-throughs persist to the settings store)
const show3D = computed<boolean>({
  get: () => currentSetUid.value ? settings.getShow3D(currentSetUid.value) : false,
  set: v => { if (currentSetUid.value) settings.setShow3D(currentSetUid.value, v) } })
const pointSize = computed<number>({
  get: () => currentSetUid.value ? settings.getPointSize(currentSetUid.value) : 6,
  set: v => { if (currentSetUid.value) settings.setPointSize(currentSetUid.value, v) } })
const popVisible = (popType: string): boolean =>
  currentSetUid.value ? settings.getPopVisible(currentSetUid.value, popType) : false
const setPopVisible = (popType: string, v: boolean) => {
  if (currentSetUid.value) settings.setPopVisible(currentSetUid.value, popType, v) }
// timelapse-recording params (per set): frame rate + resolution supersample factor
const movieFps = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).fps : 15,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { fps: v }) } })
const movieScale = computed<number>({
  get: () => currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).scale : 1,
  set: v => { if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { scale: v }) } })
// Title card (Phase H, H3) — per-set, merge-patched so each control keeps the others' values.
const movieTitleCard = computed<TitleCardCfg>(() =>
  currentSetUid.value ? settings.getMovieConfig(currentSetUid.value).titleCard : { enabled: true, note: '', durationSec: 3 })
function patchMovieTitle(p: Partial<TitleCardCfg>) {
  if (currentSetUid.value) settings.setMovieConfig(currentSetUid.value, { titleCard: { ...movieTitleCard.value, ...p } }) }
const titleCardOn = computed<boolean>({ get: () => movieTitleCard.value.enabled, set: v => patchMovieTitle({ enabled: v }) })
const titleNote   = computed<string>({  get: () => movieTitleCard.value.note,    set: v => patchMovieTitle({ note: v }) })
const titleDur    = computed<number>({  get: () => movieTitleCard.value.durationSec, set: v => patchMovieTitle({ durationSec: Math.min(10, Math.max(1, v)) }) })


watch(napariImage, (img) => {
  // restore the remembered preference rather than always starting hidden — the actual layers
  // are (re)pushed by onNapariOpened / onGatingChange once the image + centroids are ready.
  gatedTracksShown.value = currentSetUid.value ? settings.getShowGatedTracks(currentSetUid.value) : false
  colourByCol.value = currentSetUid.value ? settings.getColourBy(currentSetUid.value) : ''   // per-set
  if (!img) { selectedValueName.value = ''; visibleLabels.value = {}; trackVns.value = {}; obsCols.value = []; return }
  trackVns.value = settings.getTrackVisibility(img.uid, Object.keys(img.labels ?? {}))
  const names = Object.keys(img.filepaths ?? {})
  // Default to the active version (the `_active` key from the versioned filepath dict) — this is
  // what the server opens when no valueName is passed, so the dropdown must agree. Fall back to
  // last-non-default → default → first only when no active version is registered.
  const nonDefault = names.filter(n => n !== 'default')
  selectedValueName.value =
    img.activeValueName && names.includes(img.activeValueName) ? img.activeValueName
    : nonDefault.length > 0 ? nonDefault[nonDefault.length - 1]
    : names.includes('default') ? 'default' : (names[0] ?? '')
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
  if (hasLabels.value) {
    const toShow = Object.fromEntries(
      Object.entries(napariImage.value?.labels ?? {}).filter(([vn]) => visibleLabels.value[vn])
    )
    if (Object.keys(toShow).length) {
      body.showLabels = true
      body.allLabels  = toShow
    }
  }
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
// populations/colour-by are shown) to an .mp4 under the project's movies/ folder. The backend picks the
// path and returns it; we surface it in the log. Can take a while (one screenshot per timepoint), so the
// button shows a spinner + is disabled meanwhile. (F1.1 — a config/batch UI comes in F1.2/F1.3.)
async function recordTimelapse() {
  const uid        = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid || recording.value) return
  recording.value = true
  log.info('Recording timelapse… (this can take a moment)', { source: 'napari' })
  try {
    // Title card (Phase H): capture the CURRENT view state (no PNG) and build the payload via the
    // SHARED buildTitleCard — the same path the animation page uses. Channels are added by the recorder
    // from the live viewer, so the frontend supplies only title + non-channel sections.
    let titleCard: TitleCardPayload | undefined
    if (titleCardOn.value) {
      const colourBy  = currentSetUid.value ? settings.getColourBy(currentSetUid.value) : ''
      const overrides = (currentSetUid.value && colourBy) ? settings.getColourOverrides(currentSetUid.value, colourBy) : {}
      let snapshot: { layers?: Record<string, unknown> } | null = null
      try {
        const vsr = await fetch('/api/napari/view-state', {
          method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ projectUid }) })
        if (vsr.ok) snapshot = ((await vsr.json()) as { viewState?: { layers?: Record<string, unknown> } }).viewState ?? null
      } catch { /* best-effort — card still renders title + channels */ }
      titleCard = await buildTitleCard(projectUid, uid, snapshot, napariImage.value,
        { note: titleNote.value, durationSec: titleDur.value, colourBy, colourOverrides: overrides })
    }
    const res = await fetch('/api/napari/record-timelapse', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: uid, fps: movieFps.value, scale: movieScale.value, titleCard }),
    })
    if (!res.ok) { log.error(`Record timelapse failed: ${await _resError(res)}`, { source: 'napari' }); return }
    const j = (await res.json()) as { path?: string; frames?: number }
    log.info(`Recorded ${j.frames ?? '?'} frames → ${j.path ?? 'movies/'}`, { source: 'napari' })
  } catch (e) {
    log.error(`Record timelapse failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally {
    recording.value = false
  }
}

// Push a pop type's populations to napari as centroid points. The server shows EVERY segmentation's
// pops at once (each as its own value_name-tagged layer), so `valueName` no longer selects which pops
// appear — the overlay is independent of which segmentation is "active" (opening the image shows all,
// not just the active/first one). The bridge namespaces layers by `(popType) (valueName)`, so
// flow/clust overlays across segmentations coexist. `valueName` is still forwarded as the bridge's
// per-pop default for older senders; blank is fine.
async function pushPopulations(popType: string, show: boolean, valueName?: string): Promise<boolean> {
  const uid        = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return false
  // one shared request builder (utils/napariOverlays) — same shape the strip/zoom uses
  const res = await apiPushPopulations(projectUid, uid, { popType, show, valueName, pointsSize: pointSize.value })
  return !!res?.ok
}

// Per-pop-type visibility toggle; the choice is remembered (persisted) so it carries across opens.
async function togglePopType(popType: string) {
  const next = !popVisible(popType)
  if (await pushPopulations(popType, next)) setPopVisible(popType, next)
}

// Push the tracks for the currently-toggled-on segmentations (one Tracks layer per segmentation,
// named by its value_name). `valueNames` = the segmentations whose "directions" toggle is on; empty
// → the bridge clears all track layers. `colorBy` shades vertices by the chosen obs column.
const onTrackVns = computed(() => Object.keys(trackVns.value).filter(vn => trackVns.value[vn]))
async function pushTracks(): Promise<boolean> {
  const uid        = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return false
  const res = await apiPushTracks(projectUid, uid, {
    valueNames: onTrackVns.value, showGatedTracks: gatedTracksShown.value,
    showTrackclust: popVisible('trackclust'), colorBy: colourByCol.value,
    colourOverrides: userColourOverrides(),
  })
  // capture the colour-by legend from the tracks response — the Labels layer may be hidden (then
  // colour-labels returns none), so tracks are the only legend source when colouring tracks alone.
  if (res?.ok && colourByCol.value) {
    try {
      const j = (await res.json()) as { legend?: Record<string, string>; legendLabels?: Record<string, string> }
      if (Object.keys(j.legend ?? {}).length) colourLegend.value = { ...colourLegend.value, ...j.legend }
      if (Object.keys(j.legendLabels ?? {}).length) colourLegendLabels.value = { ...colourLegendLabels.value, ...j.legendLabels }
    } catch { /* legend harvest is best-effort */ }
  }
  return !!res?.ok
}

// Per-segmentation toggle: flip this segmentation's track overlay, persist, re-push the on-set.
async function toggleTrack(vn: string) {
  const uid = projectStore.napariImageUid
  trackVns.value = { ...trackVns.value, [vn]: !trackVns.value[vn] }
  if (uid) settings.setTrackVisibility(uid, trackVns.value)
  await pushTracks()
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

async function pushColourLabels(column: string): Promise<boolean> {
  const uid = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return false
  if (!column) { colourLegend.value = {}; colourLegendLabels.value = {} }   // reset → no legend
  const res = await apiPushColourLabels(projectUid, uid, {
    valueName: selectedValueName.value, column, colourOverrides: userColourOverrides(),
  })
  if (res?.ok && column) {   // categorical → {value: hex}; empty for continuous / hidden-labels
    try {
      const j = (await res.json()) as { legend?: Record<string, string>; legendLabels?: Record<string, string> }
      if (Object.keys(j.legend ?? {}).length) colourLegend.value = { ...colourLegend.value, ...j.legend }
      if (Object.keys(j.legendLabels ?? {}).length) colourLegendLabels.value = { ...colourLegendLabels.value, ...j.legendLabels }
    } catch { /* legend harvest is best-effort */ }
  }
  return !!res?.ok
}

// user picked a colour-by column: persist, recolour the tracks (if shown) and the labels layer
function onColourBy(e: Event) {
  const col = (e.target as HTMLSelectElement).value
  colourByCol.value = col
  if (currentSetUid.value) settings.setColourBy(currentSetUid.value, col)   // per-set
  colourLegend.value = {}; colourLegendLabels.value = {}   // clear old column's legend; pushes below repopulate
  if (onTrackVns.value.length || gatedTracksShown.value) pushTracks()   // re-push tracks w/ new color_by
  pushColourLabels(col)                       // recolour labels (or reset when col === '')
}

// user's per-set/per-column colour recolouring ({value → hex}); sent alongside the pop colours so the
// bridge uses them (user wins). Empty when no set/column — the pop colours + default palette then apply.
function userColourOverrides(): Record<string, string> {
  return currentSetUid.value && colourByCol.value
    ? settings.getColourOverrides(currentSetUid.value, colourByCol.value) : {}
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

// Live update while gating: when the population tree changes for the image open in napari
// (gate edit, pop add/remove/rename, cell selection, dot-size change), re-push so the overlay
// tracks the gating. The dot-size slider lives in the population manager Options box.
function onGatingChange(data: Record<string, unknown>) {
  if (String(data.imageUid ?? '') !== projectStore.napariImageUid) return
  const vn = data.valueName as string | undefined
  // track-grained edits (track / trackclust) re-push the RIBBONS (never points — points would be
  // wrong for track_ids and orphaned by the viewer's toggles). Cell-grained edits (flow / clust)
  // re-push that pop type's POINT overlay if it's visible.
  const pt = String(data.popType ?? 'flow')
  if (pt === 'track' || pt === 'trackclust') {
    if (gatedTracksShown.value || popVisible('trackclust')) pushTracks()
  } else if (popVisible(pt)) {
    pushPopulations(pt, true, vn)
  }
}

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
    const res = await fetch('/api/napari/show-labels', {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({ allLabels: { [valueName]: files }, showLabels: !wasVisible }),
    })
    if (res.ok) {
      visibleLabels.value = { ...visibleLabels.value, [valueName]: !wasVisible }
      if (uid) settings.setLabelVisibility(uid, visibleLabels.value)
    } else {
      log.error(`Show labels "${valueName}" failed: ${await _resError(res)}`, { source: 'napari' })
    }
  } catch (e) {
    log.error(`Show labels "${valueName}" failed: ${e instanceof Error ? e.message : String(e)}`,
              { source: 'napari' })
  }
}

// two-click delete → the shared ConfirmDeleteButton (arms trash → warning, second click deletes).
async function deleteLabel(valueName: string) {
  const uid        = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return

  // Hide in napari first if visible
  const files = napariImage.value?.labels?.[valueName] ?? []
  if (visibleLabels.value[valueName] && files.length) {
    await fetch('/api/napari/show-labels', {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({ allLabels: { [valueName]: files }, showLabels: false }),
    }).catch(() => {})
  }

  try {
    const res = await fetch('/api/images/labels/delete', {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({ projectUid, imageUid: uid, valueName }),
    })
    if (res.ok) {
      projectStore.removeLabelSet(uid, valueName)
      const next = { ...visibleLabels.value }
      delete next[valueName]
      visibleLabels.value = next
      settings.setLabelVisibility(uid, next)
    }
  } catch {}
}

function onTaskStatus(data: Record<string, unknown>) {
  const status = String(data.status ?? '')
  if (!settings.napariUpdateImage) return
  if (status !== 'done') return
  const napariUid = projectStore.napariImageUid
  if (!napariUid || String(data.imageUid ?? '') !== napariUid) return
  reloadViewer()   // data-only unless the user ticked reset (task changed pixels → reopen)
}

// Re-push ALL of the image's DATA overlays (labels + colour-by + population/track points/ribbons),
// each re-read from disk by its endpoint (show-labels/show-populations/show-tracks all replace their
// layer in place). This is the "reload data" path — it touches NO image pyramid. Shared by the open
// handler (after a full reopen) and reloadViewer's data-only branch.
function pushAllOverlays() {
  // run in nextTick so the napariImage-derived state (labelNames, visibleLabels) has settled before we
  // push overlays — the labels early-return must NOT skip the pop/track overlays.
  nextTick(() => {
    if (hasLabels.value) {
      const toShow = Object.fromEntries(
        Object.entries(napariImage.value?.labels ?? {}).filter(([vn]) => visibleLabels.value[vn])
      )
      if (Object.keys(toShow).length) {
        fetch('/api/napari/show-labels', {
          method:  'POST',
          headers: { 'Content-Type': 'application/json' },
          body:    JSON.stringify({ allLabels: toShow, showLabels: true }),
        }).then(async res => {
          if (!res.ok) { log.error(`Show labels on open failed: ${await _resError(res)}`, { source: 'napari' }); return }
          // apply the remembered colour-by ONLY if this segmentation actually has that column —
          // otherwise a stale/absent column would recolour (and hide) napari's distinct default labels
          if (colourByCol.value && obsCols.value.includes(colourByCol.value)) pushColourLabels(colourByCol.value)
        }).catch(e =>
          log.error(`Show labels on open failed: ${e instanceof Error ? e.message : String(e)}`,
                    { source: 'napari' }))
      }
    }
    // auto-show each pop type's point overlay that the user last had on (remembered preference)
    for (const { key } of POP_TYPES) if (popVisible(key)) pushPopulations(key, true)
    // re-show the track overlays from the REMEMBERED state, read straight from settings (the trackVns
    // ref is restored by the napariImage watch, which may not have run yet when this open event fires).
    const uid = projectStore.napariImageUid
    if (uid) {
      trackVns.value = settings.getTrackVisibility(uid, labelNames.value)   // keep the ref in sync (per-image)
      gatedTracksShown.value = currentSetUid.value ? settings.getShowGatedTracks(currentSetUid.value) : false
      if (onTrackVns.value.length || gatedTracksShown.value || popVisible('trackclust')) pushTracks()
    }
  })
}

function onNapariOpened() { pushAllOverlays() }

// Refresh the SHOWN image. Data-only by default (re-push overlays, re-read from disk — the pyramid and
// camera stay); only reopen the whole image when the user ticked reset, or nothing is shown yet. This
// is what the eye (on the already-open image) and finished tasks call, so a plain reload no longer
// yanks the image out from under the user (mirrors viewerManager.R: reopen only on reset / uID change).
function reloadViewer() {
  if (settings.napariResetOnReload || !projectStore.napariImageUid) openInNapari(selectedValueName.value)
  else pushAllOverlays()
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
        fetch('/api/napari/show-labels', {
          method:  'POST',
          headers: { 'Content-Type': 'application/json' },
          body:    JSON.stringify({ allLabels: { [labelValueName]: files }, showLabels: true }),
        }).catch(() => {})
      }
    })
  }
}

// the image-table eye, clicked on the ALREADY-open image, asks us to reload it (data-only unless reset)
watch(() => projectStore.napariReloadTick, () => reloadViewer())

// Stale-bridge flag: napari is a separate process that survives a backend restart, so after editing
// napari code you can be looking at old behaviour without knowing. Poll the backend (which compares the
// bridge's start time to the napari source mtimes) and warn right here, where you'd act on it.
const bridgeStale = ref(false)
let bridgeTimer: number | undefined
async function pollBridge() {
  try {
    const s = await (await fetch('/api/napari/status')).json() as { bridgeStale?: boolean }
    bridgeStale.value = !!s.bridgeStale
  } catch { bridgeStale.value = false }
}
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
  pollBridge(); bridgeTimer = window.setInterval(pollBridge, 5000)
  ws.on('napari:opened', onNapariOpened)
  ws.on('task:status', onTaskStatus)
  ws.on('task:result', onTaskResult)
  ws.on('gating:popmap', onGatingChange)
})
onUnmounted(() => {
  if (bridgeTimer) clearInterval(bridgeTimer)
  ws.off('napari:opened', onNapariOpened)
  ws.off('task:status', onTaskStatus)
  ws.off('task:result', onTaskResult)
  ws.off('gating:popmap', onGatingChange)
})
</script>

<template>
  <div class="viewer-panel">
    <!-- stale-bridge warning: napari started before the latest napari-code changes (it survives a
         backend restart). Brief here; the action is the Restart button + the tooltip. -->
    <div v-if="bridgeStale" class="viewer-stale"
         v-tooltip.bottom="'Napari started before your latest changes — restart it, then reopen the image.'">
      <i class="pi pi-exclamation-triangle" />
      <span class="viewer-stale-txt">Napari running old code</span>
      <button class="viewer-stale-btn" @click="restartNapari">Restart</button>
    </div>
    <!-- ── View: viewer behaviour toggles (global prefs; apply on next open) ──
         Top of the panel — these are always available, even before an image is open. -->
    <!-- Convention: append new toggles at the END of the row. -->
    <div class="viewer-section first">
      <div class="viewer-section-title">View</div>
      <div class="viewer-opts">
        <button
          class="opt-btn" :class="{ active: settings.napariUpdateImage }"
          @click="settings.napariUpdateImage = !settings.napariUpdateImage"
          v-tooltip.bottom="'Auto-update: refresh Napari whenever a task finishes on that image'"
        ><i class="pi pi-refresh" /></button>

        <button
          class="opt-btn" :class="{ active: settings.napariResetOnReload }"
          @click="settings.napariResetOnReload = !settings.napariResetOnReload"
          v-tooltip.bottom="'Reset on reload: reopen the whole image (not just data) when reloading — needed when a task changed the image pixels (drift/denoise). Off = reload data only.'"
        ><i class="pi pi-image" /></button>

        <button
          class="opt-btn" :class="{ active: settings.napariAutoSaveLayerProps }"
          @click="settings.napariAutoSaveLayerProps = !settings.napariAutoSaveLayerProps"
          v-tooltip.bottom="'Auto-save layer props: save brightness/contrast, colormap and the T/Z slider the moment you change them (survives navigation and crashes); reload on next open'"
        ><i class="pi pi-bookmark" /></button>

        <button
          class="opt-btn" :class="{ active: show3D }" :disabled="!currentSetUid"
          @click="show3D = !show3D"
          v-tooltip.bottom="'3D view: open images in 3D where they have a z-axis (per experiment/set)'"
        ><span class="opt-text">3D</span></button>

        <button
          class="opt-btn" :class="{ active: settings.napariAsDask }"
          @click="settings.napariAsDask = !settings.napariAsDask"
          v-tooltip.bottom="'Lazy load (Dask): fast open, slices computed on demand. Untick to load full zarr into memory — slower to open but smoother viewing.'"
        ><i class="pi pi-database" /></button>
      </div>
    </div>

    <template v-if="napariImage">
      <!-- ── Current image: what's open + its versions + segmentation label sets ── -->
      <div class="viewer-section">
        <div class="viewer-section-title">Current image</div>
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
        <span v-else class="viewer-hint">No versions registered.</span>

        <!-- segmentation label sets: show labels / tracks, delete -->
        <div v-if="hasLabels" class="viewer-labels-list">
          <div v-for="vn in labelNames" :key="vn" class="viewer-label-row">
            <i class="pi pi-th-large viewer-label-icon" />
            <span class="viewer-label-name" :title="vn">{{ vn }}</span>
            <!-- action icons are hidden until row hover (keeps the narrow sidebar tidy); an ACTIVE
                 toggle stays visible so you can see what's shown without hovering -->
            <button
              class="opt-btn row-act" :class="{ active: visibleLabels[vn] }"
              @click="toggleLabel(vn)"
              v-tooltip.right="visibleLabels[vn] ? 'Hide labels in Napari' : 'Show labels in Napari'"
            ><i class="pi pi-eye" /></button>
            <button
              class="opt-btn row-act" :class="{ active: trackVns[vn] }"
              @click="toggleTrack(vn)"
              v-tooltip.right="trackVns[vn] ? 'Hide this segmentation\'s tracks' : 'Show this segmentation\'s tracks'"
            ><i class="pi pi-share-alt" /></button>
            <ConfirmDeleteButton class="row-act"
              title="Delete label set from disk"
              armed-title="Click again to permanently delete this label set"
              @confirm="deleteLabel(vn)" />
          </div>
        </div>
      </div>

    <!-- ── Populations & tracks: overlays on the open image ────────────────── -->
    <!-- pop toggles show coloured centroid POINTS (layers namespaced by pop type, so they coexist);
         the ribbon toggles show gated / cluster track populations as napari Tracks layers. -->
    <div class="viewer-section">
      <div class="viewer-section-title">Populations &amp; tracks</div>
      <div class="viewer-opts">
        <button
          v-for="pt in POP_TYPES" :key="pt.key"
          class="opt-btn" :class="{ active: popVisible(pt.key) }"
          @click="togglePopType(pt.key)"
          v-tooltip.bottom="`${popVisible(pt.key) ? 'Hide' : 'Show'} ${pt.label} (points)`"
        ><i :class="['pi', pt.icon]" /></button>
        <!-- Tracks as ribbons (TEST/SDGF gated track pops); per-segmentation _tracked toggles live
             in the Segmentations list above (directions icon per row) -->
        <button
          class="opt-btn" :class="{ active: gatedTracksShown }"
          @click="toggleGatedTracks"
          v-tooltip.bottom="gatedTracksShown ? 'Hide track-pop ribbons' : 'Show track populations as ribbons (track-measure gates)'"
        ><i class="pi pi-share-alt" /></button>
        <button
          class="opt-btn" :class="{ active: popVisible('trackclust') }"
          @click="toggleTrackclust"
          v-tooltip.bottom="popVisible('trackclust') ? 'Hide track-cluster ribbons' : 'Show track-cluster populations as ribbons'"
        ><i class="pi pi-sitemap" /></button>
      </div>
    </div>

      <!-- ── Colour by: shade tracks + labels by an obs column (e.g. HMM state); '' = default ── -->
      <div v-if="obsCols.length" class="viewer-section">
        <div class="viewer-section-title">Colour by</div>
        <select class="opt-colourby" :value="colourByCol" @change="onColourBy"
                v-tooltip.right="'Colour tracks + labels by a cell property (e.g. HMM state). Values matching a population use its colour.'">
          <option value="">default</option>
          <option v-for="c in obsCols" :key="c" :value="c">{{ c }}</option>
        </select>
        <!-- legend for a categorical colour-by: value → colour (a population's colour where one matches) -->
        <div v-if="legendItems.length" class="cby-legend">
          <span v-for="item in legendItems" :key="item.key" class="cby-item"
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
                  v-tooltip.right="'Reset colours to population colours / the default palette'">reset</button>
        </div>
      </div>

      <!-- ── Movie: record the CURRENT view over time → mp4 (project's movies/ folder) ──
           Records exactly what's shown (channels, populations, tracks, colour-by). fps + resolution
           are per-set; the fuller config (which channels/pops, T-range, batch) is F1.2/F1.3. -->
      <div class="viewer-section">
        <div class="viewer-section-title">Movie</div>
        <div class="movie-row">
          <span class="movie-lbl" v-tooltip.bottom="'Frames per second'">fps</span>
          <input type="range" min="1" max="60" step="1" v-model.number="movieFps" class="movie-range" />
          <span class="movie-val">{{ movieFps }}</span>
          <span class="movie-lbl" v-tooltip.bottom="'Resolution supersample (2× = double resolution)'">res</span>
          <input type="range" min="1" max="3" step="1" v-model.number="movieScale" class="movie-range" />
          <span class="movie-val">{{ movieScale }}×</span>
          <button class="opt-btn movie-rec" :class="{ active: recording }" :disabled="recording"
                  @click="recordTimelapse"
                  v-tooltip.bottom="'Record the current view over the time axis → mp4 in the project\'s movies/ folder'">
            <i :class="['pi', recording ? 'pi-spin pi-spinner' : 'pi-video']" />
          </button>
        </div>
        <!-- Title card (Phase H): prepend a description slide (name, attributes, channels & colours) -->
        <div class="movie-row movie-title-row">
          <label class="movie-lbl movie-title-toggle"
                 v-tooltip.bottom="'Prepend a title slide: image name, attributes, channels & their colours'">
            <input type="checkbox" v-model="titleCardOn" /> title
          </label>
          <template v-if="titleCardOn">
            <input type="range" min="1" max="10" step="1" v-model.number="titleDur" class="movie-range"
                   v-tooltip.bottom="'Title-card duration (seconds)'" />
            <span class="movie-val">{{ titleDur }}s</span>
            <input type="text" v-model="titleNote" class="movie-note" placeholder="note (optional)" />
          </template>
        </div>
      </div>
    </template>
    <div v-else class="viewer-section"><span class="viewer-hint">No image open in Napari.</span></div>
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
  border: 1px solid var(--cc-warn);
  border-radius: 0.3rem;
  background: color-mix(in srgb, var(--cc-warn) 14%, transparent);
  color: var(--cc-warn);
  font-size: 0.7rem;
}
.viewer-stale-txt { flex: 1; min-width: 0; }
.viewer-stale-btn {
  flex-shrink: 0;
  font-size: 0.66rem;
  font-weight: 600;
  padding: 0.15rem 0.45rem;
  border-radius: 0.25rem;
  border: 1px solid var(--cc-warn);
  background: none;
  color: var(--cc-warn);
  cursor: pointer;
}
.viewer-stale-btn:hover { background: color-mix(in srgb, var(--cc-warn) 22%, transparent); }

.viewer-image {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  min-width: 0;
}
.viewer-eye { font-size: 0.72rem; color: #f97316; flex-shrink: 0; }
.viewer-name {
  font-size: 0.75rem;
  color: var(--cc-text);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

/* visual styling from the global form base (style.css) */
.viewer-select { width: 100%; font-size: 0.72rem; }
/* colour-by dropdown: full width on its own line (the sidebar is narrow, so inline it clipped) */
.opt-colourby { font-size: 0.7rem; width: 100%; min-width: 0; }
/* movie recording params — one compact row: fps slider · res slider · record button */
.movie-row { display: flex; align-items: center; gap: 0.3rem; }
.movie-lbl { font-size: 0.6rem; color: var(--cc-text-dim); flex-shrink: 0; text-transform: uppercase; letter-spacing: 0.04em; }
.movie-range { flex: 1; min-width: 2.5rem; accent-color: #7c3aed; }
.movie-val { font-size: 0.64rem; color: var(--cc-text); width: 1.4rem; text-align: right; flex-shrink: 0; font-variant-numeric: tabular-nums; }
.movie-rec { margin-left: 0.1rem; }
.movie-title-row { margin-top: 0.25rem; }
.movie-title-toggle { display: inline-flex; align-items: center; gap: 0.25rem; cursor: pointer; text-transform: none; letter-spacing: 0; }
.movie-note { flex: 2; min-width: 3rem; font-size: 0.64rem; padding: 1px 4px;
  border: 1px solid var(--cc-border); border-radius: 3px; background: var(--cc-surface-1); color: var(--cc-text); }

/* colour-by legend: value → swatch (a population's colour where one matches, else default) */
.cby-legend { display: flex; flex-wrap: wrap; gap: 0.15rem 0.5rem; margin-top: 0.25rem; }
.cby-item { display: inline-flex; align-items: center; gap: 0.25rem; font-size: 0.66rem; color: var(--cc-text-dim); }
.cby-swatch { width: 0.7rem; height: 0.7rem; border-radius: 2px; flex-shrink: 0; border: 1px solid var(--cc-border); }
/* editable swatch: a native colour input squeezed to swatch size (categories with no population) */
.cby-swatch-edit { padding: 0; cursor: pointer; background: none; -webkit-appearance: none; appearance: none; }
.cby-swatch-edit::-webkit-color-swatch-wrapper { padding: 0; }
.cby-swatch-edit::-webkit-color-swatch { border: none; border-radius: 2px; }
.cby-swatch-edit::-moz-color-swatch { border: none; border-radius: 2px; }
.cby-reset {
  font-size: 0.6rem; color: var(--cc-text-dim); background: none; border: none; cursor: pointer;
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
.viewer-section-title {
  font-size: 0.6rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--cc-text-dim);
}

.viewer-hint {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
}

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
  font-size: 0.72rem;
  color: var(--cc-accent);
  flex-shrink: 0;
}
.viewer-label-name {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  flex: 1;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.opt-btn.danger:hover { border-color: #ef4444; color: #ef4444; }
/* row action icons (eye / directions / trash): hidden until the row is hovered to keep the narrow
   sidebar uncluttered; an ACTIVE toggle (shown layer/tracks) stays visible so state is readable */
.row-act { opacity: 0; transition: opacity 0.12s; }
.viewer-label-row:hover .row-act, .row-act.active { opacity: 1; }
/* the delete affordance is the shared ConfirmDeleteButton (its own .cc-del button); apply the same
   hover-reveal to it via :deep, and keep it visible while armed so the confirm step never vanishes. */
.viewer-label-row :deep(.cc-del) { opacity: 0; transition: opacity 0.12s; }
.viewer-label-row:hover :deep(.cc-del), .viewer-label-row :deep(.cc-del.armed) { opacity: 1; }

/* ── Option toggles ──────────────────────────────────────────────────── */

.viewer-opts {
  display: flex;
  flex-wrap: wrap;
  gap: 0.25rem;
}

.opt-btn {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 1.8rem;
  height: 1.8rem;
  border-radius: 0.3rem;
  border: 1px solid var(--cc-border);
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  cursor: pointer;
  font-size: 0.75rem;
  line-height: 1;
  transition: background 0.1s, color 0.1s, border-color 0.1s;
  flex-shrink: 0;
}
.opt-btn:hover        { color: var(--cc-text); border-color: #484f58; }
.opt-btn.active       { background: #2d1b69; border-color: #7c3aed; color: #c4b5fd; }
.opt-btn.active:hover { background: #3b2382; }

.opt-text {
  font-size: 0.6rem;
  font-weight: 700;
  letter-spacing: 0.03em;
  line-height: 1;
}

</style>
