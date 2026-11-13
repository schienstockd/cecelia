<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import { useViewerStore } from '../stores/viewer'
import { openViewerWindow } from '../utils/viewerWindow'
import { getOpenPopoutWindow } from '../lib/popout'
import { screenshotFilename } from '../utils/viewerScreenshot'
import { downloadDataUrl } from '../plots/export'
import {
  colourLegend, colourLegendLabels, resetColourLegend,
  livePreviews, previewShown, togglePreview,
} from '../composables/useOverlayAutoShow'
import { activeValueName, CELL_POP_TYPES, type CellPopType, trackableValueNames } from '../utils/overlayAutoShow'
import { resolveValueName } from '../utils/valueName'
import { fetchColourByColumns } from '../utils/viewer/colourByColumns'
import { pingViewerOverlays as pingOverlays } from '../composables/useViewerAutoUpdate'
import { flipMaskVisibility, flipTracks, mismatchTip, shortLevelsTip } from '../utils/viewer/segRows'
import InlineNote from './InlineNote.vue'
import QuickMovie from './QuickMovie.vue'
import ChipSelect from './ChipSelect.vue'
import ColourPicker from './ColourPicker.vue'
import { publishViewerCacheClear } from '../lib/viewerCacheClearChannel'

const projectStore = useProjectStore()
const projectMeta  = useProjectMetaStore()

const settings     = useSettingsStore()
const ws           = useWsStore()
const log          = useLogStore()
const viewerStore  = useViewerStore()


// The version the viewer is showing: the remembered pick while the image still has it, else the
// active version (`activeValueName` mirrors what the server opens when no valueName is passed).
// Resolved live — the image is patched IN PLACE (`updateImageMeta`) and the pick is written from
// elsewhere (a version delete, a prune, the pop-out, another tab), none of which re-fires an
// `openedImage` watch. Writing it persists the pick, which the pop-out follows.
const selectedValueName = computed<string>({
  get: () => {
    const img = openedImage.value
    if (!img) return ''
    const names = Object.keys(img.filepaths ?? {})
    return resolveValueName(settings.getImageVersion(img.uid), names, names, activeValueName(img))
  },
  set: v => { const uid = openedImage.value?.uid; if (uid) settings.setImageVersion(uid, v) },
})
// Label visibility for the open image, read live like the version above — a label set added or
// removed in place must read as the viewer draws it. Unknown names default to true.
const visibleLabels     = computed<Record<string, boolean>>(() => openedImage.value
  ? settings.getLabelVisibility(openedImage.value.uid, Object.keys(openedImage.value.labels ?? {})) : {})
// Master "show cell-track ribbons" toggle: draws ribbons for flow pops whose cells have
// `track_id > 0` (a hand-drawn cell gate over cells that were later tracked). Distinct from
// "gated tracks" — a future track-poptype pop gated on TRACK measures (speed, straightness);
// see docs/TRACKING.md → "Track-property gating — backend done, frontend deferred".
// The internal setting key is still `showGatedTracks` for continuity with persisted user state.
const gatedTracksShown  = computed(() =>
  currentSetUid.value ? settings.getShowGatedTracks(currentSetUid.value) : false)

// per-pop-type population overlays as centroid POINTS. WHICH pop types these are is defined once, in
// utils/overlayAutoShow (CELL_POP_TYPES) — the app-level autoshow restores exactly the same set, so a
// new pop type can't end up toggleable-but-never-restored. Only CELL-grained types are in that list:
// show-populations plots by cell label, whereas track/trackclust are track-grained (membership is
// track_ids) — their viz is ribbons (the Tracks-ribbon toggle below / per-segmentation
// directions, plus the pi-sitemap trackclust ribbon toggle). Layers are namespaced by pop type in the
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
// Read straight off the settings store, not copied on image open: other surfaces write these for the
// image already open (cell cards' / the cockpit's Show, a strip frame's zoom-to-source, another tab),
// and a copy left the toggle showing — and the next toggle writing back — the old state.
// Tracks span BOTH registries (labels ∪ branchLabels) — a points-only set is exactly the one whose
// tracks you want. Label visibility is NOT unioned: a set with no `labels` has no pixels to show.
const trackVns          = computed<Record<string, boolean>>(() => openedImage.value   // per-segmentation track-overlay visibility
  ? settings.getTrackVisibility(openedImage.value.uid, trackableValueNames(openedImage.value)) : {})
const branchVns         = computed<Record<string, boolean>>(() => openedImage.value   // per-segmentation branch-overlay visibility
  ? settings.getBranchVisibility(openedImage.value.uid, Object.keys(openedImage.value.branchLabels ?? {})) : {})
const obsCols           = ref<string[]>([])   // obs columns of the open segmentation (colour-by options)
// The set's colour-by column, shown only when this image's segmentation has it (segmentations differ
// across a set) — the stored per-set value is kept for the images that do. '' = default.
const storedColourBy    = computed(() => currentSetUid.value ? settings.getColourBy(currentSetUid.value) : '')
const colourByCol       = computed(() => obsCols.value.includes(storedColourBy.value) ? storedColourBy.value : '')

// The image the user is focused on — whichever viewer is open. The read is `openImageUid`: the
// panel gates its content on ANY viewer being open. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P1.
const openedImage = computed(() => {
  const uid = projectStore.openImageUid
  if (!uid) return null
  for (const set of projectStore.sets) {
    const img = set.images.find(i => i.uid === uid)
    if (img) return img
  }
  return null
})

const valueNames = computed(() => Object.keys(openedImage.value?.filepaths ?? {}))
const labelNames  = computed(() => Object.keys(openedImage.value?.labels ?? {}))

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
  (openedImage.value?.labelPropsNames ?? []).filter(vn => !labelNames.value.includes(vn)))

// Mask stores whose spatial dims don't match the OPEN image version (segmented on a
// drift-expanded / cropped version). Published by ViewerWindow on meta load; consumed here to flag
// those rows. Scoped to the OPEN image: a mismatch payload for a different imageUid is stale and
// ignored. Empty when the viewer isn't open (no flag until we've checked).
const dimMismatchInfo = computed(() => {
  const m = viewerStore.labelsDimMismatch
  const uid = projectStore.openImageUid
  if (!m || !uid || m.imageUid !== uid) return null
  return m
})
const mismatchedMaskVns = computed<Set<string>>(() =>
  new Set(dimMismatchInfo.value?.mismatched ?? []))

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
    dimMismatch: mismatchedMaskVns.value.has(valueName),
    shortLevels: !!dimMismatchInfo.value?.shortLevels?.[valueName],
  }))
})
const hasLabelRows = computed(() => labelRows.value.length > 0)
/**
 * Segmentations worth a row without being asked for: the ones DOING something — a mask, tracks or
 * branches on screen, or a run writing right now.
 *
 * Seven registered segmentations is an ordinary number on a real image (`fXgbTl` has seven), and a row
 * each made this section taller than everything under it put together — the panel became a scroll to
 * reach Populations. The rest are one click away rather than gone, because which
 * segmentations EXIST is still the question this section answers.
 */
const activeLabelRows = computed(() => labelRows.value.filter(
  r => r.live || previewShown.value[r.valueName] || visibleLabels.value[r.valueName]
    || trackVns.value[r.valueName] || branchVns.value[r.valueName]))
const labelsExpanded = ref(false)
const shownLabelRows = computed(() =>
  labelsExpanded.value ? labelRows.value : activeLabelRows.value)
const foldedLabelCount = computed(() => labelRows.value.length - activeLabelRows.value.length)

// the set the open image belongs to — the key for per-set viewer prefs (colour-by, show-3D,
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
const popVisible = (popType: string): boolean =>
  currentSetUid.value ? settings.getPopVisible(currentSetUid.value, popType) : false
const setPopVisible = (popType: string, v: boolean) => {
  if (currentSetUid.value) settings.setPopVisible(currentSetUid.value, popType, v) }
// The toggle state above drives the TOGGLE UI only. The layers themselves are (re)pushed by the
// app-level useOverlayAutoShow (on open) and onGatingChange — neither reads the panel's state.
watch(() => openedImage.value?.uid, () => { labelsExpanded.value = false })   // a different image is a different list
// Colour-by options follow the image AND the version shown on it — however the version moved.
watch(() => [openedImage.value?.uid, selectedValueName.value], () => loadObsCols(), { immediate: true })

// Follow the server's active version into the popup viewer when a TASK swaps `_active` for the image
// on screen (e.g. a denoise run produces "smoothed" and marks it active). Watches the active version
// itself: the image object is patched in place, so a watch on `openedImage` misses the swap. The
// viewer only reacts to `cc.viewerImageVersion`. Without a mirror the user
// has to toggle the dropdown back and forth to force a switch. Guard with `lastServerActive` so we only
// follow when the persisted pick was tracking the OLD active (empty or equal to it) — never override a
// user pick that intentionally names a different version.
const lastServerActive = ref<Record<string, string>>({})
watch(() => [openedImage.value?.uid, openedImage.value?.activeValueName], () => {
  const img = openedImage.value
  if (!img) return
  const uid = img.uid
  const active = img.activeValueName ?? ''
  const prev = lastServerActive.value[uid]
  const picked = settings.getImageVersion(uid)
  if (prev && active && prev !== active && (picked === '' || picked === prev)) {
    settings.setImageVersion(uid, active)
  }
  lastServerActive.value[uid] = active
}, { immediate: true })

function openInViewer(valueName: string) {
  const uid        = projectStore.openImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return
  openViewerWindow({ projectUid, imageUid: uid, valueName: valueName || undefined })
}

// Napari-parity Save PNG. Reads the current composed frame from the popped-out viewer window
// (`modules/ViewerWindow.vue`'s `__cceceliaViewerScreenshot`) and downloads it — no project side
// effect, no task, no server round-trip. Fails cleanly when the viewer popup is not open, matching
// the same pattern `components/plots/ImageStripView.vue` uses for its own capture path: the panel
// cannot screenshot what the user cannot see.
interface ScreenshotNote { severity: 'warn' | 'fail'; short: string; detail?: string }
const screenshotBusy = ref(false)
const screenshotNote = ref<ScreenshotNote | null>(null)
async function saveScreenshot() {
  screenshotNote.value = null
  const vw = getOpenPopoutWindow('/viewer-window')
  if (!vw) {
    screenshotNote.value = { severity: 'warn', short: 'Open the viewer window first',
      detail: 'Click the eye on an image in the table to pop the viewer out, then try again.' }
    return
  }
  const shot = (vw as unknown as { __cceceliaViewerScreenshot?: () => Promise<{ png: string; imageName: string }> }).__cceceliaViewerScreenshot
  if (!shot) {
    screenshotNote.value = { severity: 'warn', short: 'Viewer not ready yet',
      detail: 'The viewer window is still loading. Wait for the image to appear and try again.' }
    return
  }
  screenshotBusy.value = true
  try {
    const { png, imageName } = await shot()
    downloadDataUrl(screenshotFilename(imageName || openedImage.value?.name), png)
  } catch (e) {
    screenshotNote.value = { severity: 'fail', short: 'Screenshot failed',
      detail: e instanceof Error ? e.message : String(e) }
  } finally { screenshotBusy.value = false }
}

// Push a pop type's populations to the viewer as centroid points. The server shows EVERY
// segmentation's pops at once (each as its own value_name-tagged layer), so `valueName` no longer
// selects which pops appear — the overlay is independent of which segmentation is "active" (opening
// the image shows all, not just the active/first one). The WebGPU viewer reads pop visibility off
// the shared settings bag (P5).

// Per-pop-type visibility toggle. Persist + ping the WebGPU viewer to re-derive its hidden-pop set.
function togglePopType(popType: string) {
  setPopVisible(popType, !popVisible(popType))
  pingViewerOverlays()
}

// Tracks: the WebGPU viewer draws per-segmentation ribbons off `settings.getTrackVisibility` (P7).

// Per-segmentation toggle: flip this segmentation's track overlay, persist, ping the viewer.
function toggleTrack(vn: string) {
  // `openImageUid`, not `viewerImageUid`: this write must land whether or not any legacy image-open
  // signal has fired. Before P6 the persist was gated on `viewerImageUid=null`, which meant the
  // WebGPU viewer never saw the write.
  const uid = projectStore.openImageUid
  if (uid) settings.setTrackVisibility(uid, flipTracks(trackVns.value, vn))
  pingViewerOverlays()
}

// Per-segmentation toggle: flip this segmentation's branch (skeleton) label overlay. Persist +
// ping; the WebGPU viewer reads `settings.getBranchVisibility` (via P4-style store).
function toggleBranch(vn: string) {
  const uid   = projectStore.openImageUid
  const files = openedImage.value?.branchLabels?.[vn] ?? []
  if (!files.length) {
    log.error(`No branch label files registered for "${vn}"`, { source: 'viewer' })
    return
  }
  const wasVisible = branchVns.value[vn] ?? true
  if (uid) settings.setBranchVisibility(uid, { ...branchVns.value, [vn]: !wasVisible })
  pingViewerOverlays()
}

// Master toggle for cell-track ribbons (see the ref comment above), matching the Show populations toggle.
function toggleGatedTracks() {
  if (currentSetUid.value) settings.setShowGatedTracks(currentSetUid.value, !gatedTracksShown.value)
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
  // `openImageUid` — same reason as `openedImage` / `recordTimelapse`: `viewerImageUid` was the napari
  // WS `open` field, which nothing writes now that the browser viewer is the only viewer, so the
  // colour-by dropdown stayed empty for every image opened via the eye click.
  const uid = projectStore.openImageUid
  const projectUid = projectMeta.current?.uid
  const vn = selectedValueName.value
  obsCols.value = uid && projectUid && vn ? await fetchColourByColumns(projectUid, uid, vn) : []
}

// User picked a colour-by column: persist per-set; the WebGPU viewer picks it up via the overlay
// tick and re-derives label + track colours from the settings bag.
function onColourBy(e: Event) {
  const col = (e.target as HTMLSelectElement).value
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
 * the popup viewer window (VIEWER_CONTROLS_SPLIT_PLAN.md P3 extended, ): the panel
 * is the single control now, so the advisory sits next to the control that changes it.
 *
 * `null` when there is only one version, or `activeValueName` isn't reported — an absent answer
 * must read as no claim, not as a pass.
 */
const versionNote = computed(() => {
  const img = openedImage.value
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
  // Broadcast to the popup viewer — the panel is the single source of truth for the version now
  // (VIEWER_CONTROLS_SPLIT_PLAN.md P3 extended). The popup's storage-event bridge (P2) picks this
  // up via settings.setImageVersion → `cc.viewerImageVersion` and calls its `changeVersion`
  // internally, so the two windows never disagree about the version on screen. If no popup is up
  // yet, `openInViewer` opens one on the picked version.
  if (projectStore.openImageUid) selectedValueName.value = name
  else openInViewer(name)
}

// Fire the WebGPU-viewer overlays-refetch ping. Every panel toggle that changes what the viewer
// should draw calls this after persisting settings, so a popup viewer with its own store re-reads
// its inputs (label bag, pop-type bag, track bag) via the `storage` bridge. See P5 of
// VIEWER_CONTROLS_SPLIT_PLAN.md.
function pingViewerOverlays() { pingOverlays(projectStore.openImageUid) }

/** Tooltip text for a mask whose spatial dims don't match the open image version. Quotes both
 *  dims so the user can see WHY the row is flagged — a mask segmented on a drift-expanded /
 *  cropped version cannot be overlaid without either mis-striding the texture or landing
 *  spatially shifted from the underlying cells (rendering it silently would be a lie). Falls
 *  back to a short claim if `byVn` didn't carry dims for this vn (unreadable pyramid). */
function mismatchTooltip(vn: string): string { return mismatchTip(dimMismatchInfo.value, vn) }

/** Tooltip for a mask with fewer zoom levels than the image: it draws only where it has a level. */
function shortLevelsTooltip(vn: string): string { return shortLevelsTip(dimMismatchInfo.value, vn) }

/** A segmentation's derived label files (`halo`, `nuc`) — from the open viewer's meta, so only while a
 *  viewer is open, which is the only time the chips do anything. */
function labelFilesOf(vn: string): string[] {
  return dimMismatchInfo.value?.labelFiles?.[vn] ?? []
}
function shownFilesOf(vn: string): string[] {
  const uid = projectStore.openImageUid
  const hidden = uid ? settings.getLabelFilesHidden(uid, vn) : new Set<string>()
  return labelFilesOf(vn).filter(f => !hidden.has(f))
}
// Stored as what is HIDDEN (settings.getLabelFilesHidden); the pop-out reads it through the bag channel.
function setShownFiles(vn: string, shown: string[]) {
  const uid = projectStore.openImageUid
  if (uid) settings.setLabelFilesHidden(uid, vn, new Set(labelFilesOf(vn).filter(f => !shown.includes(f))))
}

function toggleLabel(valueName: string) {
  // Write the settings bag; the WebGPU viewer reads it via `storage` events.
  //
  // Radio-like: the WebGPU viewer draws one label mask at a time (r32uint, single-slot bind group;
  // multi-mask is deferred to PX). Ticking a segmentation UNticks the others so what you see in the
  // panel matches what you see in the viewer, instead of the viewer silently picking one of several
  // ticked.
  const uid = projectStore.openImageUid
  // One mask at a time, every name explicit (`flipMaskVisibility`): the panel's list plus the store's.
  const bag = flipMaskVisibility(Object.keys(openedImage.value?.labels ?? {}), visibleLabels.value, valueName)
  if (uid) settings.setLabelVisibility(uid, bag)
  pingViewerOverlays()
}


// Manual escape hatch for the "stale slab in browser cache" case. Publishes a cache-clear on the
// open image — same channel the task-done auto path uses — which bumps the rev to a fresh
// Date.now() and every slab URL from ViewerWindow onwards carries `_r=<new>` so the browser HTTP
// cache misses. Covers cases the auto path can't: a viewer window mounted before the #814 fix
// shipped, or a task:status that raced the viewer's listener (multi-window edge case).
function forceViewerResync() {
  const uid = projectStore.openImageUid
  if (!uid) return
  publishViewerCacheClear({ imageUid: uid, force: true })
}

// Task results reach the viewer through the app-level `useViewerAutoUpdate`; the panel only refreshes
// its own colour-by options when a task wrote a new column.
function onTaskResult(data: Record<string, unknown>) {
  if (String(data.imageUid ?? '') !== projectStore.openImageUid) return
  if ((data.meta as Record<string, unknown> | undefined)?.valueName && settings.viewerAutoUpdate) void loadObsCols()
}



onMounted(() => ws.on('task:result', onTaskResult))
onUnmounted(() => ws.off('task:result', onTaskResult))
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
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': settings.viewerAutoSaveLayerProps }"
          @click="settings.viewerAutoSaveLayerProps = !settings.viewerAutoSaveLayerProps"
          v-tooltip.bottom="'Save contrast, colormap and T/Z as you change them'"
        ><i class="pi pi-bookmark" /></button>

        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': show3D }" :disabled="!currentSetUid"
          @click="show3D = !show3D"
          v-tooltip.bottom="'3D view: open images in 3D where they have a z-axis (per experiment/set)'"
        ><span class="opt-text">3D</span></button>

        <!-- Escape hatch for the "Slab is AxBxC but XxYxZ was asked for" case: republishes the
             cache-clear signal for the open image AND bumps the local rev to a fresh value, so the
             next slab URLs carry a new `_r=<rev>` and browser HTTP cache is bypassed. The auto path
             (task-done → cache-clear) covers dim-changing rewrites once the fix from #814 shipped,
             but a viewer window mounted before the fix landed OR a task-done that raced the
             viewer's task:status listener leaves the user stuck. One click here rescues it. -->
        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :disabled="!openedImage"
          @click="forceViewerResync"
          v-tooltip.bottom="'Re-read pixels from disk (use if the viewer looks stale after a task rerun)'"
        ><i class="pi pi-sync" /></button>

      </div>
    </div>

    <template v-if="openedImage">
      <!-- ── Current image: what's open + its versions + segmentation label sets ── -->
      <div class="viewer-section">
        <div class="viewer-section-title cc-eyebrow cc-fs-2xs">Current image</div>
        <div class="viewer-image">
          <i class="pi pi-eye viewer-eye" />
          <span class="viewer-name" :title="openedImage.name">{{ openedImage.name }}</span>
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
          <template v-for="row in shownLabelRows" :key="row.valueName">
          <div class="viewer-label-row">
            <i :class="['pi', row.masked ? 'pi-th-large' : 'pi-circle-fill', 'viewer-label-icon']"
               v-tooltip.right="row.masked ? undefined : 'Points only — tracks, no mask'" />
            <span class="viewer-label-name cc-muted" :title="row.valueName">{{ row.valueName }}</span>
            <!-- Mask segmented on a DIFFERENT image version (drift-expanded / cropped) — its
                 spatial dims don't fit the open version. Overlay is silently skipped rather
                 than mis-strided; the tooltip quotes the two dims so it's obvious why. -->
            <i v-if="row.dimMismatch"
               class="pi pi-exclamation-triangle viewer-label-warn"
               v-tooltip.right="mismatchTooltip(row.valueName)" />
            <i v-else-if="row.shortLevels"
               class="pi pi-exclamation-triangle viewer-label-warn"
               v-tooltip.right="shortLevelsTooltip(row.valueName)" />
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
                v-if="row.masked && (openedImage?.branchLabels?.[row.valueName]?.length ?? 0) > 0"
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
                v-tooltip.right="row.dimMismatch
                  ? mismatchTooltip(row.valueName)
                  : (visibleLabels[row.valueName] ? 'Hide labels in Viewer' : 'Show labels in Viewer')"
              ><i class="pi pi-eye" /></button>
              <!-- No delete here. Deleting a label set is one scope of the Import page's Delete modal
                   (docs/todo/IMAGE_DELETE_PLAN.md Decision 4) — the viewer shows and hides layers, it
                   does not curate what exists on disk. -->
            </template>
          </div>
          <!-- The shown mask's derived files (halo, nuc): drawn as outlines on it, all on by default
               (docs/todo/MASK_FILE_LAYERS_PLAN.md). Only while the mask is on — they are part of it. -->
          <ChipSelect
            v-if="row.masked && visibleLabels[row.valueName] && labelFilesOf(row.valueName).length"
            class="viewer-label-files cc-fs-2xs" multiple :collapse-above="0"
            :options="labelFilesOf(row.valueName).map(f => ({ value: f, tip: `Outline the ${f}` }))"
            :model-value="shownFilesOf(row.valueName)"
            :aria-label="`${row.valueName} label files`"
            @update:model-value="v => setShownFiles(row.valueName, v as string[])"
          />
          </template>
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
         the ribbon toggles show cell-track / cluster-track populations as Tracks layers. -->
    <div class="viewer-section">
      <div class="viewer-section-title cc-eyebrow cc-fs-2xs">Populations &amp; tracks</div>
      <div class="viewer-opts cc-row cc-row-tight">
        <button
          v-for="pt in POP_TYPES" :key="pt.key"
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': popVisible(pt.key) }"
          @click="togglePopType(pt.key)"
          v-tooltip.bottom="`${popVisible(pt.key) ? 'Hide' : 'Show'} ${pt.label} (points)`"
        ><i :class="['pi', pt.icon]" /></button>
        <!-- Cell-track ribbons: flow pops whose cells hold `track_id > 0`. Per-segmentation
             `_tracked` toggles live in the Segmentations list above (directions icon per row). -->
        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': gatedTracksShown }"
          @click="toggleGatedTracks"
          v-tooltip.bottom="gatedTracksShown ? 'Hide cell-track ribbons' : 'Show cell-track ribbons'"
        ><i class="pi pi-share-alt" /></button>
        <button
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on cc-btn-on-tint': popVisible('trackclust') }"
          @click="toggleTrackclust"
          v-tooltip.bottom="popVisible('trackclust') ? 'Hide track-cluster ribbons' : 'Show track-cluster populations as ribbons'"
        ><i class="pi pi-sitemap" /></button>
        <!-- Clear highlight — only when one is active. TrackSchemeView + CorrectionCockpit + the
             cell-cards detail all publish `viewerStore.trackHighlight` to narrow the ribbons to a
             selection, but nothing local to those callers un-narrows on close. Surfacing the reset
             in the viewer panel is the single unmute — click it and the ribbons go back to all
             tracks. Reason for centralising here: three publishers, one place that always shows
             what's currently narrowed. -->
        <button
          v-if="viewerStore.trackHighlight"
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-on cc-btn-on-tint"
          @click="viewerStore.setTrackHighlight(null)"
          v-tooltip.bottom="`Clear ${viewerStore.trackHighlight.trackIds.length} highlighted track${viewerStore.trackHighlight.trackIds.length === 1 ? '' : 's'}`"
        ><i class="pi pi-filter-slash" /></button>
        <!-- Same reset for the cell-pick outline (`pickHighlight`) — Claude's `mark_cells` /
             `select_on_plot`, the cockpit, a plot brush. Without it an assistant mark could only wait
             out its TTL. -->
        <button
          v-if="viewerStore.pickHighlight"
          class="opt-btn cc-btn cc-btn-ghost cc-btn-icon cc-btn-on cc-btn-on-tint"
          @click="viewerStore.setPickHighlight(null)"
          v-tooltip.bottom="`Clear ${viewerStore.pickHighlight.labels.length} highlighted cell${viewerStore.pickHighlight.labels.length === 1 ? '' : 's'}`"
        ><i class="pi pi-eraser" /></button>
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
                 value with no population → the shared ColourPicker (its colour isn't defined anywhere else) -->
            <span v-if="!item.editable" class="cby-swatch" :style="{ background: item.hex }" />
            <ColourPicker v-else :model-value="item.hex" tip=""
                          @update:model-value="c => onRecolour(item.value, c)" />
            {{ item.label }}
          </span>
          <button class="cby-reset" @click="resetColours"
                  v-tooltip.right="'Reset colours to population colours / the default palette'">Reset</button>
        </div>
      </div>

      <!-- ── Screenshot: save the CURRENT viewport as a PNG (napari-parity, canvas + overlays) ── -->
      <div class="viewer-section">
        <div class="viewer-section-title cc-eyebrow cc-fs-2xs">Screenshot</div>
        <div class="cc-row cc-row-tight">
          <button class="opt-btn cc-btn cc-btn-ghost cc-btn-icon" :disabled="screenshotBusy"
                  @click="saveScreenshot"
                  v-tooltip.bottom="'Save the viewport as a PNG (canvas + overlays)'">
            <i :class="['pi', screenshotBusy ? 'pi-spin pi-spinner' : 'pi-camera']" />
          </button>
        </div>
        <InlineNote v-if="screenshotNote" :severity="screenshotNote.severity"
                    :short="screenshotNote.short" :detail="screenshotNote.detail" />
      </div>

      <!-- Movie: record or save a figure of the image as shown (`QuickMovie`, shared with the viewer). -->
      <div class="viewer-section">
        <QuickMovie heading :img="openedImage" :project-uid="projectMeta.current?.uid ?? ''"
                    :set-uid="currentSetUid ?? ''" :shown-version="selectedValueName" />
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
.viewer-eye { font-size: var(--cc-fs-sm); color: var(--cc-text); flex-shrink: 0; }
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
/* the popover is free of the panel's width, so give the controls room to lay out on one line each */
/* .movie-lbl/-range/-val/-controls were left behind when MovieOutputControls was extracted — the
   component owns them now, so the dead rules are gone rather than re-orphaned here. */

/* colour-by legend: value → swatch (a population's colour where one matches, else default) */
.cby-legend { margin-top: 0.25rem; }
.cby-item { display: inline-flex; align-items: center; gap: 0.25rem; }
.cby-swatch { width: 0.7rem; height: 0.7rem; border-radius: var(--cc-radius-xs); flex-shrink: 0; border: 1px solid var(--cc-border); }
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
/* the shown mask's file chips, under its row, indented past the row icon */
.viewer-label-files { margin: 0 0 0.15rem 1.2rem; }
/* Mask-doesn't-fit-image flag: warn severity, sits between name and actions. Always shown (the
   whole point is that it must NOT wait for hover), and its host row's eye is disabled. */
.viewer-label-warn { font-size: var(--cc-fs-sm); color: var(--cc-sev-warn); flex-shrink: 0; }
.opt-btn.danger:hover { border-color: var(--cc-danger); color: var(--cc-danger); }
/* row action icons (eye / directions): hidden until the row is hovered to keep the narrow sidebar
   uncluttered; an ACTIVE toggle (shown layer/tracks) stays visible so state is readable */
.row-act { opacity: 0; transition: opacity 0.12s; }
/* hover-reveal: the row's actions stay hidden until hover, but an ENGAGED one must always show
   (it is the only indication the layer/track is on). Keyed on .cc-btn-on, the state primitive. */
.viewer-label-row:hover .row-act, .row-act.cc-btn-on { opacity: 1; }
/* ── Option toggles ──────────────────────────────────────────────────── */



.opt-btn { transition: background 0.1s, color 0.1s, border-color 0.1s; }   /* + cc-btn cc-btn-ghost cc-btn-icon */
.opt-btn:hover        { color: var(--cc-text); border-color: var(--cc-border-hover); }

.opt-text {
  font-size: var(--cc-fs-2xs);
  font-weight: 700;
  letter-spacing: 0.03em;
  line-height: 1;
}


</style>
