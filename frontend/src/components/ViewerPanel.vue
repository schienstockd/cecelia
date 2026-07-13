<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
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

// per-pop-type population overlays as centroid POINTS. Only the CELL-grained pop types (flow, clust)
// belong here: show-populations plots by cell label, whereas track/trackclust are track-grained
// (membership is track_ids) — their napari viz is ribbons (the Tracks-ribbon toggle below /
// per-segmentation directions), and trackclust ribbons are still to come. Layers are namespaced by
// pop type in the bridge, so flow + clust coexist.
// icons MATCH the sidebar module nav (Gate = pi-chart-scatter, Cluster cells = pi-palette,
// Track = pi-share-alt, Cluster tracks = pi-sitemap) so a pop type reads the same everywhere.
const POP_TYPES: { key: string; icon: string; label: string }[] = [
  { key: 'flow',  icon: 'pi-chart-scatter', label: 'gating populations' },
  { key: 'clust', icon: 'pi-palette',       label: 'cell-cluster populations' },
]
const trackVns          = ref<Record<string, boolean>>({})   // per-segmentation track-overlay visibility
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
  try {
    const res = await fetch('/api/napari/show-populations', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: uid,
                             valueName: valueName || undefined,   // blank → server uses active segmentation
                             popType, show, pointsSize: pointSize.value }),
    })
    return res.ok
  } catch { return false }   // napari not running, etc.
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
  try {
    const res = await fetch('/api/napari/show-tracks', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: uid, valueNames: onTrackVns.value,
                             showGatedTracks: gatedTracksShown.value,
                             showTrackclust: popVisible('trackclust'),
                             colorBy: colourByCol.value }),
    })
    return res.ok
  } catch { return false }   // napari not running, etc.
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
    obsCols.value = res.ok ? ((await res.json() as { obsColumns?: string[] }).obsColumns ?? []) : []
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
  try {
    const res = await fetch('/api/napari/colour-labels', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: uid, valueName: selectedValueName.value, column }),
    })
    return res.ok
  } catch { return false }
}

// user picked a colour-by column: persist, recolour the tracks (if shown) and the labels layer
function onColourBy(e: Event) {
  const col = (e.target as HTMLSelectElement).value
  colourByCol.value = col
  if (currentSetUid.value) settings.setColourBy(currentSetUid.value, col)   // per-set
  if (onTrackVns.value.length || gatedTracksShown.value) pushTracks()   // re-push tracks w/ new color_by
  pushColourLabels(col)                       // recolour labels (or reset when col === '')
}

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
  if (!settings.napariUpdateImage) return
  if (String(data.status ?? '') !== 'done') return
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

onMounted(() => {
  ws.on('napari:opened', onNapariOpened)
  ws.on('task:status', onTaskStatus)
  ws.on('task:result', onTaskResult)
  ws.on('gating:popmap', onGatingChange)
})
onUnmounted(() => {
  ws.off('napari:opened', onNapariOpened)
  ws.off('task:status', onTaskStatus)
  ws.off('task:result', onTaskResult)
  ws.off('gating:popmap', onGatingChange)
})
</script>

<template>
  <div class="viewer-panel">
    <template v-if="napariImage">
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

      <!-- ── Labels ─────────────────────────────────────────────────── -->
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
    </template>
    <span v-else class="viewer-hint">No image open in Napari.</span>

    <!-- ── Viewer option toggles ──────────────────────────────────────── -->
    <!-- Convention: append new toggles at the END of the row. -->
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

      <!-- Populations sub-menu: one toggle per pop type, shown as coloured centroid points in napari
           (layers namespaced by pop type, so they coexist). Distinct from the Tracks-ribbon toggle. -->
      <!-- population / overlay toggles: their OWN line (flex row break), led by a decorative dot at the
           left — a distinct control group from the image-open options above, aligned with the
           label/track-prop rows and the colour-by row below. -->
      <div v-if="napariImage" class="opt-group">
        <i class="pi pi-circle-fill opt-deco"
           v-tooltip.bottom="'Toggles for the population types to show'" />
        <button
          v-for="pt in POP_TYPES" :key="pt.key"
          class="opt-btn" :class="{ active: popVisible(pt.key) }"
          @click="togglePopType(pt.key)"
          v-tooltip.bottom="`${popVisible(pt.key) ? 'Hide' : 'Show'} ${pt.label} (points)`"
        ><i :class="['pi', pt.icon]" /></button>
        <!-- Tracks as ribbons (TEST/SDGF gated track pops); per-segmentation _tracked toggles live
             in the labels list above (directions icon per row) -->
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
      <!-- colour tracks + labels by an obs column (e.g. HMM state); '' = default colouring -->
      <span
        v-if="napariImage && obsCols.length" class="opt-colourby-wrap"
        v-tooltip.bottom="'Colour tracks + labels by a cell property (e.g. HMM state)'"
      >
        <i class="pi pi-circle-fill" />
        <select class="opt-colourby" :value="colourByCol" @change="onColourBy">
          <option value="">colour: default</option>
          <option v-for="c in obsCols" :key="c" :value="c">{{ c }}</option>
        </select>
      </span>
    </div>
  </div>
</template>

<style scoped>
.viewer-panel {
  padding: 0.35rem 0.6rem 0.4rem;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}

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
/* colour-by dropdown: full width on its OWN line (the sidebar is narrow, so inline it clipped),
   with a leading palette icon so it doesn't read as a stray dropdown among the icon toggles */
.opt-colourby-wrap { flex: 1 0 100%; display: flex; align-items: center; gap: 0.3rem; color: var(--cc-text-dim); min-width: 0; }
.opt-colourby-wrap .pi { font-size: 0.72rem; flex-shrink: 0; }
.opt-colourby { font-size: 0.7rem; flex: 1; min-width: 0; }

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

/* subtle divider between viewer-option toggles and population/overlay toggles */
.opt-sep {
  width: 1px;
  align-self: stretch;
  margin: 0.1rem 0.15rem;
  background: var(--cc-border);
  flex-shrink: 0;
}
/* population/overlay toggles on their own line (breaks the flex row), led by the dot at the left —
   aligned with the label/track-prop rows above and the colour-by row below */
.opt-group {
  flex: 1 0 100%;
  display: flex;
  align-items: center;
  gap: 0.25rem;
}
/* dot marking that group — matches the colour-by dot (.opt-colourby-wrap .pi): same colour + size, and
   carries a tooltip. Non-interactive (no click), but hoverable so the tooltip shows. */
.opt-deco {
  color: var(--cc-text-dim);
  font-size: 0.72rem;
  margin-left: 0.05rem;
  flex-shrink: 0;
  cursor: default;
}
</style>
