<script setup lang="ts">
import { ref, computed, watch, nextTick, onMounted, onUnmounted } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'

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
const populationsShown  = ref(false)   // master "show gating populations as points" toggle
const gatedTracksShown  = ref(false)   // master "show gated track populations" toggle (TEST/SDGF)
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


watch(napariImage, (img) => {
  // restore the remembered preference rather than always starting hidden — the actual layers
  // are (re)pushed by onNapariOpened / onGatingChange once the image + centroids are ready.
  populationsShown.value = settings.napariShowPopulations
  gatedTracksShown.value = settings.napariShowGatedTracks
  colourByCol.value = settings.napariColourBy
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
    show3D:        settings.napariShow3D,
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

// Push the gating populations to napari as centroid points. `valueName` defaults to the first
// segmentation (label set); the server falls back to the active labelProps version when blank.
async function pushPopulations(show: boolean, valueName?: string): Promise<boolean> {
  const uid        = projectStore.napariImageUid
  const projectUid = projectMeta.current?.uid
  if (!uid || !projectUid) return false
  try {
    const res = await fetch('/api/napari/show-populations', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid: uid,
                             valueName: valueName ?? labelNames.value[0] ?? undefined,
                             popType: 'flow', show, pointsSize: settings.napariPointSize }),
    })
    return res.ok
  } catch { return false }   // napari not running, etc.
}

// Master toggle for showing populations on the image; the choice is remembered (persisted in
// settings) so it carries across image opens / sessions.
async function togglePopulations() {
  const next = !populationsShown.value
  if (await pushPopulations(next)) {
    populationsShown.value = next
    settings.napariShowPopulations = next
  }
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
                             showGatedTracks: gatedTracksShown.value, colorBy: colourByCol.value }),
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
  settings.napariShowGatedTracks = next
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
  // drop a remembered colour-by column that isn't available for this segmentation
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
  settings.napariColourBy = col
  if (onTrackVns.value.length || gatedTracksShown.value) pushTracks()   // re-push tracks w/ new color_by
  pushColourLabels(col)                       // recolour labels (or reset when col === '')
}

// Live update while gating: when the population tree changes for the image open in napari
// (gate edit, pop add/remove/rename, cell selection, dot-size change), re-push so the overlay
// tracks the gating. The dot-size slider lives in the population manager Options box.
function onGatingChange(data: Record<string, unknown>) {
  if (String(data.imageUid ?? '') !== projectStore.napariImageUid) return
  const vn = data.valueName as string | undefined
  // points follow flow gating; gated TRACK pops follow track-gate edits → re-push when shown. The
  // per-segmentation `_tracked` overlay is whole-segmentation, so it needs no gate-driven re-push.
  if (data.popType === 'track') { if (gatedTracksShown.value) pushTracks() }
  else if (populationsShown.value) pushPopulations(true, vn)
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

// Inline two-click delete confirmation (no browser popup): first click arms the row (the trash flips
// to a warning icon), a second click within a few seconds actually deletes; otherwise it disarms.
const confirmDeleteVn = ref<string | null>(null)
let confirmTimer: ReturnType<typeof setTimeout> | null = null
function onDeleteClick(valueName: string) {
  if (confirmDeleteVn.value === valueName) {        // second click → confirmed
    if (confirmTimer) { clearTimeout(confirmTimer); confirmTimer = null }
    confirmDeleteVn.value = null
    deleteLabel(valueName)
  } else {                                          // first click → arm, auto-disarm after 3.5s
    confirmDeleteVn.value = valueName
    if (confirmTimer) clearTimeout(confirmTimer)
    confirmTimer = setTimeout(() => { confirmDeleteVn.value = null; confirmTimer = null }, 3500)
  }
}

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
  openInNapari(selectedValueName.value)
}

function onNapariOpened() {
  nextTick(() => {
    if (!hasLabels.value) return
    const toShow = Object.fromEntries(
      Object.entries(napariImage.value?.labels ?? {}).filter(([vn]) => visibleLabels.value[vn])
    )
    if (!Object.keys(toShow).length) return
    fetch('/api/napari/show-labels', {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({ allLabels: toShow, showLabels: true }),
    }).then(async res => {
      if (!res.ok) { log.error(`Show labels on open failed: ${await _resError(res)}`, { source: 'napari' }); return }
      if (colourByCol.value) pushColourLabels(colourByCol.value)   // apply remembered colour-by
    }).catch(e =>
      log.error(`Show labels on open failed: ${e instanceof Error ? e.message : String(e)}`,
                { source: 'napari' }))
  })
  // auto-show gating populations on open when the user last had them on (remembered preference)
  if (settings.napariShowPopulations) {
    pushPopulations(true).then(ok => { populationsShown.value = ok })
  }
  // re-show the track overlays from the REMEMBERED state, read straight from settings (the trackVns
  // ref is restored by the napariImage watch, which may not have run yet when this open event fires —
  // reading settings directly, like populations above, avoids that race).
  const uid = projectStore.napariImageUid
  if (uid) {
    trackVns.value = settings.getTrackVisibility(uid, labelNames.value)   // keep the ref in sync
    gatedTracksShown.value = settings.napariShowGatedTracks
    if (onTrackVns.value.length || gatedTracksShown.value) pushTracks()
  }
}

function onTaskResult(data: Record<string, unknown>) {
  const imageUid = String(data.imageUid ?? '')
  if (!imageUid || imageUid !== projectStore.napariImageUid) return
  const meta = (data.meta ?? {}) as Record<string, unknown>

  const addedValueName = meta.valueName as string | undefined
  if (addedValueName) {
    selectedValueName.value = addedValueName
    if (settings.napariUpdateImage) openInNapari(addedValueName)
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
          ><i class="pi pi-directions" /></button>
          <button
            class="opt-btn danger row-act" :class="{ 'confirm': confirmDeleteVn === vn }"
            @click="onDeleteClick(vn)"
            v-tooltip.right="confirmDeleteVn === vn ? 'Click again to permanently delete this label set' : 'Delete label set from disk'"
          ><i :class="confirmDeleteVn === vn ? 'pi pi-exclamation-triangle' : 'pi pi-trash'" /></button>
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
        v-tooltip.bottom="'Auto-update: reload image in Napari whenever a task finishes on that image'"
      ><i class="pi pi-refresh" /></button>

      <button
        class="opt-btn" :class="{ active: settings.napariAutoSaveLayerProps }"
        @click="settings.napariAutoSaveLayerProps = !settings.napariAutoSaveLayerProps"
        v-tooltip.bottom="'Auto-save layer props: save brightness/contrast and colormap when switching images, reload on next open'"
      ><i class="pi pi-bookmark" /></button>

      <button
        class="opt-btn" :class="{ active: settings.napariShow3D }"
        @click="settings.napariShow3D = !settings.napariShow3D"
        v-tooltip.bottom="'3D view: open image in 3D viewer instead of 2D'"
      ><span class="opt-text">3D</span></button>

      <button
        class="opt-btn" :class="{ active: settings.napariAsDask }"
        @click="settings.napariAsDask = !settings.napariAsDask"
        v-tooltip.bottom="'Lazy load (Dask): fast open, slices computed on demand. Untick to load full zarr into memory — slower to open but smoother viewing.'"
      ><i class="pi pi-database" /></button>

      <!-- population overlay (separated; more population-related toggles may join it here) -->
      <span v-if="napariImage" class="opt-sep" aria-hidden="true" />
      <button
        v-if="napariImage"
        class="opt-btn" :class="{ active: populationsShown }"
        @click="togglePopulations"
        v-tooltip.bottom="populationsShown ? 'Hide gating populations on the image' : 'Show gating populations as coloured cell points'"
      ><i class="pi pi-palette" /></button>
      <!-- master gated-track-populations toggle (TEST/SDGF); per-segmentation _tracked toggles live
           in the labels list above (directions icon per row) -->
      <button
        v-if="napariImage"
        class="opt-btn" :class="{ active: gatedTracksShown }"
        @click="toggleGatedTracks"
        v-tooltip.bottom="gatedTracksShown ? 'Hide gated track populations' : 'Show gated track populations (track-measure gates)'"
      ><i class="pi pi-directions" /></button>
      <!-- colour tracks + labels by an obs column (e.g. HMM state); '' = default colouring -->
      <select
        v-if="napariImage && obsCols.length"
        class="opt-colourby" :value="colourByCol" @change="onColourBy"
        v-tooltip.bottom="'Colour tracks + labels by a cell property (e.g. HMM state)'"
      >
        <option value="">colour: default</option>
        <option v-for="c in obsCols" :key="c" :value="c">{{ c }}</option>
      </select>
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
/* compact colour-by dropdown in the options row */
.opt-colourby { font-size: 0.7rem; max-width: 11rem; }

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
.viewer-label-row:hover .row-act, .row-act.active, .row-act.confirm { opacity: 1; }
/* armed delete: clearly red so the second-click-to-confirm state is obvious */
.opt-btn.danger.confirm { color: #fff; background: #ef4444; border-color: #ef4444; }

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
</style>
