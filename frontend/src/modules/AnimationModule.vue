<script setup lang="ts">
// Animation page — a "super-simple OpenShot": a per-image TIMELINE of view snapshots (keyframes) with a
// row/track matrix. Columns = keyframes (captured napari views + duration); rows = channels /
// populations / camera, all INFERRED from each keyframe's viewState (layers by type + camera). Capture
// establishes a base "look" (contrast/colormap/framing); add-keyframe copies it; a cell toggle overrides
// that keyframe's layer.visible. Render interpolates between keyframes → mp4 (WS `movie:record`, which
// puts the render on the task rail with progress + Cancel).
// The data model is just an ordered list of viewStates. See docs/todo/ANIMATION_PLAN.md (F2).
import { ref, computed } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import { useAnimationStore, type AnimSnapshot } from '../stores/animation'
import { useSettingsStore } from '../stores/settings'
import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { buildTitleCard, unionViewSnapshot, applyViewState, type TitleCardPayload } from '../utils/napariOverlays'
import { napariColormapHex } from '../utils/napariColormap'
import { elapsedLabel } from '../utils/stillOverlay'
import ConfirmDeleteButton from '../components/ConfirmDeleteButton.vue'
import CcToggle from '../components/CcToggle.vue'
import ModulePage from '../components/ModulePage.vue'
import TitleCardControls from '../components/TitleCardControls.vue'
import MovieOutputControls from '../components/MovieOutputControls.vue'
import MovieOptionsButton from '../components/MovieOptionsButton.vue'
import { movieSizeParams } from '../utils/movieSize'
import { useNapariStatus } from '../composables/useNapariStatus'

const projectMeta = useProjectMetaStore()
const projectStore = useProjectStore()
const log = useLogStore()
const anim = useAnimationStore()
const settings = useSettingsStore()
const tasks = useTaskStore()
const ws = useWsStore()
// the canvas size napari would record at, for the size fields' placeholder (shared poll)
const { canvasSizeX, canvasSizeY } = useNapariStatus()

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const hasProject = computed(() => projectMeta.hasProject)
const openImageUid = computed(() => projectStore.napariImageUid)

const capturing = ref(false)
const rendering = ref(false)
const updating = ref(false)
const dragId = ref<string | null>(null)   // keyframe being dragged (drag-to-reorder)
const selectedId = ref<string | null>(null)   // the currently-selected keyframe (the highlighted box)
const syncNapari = ref(false)             // when on, selecting a keyframe applies its view to napari
function onDrop(targetId: string) {
  if (dragId.value) anim.reorder(dragId.value, targetId)
  dragId.value = null
}

// apply a keyframe's saved view to the running napari viewer, so you SEE that snapshot (and can then
// tweak it in napari + Update). No-op if napari isn't running / the image isn't open.
async function applyToNapari(s: AnimSnapshot) {
  if (!s.snapshot) return
  await applyViewState(s.snapshot)   // shared builder; swallows a network error (napari not running)
}
// select a keyframe; if Sync is on, push it to napari
function selectKeyframe(s: AnimSnapshot) {
  selectedId.value = s.id
  if (syncNapari.value) applyToNapari(s)
}
// toggling Sync on immediately mirrors the selected keyframe into napari
function onToggleSync(on: boolean) {
  syncNapari.value = on
  const sel = frames.value.find(f => f.id === selectedId.value)
  if (on && sel) applyToNapari(sel)
}

// Update the selected keyframe FROM the current napari view — re-screenshot and replace its snapshot +
// thumbnail (and reset its baseline). This is how you "change" a snapshot: sync it, tweak in napari, save.
async function updateSelected() {
  const sel = frames.value.find(f => f.id === selectedId.value)
  if (!sel || !projectUid.value || !openImageUid.value || updating.value) return
  updating.value = true
  try {
    const res = await fetch('/api/napari/screenshot', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value }),
    })
    if (!res.ok) { log.error(`Update failed: ${(await res.json().catch(() => ({}))).error ?? res.status}`, { source: 'napari' }); return }
    const j = (await res.json()) as { assetId?: string; viewState?: Record<string, unknown> }
    const oldAsset = sel.assetId
    sel.snapshot = j.viewState
    sel.original = JSON.parse(JSON.stringify(j.viewState ?? {}))   // new baseline (no longer "edited")
    sel.assetId = j.assetId
    if (oldAsset && oldAsset !== j.assetId && !anim.snapshots.some(o => o.assetId === oldAsset)) {
      fetch('/api/board-assets/delete', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: projectUid.value, assetId: oldAsset }),
      }).catch(() => {})
    }
  } catch (e) {
    log.error(`Update failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { updating.value = false }
}

function imageName(uid: string | null | undefined): string {
  if (!uid) return '(unknown image)'
  for (const set of projectStore.sets) {
    const img = set.images.find(i => i.uid === uid)
    if (img) return img.name
  }
  return uid
}
function assetUrl(s: AnimSnapshot): string {
  return s.assetId ? `/api/board-assets?projectUid=${projectUid.value}&assetId=${s.assetId}` : ''
}

const openImage = computed(() => {
  for (const set of projectStore.sets) {
    const img = set.images.find(i => i.uid === openImageUid.value)
    if (img) return img
  }
  return null
})

// a timeline is per-image: the keyframes of whichever image is open in napari, in list order
const frames = computed(() => anim.snapshots.filter(s => s.imageUid === openImageUid.value))

// where a snapshot sits in the timelapse — its T index + wall-clock (h/min) via the image's
// TimeIncrement, so you can tell which frame it's from. T is the first dims axis for these stacks.
function keyframeTime(s: AnimSnapshot): string {
  const step = (s.snapshot?.dims as { current_step?: number[] } | undefined)?.current_step
  const t = Array.isArray(step) ? step[0] : undefined
  if (t === undefined || t === null) return ''
  const e = elapsedLabel(t, openImage.value?.timeIncrement, openImage.value?.timeIncrementUnit)
  return openImage.value?.timeIncrement ? `t${t} · ${e}` : `t${t}`   // shared formatter (utils/stillOverlay)
}

type Layers = Record<string, { visible?: boolean; colormap?: string }>
const layersOf = (s: AnimSnapshot) => (s.snapshot?.layers ?? {}) as Layers
// overlays (populations / tracks / labels) are napari layers whose name is parenthesised
// ("(popType) (vn) …", "(vn) Labels"); image channels are the plain-named layers.
const isOverlay = (name: string) => name.startsWith('(')

// row sets = the union of layer names across this image's keyframes, split into channels vs overlays
function unionRows(pred: (n: string) => boolean): string[] {
  const set = new Set<string>()
  for (const f of frames.value) for (const n of Object.keys(layersOf(f))) if (pred(n)) set.add(n)
  return [...set]
}
const channelRows = computed(() => unionRows(n => !isOverlay(n)))
const popRows = computed(() => unionRows(isOverlay))

// cell state: is a layer visible in a keyframe? null = the layer isn't in that keyframe at all
function cellState(s: AnimSnapshot, name: string): boolean | null {
  const l = layersOf(s)[name]
  return l === undefined ? null : l.visible !== false
}
function toggleCell(s: AnimSnapshot, name: string) {
  const l = layersOf(s)[name]
  if (l) l.visible = l.visible === false   // flip; deep autosave persists the edited viewState
}
const cameraZoom = (s: AnimSnapshot) => {
  const z = (s.snapshot?.camera as { zoom?: number } | undefined)?.zoom
  return typeof z === 'number' ? z.toFixed(1) : '—'
}
// the "on" colour of a cell = the layer's real colour (channel colormap tint), else the accent — so a
// green channel reads green, not a generic dot.
const layerColour = (s: AnimSnapshot, name: string) =>
  napariColormapHex(layersOf(s)[name]?.colormap) ?? '#a78bfa'

// capture the CURRENT napari view as a new keyframe (a base "look")
async function capture() {
  if (!projectUid.value || !openImageUid.value || capturing.value) return
  capturing.value = true
  try {
    const res = await fetch('/api/napari/screenshot', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value }),
    })
    if (!res.ok) { log.error(`Capture failed: ${(await res.json().catch(() => ({}))).error ?? res.status}`, { source: 'napari' }); return }
    const j = (await res.json()) as { assetId?: string; viewState?: Record<string, unknown>; imageUid?: string }
    const uid = j.imageUid ?? openImageUid.value
    anim.add({ id: crypto.randomUUID(), assetId: j.assetId, snapshot: j.viewState,
               original: JSON.parse(JSON.stringify(j.viewState ?? {})),   // reset target
               imageUid: uid, imageName: imageName(uid), duration: 1 })
  } catch (e) {
    log.error(`Capture failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { capturing.value = false }
}

// add a keyframe by duplicating the last one (a copy to vary via the rows — no re-capture needed)
function addKeyframe() {
  const last = frames.value[frames.value.length - 1]
  if (!last) { capture(); return }   // nothing yet → capture a base
  const copy = JSON.parse(JSON.stringify(last.snapshot ?? {}))
  anim.add({
    id: crypto.randomUUID(), assetId: last.assetId, imageUid: last.imageUid, imageName: last.imageName,
    duration: last.duration ?? 1,
    snapshot: copy,
    original: JSON.parse(JSON.stringify(copy)),   // baseline = what it starts as; reset returns here
  })
}

// a keyframe is "edited" once its working viewState diverges from the captured baseline
function isEdited(s: AnimSnapshot): boolean {
  return !!s.original && JSON.stringify(s.snapshot) !== JSON.stringify(s.original)
}
function resetKeyframe(s: AnimSnapshot) {
  if (s.original) s.snapshot = JSON.parse(JSON.stringify(s.original))
}

async function deleteKeyframe(s: AnimSnapshot) {
  // only drop the sidecar PNG if no OTHER keyframe still references it (add-keyframe shares the asset)
  if (s.assetId && !anim.snapshots.some(o => o.id !== s.id && o.assetId === s.assetId)) {
    fetch('/api/board-assets/delete', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, assetId: s.assetId }),
    }).catch(() => {})
  }
  anim.remove(s.id)
}

const canRender = computed(() => !!openImageUid.value && frames.value.length >= 2 && !rendering.value)
async function render() {
  if (!canRender.value) return
  rendering.value = true
  log.info('Rendering animation… (this can take a moment)', { source: 'napari' })
  try {
    const keyframes = frames.value.map(f => ({
      viewState: f.snapshot,
      steps: Math.max(1, Math.round((f.duration ?? 1) * anim.fps)),
    }))
    // Title card (Phase H4): describe everything shown "at some point" across the animation — build from
    // a UNION of all keyframes' views (channels + overlays merged), via the SHARED buildTitleCard. It
    // includes the Channels section itself (from the union), since the recorder can't reconstruct the
    // union from one live view.
    let titleCard: TitleCardPayload | undefined
    if (anim.titleCard.enabled && frames.value.length) {
      const setUid   = projectStore.setUidOfImage(openImageUid.value ?? '') ?? ''
      const colourBy = setUid ? settings.getColourBy(setUid) : ''
      const overrides = (setUid && colourBy) ? settings.getColourOverrides(setUid, colourBy) : {}
      const union = unionViewSnapshot(frames.value.map(f => f.snapshot as { layers?: Record<string, unknown> } | undefined))
      titleCard = await buildTitleCard(projectUid.value, openImageUid.value ?? '', union, openImage.value,
        { note: anim.titleCard.note, durationSec: anim.titleCard.durationSec, colourBy, colourOverrides: overrides, includeChannels: true })
    }
    // Over the task rail (`movie:record` with keyframes), like the viewer's Record and the batch: the
    // render shows up in the task list with a progress bar and a Cancel instead of blocking here.
    const t = tasks.add({
      module: 'animation', label: `Render ${openImage.value?.name ?? 'animation'}`,
      imageUid: openImageUid.value ?? '', imageName: openImage.value?.name ?? '', status: 'queued',
      taskName: 'movie.animation', funName: 'movie.animation', params: {}, projectUid: projectUid.value,
    })
    ws.send({
      type: 'movie:record', taskId: t.id, projectUid: projectUid.value, imageUid: openImageUid.value,
      keyframes, fps: anim.fps, suffix: anim.suffix, titleCard, apiUrl: window.location.origin,
      ...movieSizeParams(anim.sizeX, anim.sizeY),
    })
  } catch (e) {
    log.error(`Render failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { rendering.value = false }
}
</script>

<template>
  <ModulePage title="Animation" layout="scroll">
    <template #controls>
      <!-- Movie options behind the gear, the same component the viewer's recorder uses -->
      <MovieOptionsButton>
        <MovieOutputControls v-model:fps="anim.fps" v-model:sizeX="anim.sizeX" v-model:sizeY="anim.sizeY"
                             v-model:suffix="anim.suffix" :canvas-x="canvasSizeX" :canvas-y="canvasSizeY" />
        <TitleCardControls v-model="anim.titleCard" />
      </MovieOptionsButton>
      <button class="cc-btn cc-btn-primary" :disabled="!canRender" @click="render"
              v-tooltip.bottom="canRender ? 'Render the timeline to an mp4'
                : 'Need ≥2 keyframes for this image, open in napari'">
        <i :class="['pi', rendering ? 'pi-spin pi-spinner' : 'pi-play']" /> Render
      </button>
    </template>

    <p v-if="!hasProject" class="cc-empty">Open a project to build an animation.</p>
    <p v-else-if="!openImageUid" class="cc-empty">Open an image in napari to start capturing keyframes.</p>

    <template v-else>
      <div class="anim-toolbar">
        <span class="anim-img">{{ imageName(openImageUid) }}</span>
        <button class="cc-btn cc-btn-ghost" :disabled="capturing" @click="capture"
                v-tooltip.bottom="'Capture the current napari view as a keyframe (a base look)'">
          <i :class="['pi', capturing ? 'pi-spin pi-spinner' : 'pi-camera']" /> Capture view
        </button>
        <button class="cc-btn cc-btn-ghost" :disabled="!frames.length" @click="addKeyframe"
                v-tooltip.bottom="'Duplicate the last keyframe to vary it via the rows'">
          <i class="pi pi-plus" /> Add keyframe
        </button>
        <button class="cc-btn cc-btn-ghost" :disabled="!selectedId || updating" @click="updateSelected"
                v-tooltip.bottom="'Replace the selected keyframe with the current napari view (re-capture)'">
          <i :class="['pi', updating ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Update selected
        </button>
        <CcToggle class="anim-sync" label="Sync napari"
          :model-value="syncNapari" @update:model-value="onToggleSync($event)"
          v-tooltip.bottom="'Show the selected keyframe in napari when you click it (so you can see / tweak it)'" />
      </div>

      <p v-if="!frames.length" class="cc-empty">No keyframes yet — set up the view in napari and
        <strong>Capture view</strong>.</p>

      <div v-else class="anim-timeline">
        <table class="tl">
          <thead>
            <tr>
              <th class="tl-rowhead tl-corner"></th>
              <th v-for="(f, i) in frames" :key="f.id" class="tl-col" :class="{ dragover: dragId && dragId !== f.id }"
                  @dragover.prevent @drop="onDrop(f.id)">
                <div class="tl-thumb" :class="{ selected: selectedId === f.id, dragging: dragId === f.id }"
                     draggable="true" @dragstart="dragId = f.id" @dragend="dragId = null"
                     @click="selectKeyframe(f)" v-tooltip.bottom="'Click to select (drag to reorder)'">
                  <img v-if="f.assetId" :src="assetUrl(f)" :alt="`keyframe ${i+1}`" />
                  <span v-if="isEdited(f)" class="tl-badge" v-tooltip.bottom="'Edited from the captured view — use ↺ to reset'">edited</span>
                </div>
                <div v-if="keyframeTime(f)" class="tl-time cc-readout cc-fs-2xs">{{ keyframeTime(f) }}</div>
                <div class="tl-colctl">
                  <button class="tl-ico cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" :disabled="i === 0" @click="anim.move(f.id, -1)" v-tooltip.bottom="'Move earlier'"><i class="pi pi-chevron-left" /></button>
                  <span class="tl-kf cc-muted cc-fs-xs">{{ i + 1 }}</span>
                  <button class="tl-ico cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" :disabled="i === frames.length - 1" @click="anim.move(f.id, 1)" v-tooltip.bottom="'Move later'"><i class="pi pi-chevron-right" /></button>
                  <button class="tl-ico cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" :disabled="!isEdited(f)" @click="resetKeyframe(f)" v-tooltip.bottom="'Reset to the captured view'"><i class="pi pi-refresh" /></button>
                  <ConfirmDeleteButton title="Delete keyframe" armed-title="Click again to delete" @confirm="deleteKeyframe(f)" />
                </div>
                <div class="tl-dur" v-tooltip.bottom="'Seconds this keyframe tweens from the previous'">
                  <input type="range" min="0.1" max="10" step="0.1" :value="f.duration ?? 1" class="tl-durrange"
                         @input="f.duration = Number(($event.target as HTMLInputElement).value)" />
                  <span class="tl-durval cc-readout cc-fs-2xs">{{ (f.duration ?? 1).toFixed(1) }}s</span>
                </div>
              </th>
            </tr>
          </thead>
          <tbody>
            <tr class="tl-group"><td class="tl-rowhead cc-eyebrow cc-fs-3xs">Channels</td><td v-for="f in frames" :key="f.id" /></tr>
            <tr v-for="name in channelRows" :key="'c'+name" class="tl-row">
              <td class="tl-rowhead cc-fs-sm" :title="name">{{ name }}</td>
              <td v-for="f in frames" :key="f.id" class="tl-cell">
                <button v-if="cellState(f, name) !== null" class="tl-dot" :class="{ on: cellState(f, name) }"
                        :style="cellState(f, name) ? { background: layerColour(f, name), borderColor: layerColour(f, name) } : undefined"
                        @click="toggleCell(f, name)" v-tooltip.bottom="cellState(f, name) ? 'Shown — click to hide' : 'Hidden — click to show'" />
                <span v-else class="tl-absent">·</span>
              </td>
            </tr>

            <template v-if="popRows.length">
              <tr class="tl-group"><td class="tl-rowhead cc-eyebrow cc-fs-3xs">Populations &amp; overlays</td><td v-for="f in frames" :key="f.id" /></tr>
              <tr v-for="name in popRows" :key="'p'+name" class="tl-row">
                <td class="tl-rowhead cc-fs-sm" :title="name">{{ name }}</td>
                <td v-for="f in frames" :key="f.id" class="tl-cell">
                  <button v-if="cellState(f, name) !== null" class="tl-dot" :class="{ on: cellState(f, name) }"
                          :style="cellState(f, name) ? { background: layerColour(f, name), borderColor: layerColour(f, name) } : undefined"
                          @click="toggleCell(f, name)" v-tooltip.bottom="cellState(f, name) ? 'Shown — click to hide' : 'Hidden — click to show'" />
                  <span v-else class="tl-absent">·</span>
                </td>
              </tr>
            </template>

            <tr class="tl-group"><td class="tl-rowhead cc-eyebrow cc-fs-3xs">Camera</td><td v-for="f in frames" :key="f.id" /></tr>
            <tr class="tl-row">
              <td class="tl-rowhead cc-fs-sm">zoom</td>
              <td v-for="f in frames" :key="f.id" class="tl-cell tl-cam cc-readout cc-fs-xs">{{ cameraZoom(f) }}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </template>
  </ModulePage>
</template>

<style scoped>
/* (.anim-range/.anim-num were left behind when MovieOutputControls was extracted — nothing in the
   template referenced them.) */
.anim-toolbar { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.9rem; }
.anim-img { font-size: var(--cc-fs-sm); font-weight: 600; color: var(--cc-text); margin-right: 0.2rem; }
.anim-sync { display: inline-flex; align-items: center; gap: 0.3rem; font-size: var(--cc-fs-sm); color: var(--cc-text-dim); cursor: pointer; }

/* clean matrix (not a bordered table): sticky row labels, colour-coded toggle dots, rounded thumbs */
.anim-timeline { overflow-x: auto; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-lg);
  background: var(--cc-surface-1); padding: 0.6rem 0.7rem 0.8rem; }
.tl { border-collapse: separate; border-spacing: 0; }
/* No colour or size here: the group-header cells compose .cc-eyebrow, and a scoped class outranks a
   global utility (0,2,0 vs 0,1,0), so owning either property here made the utility a no-op. The
   colour was redundant regardless — nothing above the timeline dims, so these inherit --cc-text. */
.tl-rowhead { position: sticky; left: 0; background: var(--cc-surface-1); text-align: left;
  padding: 0.25rem 0.9rem 0.25rem 0.1rem; max-width: 11rem; overflow: hidden;
  text-overflow: ellipsis; white-space: nowrap; z-index: 1; }
.tl-corner { min-width: 7rem; }
.tl-col { padding: 0 0.35rem 0.4rem; vertical-align: top; text-align: center; }
.tl-thumb { position: relative; width: 96px; height: 96px; background: #000; border-radius: var(--cc-radius-lg);
  overflow: hidden; border: 1px solid var(--cc-border); transition: box-shadow 0.12s, border-color 0.12s; }
.tl-thumb img { width: 100%; height: 100%; object-fit: contain; }
.tl-thumb { cursor: grab; }
.tl-thumb.dragging { opacity: 0.4; }
.tl-col.dragover .tl-thumb { outline: 2px dashed var(--cc-selected); outline-offset: 2px; }
/* selected keyframe = the highlighted box → amber ring (--cc-selected), matching the plot panels'
   selected state. "edited" is a separate flag (the badge), not a ring. */
.tl-thumb.selected { border-color: var(--cc-selected); box-shadow: 0 0 0 2px color-mix(in srgb, var(--cc-selected) 55%, transparent); }
.tl-badge { position: absolute; top: 4px; right: 4px; font-size: var(--cc-fs-3xs); font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.04em; color: #1f1400; background: var(--cc-warn); padding: 1px 5px; border-radius: var(--cc-radius-pill); }
.tl-time { margin-top: 0.15rem; }
.tl-colctl { display: flex; align-items: center; justify-content: center; gap: 0.1rem; margin-top: 0.3rem; }
.tl-kf { min-width: 0.9rem; text-align: center; }
/* .tl-ico → cc-btn cc-btn-bare cc-btn-icon cc-btn-micro */
.tl-ico:hover:not(:disabled) { color: var(--cc-text); background: var(--cc-surface-2); }
.tl-ico:disabled { opacity: 0.3; cursor: default; }
.tl-dur { display: flex; align-items: center; justify-content: center; gap: 0.3rem; margin-top: 0.3rem; }
.tl-durrange { width: 68px; accent-color: var(--cc-accent); }
.tl-durval { min-width: 1.8rem; text-align: left; }
/* + .cc-eyebrow .cc-fs-3xs on the cell — only the spacing is the timeline's business */
.tl-group .tl-rowhead { padding-top: 0.7rem; padding-bottom: 0.2rem; }
.tl-row:hover .tl-cell, .tl-row:hover .tl-rowhead { background: rgba(255, 255, 255, 0.03); }
.tl-cell { text-align: center; padding: 0.22rem 0.35rem; }
.tl-dot { width: 15px; height: 15px; border-radius: var(--cc-radius-pill); border: 1.5px solid var(--cc-border);
  background: transparent; cursor: pointer; padding: 0; transition: transform 0.1s; }
.tl-dot:hover { transform: scale(1.18); }
.tl-dot.on { border-style: solid; }         /* on: filled with the layer colour (set inline) */
.tl-absent { color: var(--cc-text-dim); opacity: 0.35; }

/* buttons use the global .cc-btn utilities (style.css) */
</style>
