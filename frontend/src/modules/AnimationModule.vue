<script setup lang="ts">
// Animation page — a "super-simple OpenShot": a per-image TIMELINE of view snapshots (keyframes) with a
// row/track matrix. Columns = keyframes (captured napari views + duration); rows = channels /
// populations / camera, all INFERRED from each keyframe's viewState (layers by type + camera). Capture
// establishes a base "look" (contrast/colormap/framing); add-keyframe copies it; a cell toggle overrides
// that keyframe's layer.visible. Render interpolates between keyframes → mp4 (POST record-animation).
// The data model is just an ordered list of viewStates. See docs/todo/ANIMATION_PLAN.md (F2).
import { ref, computed } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import { useAnimationStore, type AnimSnapshot } from '../stores/animation'
import { napariColormapHex } from '../utils/napariColormap'
import { elapsedLabel } from '../utils/stillOverlay'
import ConfirmDeleteButton from '../components/ConfirmDeleteButton.vue'

const projectMeta = useProjectMetaStore()
const projectStore = useProjectStore()
const log = useLogStore()
const anim = useAnimationStore()

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
  try {
    await fetch('/api/napari/apply-view-state', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ viewState: s.snapshot }),
    })
  } catch { /* napari not running */ }
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
    const res = await fetch('/api/napari/record-animation', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, imageUid: openImageUid.value, keyframes, fps: anim.fps }),
    })
    const j = await res.json().catch(() => ({}))
    if (!res.ok) { log.error(`Render failed: ${j?.error ?? res.status}`, { source: 'napari' }); return }
    log.info(`Rendered ${j.frames ?? '?'} frames → ${j.path ?? 'movies/'}`, { source: 'napari' })
  } catch (e) {
    log.error(`Render failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { rendering.value = false }
}
</script>

<template>
  <div class="anim-page">
    <header class="anim-head">
      <div>
        <h1>Animation</h1>
        <p class="anim-sub">A timeline of napari view snapshots. Capture a base look, add keyframes, toggle
          channels/populations per keyframe, then render — the movie interpolates between keyframes.</p>
      </div>
      <div class="anim-head-ctl">
        <label class="anim-fps" v-tooltip.bottom="'Output frames per second'">
          fps <input type="range" min="1" max="40" step="1" v-model.number="anim.fps" class="anim-range" />
          <span class="anim-num">{{ anim.fps }}</span>
        </label>
        <button class="btn-primary" :disabled="!canRender" @click="render"
                v-tooltip.bottom="canRender ? 'Render the timeline to an mp4'
                  : 'Need ≥2 keyframes for this image, open in napari'">
          <i :class="['pi', rendering ? 'pi-spin pi-spinner' : 'pi-play']" /> Render
        </button>
      </div>
    </header>

    <p v-if="!hasProject" class="anim-empty">Open a project to build an animation.</p>
    <p v-else-if="!openImageUid" class="anim-empty">Open an image in napari to start capturing keyframes.</p>

    <template v-else>
      <div class="anim-toolbar">
        <span class="anim-img">{{ imageName(openImageUid) }}</span>
        <button class="btn-sm" :disabled="capturing" @click="capture"
                v-tooltip.bottom="'Capture the current napari view as a keyframe (a base look)'">
          <i :class="['pi', capturing ? 'pi-spin pi-spinner' : 'pi-camera']" /> Capture view
        </button>
        <button class="btn-sm" :disabled="!frames.length" @click="addKeyframe"
                v-tooltip.bottom="'Duplicate the last keyframe to vary it via the rows'">
          <i class="pi pi-plus" /> Add keyframe
        </button>
        <button class="btn-sm" :disabled="!selectedId || updating" @click="updateSelected"
                v-tooltip.bottom="'Replace the selected keyframe with the current napari view (re-capture)'">
          <i :class="['pi', updating ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Update selected
        </button>
        <label class="anim-sync" v-tooltip.bottom="'Show the selected keyframe in napari when you click it (so you can see / tweak it)'">
          <input type="checkbox" :checked="syncNapari" @change="onToggleSync(($event.target as HTMLInputElement).checked)" />
          Sync napari
        </label>
      </div>

      <p v-if="!frames.length" class="anim-empty">No keyframes yet — set up the view in napari and
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
                <div v-if="keyframeTime(f)" class="tl-time">{{ keyframeTime(f) }}</div>
                <div class="tl-colctl">
                  <button class="tl-ico" :disabled="i === 0" @click="anim.move(f.id, -1)" v-tooltip.bottom="'Move earlier'"><i class="pi pi-chevron-left" /></button>
                  <span class="tl-kf">{{ i + 1 }}</span>
                  <button class="tl-ico" :disabled="i === frames.length - 1" @click="anim.move(f.id, 1)" v-tooltip.bottom="'Move later'"><i class="pi pi-chevron-right" /></button>
                  <button class="tl-ico" :disabled="!isEdited(f)" @click="resetKeyframe(f)" v-tooltip.bottom="'Reset to the captured view'"><i class="pi pi-refresh" /></button>
                  <ConfirmDeleteButton title="Delete keyframe" armed-title="Click again to delete" @confirm="deleteKeyframe(f)" />
                </div>
                <div class="tl-dur" v-tooltip.bottom="'Seconds this keyframe tweens from the previous'">
                  <input type="range" min="0.1" max="10" step="0.1" :value="f.duration ?? 1" class="tl-durrange"
                         @input="f.duration = Number(($event.target as HTMLInputElement).value)" />
                  <span class="tl-durval">{{ (f.duration ?? 1).toFixed(1) }}s</span>
                </div>
              </th>
            </tr>
          </thead>
          <tbody>
            <tr class="tl-group"><td class="tl-rowhead">Channels</td><td v-for="f in frames" :key="f.id" /></tr>
            <tr v-for="name in channelRows" :key="'c'+name" class="tl-row">
              <td class="tl-rowhead" :title="name">{{ name }}</td>
              <td v-for="f in frames" :key="f.id" class="tl-cell">
                <button v-if="cellState(f, name) !== null" class="tl-dot" :class="{ on: cellState(f, name) }"
                        :style="cellState(f, name) ? { background: layerColour(f, name), borderColor: layerColour(f, name) } : undefined"
                        @click="toggleCell(f, name)" v-tooltip.bottom="cellState(f, name) ? 'Shown — click to hide' : 'Hidden — click to show'" />
                <span v-else class="tl-absent">·</span>
              </td>
            </tr>

            <template v-if="popRows.length">
              <tr class="tl-group"><td class="tl-rowhead">Populations &amp; overlays</td><td v-for="f in frames" :key="f.id" /></tr>
              <tr v-for="name in popRows" :key="'p'+name" class="tl-row">
                <td class="tl-rowhead" :title="name">{{ name }}</td>
                <td v-for="f in frames" :key="f.id" class="tl-cell">
                  <button v-if="cellState(f, name) !== null" class="tl-dot" :class="{ on: cellState(f, name) }"
                          :style="cellState(f, name) ? { background: layerColour(f, name), borderColor: layerColour(f, name) } : undefined"
                          @click="toggleCell(f, name)" v-tooltip.bottom="cellState(f, name) ? 'Shown — click to hide' : 'Hidden — click to show'" />
                  <span v-else class="tl-absent">·</span>
                </td>
              </tr>
            </template>

            <tr class="tl-group"><td class="tl-rowhead">Camera</td><td v-for="f in frames" :key="f.id" /></tr>
            <tr class="tl-row">
              <td class="tl-rowhead">zoom</td>
              <td v-for="f in frames" :key="f.id" class="tl-cell tl-cam">{{ cameraZoom(f) }}</td>
            </tr>
          </tbody>
        </table>
      </div>
    </template>
  </div>
</template>

<style scoped>
.anim-page { padding: 1rem 1.25rem; height: 100%; overflow: auto; }
.anim-head { display: flex; align-items: flex-start; justify-content: space-between; gap: 1rem; margin-bottom: 0.8rem; }
.anim-head h1 { font-size: 1.1rem; margin: 0 0 0.2rem; }
.anim-sub { font-size: 0.8rem; color: var(--cc-text-dim); max-width: 46rem; margin: 0; }
.anim-head-ctl { display: flex; align-items: center; gap: 0.9rem; flex-shrink: 0; }
.anim-fps { font-size: 0.72rem; color: var(--cc-text-dim); display: inline-flex; align-items: center; gap: 0.4rem; }
.anim-range { width: 5rem; accent-color: var(--cc-accent); }
.anim-num { font-size: 0.72rem; color: var(--cc-text); font-variant-numeric: tabular-nums; min-width: 1.2rem; }
.anim-empty { font-size: 0.85rem; color: var(--cc-text-dim); margin-top: 1.5rem; }
.anim-toolbar { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.9rem; }
.anim-img { font-size: 0.78rem; font-weight: 600; color: var(--cc-text); margin-right: 0.2rem; }
.anim-sync { display: inline-flex; align-items: center; gap: 0.3rem; font-size: 0.72rem; color: var(--cc-text-dim); cursor: pointer; }

/* clean matrix (not a bordered table): sticky row labels, colour-coded toggle dots, rounded thumbs */
.anim-timeline { overflow-x: auto; border: 1px solid var(--cc-border); border-radius: 0.6rem;
  background: var(--cc-surface-1); padding: 0.6rem 0.7rem 0.8rem; }
.tl { border-collapse: separate; border-spacing: 0; }
.tl-rowhead { position: sticky; left: 0; background: var(--cc-surface-1); text-align: left; font-size: 0.72rem;
  color: var(--cc-text); padding: 0.25rem 0.9rem 0.25rem 0.1rem; max-width: 11rem; overflow: hidden;
  text-overflow: ellipsis; white-space: nowrap; z-index: 1; }
.tl-corner { min-width: 7rem; }
.tl-col { padding: 0 0.35rem 0.4rem; vertical-align: top; text-align: center; }
.tl-thumb { position: relative; width: 96px; height: 96px; background: #000; border-radius: 0.5rem;
  overflow: hidden; border: 1px solid var(--cc-border); transition: box-shadow 0.12s, border-color 0.12s; }
.tl-thumb img { width: 100%; height: 100%; object-fit: contain; }
.tl-thumb { cursor: grab; }
.tl-thumb.dragging { opacity: 0.4; }
.tl-col.dragover .tl-thumb { outline: 2px dashed var(--cc-selected); outline-offset: 2px; }
/* selected keyframe = the highlighted box → amber ring (--cc-selected), matching the plot panels'
   selected state. "edited" is a separate flag (the badge), not a ring. */
.tl-thumb.selected { border-color: var(--cc-selected); box-shadow: 0 0 0 2px color-mix(in srgb, var(--cc-selected) 55%, transparent); }
.tl-badge { position: absolute; top: 4px; right: 4px; font-size: 0.55rem; font-weight: 700; text-transform: uppercase;
  letter-spacing: 0.04em; color: #1f1400; background: var(--cc-warn); padding: 1px 5px; border-radius: 999px; }
.tl-time { font-size: 0.6rem; color: var(--cc-text-dim); text-align: center; margin-top: 0.15rem; font-variant-numeric: tabular-nums; }
.tl-colctl { display: flex; align-items: center; justify-content: center; gap: 0.1rem; margin-top: 0.3rem; }
.tl-kf { font-size: 0.66rem; color: var(--cc-text-dim); min-width: 0.9rem; text-align: center; }
.tl-ico { display: inline-flex; border: none; background: none; color: var(--cc-text-dim); cursor: pointer;
  padding: 0.15rem; font-size: 0.62rem; border-radius: 0.25rem; }
.tl-ico:hover:not(:disabled) { color: var(--cc-text); background: var(--cc-surface-2); }
.tl-ico:disabled { opacity: 0.3; cursor: default; }
.tl-dur { display: flex; align-items: center; justify-content: center; gap: 0.3rem; margin-top: 0.3rem; }
.tl-durrange { width: 68px; accent-color: var(--cc-accent); }
.tl-durval { font-size: 0.62rem; color: var(--cc-text-dim); font-variant-numeric: tabular-nums; min-width: 1.8rem; text-align: left; }
.tl-group .tl-rowhead { font-size: 0.58rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.06em;
  color: var(--cc-text-dim); padding-top: 0.7rem; padding-bottom: 0.2rem; }
.tl-row:hover .tl-cell, .tl-row:hover .tl-rowhead { background: rgba(255, 255, 255, 0.03); }
.tl-cell { text-align: center; padding: 0.22rem 0.35rem; }
.tl-dot { width: 15px; height: 15px; border-radius: 50%; border: 1.5px solid var(--cc-border);
  background: transparent; cursor: pointer; padding: 0; transition: transform 0.1s; }
.tl-dot:hover { transform: scale(1.18); }
.tl-dot.on { border-style: solid; }         /* on: filled with the layer colour (set inline) */
.tl-absent { color: var(--cc-text-dim); opacity: 0.35; }
.tl-cam { font-size: 0.68rem; color: var(--cc-text-dim); font-variant-numeric: tabular-nums; }

.btn-sm { display: inline-flex; align-items: center; gap: 0.3rem; font-size: 0.72rem; padding: 0.35rem 0.6rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-2); color: var(--cc-text); cursor: pointer; }
.btn-sm:hover:not(:disabled) { border-color: var(--cc-accent); }
.btn-sm:disabled { opacity: 0.55; cursor: default; }
.btn-primary { display: inline-flex; align-items: center; gap: 0.4rem; font-size: 0.78rem; padding: 0.4rem 0.8rem;
  border: 1px solid var(--cc-accent); border-radius: 0.35rem; background: var(--cc-accent); color: #fff; cursor: pointer; }
.btn-primary:hover:not(:disabled) { filter: brightness(1.1); }
.btn-primary:disabled { opacity: 0.55; cursor: default; }
</style>
