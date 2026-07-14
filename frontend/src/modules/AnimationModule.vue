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

// a timeline is per-image: the keyframes of whichever image is open in napari, in list order
const frames = computed(() => anim.snapshots.filter(s => s.imageUid === openImageUid.value))

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
               imageUid: uid, imageName: imageName(uid), duration: 1 })
  } catch (e) {
    log.error(`Capture failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally { capturing.value = false }
}

// add a keyframe by duplicating the last one (a copy to vary via the rows — no re-capture needed)
function addKeyframe() {
  const last = frames.value[frames.value.length - 1]
  if (!last) { capture(); return }   // nothing yet → capture a base
  anim.add({
    id: crypto.randomUUID(), assetId: last.assetId, imageUid: last.imageUid, imageName: last.imageName,
    duration: last.duration ?? 1,
    snapshot: JSON.parse(JSON.stringify(last.snapshot ?? {})),   // deep copy of the viewState
  })
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
          fps <input type="number" min="1" max="60" step="1" :value="anim.fps"
                     @change="anim.fps = Math.min(60, Math.max(1, Number(($event.target as HTMLInputElement).value) || 15))" />
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
      </div>

      <p v-if="!frames.length" class="anim-empty">No keyframes yet — set up the view in napari and
        <strong>Capture view</strong>.</p>

      <div v-else class="anim-timeline">
        <table class="tl">
          <thead>
            <tr>
              <th class="tl-rowhead"></th>
              <th v-for="(f, i) in frames" :key="f.id" class="tl-col">
                <div class="tl-thumb"><img v-if="f.assetId" :src="assetUrl(f)" :alt="`keyframe ${i+1}`" /></div>
                <div class="tl-colctl">
                  <button class="tl-ico" :disabled="i === 0" @click="anim.move(f.id, -1)" v-tooltip.bottom="'Move earlier'"><i class="pi pi-chevron-left" /></button>
                  <span class="tl-kf">{{ i + 1 }}</span>
                  <button class="tl-ico" :disabled="i === frames.length - 1" @click="anim.move(f.id, 1)" v-tooltip.bottom="'Move later'"><i class="pi pi-chevron-right" /></button>
                  <ConfirmDeleteButton title="Delete keyframe" armed-title="Click again to delete" @confirm="deleteKeyframe(f)" />
                </div>
                <label class="tl-dur" v-tooltip.bottom="'Seconds this keyframe tweens from the previous'">
                  <input type="number" min="0.1" step="0.1" :value="f.duration ?? 1"
                         @change="f.duration = Math.max(0.1, Number(($event.target as HTMLInputElement).value) || 1)" />s
                </label>
              </th>
            </tr>
          </thead>
          <tbody>
            <tr class="tl-group"><td :colspan="frames.length + 1">Channels</td></tr>
            <tr v-for="name in channelRows" :key="'c'+name">
              <td class="tl-rowhead" :title="name">{{ name }}</td>
              <td v-for="f in frames" :key="f.id" class="tl-cell">
                <button v-if="cellState(f, name) !== null" class="tl-dot" :class="{ on: cellState(f, name) }"
                        @click="toggleCell(f, name)" v-tooltip.bottom="cellState(f, name) ? 'Shown — click to hide' : 'Hidden — click to show'" />
                <span v-else class="tl-absent">·</span>
              </td>
            </tr>

            <template v-if="popRows.length">
              <tr class="tl-group"><td :colspan="frames.length + 1">Populations &amp; overlays</td></tr>
              <tr v-for="name in popRows" :key="'p'+name">
                <td class="tl-rowhead" :title="name">{{ name }}</td>
                <td v-for="f in frames" :key="f.id" class="tl-cell">
                  <button v-if="cellState(f, name) !== null" class="tl-dot" :class="{ on: cellState(f, name) }"
                          @click="toggleCell(f, name)" v-tooltip.bottom="cellState(f, name) ? 'Shown — click to hide' : 'Hidden — click to show'" />
                  <span v-else class="tl-absent">·</span>
                </td>
              </tr>
            </template>

            <tr class="tl-group"><td :colspan="frames.length + 1">Camera</td></tr>
            <tr>
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
.anim-head-ctl { display: flex; align-items: center; gap: 0.6rem; flex-shrink: 0; }
.anim-fps { font-size: 0.72rem; color: var(--cc-text-dim); display: inline-flex; align-items: center; gap: 0.3rem; }
.anim-fps input { width: 3rem; font-size: 0.72rem; }
.anim-empty { font-size: 0.85rem; color: var(--cc-text-dim); margin-top: 1.5rem; }
.anim-toolbar { display: flex; align-items: center; gap: 0.6rem; margin-bottom: 0.7rem; }
.anim-img { font-size: 0.78rem; font-weight: 600; color: var(--cc-text); }

.anim-timeline { overflow-x: auto; }
.tl { border-collapse: collapse; }
.tl-rowhead { position: sticky; left: 0; background: var(--cc-bg); text-align: left; font-size: 0.68rem;
  color: var(--cc-text-dim); padding: 0.2rem 0.6rem 0.2rem 0.2rem; max-width: 9rem; overflow: hidden;
  text-overflow: ellipsis; white-space: nowrap; z-index: 1; }
.tl-col { padding: 0 0.15rem 0.3rem; vertical-align: top; }
.tl-thumb { width: 88px; height: 88px; background: #000; border-radius: 0.3rem; overflow: hidden; }
.tl-thumb img { width: 100%; height: 100%; object-fit: contain; }
.tl-colctl { display: flex; align-items: center; justify-content: center; gap: 0.15rem; margin-top: 0.15rem; }
.tl-kf { font-size: 0.66rem; color: var(--cc-text-dim); min-width: 1rem; text-align: center; }
.tl-ico { border: none; background: none; color: var(--cc-text-dim); cursor: pointer; padding: 0.1rem; font-size: 0.62rem; }
.tl-ico:hover:not(:disabled) { color: var(--cc-text); }
.tl-ico:disabled { opacity: 0.3; cursor: default; }
.tl-dur { display: flex; align-items: center; justify-content: center; gap: 0.15rem; font-size: 0.62rem; color: var(--cc-text-dim); margin-top: 0.1rem; }
.tl-dur input { width: 2.6rem; font-size: 0.66rem; }
.tl-group td { font-size: 0.6rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em;
  color: var(--cc-text-dim); padding: 0.5rem 0.2rem 0.15rem; border-bottom: 1px solid var(--cc-border); }
.tl-cell { text-align: center; padding: 0.18rem 0.15rem; }
.tl-dot { width: 14px; height: 14px; border-radius: 50%; border: 1px solid var(--cc-border);
  background: var(--cc-surface-2); cursor: pointer; padding: 0; }
.tl-dot.on { background: #7c3aed; border-color: #7c3aed; }
.tl-absent { color: var(--cc-text-dim); opacity: 0.4; }
.tl-cam { font-size: 0.66rem; color: var(--cc-text-dim); font-variant-numeric: tabular-nums; }

.btn-sm { display: inline-flex; align-items: center; gap: 0.3rem; font-size: 0.72rem; padding: 0.35rem 0.6rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-2); color: var(--cc-text); cursor: pointer; }
.btn-sm:hover:not(:disabled) { border-color: #7c3aed; }
.btn-sm:disabled { opacity: 0.55; cursor: default; }
.btn-primary { display: inline-flex; align-items: center; gap: 0.4rem; font-size: 0.78rem; padding: 0.4rem 0.8rem;
  border: 1px solid #7c3aed; border-radius: 0.35rem; background: #2d1b69; color: #ddd6fe; cursor: pointer; }
.btn-primary:hover:not(:disabled) { background: #3b2382; }
.btn-primary:disabled { opacity: 0.55; cursor: default; }
</style>
