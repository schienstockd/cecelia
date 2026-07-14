<script setup lang="ts">
// Animation page — capture napari "view snapshots" and turn them into movies. Each snapshot is the
// current napari view (channels/contrast/colour-by/camera + T/Z), captured via the same screenshot
// path as the board strip (sidecar PNG + view state), shown with the shared <ViewLegend>. "Record
// movie" re-applies a snapshot to the open image and records the T-sweep (F1.1 record engine). MVP of
// the F2 animation page; batch across images is the follow-on. See docs/todo/ANIMATION_PLAN.md.
import { ref, computed } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useLogStore } from '../stores/log'
import { useAnimationStore, type AnimSnapshot } from '../stores/animation'
import { channelLegend, viewLegendSections } from '../utils/viewLegend'
import ViewLegend from '../components/ViewLegend.vue'
import ConfirmDeleteButton from '../components/ConfirmDeleteButton.vue'

const projectMeta = useProjectMetaStore()
const projectStore = useProjectStore()
const log = useLogStore()
const anim = useAnimationStore()

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const hasProject = computed(() => projectMeta.hasProject)
const openImageUid = computed(() => projectStore.napariImageUid)

const capturing = ref(false)
const recordingId = ref<string | null>(null)

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

function legendSections(s: AnimSnapshot) {
  const layers = (s.snapshot?.layers ?? {}) as Record<string, { colormap?: string; visible?: boolean }>
  return viewLegendSections({ channels: channelLegend(layers) })
}

// capture the CURRENT napari view (screenshot → sidecar PNG + view state) as a new snapshot
async function capture() {
  if (!projectUid.value || !openImageUid.value || capturing.value) return
  capturing.value = true
  try {
    const res = await fetch('/api/napari/screenshot', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value }),
    })
    if (!res.ok) {
      const j = await res.json().catch(() => ({}))
      log.error(`Capture failed: ${j?.error ?? res.status}`, { source: 'napari' }); return
    }
    const j = (await res.json()) as { assetId?: string; viewState?: Record<string, unknown>; imageUid?: string }
    anim.add({
      id: crypto.randomUUID(),
      assetId: j.assetId,
      snapshot: j.viewState,
      imageUid: j.imageUid ?? openImageUid.value,
      imageName: imageName(j.imageUid ?? openImageUid.value),
      title: `${imageName(j.imageUid ?? openImageUid.value)} view`,
    })
  } catch (e) {
    log.error(`Capture failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally {
    capturing.value = false
  }
}

// re-apply a snapshot to the open image, then record the T-sweep to an mp4. Only when the snapshot's
// image is the one currently open in napari (applying a view to a different image is meaningless).
const canRecord = (s: AnimSnapshot) => !!openImageUid.value && s.imageUid === openImageUid.value
async function record(s: AnimSnapshot) {
  if (!canRecord(s) || recordingId.value) return
  recordingId.value = s.id
  log.info(`Recording "${s.title ?? 'view'}"… (this can take a moment)`, { source: 'napari' })
  try {
    if (s.snapshot) {
      await fetch('/api/napari/apply-view-state', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ viewState: s.snapshot }),
      })
    }
    const res = await fetch('/api/napari/record-timelapse', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, imageUid: s.imageUid }),
    })
    const j = await res.json().catch(() => ({}))
    if (!res.ok) { log.error(`Record failed: ${j?.error ?? res.status}`, { source: 'napari' }); return }
    log.info(`Recorded ${j.frames ?? '?'} frames → ${j.path ?? 'movies/'}`, { source: 'napari' })
  } catch (e) {
    log.error(`Record failed: ${e instanceof Error ? e.message : String(e)}`, { source: 'napari' })
  } finally {
    recordingId.value = null
  }
}

// remove a snapshot + best-effort delete its sidecar PNG so it doesn't orphan in board-assets/
async function deleteSnapshot(s: AnimSnapshot) {
  if (s.assetId) {
    fetch('/api/board-assets/delete', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: projectUid.value, assetId: s.assetId }),
    }).catch(() => { /* best-effort */ })
  }
  anim.remove(s.id)
}
</script>

<template>
  <div class="anim-page">
    <header class="anim-head">
      <div>
        <h1>Animation</h1>
        <p class="anim-sub">Capture napari view snapshots (channels, populations, colour-by, camera) and
          record them as movies. Snapshots are saved with the project.</p>
      </div>
      <button class="btn-primary" :disabled="!openImageUid || capturing" @click="capture"
              v-tooltip.bottom="openImageUid ? 'Capture the current napari view as a snapshot'
                                             : 'Open an image in napari first'">
        <i :class="['pi', capturing ? 'pi-spin pi-spinner' : 'pi-camera']" />
        {{ capturing ? 'capturing…' : 'Capture current view' }}
      </button>
    </header>

    <p v-if="!hasProject" class="anim-empty">Open a project to capture view snapshots.</p>
    <p v-else-if="!anim.snapshots.length" class="anim-empty">
      No snapshots yet. Open an image in napari, set up the view, then <strong>Capture current view</strong>.
    </p>

    <div v-else class="anim-grid">
      <div v-for="s in anim.snapshots" :key="s.id" class="anim-card">
        <div class="anim-thumb">
          <img v-if="s.assetId" :src="assetUrl(s)" :alt="s.title" />
          <div v-else class="anim-noimg">no image</div>
          <ViewLegend v-if="legendSections(s).length" :sections="legendSections(s)" :swatch="9" class="anim-legend" />
        </div>
        <input class="anim-title" :value="s.title" @change="s.title = ($event.target as HTMLInputElement).value" />
        <div class="anim-meta">{{ s.imageName }}</div>
        <div class="anim-actions">
          <button class="btn-sm" :disabled="!canRecord(s) || !!recordingId" @click="record(s)"
                  v-tooltip.bottom="canRecord(s) ? 'Apply this view and record the timelapse → mp4'
                                                 : 'Open this snapshot\'s image in napari to record it'">
            <i :class="['pi', recordingId === s.id ? 'pi-spin pi-spinner' : 'pi-video']" />
            {{ recordingId === s.id ? 'recording…' : 'Record movie' }}
          </button>
          <ConfirmDeleteButton title="Delete snapshot" armed-title="Click again to delete"
                               @confirm="deleteSnapshot(s)" />
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.anim-page { padding: 1rem 1.25rem; height: 100%; overflow: auto; }
.anim-head { display: flex; align-items: flex-start; justify-content: space-between; gap: 1rem; margin-bottom: 1rem; }
.anim-head h1 { font-size: 1.1rem; margin: 0 0 0.2rem; }
.anim-sub { font-size: 0.8rem; color: var(--cc-text-dim); max-width: 46rem; margin: 0; }
.anim-empty { font-size: 0.85rem; color: var(--cc-text-dim); margin-top: 2rem; }
.anim-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 0.9rem; }
.anim-card { display: flex; flex-direction: column; gap: 0.35rem; border: 1px solid var(--cc-border);
  border-radius: 0.5rem; background: var(--cc-surface-1); padding: 0.5rem; }
.anim-thumb { position: relative; aspect-ratio: 1; background: #000; border-radius: 0.3rem; overflow: hidden; }
.anim-thumb img { width: 100%; height: 100%; object-fit: contain; }
.anim-noimg { display: flex; align-items: center; justify-content: center; height: 100%; color: var(--cc-text-dim); font-size: 0.75rem; }
.anim-legend { position: absolute; bottom: 5px; left: 5px; padding: 3px 5px; border-radius: 3px;
  background: rgba(0,0,0,0.45); color: #fff; font-size: 10px; font-weight: 600;
  text-shadow: 0 1px 2px rgba(0,0,0,0.85); pointer-events: none; }
.anim-title { width: 100%; font-size: 0.78rem; }
.anim-meta { font-size: 0.68rem; color: var(--cc-text-dim); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.anim-actions { display: flex; align-items: center; gap: 0.4rem; }
.btn-sm { display: inline-flex; align-items: center; gap: 0.3rem; flex: 1; justify-content: center;
  font-size: 0.7rem; padding: 0.3rem 0.4rem; border: 1px solid var(--cc-border); border-radius: 0.3rem;
  background: var(--cc-surface-2); color: var(--cc-text); cursor: pointer; }
.btn-sm:hover:not(:disabled) { border-color: #7c3aed; }
.btn-sm:disabled { opacity: 0.55; cursor: default; }
.btn-primary { display: inline-flex; align-items: center; gap: 0.4rem; flex-shrink: 0;
  font-size: 0.78rem; padding: 0.45rem 0.7rem; border: 1px solid #7c3aed; border-radius: 0.35rem;
  background: #2d1b69; color: #ddd6fe; cursor: pointer; }
.btn-primary:hover:not(:disabled) { background: #3b2382; }
.btn-primary:disabled { opacity: 0.55; cursor: default; }
</style>
