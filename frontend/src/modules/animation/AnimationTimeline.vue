<!--
  The animation timeline — a row/track matrix over one image's keyframes. Columns = keyframes (the
  captured viewer view + its duration); rows = channels / populations / camera, all INFERRED from
  each keyframe's viewState rather than configured (utils/animationTimeline.ts). A cell toggle
  overrides that keyframe's layer.visible; the render interpolates between the columns.

  Lives in ModuleLayout's #plots slot, the same consistent collapsible canvas every module page hosts
  its plots in — the controls that ACT on this (capture, render, output) are in the side panel next to
  it (AnimationPanel.vue).
-->
<script setup lang="ts">
import { computed, ref } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useProjectStore } from '../../stores/project'
import { useSettingsStore } from '../../stores/settings'
import { useAnimationStore, type AnimSnapshot } from '../../stores/animation'
import { useViewerStore } from '../../stores/viewer'
import type { ViewerViewState } from '../../utils/viewer/viewState'
import { viewerColormapHex } from '../../utils/viewerColormap'
import { framesFor, layersOf, channelRows, popRows, cellState, cellToggle, cameraZoom, isEdited,
         keyframeTime, type Layers } from '../../utils/animationTimeline'
import { useColumnResize } from '../../composables/useColumnResize'
import ConfirmDeleteButton from '../../components/ConfirmDeleteButton.vue'

const props = defineProps<{ imageUid: string }>()

const projectMeta = useProjectMetaStore()
const project = useProjectStore()
const settings = useSettingsStore()
const anim = useAnimationStore()
const viewer = useViewerStore()

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const image = computed(() => (props.imageUid ? project.imageByUid(props.imageUid) : null))
const frames = computed(() => framesFor(anim.snapshots, props.imageUid))
const chRows = computed(() => channelRows(frames.value))
const pRows = computed(() => popRows(frames.value))

const assetUrl = (s: AnimSnapshot) =>
  s.assetId ? `/api/board-assets?projectUid=${projectUid.value}&assetId=${s.assetId}` : ''
const frameTime = (s: AnimSnapshot) =>
  keyframeTime(s, image.value?.timeIncrement, image.value?.timeIncrementUnit)

// select a keyframe; with Sync on, push its saved view into the browser viewer so you SEE that
// snapshot (and can then tweak it there and Update). Writes to the store's `pendingViewState`
// bridge — ViewerWindow watches, applies through `applyViewStateToBrowser`. A closed viewer is a
// no-op by construction (nothing consumes the pending), not a failure.
function selectKeyframe(s: AnimSnapshot) {
  anim.selectedId = s.id
  if (settings.animationSyncViewer && s.snapshot) {
    viewer.setPendingViewState(s.snapshot as unknown as ViewerViewState)
  }
}

// ── drag-to-reorder ───────────────────────────────────────────────────────────
const dragId = ref<string | null>(null)
function onDrop(targetId: string) {
  if (dragId.value) anim.reorder(dragId.value, targetId)
  dragId.value = null
}

// the "on" colour of a cell = the layer's real colour (channel colormap tint), else the accent — so a
// green channel reads green, not a generic dot
const layerColour = (s: AnimSnapshot, name: string) =>
  viewerColormapHex(layersOf(s)[name]?.colormap) ?? '#a78bfa'
const cellStyle = (s: AnimSnapshot, name: string) =>
  cellState(s, name) ? { background: layerColour(s, name), borderColor: layerColour(s, name) } : undefined
// Three states, and the third is worth naming: the keyframe has no entry for this layer, so at render
// time it simply keeps whatever the previous keyframe left it as.
const cellTip = (s: AnimSnapshot, name: string) => {
  const st = cellState(s, name)
  return st === null ? 'Not set here — click to show it' : st ? 'Shown — click to hide' : 'Hidden — click to show'
}

// Flip this keyframe's layer — or ADD it, when the layer was captured later and this keyframe has no
// entry for it (`cellToggle` decides which and seeds the new entry). Writing into the keyframe's own
// viewState; the store's deep autosave persists it and the "edited" badge follows.
function toggleCell(s: AnimSnapshot, name: string) {
  const next = cellToggle(frames.value, s, name)
  if (!next) return
  if (!s.snapshot) s.snapshot = {}
  if (!s.snapshot.layers) s.snapshot.layers = {}
  ;(s.snapshot.layers as Layers)[name] = next as Layers[string]
}

// ── the row-label column ──────────────────────────────────────────────────────
// Drag-resizable and persisted, the same primitive the image and movie tables use. An overlay's
// layer name is long by construction — "(track) (memTom) Tracks /…" — and a fixed label column
// ellipsised exactly the part that says WHICH one (Dominik, 2026-08-10).
const LABEL_KEY = 'label'
const { widthOf, onColumnResizeStart, resetWidths } = useColumnResize({
  defaultWidth: () => 190, min: 90, storageKey: 'cc.anim.labelw',
})

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
  if (anim.selectedId === s.id) anim.selectedId = null
  anim.remove(s.id)
}
</script>

<template>
  <p v-if="!imageUid" class="cc-empty">Select an image to see its timeline.</p>
  <p v-else-if="!frames.length" class="cc-empty">No keyframes yet — set up the view in the viewer
    and <strong>Capture view</strong>.</p>

  <div v-else class="anim-timeline" data-guide="animation.timeline">
    <table class="tl" :style="{ '--tl-label-w': widthOf(LABEL_KEY) }">
      <thead>
        <tr>
          <th class="tl-rowhead tl-corner">
            <!-- Dim until hovered — a rescue after a drag left the column too narrow, not something to
                 reach for. Same idiom as SelectionTable's. -->
            <button class="tl-reset-w cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="resetWidths"
                    v-tooltip.bottom="'Reset the label width'">
              <i class="pi pi-arrows-h" />
            </button>
            <div class="tl-col-resize" @mousedown.stop="onColumnResizeStart(LABEL_KEY, $event)"
                 v-tooltip.bottom="'Drag to resize the labels'" />
          </th>
          <th v-for="(f, i) in frames" :key="f.id" class="tl-col" :class="{ dragover: dragId && dragId !== f.id }"
              @dragover.prevent @drop="onDrop(f.id)">
            <div class="tl-thumb" :class="{ selected: anim.selectedId === f.id, dragging: dragId === f.id }"
                 draggable="true" @dragstart="dragId = f.id" @dragend="dragId = null"
                 @click="selectKeyframe(f)">
              <!-- tip on the thumbnail itself, not the cell: the "edited" badge inside carries its
                   own, and a cell tip fired on top of it (docs/UI.md → nested tooltips) -->
              <img v-if="f.assetId" :src="assetUrl(f)" :alt="`keyframe ${i+1}`"
                   v-tooltip.bottom="'Click to select (drag to reorder)'" />
              <span v-if="isEdited(f)" class="tl-badge" v-tooltip.bottom="'Edited from the captured view — use ↺ to reset'">edited</span>
            </div>
            <div v-if="frameTime(f)" class="tl-time cc-readout cc-fs-2xs">{{ frameTime(f) }}</div>
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
        <tr v-for="name in chRows" :key="'c'+name" class="tl-row">
          <td class="tl-rowhead cc-fs-sm" :title="name">{{ name }}</td>
          <td v-for="f in frames" :key="f.id" class="tl-cell">
            <!-- EVERY cell is a toggle, including one this keyframe has no entry for — see cellToggle -->
            <button class="tl-dot" :class="{ on: cellState(f, name), unset: cellState(f, name) === null }"
                    :style="cellStyle(f, name)" @click="toggleCell(f, name)"
                    v-tooltip.bottom="cellTip(f, name)" />
          </td>
        </tr>

        <template v-if="pRows.length">
          <tr class="tl-group"><td class="tl-rowhead cc-eyebrow cc-fs-3xs">Populations &amp; overlays</td><td v-for="f in frames" :key="f.id" /></tr>
          <tr v-for="name in pRows" :key="'p'+name" class="tl-row">
            <td class="tl-rowhead cc-fs-sm" :title="name">{{ name }}</td>
            <td v-for="f in frames" :key="f.id" class="tl-cell">
              <button class="tl-dot" :class="{ on: cellState(f, name), unset: cellState(f, name) === null }"
                      :style="cellStyle(f, name)" @click="toggleCell(f, name)"
                      v-tooltip.bottom="cellTip(f, name)" />
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

<style scoped>
/* clean matrix (not a bordered table): sticky row labels, colour-coded toggle dots, rounded thumbs */
.anim-timeline { overflow-x: auto; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-lg);
  background: var(--cc-surface-1); padding: 0.6rem 0.7rem 0.8rem; }
.tl { border-collapse: separate; border-spacing: 0; }
/* No colour or size here: the group-header cells compose .cc-eyebrow, and a scoped class outranks a
   global utility (0,2,0 vs 0,1,0), so owning either property here made the utility a no-op. The
   colour was redundant regardless — nothing above the timeline dims, so these inherit --cc-text. */
/* Width comes from the drag (--tl-label-w on the table), so widening the labels is the user's call
   rather than a constant that has to fit the longest overlay name anyone will ever have. */
.tl-rowhead { position: sticky; left: 0; background: var(--cc-surface-1); text-align: left;
  padding: 0.25rem 0.9rem 0.25rem 0.1rem;
  width: var(--tl-label-w); max-width: var(--tl-label-w); overflow: hidden;
  text-overflow: ellipsis; white-space: nowrap; z-index: 1; }
/* The drag strip is absolute against the corner cell — `.tl-rowhead`'s `position: sticky` is already a
   positioned ancestor, so it needs no `relative` of its own. */
.tl-col-resize { position: absolute; top: 0; right: 0; width: 7px; height: 100%; cursor: col-resize; }
.tl-col-resize::after { content: ''; position: absolute; top: 0; right: 3px; width: 1px; height: 100%;
  background: var(--cc-border); }
.tl-col-resize:hover::after { background: var(--cc-accent); }
/* dim until the corner is hovered — it is a rescue, not something to reach for */
.tl-reset-w { opacity: 0.25; }
.tl-corner:hover .tl-reset-w { opacity: 0.7; }
.tl-reset-w:hover { opacity: 1; color: var(--cc-text); background: var(--cc-surface-2); }
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
/* The third state: this keyframe has no entry for the layer. Still a button — clicking it adds one —
   but dashed and faint, so "nothing said here" doesn't read the same as a deliberate off. */
.tl-dot.unset { border-style: dashed; opacity: 0.4; }
.tl-dot.unset:hover { opacity: 0.85; }
</style>
