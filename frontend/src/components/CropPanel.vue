<script setup lang="ts">
// In-app crop panel. Renders a coloured, scrubbable z-MIP of the image (backend /api/crop/frame, Julia
// in-process — no napari), lets you draw an XY rectangle over it and pick z/t ranges, then Save → the
// existing editImages.cropImage task (→ a new image in the set). See docs/todo/CROP_PANEL_PLAN.md.
import { ref, watch, computed, onUnmounted } from 'vue'
import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useSettingsStore } from '../stores/settings'
import { useLogStore } from '../stores/log'
import { cropBoxFromRect, fracRangeLabel, normalizeRange, type CropInfo, type NormRect } from '../utils/crop3d'
import RangeSlider from './RangeSlider.vue'

const props = defineProps<{ projectUid: string; imageUid: string; imageName: string; valueName: string; setUid: string }>()

const taskStore = useTaskStore()
const ws        = useWsStore()
const settings  = useSettingsStore()
const log       = useLogStore()

const info     = ref<CropInfo | null>(null)
const t        = ref(0)
const frameUrl = ref<string | null>(null)
const loading  = ref(false)
const err      = ref('')
const urlCache = new Map<string, string>()   // key `${t}|${zLo}|${zHi}` → object URL (z affects the MIP)

const qs = () =>
  `projectUid=${encodeURIComponent(props.projectUid)}&imageUid=${encodeURIComponent(props.imageUid)}` +
  `&valueName=${encodeURIComponent(props.valueName)}`

// z/t crop ranges (0–100 %) persisted per set, like the old crop control.
const zLo = computed({ get: () => props.setUid ? settings.getCropZ(props.setUid).lo : 0,   set: v => props.setUid && settings.setCropZ(props.setUid, { lo: v }) })
const zHi = computed({ get: () => props.setUid ? settings.getCropZ(props.setUid).hi : 100, set: v => props.setUid && settings.setCropZ(props.setUid, { hi: v }) })
const tLo = computed({ get: () => props.setUid ? settings.getCropT(props.setUid).lo : 0,   set: v => props.setUid && settings.setCropT(props.setUid, { lo: v }) })
const tHi = computed({ get: () => props.setUid ? settings.getCropT(props.setUid).hi : 100, set: v => props.setUid && settings.setCropT(props.setUid, { hi: v }) })
const zLabel = computed(() => info.value ? fracRangeLabel(zLo.value, zHi.value, info.value.nZ) : '')
const tLabel = computed(() => info.value ? fracRangeLabel(tLo.value, tHi.value, info.value.nT) : '')

// Drawn XY rectangle in normalised image coords ([0,1]); resolution-independent.
const viewRef = ref<HTMLElement | null>(null)
const rect    = ref<NormRect | null>(null)
const drawing = ref(false)
const saving  = ref(false)
const clamp01 = (v: number) => Math.max(0, Math.min(1, v))
function normPt(e: PointerEvent) {
  const r = viewRef.value!.getBoundingClientRect()
  return { x: clamp01((e.clientX - r.left) / r.width), y: clamp01((e.clientY - r.top) / r.height) }
}
function onDown(e: PointerEvent) {
  if (!info.value) return
  const p = normPt(e); rect.value = { x0: p.x, y0: p.y, x1: p.x, y1: p.y }; drawing.value = true
  ;(e.target as Element).setPointerCapture?.(e.pointerId)
}
function onMove(e: PointerEvent) {
  if (!drawing.value || !rect.value) return
  const p = normPt(e); rect.value = { ...rect.value, x1: p.x, y1: p.y }
}
function onUp() { drawing.value = false }
const rectStyle = computed(() => {
  const r = rect.value; if (!r) return { display: 'none' }
  const l = Math.min(r.x0, r.x1), tp = Math.min(r.y0, r.y1)
  return { left: `${l * 100}%`, top: `${tp * 100}%`, width: `${Math.abs(r.x1 - r.x0) * 100}%`, height: `${Math.abs(r.y1 - r.y0) * 100}%` }
})

function clearCache() { urlCache.forEach(u => URL.revokeObjectURL(u)); urlCache.clear(); frameUrl.value = null }

async function loadInfo() {
  clearCache(); err.value = ''; info.value = null; rect.value = null
  if (!props.projectUid || !props.imageUid) return
  try {
    const r = await fetch(`/api/crop/info?${qs()}`)
    const d = await r.json()
    if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`)
    info.value = d as CropInfo
    t.value = Math.floor(((d.nT as number) - 1) / 2)
    loadFrame()
  } catch (e) { err.value = e instanceof Error ? e.message : String(e) }
}

// The displayed MIP projects only over the KEPT z-range, so the z slider previews what you'll keep.
const frameKey = () => { const z = normalizeRange(zLo.value, zHi.value); return `${t.value}|${z.lo.toFixed(4)}|${z.hi.toFixed(4)}` }
async function loadFrame() {
  const key = frameKey()
  const cached = urlCache.get(key)
  if (cached) { frameUrl.value = cached; return }
  loading.value = true
  try {
    const z = normalizeRange(zLo.value, zHi.value)
    const r = await fetch(`/api/crop/frame?${qs()}&t=${t.value}&zLo=${z.lo}&zHi=${z.hi}`)
    if (!r.ok) throw new Error(((await r.json().catch(() => ({}))) as { error?: string }).error ?? `HTTP ${r.status}`)
    const url = URL.createObjectURL(new Blob([await r.arrayBuffer()], { type: 'image/png' }))
    urlCache.set(key, url)
    if (frameKey() === key) frameUrl.value = url    // ignore a stale response if the user moved on
  } catch (e) { err.value = e instanceof Error ? e.message : String(e) }
  finally { loading.value = false }
}

let deb: ReturnType<typeof setTimeout> | null = null
const debFrame = () => { if (deb) clearTimeout(deb); deb = setTimeout(loadFrame, 130) }
watch([t, zLo, zHi], debFrame)   // t OR the z-range → re-render the MIP
watch(() => [props.projectUid, props.imageUid, props.valueName], loadInfo, { immediate: true })
onUnmounted(() => { if (deb) clearTimeout(deb); clearCache() })

function save() {
  if (!rect.value || !info.value || saving.value) return
  const box = cropBoxFromRect(rect.value, info.value, { lo: zLo.value, hi: zHi.value }, { lo: tLo.value, hi: tHi.value })
  const params = { valueName: props.valueName || 'default', ...box }
  saving.value = true
  const task = taskStore.add({
    module: 'crop', label: 'Crop image', imageUid: props.imageUid, imageName: props.imageName || props.imageUid,
    status: 'queued', taskName: 'cropImage', funName: 'editImages.cropImage', params, projectUid: props.projectUid,
  })
  ws.send({
    type: 'task:run', taskId: task.id, funName: 'editImages.cropImage', params,
    imageUid: props.imageUid, projectUid: props.projectUid, setUid: props.setUid, poolName: 'io',
  })
  log.info('Cropping image → new image in the set (appears when the task finishes).', { source: 'crop' })
  rect.value = null
  setTimeout(() => { saving.value = false }, 1500)
}
</script>

<template>
  <div class="crop-panel">
    <div v-if="err" class="crop-err">{{ err }}</div>
    <template v-else-if="info">
      <div ref="viewRef" class="crop-view" @pointerdown="onDown" @pointermove="onMove" @pointerup="onUp" @pointerleave="onUp">
        <img v-if="frameUrl" :src="frameUrl" class="crop-img" alt="crop projection" draggable="false" />
        <div v-else class="crop-img crop-placeholder"><i class="pi pi-spin pi-spinner" /></div>
        <div class="crop-rect" :style="rectStyle" />
        <div v-if="loading && frameUrl" class="crop-loading"><i class="pi pi-spin pi-spinner" /></div>
      </div>

      <div v-if="info.nT > 1" class="crop-row">
        <span class="crop-lbl" v-tooltip.top="'Scrub timepoints to pick the clearest footprint'">frame</span>
        <input type="range" min="0" :max="info.nT - 1" v-model.number="t" />
        <span class="crop-tval">{{ t + 1 }}/{{ info.nT }}</span>
      </div>
      <div v-if="info.nZ > 1" class="crop-row">
        <span class="crop-lbl" v-tooltip.top="'Keep this z-range — also re-projects the preview to just these slices'">z</span>
        <RangeSlider v-model:lo="zLo" v-model:hi="zHi" />
        <span class="crop-tval">{{ zLabel }}</span>
      </div>
      <div v-if="info.nT > 1" class="crop-row">
        <span class="crop-lbl" v-tooltip.top="'Keep this time range'">t</span>
        <RangeSlider v-model:lo="tLo" v-model:hi="tHi" />
        <span class="crop-tval">{{ tLabel }}</span>
      </div>

      <div class="crop-actions">
        <button class="cc-btn cc-btn-primary" :disabled="!rect || saving" @click="save"
                v-tooltip.top="'Save the drawn region as a NEW image in the set (crops X/Y/Z/T, all channels)'">
          <i class="pi pi-save" /> Save crop
        </button>
        <button class="cc-btn cc-btn-ghost" :disabled="!rect" @click="rect = null">
          Cancel
        </button>
      </div>
      <span class="crop-hint">{{ rect ? 'Drag to redraw the rectangle, set z/t, then Save.' : 'Drag a rectangle over the structure to crop.' }}</span>
    </template>
    <div v-else class="crop-hint">Loading crop preview…</div>
  </div>
</template>

<style scoped>
.crop-panel { display: flex; flex-direction: column; gap: 0.4rem; }
/* Fill the modal width so switching versions doesn't jump the preview size — the server downsamples each
   version's MIP by an INTEGER factor to a ≤512px long side, so different native dims render at different
   pixel sizes. width:100% pins the width; only the true aspect ratio (height) varies between versions. */
.crop-view { position: relative; display: block; width: 100%; line-height: 0; cursor: crosshair; touch-action: none; user-select: none; }
.crop-img { display: block; width: 100%; height: auto; border: 1px solid var(--cc-border, #2a2a2a); border-radius: 0.3rem; background: #000; }
.crop-placeholder { display: flex; align-items: center; justify-content: center; width: 100%; min-height: 8rem; color: var(--cc-text-muted, #888); }
.crop-rect { position: absolute; border: 1.5px solid var(--cc-accent, #a855f7); background: color-mix(in srgb, var(--cc-accent, #a855f7) 12%, transparent); pointer-events: none; }
.crop-loading { position: absolute; top: 0.3rem; right: 0.3rem; color: var(--cc-accent); font-size: 0.8rem; }
.crop-row { display: flex; align-items: center; gap: 0.4rem; }
.crop-row input[type=range] { flex: 1 1 auto; }
.crop-lbl { color: var(--cc-text-muted, #888); font-size: 0.7rem; width: 2.6rem; }
.crop-tval { font-size: 0.62rem; color: var(--cc-text); width: 4.2rem; text-align: right; font-variant-numeric: tabular-nums; }
.crop-actions { display: flex; gap: 0.4rem; }
.crop-hint { font-size: 0.68rem; color: var(--cc-text-muted, #888); font-style: italic; }
.crop-err { font-size: 0.7rem; color: #f85149; }
</style>
