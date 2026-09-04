<!--
  ImagePickerModal — a task-param picker that renders a coloured, scrubbable z-MIP of one image
  (backend /api/crop/frame, Julia in-process — no viewer), lets the user draw a spatial selection on
  it, then emits the value up. Generalises the old CropDialog + CropPanel so tasks other than crop
  can define spatial params visually (see docs/todo/PREPROCESSING_PLAN once written).

  v1: geometry = 'box3d' (crop's use case — X/Y rectangle + Z + T ranges). box2d, point, line and
  polygon can slot into the same modal; leave the geometry knob visible so the widget's contract is
  the same for the next caller.
-->
<script setup lang="ts">
import { ref, watch, computed, onUnmounted } from 'vue'
import BaseModal from './BaseModal.vue'
import RangeSlider from './RangeSlider.vue'
import { cropBoxFromRect, fracRangeLabel, frameCacheKey, normalizeRange,
         type CropInfo, type NormRect } from '../utils/crop3d'
import { debouncedLatest } from '../utils/debouncedLatest'

export type ImagePickerGeometry = 'box3d'
export type ImagePickerValue = Record<string, number> | null

const props = defineProps<{
  projectUid: string
  imageUid: string
  imageName: string
  valueName: string
  geometry: ImagePickerGeometry
  extraImageCount?: number   // >0 if the same box will apply to more than the previewed image
}>()

const emit = defineEmits<{
  (e: 'save', value: ImagePickerValue): void
  (e: 'close'): void
}>()

const info     = ref<CropInfo | null>(null)
const t        = ref(0)
const frameUrl = ref<string | null>(null)
const loading  = ref(false)
const err      = ref('')
const urlCache = new Map<string, string>()
let reqGen = 0

const qs = () =>
  `projectUid=${encodeURIComponent(props.projectUid)}&imageUid=${encodeURIComponent(props.imageUid)}` +
  `&valueName=${encodeURIComponent(props.valueName)}`

// Modal-local range state. Not persisted per set — the picker is invoked from a task form where
// each run's box is captured on the task's own funParams record (docs/MODULES.md → *Remembering task
// params*). A per-set setting would silently outlive the run.
const zLo = ref(0); const zHi = ref(100)
const tLo = ref(0); const tHi = ref(100)
const zLabel = computed(() => info.value ? fracRangeLabel(zLo.value, zHi.value, info.value.nZ) : '')
const tLabel = computed(() => info.value ? fracRangeLabel(tLo.value, tHi.value, info.value.nT) : '')

const viewRef = ref<HTMLElement | null>(null)
const rect    = ref<NormRect | null>(null)
const drawing = ref(false)
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
  return { left: `${l * 100}%`, top: `${tp * 100}%`,
           width: `${Math.abs(r.x1 - r.x0) * 100}%`, height: `${Math.abs(r.y1 - r.y0) * 100}%` }
})

function clearCache() { urlCache.forEach(u => URL.revokeObjectURL(u)); urlCache.clear(); frameUrl.value = null }

async function loadInfo() {
  const gen = ++reqGen
  clearCache(); err.value = ''; info.value = null; rect.value = null
  if (!props.projectUid || !props.imageUid) return
  try {
    const r = await fetch(`/api/crop/info?${qs()}`)
    const d = await r.json()
    if (gen !== reqGen) return
    if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`)
    info.value = d as CropInfo
    t.value = Math.floor(((d.nT as number) - 1) / 2)
    requestFrame(); frameRun.flush()
  } catch (e) { if (gen === reqGen) err.value = e instanceof Error ? e.message : String(e) }
}

const frameKey = () => frameCacheKey(props.valueName, t.value, { lo: zLo.value, hi: zHi.value })

interface FrameReq { gen: number; key: string; t: number; zLo: number; zHi: number }
const frameRun = debouncedLatest<FrameReq>(async (req, isCurrent) => {
  const cached = urlCache.get(req.key)
  if (cached) { frameUrl.value = cached; return }
  loading.value = true
  try {
    const z = normalizeRange(req.zLo, req.zHi)
    const r = await fetch(`/api/crop/frame?${qs()}&t=${req.t}&zLo=${z.lo}&zHi=${z.hi}`)
    if (!r.ok) throw new Error(((await r.json().catch(() => ({}))) as { error?: string }).error ?? `HTTP ${r.status}`)
    const blob = new Blob([await r.arrayBuffer()], { type: 'image/png' })
    if (req.gen !== reqGen) return
    const url = URL.createObjectURL(blob)
    urlCache.set(req.key, url)
    if (isCurrent() && frameKey() === req.key) frameUrl.value = url
  } catch (e) { if (req.gen === reqGen) err.value = e instanceof Error ? e.message : String(e) }
  finally { if (req.gen === reqGen) loading.value = false }
}, { wait: 130, onError: e => { err.value = e instanceof Error ? e.message : String(e); loading.value = false } })

const requestFrame = () =>
  frameRun.schedule({ gen: reqGen, key: frameKey(), t: t.value, zLo: zLo.value, zHi: zHi.value })

watch([t, zLo, zHi], requestFrame)
watch(() => [props.projectUid, props.imageUid, props.valueName], loadInfo, { immediate: true })
onUnmounted(() => { frameRun.cancel(); clearCache() })

function save() {
  if (!rect.value || !info.value) return
  const box = cropBoxFromRect(rect.value, info.value, { lo: zLo.value, hi: zHi.value }, { lo: tLo.value, hi: tHi.value })
  emit('save', box)
  emit('close')
}
</script>

<template>
  <BaseModal width="640px" @close="emit('close')">
    <template #title>
      <i class="pi pi-image" /> Crop area — {{ imageName }}
      <span v-if="valueName" class="ipm-version-tag">{{ valueName }}</span>
    </template>
    <div class="ipm-panel">
      <div v-if="err" class="ipm-err">{{ err }}</div>
      <template v-else-if="info">
        <div ref="viewRef" class="ipm-view" @pointerdown="onDown" @pointermove="onMove"
             @pointerup="onUp" @pointerleave="onUp">
          <img v-if="frameUrl" :src="frameUrl" class="ipm-img" alt="crop projection" draggable="false" />
          <div v-else class="ipm-img ipm-placeholder"><i class="pi pi-spin pi-spinner" /></div>
          <div class="ipm-rect" :style="rectStyle" />
          <div v-if="loading && frameUrl" class="ipm-loading"><i class="pi pi-spin pi-spinner" /></div>
        </div>

        <div v-if="info.nT > 1" class="ipm-row">
          <span class="ipm-lbl cc-muted cc-fs-xs" v-tooltip.top="'Scrub timepoints to select the clearest footprint'">frame</span>
          <input type="range" min="0" :max="info.nT - 1" v-model.number="t"
                 v-tooltip.top="'Preview frame; does not affect the crop'" />
          <span class="ipm-tval">{{ t + 1 }}/{{ info.nT }}</span>
        </div>
        <div v-if="info.nZ > 1" class="ipm-row">
          <span class="ipm-lbl cc-muted cc-fs-xs" v-tooltip.top="'Keep this z-range — also re-projects the preview to just these slices'">z</span>
          <RangeSlider v-model:lo="zLo" v-model:hi="zHi" v-tooltip.top="'First and last z slice to keep'" />
          <span class="ipm-tval">{{ zLabel }}</span>
        </div>
        <div v-if="info.nT > 1" class="ipm-row">
          <span class="ipm-lbl cc-muted cc-fs-xs" v-tooltip.top="'Keep this time range'">t</span>
          <RangeSlider v-model:lo="tLo" v-model:hi="tHi" v-tooltip.top="'First and last frame to keep'" />
          <span class="ipm-tval">{{ tLabel }}</span>
        </div>

        <span v-if="(extraImageCount ?? 0) > 0" class="ipm-hint cc-muted cc-fs-xs">
          Same box will apply to {{ (extraImageCount ?? 0) + 1 }} images.
        </span>
        <span v-else class="ipm-hint cc-muted cc-fs-xs">
          {{ rect ? 'Drag to redraw the rectangle, set z/t, then Save.' : 'Drag a rectangle over the structure to crop.' }}
        </span>
      </template>
      <div v-else class="ipm-hint cc-muted cc-fs-xs">Loading crop preview…</div>
    </div>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="emit('close')">Cancel</button>
      <button class="cc-btn cc-btn-primary" :disabled="!rect" @click="save"
              v-tooltip.top="'Save this region as the crop area for the task'">
        <i class="pi pi-check" /> Save
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.ipm-panel { display: flex; flex-direction: column; gap: 0.4rem; }
.ipm-view { position: relative; display: block; width: 100%; line-height: 0; cursor: crosshair; touch-action: none; user-select: none; }
.ipm-img { display: block; width: 100%; height: auto; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm); background: #000; }
.ipm-placeholder { display: flex; align-items: center; justify-content: center; width: 100%; min-height: 8rem; color: var(--cc-text-dim); }
.ipm-rect { position: absolute; border: 1.5px solid var(--cc-accent); background: color-mix(in srgb, var(--cc-accent) 12%, transparent); pointer-events: none; }
.ipm-loading { position: absolute; top: 0.3rem; right: 0.3rem; color: var(--cc-accent); font-size: var(--cc-fs-md); }
.ipm-row { display: flex; align-items: center; gap: 0.4rem; }
.ipm-row input[type=range] { flex: 1 1 auto; }
.ipm-lbl { width: 2.6rem; }
.ipm-tval { font-size: var(--cc-fs-2xs); color: var(--cc-text); width: 4.2rem; text-align: right; font-variant-numeric: tabular-nums; }
.ipm-hint { font-style: italic; }
.ipm-err { font-size: var(--cc-fs-xs); color: #f85149; }
.ipm-version-tag {
  margin-left: 0.4rem; padding: 0.05rem 0.4rem; border-radius: var(--cc-radius-lg);
  font-size: var(--cc-fs-2xs); font-weight: 600; vertical-align: middle;
  color: var(--cc-accent);
  background: color-mix(in srgb, var(--cc-accent) 15%, transparent);
}
</style>
