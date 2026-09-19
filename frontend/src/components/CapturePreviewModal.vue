<!--
  CapturePreviewModal — the "see what I shared with Claude, and what Claude drew on it" modal.
  Fills the gap `mark_freeform` opened: circles drawn against a captureId used to be invisible
  because the user never saw the frozen frame again after Save. This modal shows the frame + the
  user's OWN overlay from when they shared + any of Claude's freeform marks whose `target` is
  this capture, so a pointer AT a shared frame reads as a pointer at a real thing.

  Composition:
    • `BaseModal` for the shell (same convention as every other dialog).
    • `getCapture` (utils/capturesApi.ts) for the envelope + inlined PNG.
    • `paintableFor` (utils/freeformRender.ts) to resolve overlay coords — the SAME resolver
      FreeformOverlay uses on the live viewer, so a captureId mark reads identically here.
    • `stores/viewer.ts::freeformMarks` to find Claude's marks that target this capture (their
      geom is 0..1 frame-relative — perfect for painting against the frozen PNG).

  Coord frame is the FRAME's own pixel space (the PNG dims). We render the PNG into a wrapper of
  known CSS px, measure the wrapper, and hand those dims to `paintableFor`. Both the user's own
  overlay and Claude's freeform marks use the same 0..1 space, so both paint against the same box.
-->
<script setup lang="ts">
import { computed, nextTick, onMounted, onBeforeUnmount, ref, watch } from 'vue'
import BaseModal from './BaseModal.vue'
import { useViewerStore } from '../stores/viewer'
import { getCapture, addressLine, type CaptureEnvelope } from '../utils/capturesApi'
import {
  paintableFor, pointsToSvgAttr, type Rect, type Circle, type Arrow,
} from '../utils/freeformRender'

const props = defineProps<{ projectUid: string; captureId: string }>()
const emit  = defineEmits<{ (e: 'close'): void }>()

const viewer = useViewerStore()

const env    = ref<CaptureEnvelope | null>(null)
const err    = ref<string>('')
const busy   = ref(false)

const wrap = ref<HTMLElement | null>(null)
const boxW = ref(0)
const boxH = ref(0)

function measureBox() {
  const r = wrap.value?.getBoundingClientRect()
  if (!r) return
  boxW.value = r.width; boxH.value = r.height
}

// Fetch on mount + whenever the captureId changes (the same modal instance can be reused for a
// different capture, e.g. clicking a different thumbnail without closing first).
async function fetchOnce() {
  err.value = ''
  env.value = null
  busy.value = true
  try {
    const e = await getCapture(props.projectUid, props.captureId)
    if (!e) { err.value = 'Capture not found. It may have been deleted, or the id is stale.'; return }
    env.value = e
    await nextTick()
    measureBox()
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
  } finally { busy.value = false }
}
watch(() => [props.projectUid, props.captureId], fetchOnce, { immediate: true })

onMounted(() => window.addEventListener('resize', measureBox))
onBeforeUnmount(() => window.removeEventListener('resize', measureBox))

const address = computed(() => addressLine(env.value?.capture.address))

// TWO overlay sources paint against the same 0..1 space:
//   1. The capture's OWN overlay (what the USER drew when they shared it) — one snapshot.
//   2. Claude's freeform marks whose `target` is this captureId — dynamic; a session may add or
//      dismiss them while the modal is open.
// Kept separate so the user's marks read as "what I drew" (accent style) and Claude's read as
// "what Claude drew" (amber, matching the live-viewer FreeformOverlay).
const userPaintables = computed(() => {
  const w = boxW.value, h = boxH.value
  if (w <= 0 || h <= 0 || !env.value) return []
  const overlay = env.value.capture.overlay ?? []
  return paintableFor(overlay, w, h, 'norm')
})
const claudePaintables = computed(() => {
  const w = boxW.value, h = boxH.value
  if (w <= 0 || h <= 0) return []
  const out: ReturnType<typeof paintableFor> = []
  for (const m of viewer.freeformMarks) {
    if (m.target !== props.captureId) continue
    out.push(...paintableFor(m.overlay, w, h, 'norm'))
  }
  return out
})

// Vue template unions don't narrow, so keep the paint casts here.
function asRect(s: unknown):   Rect   { return s as Rect }
function asCircle(s: unknown): Circle { return s as Circle }
function asArrow(s: unknown):  Arrow  { return s as Arrow }
function asPoints(s: unknown): Array<[number, number]> {
  return (s as { pts: Array<[number, number]> }).pts
}
</script>

<template>
  <BaseModal title="Shared frame" icon="pi-send" width="min(920px, 95vw)"
             height="min(760px, 90vh)" @close="emit('close')">
    <template #toolbar>
      <div class="cp-toolbar cc-row cc-row-tight cc-fs-2xs cc-muted">
        <span v-if="address">{{ address }}</span>
        <span v-if="env" class="cp-id">{{ env.capture.captureId }}</span>
      </div>
    </template>

    <div class="cp-body">
      <div v-if="busy" class="cc-empty">Loading capture…</div>
      <div v-else-if="err" class="cc-empty cc-muted-warn">{{ err }}</div>
      <div v-else-if="env" ref="wrap" class="cp-frame-wrap">
        <img :src="env.frame" class="cp-frame" alt="Shared viewer frame" @load="measureBox" />
        <!-- Overlays paint in the frame's own 0..1 space against the measured wrapper. Layer
             transparent to clicks; the modal owns interaction. -->
        <svg v-if="boxW > 0 && boxH > 0" class="cp-svg"
             :viewBox="`0 0 ${boxW} ${boxH}`" preserveAspectRatio="none">
          <!-- User's own overlay from the share moment -->
          <g class="cp-user">
            <template v-for="(p, i) in userPaintables" :key="`u${i}`">
              <rect v-if="p.kind === 'rect'"
                    :x="asRect(p.shape).x" :y="asRect(p.shape).y"
                    :width="asRect(p.shape).w" :height="asRect(p.shape).h"
                    class="cp-shape cp-user-shape" />
              <circle v-else-if="p.kind === 'circle'"
                      :cx="asCircle(p.shape).cx" :cy="asCircle(p.shape).cy" :r="asCircle(p.shape).r"
                      class="cp-shape cp-user-shape" />
              <line v-else-if="p.kind === 'arrow'"
                    :x1="asArrow(p.shape).x1" :y1="asArrow(p.shape).y1"
                    :x2="asArrow(p.shape).x2" :y2="asArrow(p.shape).y2"
                    class="cp-shape cp-user-shape" />
              <polygon v-else-if="p.kind === 'poly'"
                       :points="pointsToSvgAttr(asPoints(p.shape))" class="cp-shape cp-user-shape" />
              <polyline v-else-if="p.kind === 'stroke'"
                        :points="pointsToSvgAttr(asPoints(p.shape))" class="cp-shape cp-user-shape" />
            </template>
          </g>
          <!-- Claude's freeform marks targeting this capture -->
          <g class="cp-claude">
            <template v-for="(p, i) in claudePaintables" :key="`c${i}`">
              <rect v-if="p.kind === 'rect'"
                    :x="asRect(p.shape).x" :y="asRect(p.shape).y"
                    :width="asRect(p.shape).w" :height="asRect(p.shape).h"
                    class="cp-shape cp-claude-shape" />
              <circle v-else-if="p.kind === 'circle'"
                      :cx="asCircle(p.shape).cx" :cy="asCircle(p.shape).cy" :r="asCircle(p.shape).r"
                      class="cp-shape cp-claude-shape" />
              <line v-else-if="p.kind === 'arrow'"
                    :x1="asArrow(p.shape).x1" :y1="asArrow(p.shape).y1"
                    :x2="asArrow(p.shape).x2" :y2="asArrow(p.shape).y2"
                    class="cp-shape cp-claude-shape" />
              <polygon v-else-if="p.kind === 'poly'"
                       :points="pointsToSvgAttr(asPoints(p.shape))" class="cp-shape cp-claude-shape" />
              <polyline v-else-if="p.kind === 'stroke'"
                        :points="pointsToSvgAttr(asPoints(p.shape))" class="cp-shape cp-claude-shape" />
            </template>
          </g>
        </svg>
      </div>
    </div>

    <template #footer>
      <div class="cc-row cc-row-tight cc-fs-2xs cc-muted">
        <span class="cp-legend"><span class="cp-swatch cp-swatch-user" /> Yours</span>
        <span class="cp-legend"><span class="cp-swatch cp-swatch-claude" /> Claude</span>
      </div>
    </template>
  </BaseModal>
</template>

<style scoped>
.cp-toolbar { padding: 0.35rem 0.8rem; border-bottom: 1px solid var(--cc-border); }
.cp-id { margin-left: auto; opacity: 0.7; }
.cp-body { display: flex; align-items: center; justify-content: center; padding: 0.5rem; height: 100%; }
.cp-frame-wrap {
  position: relative; display: inline-block; max-width: 100%; max-height: 100%;
  background: #000; border: 1px solid var(--cc-border);
}
.cp-frame { display: block; max-width: 100%; max-height: 100%; object-fit: contain; }
.cp-svg   { position: absolute; inset: 0; pointer-events: none; }

.cp-shape { fill: none; stroke-width: 2px; vector-effect: non-scaling-stroke; }
.cp-user-shape   { stroke: var(--cc-accent); }
.cp-claude-shape { stroke: var(--cc-warn); }
.cp-user-shape[class*="poly"]   { fill: rgba(128, 90, 220, 0.08); }
.cp-claude-shape[class*="poly"] { fill: rgba(245, 158, 11, 0.08); }

.cp-legend { display: inline-flex; align-items: center; gap: 0.35rem; margin-right: 0.75rem; }
.cp-swatch { width: 10px; height: 10px; border-radius: 50%; border: 1px solid; display: inline-block; }
.cp-swatch-user   { border-color: var(--cc-accent); }
.cp-swatch-claude { border-color: var(--cc-warn); }
</style>
