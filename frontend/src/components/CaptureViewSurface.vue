<!--
  CaptureViewSurface — the frozen-frame overlay that stays visible ON the pop-out viewer after a
  Save, so the user can discuss the shared frame with Claude WHILE looking at it. Replaces the
  earlier "modal + shared-frames strip" design that made captureId marks invisible in-context.

  The right frame to talk about is the shared one — the live viewer may have scrubbed. So the
  captured PNG covers the WebGPU canvas, the user's own overlay from the Save moment paints on
  top (accent), and Claude's `mark_freeform` marks whose `target` equals this captureId paint on
  top of that (amber). One layer, one truth.

  DIVISION OF LABOUR.
    • `stores/viewer.ts::freeformMarks` — the ephemeral bag Claude's marks land in.
    • `utils/freeformRender.ts::paintableFor` — the SAME resolver DrawSurface's payloads went
      through on the way out, so a mark reads the same coming back in.
    • This SFC — the PNG cover, both overlay groups, and the "Return to live" close.

  Coord frame: the SVG viewBox tracks its own client CSS px, and both overlays are already in
  0..1 frame-relative coords (captureAddress::normalisePoint). Multiply by box → paint. Same
  approach as DrawSurface / FreeformOverlay, so a mark drawn while `boxW/H` are known will land
  exactly where it did when authored.
-->
<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { useViewerStore } from '../stores/viewer'
import {
  paintableFor, pointsToSvgAttr, type Rect, type Circle, type Arrow, type Paintable,
} from '../utils/freeformRender'
import type { OverlayMark, CaptureAddress } from '../utils/captureAddress'
import { ANNOTATION_PALETTE, DEFAULT_ANNOTATION_COLOR } from '../utils/overlayCompose'
import type { OverlayColor } from '../utils/captureAddress'
import type { CaptureSurface } from '../utils/kiwiCaptures'
import FrameAnnotator from './FrameAnnotator.vue'

// User-mark stroke — resolve the palette name to a hex, defaulting to white for older captures
// that didn't carry a colour field. Claude marks keep their fixed amber (`--cc-warn`) for now;
// they're distinct in intent and having them fall into the user palette would blur the line.
function userStroke(p: Paintable): string {
  const c = p.color as OverlayColor | undefined
  return (c && ANNOTATION_PALETTE[c]) || ANNOTATION_PALETTE[DEFAULT_ANNOTATION_COLOR]
}

const props = withDefaults(defineProps<{
  projectUid: string
  captureId: string
  frameDataUrl: string
  overlay: OverlayMark[]    // user's own overlay from the Save moment
  // Full address envelope from the ORIGINAL capture (image / valueName / t / z / extent /
  // domAnchor). Passed through unchanged on a re-annotate so the refined capture stays anchored to
  // the same frame — the user is still discussing the SAME pixels, only with more strokes on top.
  address: CaptureAddress
  addressLine?: string      // short human line (imageUid · valueName · t · z)
  // Opaque `ViewerViewState` from the ORIGINAL capture — inherited on re-annotate so the refined
  // capture carries the same camera / channels / t / z the user was looking at. Lets a later
  // Refocus take the full-restore branch (Zoom-to-source) instead of the seek-only fallback.
  viewStateSnapshot?: unknown | null
  // The surface tag threaded into the re-annotate POST. Defaults to viewer_frame (the historical
  // case — this component started life mounted only over the pop-out viewer). SummaryCanvas
  // passes `'plot'` so a re-annotated multi-panel capture stays a plot capture.
  surface?: CaptureSurface
  // Extra fields merged into the re-annotate POST body. For a plot capture, callers pass
  // `{ panels }` so the refined capture inherits the same panel structure the original had —
  // Claude still reads "top-left is speed for pops B/T" on the refinement.
  extraPostFields?: Record<string, unknown>
  // Show a "Zoom to source" button in the chip that emits `zoom-to-source`. For plot captures
  // this restores the panel layout from `panels[]` so the user can keep exploring the underlying
  // plots. Off by default — viewer captures have their own zoom-to-source in Kiwi.
  showZoomToSource?: boolean
}>(), {
  addressLine: '',
  extraPostFields: () => ({}),
  surface: 'viewer_frame',
  showZoomToSource: false,
})
const emit = defineEmits<{
  (e: 'close'): void
  // Re-annotate save: a NEW capture was written that refines this one; parent updates the
  // captureView to the new envelope so DrawSurface remounts fresh and the frame carries all
  // strokes drawn so far. Kiwi's list refreshes via `captures:changed` (backend broadcasts).
  (e: 'reannotate', payload: {
    captureId: string; frameDataUrl: string; overlay: OverlayMark[]
  }): void
  // Zoom-to-source (plot captures): the caller restores the panels underneath. Fired from the
  // chip button when `showZoomToSource=true`.
  (e: 'zoom-to-source'): void
}>()

const viewer = useViewerStore()

const wrap = ref<HTMLElement | null>(null)
const boxW = ref(0)
const boxH = ref(0)
function measureBox() {
  const r = wrap.value?.getBoundingClientRect()
  if (!r) return
  boxW.value = r.width; boxH.value = r.height
}
onMounted(() => { window.addEventListener('resize', measureBox); measureBox() })
onBeforeUnmount(() => window.removeEventListener('resize', measureBox))

// ── Re-annotate (Kiwi PR B) ────────────────────────────────────────────────────────────────────
// The frozen frame is what the user is discussing WITH Claude; sometimes that discussion needs
// more marks ("look, here too, and this one over there…"). This mode mounts DrawSurface as a peer
// of the frozen `img`. On Save we composite the NEW marks OVER the already-composited frame (which
// carries the previous session of marks burned in) — so the resulting PNG carries the full history
// of strokes, and the vector overlay records them so a subsequent re-annotate can layer again.
// The lineage is preserved server-side via `previousCaptureId` so Kiwi's list can show it later.
const reannotating = ref(false)
const reannotateBusy = ref(false)
function beginReannotate() { reannotating.value = true }
function cancelReannotate() { reannotating.value = false }

async function onReannotateSave(payload: { overlay: OverlayMark[]; composedPng: string }) {
  if (!props.projectUid) { reannotating.value = false; return }
  reannotateBusy.value = true
  try {
    // FrameAnnotator hands us the composed PNG (marks baked in). Empty ⇒ compose failed or the
    // user saved with no new marks; fall back to the bare frame so the POST still succeeds.
    const composited = payload.composedPng || props.frameDataUrl
    // The vector overlay records BOTH the original marks and the new ones (colour info preserved)
    // so a further re-annotate can render them without re-fetching, and Claude can read the whole
    // conversation of shapes if it prefers vector to pixels.
    const mergedOverlay: OverlayMark[] = [...props.overlay, ...payload.overlay]
    const res = await fetch('/api/viewer/capture', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: props.projectUid,
        surface: props.surface,
        // Address is inherited — the user is still discussing the same frame.
        address: props.address,
        frames: [{ png: composited }],
        overlay: mergedOverlay,
        previousCaptureId: props.captureId,
        // Inherit the ORIGINAL viewStateSnapshot verbatim so a later Refocus on the refined
        // capture restores the exact camera / channels the original share was framed on.
        ...(props.viewStateSnapshot ? { viewStateSnapshot: props.viewStateSnapshot } : {}),
        // Surface-specific inherited fields — plot captures pass `{ panels }` here so the
        // refined capture carries the same per-panel structure the original had.
        ...(props.extraPostFields ?? {}),
      }),
    })
    if (!res.ok) {
      // A silent failure would leave the user in re-annotate mode wondering why nothing happened;
      // exit the mode and let the toast/log system elsewhere flag the error path.
      reannotating.value = false
      return
    }
    const body = await res.json() as { captureId?: string }
    if (!body.captureId) { reannotating.value = false; return }
    reannotating.value = false
    emit('reannotate', {
      captureId: body.captureId,
      frameDataUrl: composited,
      overlay: mergedOverlay,
    })
  } finally {
    reannotateBusy.value = false
  }
}

// User's overlay from the share moment — one-shot, doesn't change.
const userPaintables = computed(() => {
  if (boxW.value <= 0 || boxH.value <= 0) return []
  return paintableFor(props.overlay, boxW.value, boxH.value, 'norm')
})

// Claude's freeform marks whose `target` is THIS captureId. Filter here (rather than in the
// store) so a mark for a different capture waits in the bag rather than painting on the wrong
// frame — mid-conversation Claude may reference a capture from earlier.
const claudePaintables = computed(() => {
  if (boxW.value <= 0 || boxH.value <= 0) return []
  const out: ReturnType<typeof paintableFor> = []
  for (const m of viewer.freeformMarks) {
    if (m.target !== props.captureId) continue
    out.push(...paintableFor(m.overlay, boxW.value, boxH.value, 'norm'))
  }
  return out
})

// Template unions don't narrow; keep casts here.
function asRect(s: unknown):   Rect   { return s as Rect }
function asCircle(s: unknown): Circle { return s as Circle }
function asArrow(s: unknown):  Arrow  { return s as Arrow }
function asPoints(s: unknown): Array<[number, number]> {
  return (s as { pts: Array<[number, number]> }).pts
}

// One "clear all" dismiss for Claude's marks against this capture, rather than a per-shape ✕:
// a single mark_freeform call can carry many shapes and we don't want to leave orphans behind.
function dismissAllClaudeMarks() {
  for (const m of viewer.freeformMarks) {
    if (m.target === props.captureId) viewer.dismissFreeformMark(m.markerId)
  }
}
</script>

<template>
  <div ref="wrap" class="cvs-root">
    <!-- Frozen frame — covers the WebGPU canvas so the user is looking at what they shared, not
         at whatever the live viewer has moved to. `object-fit: contain` matches the viewer's
         own aspect on any wrap size. `crossorigin=anonymous` isn't needed (data URL, same origin
         by definition) but the ref is — re-annotate composites over THIS <img> so a fresh Image
         load isn't needed on Save. -->
    <img :src="frameDataUrl" class="cvs-frame" alt="Shared frame" @load="measureBox" />

    <!-- SVG in the wrap's OWN CSS-px frame. Both overlay groups run through the same paintables
         helper so the shapes read identically. -->
    <svg v-if="boxW > 0 && boxH > 0" class="cvs-svg"
         :viewBox="`0 0 ${boxW} ${boxH}`" preserveAspectRatio="none">
      <g class="cvs-user">
        <template v-for="(p, i) in userPaintables" :key="`u${i}`">
          <rect v-if="p.kind === 'rect'"
                :x="asRect(p.shape).x" :y="asRect(p.shape).y"
                :width="asRect(p.shape).w" :height="asRect(p.shape).h"
                class="cvs-shape" :style="{ stroke: userStroke(p) }" />
          <circle v-else-if="p.kind === 'circle'"
                  :cx="asCircle(p.shape).cx" :cy="asCircle(p.shape).cy" :r="asCircle(p.shape).r"
                  class="cvs-shape" :style="{ stroke: userStroke(p) }" />
          <line v-else-if="p.kind === 'arrow'"
                :x1="asArrow(p.shape).x1" :y1="asArrow(p.shape).y1"
                :x2="asArrow(p.shape).x2" :y2="asArrow(p.shape).y2"
                class="cvs-shape" :style="{ stroke: userStroke(p) }" />
          <polygon v-else-if="p.kind === 'poly'"
                   :points="pointsToSvgAttr(asPoints(p.shape))"
                   class="cvs-shape cvs-poly" :style="{ stroke: userStroke(p) }" />
          <polyline v-else-if="p.kind === 'stroke'"
                    :points="pointsToSvgAttr(asPoints(p.shape))"
                    class="cvs-shape" :style="{ stroke: userStroke(p) }" />
        </template>
      </g>
      <g class="cvs-claude">
        <template v-for="(p, i) in claudePaintables" :key="`c${i}`">
          <rect v-if="p.kind === 'rect'"
                :x="asRect(p.shape).x" :y="asRect(p.shape).y"
                :width="asRect(p.shape).w" :height="asRect(p.shape).h"
                class="cvs-shape cvs-claude-shape" />
          <circle v-else-if="p.kind === 'circle'"
                  :cx="asCircle(p.shape).cx" :cy="asCircle(p.shape).cy" :r="asCircle(p.shape).r"
                  class="cvs-shape cvs-claude-shape" />
          <line v-else-if="p.kind === 'arrow'"
                :x1="asArrow(p.shape).x1" :y1="asArrow(p.shape).y1"
                :x2="asArrow(p.shape).x2" :y2="asArrow(p.shape).y2"
                class="cvs-shape cvs-claude-shape" />
          <polygon v-else-if="p.kind === 'poly'"
                   :points="pointsToSvgAttr(asPoints(p.shape))" class="cvs-shape cvs-claude-shape cvs-poly-claude" />
          <polyline v-else-if="p.kind === 'stroke'"
                    :points="pointsToSvgAttr(asPoints(p.shape))" class="cvs-shape cvs-claude-shape" />
        </template>
      </g>
    </svg>

    <!-- Chip at the top: identifies the capture + offers "Return to live" and a way to clear
         Claude's marks against this capture. Absolute so it doesn't shift when the frame
         resizes. Uses the same status-chip family the viewer's other toasts use. -->
    <div v-if="!reannotating" class="cvs-chip cc-fs-2xs">
      <i class="pi pi-camera cvs-chip-icon" />
      <span class="cvs-chip-label">Viewing shared frame</span>
      <span v-if="addressLine" class="cvs-chip-addr cc-muted">{{ addressLine }}</span>
      <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              @click="beginReannotate"
              v-tooltip.bottom="'Draw more marks on this frame and share the refined capture with Claude'">
        <i class="pi pi-pencil" />
      </button>
      <!-- Plot captures: reopen the panels underneath so the user can keep exploring the source
           layout. Not shown for viewer captures — those have their own Zoom-to-source path in Kiwi
           driven off `viewStateSnapshot`. -->
      <button v-if="showZoomToSource" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              @click="emit('zoom-to-source')"
              v-tooltip.bottom="'Restore the panel layout underneath this capture'">
        <i class="pi pi-search" />
      </button>
      <button v-if="claudePaintables.length" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              @click="dismissAllClaudeMarks" v-tooltip.bottom="'Clear pointer marks'">
        <i class="pi pi-eraser" />
      </button>
      <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              @click="emit('close')"
              v-tooltip.bottom="surface === 'viewer_frame' || surface === 'viewer_slab'
                ? 'Return to live viewer' : 'Close the shared frame'">
        <i class="pi pi-times" />
      </button>
    </div>

    <!-- Re-annotate mode: FrameAnnotator (shared with canvas Share) mounts the frozen frame +
         DrawSurface, hands back the composed PNG on save. `onReannotateSave` POSTs a new capture
         referencing this one via `previousCaptureId`. Cancel dismisses back to the read-only chip
         above. -->
    <FrameAnnotator v-else :frame-data-url="frameDataUrl" :address-line="addressLine"
                    :busy="reannotateBusy"
                    @save="onReannotateSave" @cancel="cancelReannotate" />
  </div>
</template>

<style scoped>
.cvs-root {
  position: absolute; inset: 0;
  background: #000;   /* letterbox around a non-matching aspect frame */
  z-index: 20;        /* above canvas + StillOverlay/GridOverlay */
}
.cvs-frame {
  position: absolute; inset: 0;
  width: 100%; height: 100%; object-fit: contain;
  display: block;
}
.cvs-svg { position: absolute; inset: 0; pointer-events: none; }

.cvs-shape { fill: none; stroke-width: 2px; vector-effect: non-scaling-stroke; }
/* User marks: stroke set inline per mark (palette colour). Legacy CSS defaults stay for Claude
   marks (fixed amber via `--cc-warn`); user's colourless poly tint retired 2026-09 with the
   palette so the outline colour reads unambiguously. */
.cvs-claude-shape { stroke: var(--cc-warn); }
.cvs-poly-claude { fill: rgba(245, 158, 11, 0.08); }

/* Chip: same style family as `.vw-status-chip` (the existing viewer toast) so it reads as
 * "viewer chrome", not modal chrome. */
.cvs-chip {
  position: absolute; top: 8px; left: 8px;
  display: inline-flex; align-items: center; gap: 0.35rem;
  padding: 0.2rem 0.4rem 0.2rem 0.5rem;
  background: var(--cc-surface-1); border: 1px solid var(--cc-warn);
  border-radius: var(--cc-radius-pill);
  color: var(--cc-text);
  box-shadow: 0 0 0 2px rgba(0, 0, 0, 0.35);
  pointer-events: auto;
  max-width: calc(100% - 16px);
}
.cvs-chip-icon { color: var(--cc-warn); }
.cvs-chip-label { font-weight: 600; }
.cvs-chip-addr {
  overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 24ch;
}
</style>
