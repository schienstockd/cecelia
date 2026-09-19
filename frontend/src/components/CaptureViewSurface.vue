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
import type { OverlayMark } from '../utils/captureAddress'
import { ANNOTATION_PALETTE, DEFAULT_ANNOTATION_COLOR } from '../utils/overlayCompose'
import type { OverlayColor } from '../utils/captureAddress'

// User-mark stroke — resolve the palette name to a hex, defaulting to white for older captures
// that didn't carry a colour field. Claude marks keep their fixed amber (`--cc-warn`) for now;
// they're distinct in intent and having them fall into the user palette would blur the line.
function userStroke(p: Paintable): string {
  const c = p.color as OverlayColor | undefined
  return (c && ANNOTATION_PALETTE[c]) || ANNOTATION_PALETTE[DEFAULT_ANNOTATION_COLOR]
}

const props = defineProps<{
  captureId: string
  frameDataUrl: string
  overlay: OverlayMark[]    // user's own overlay from the Save moment
  addressLine?: string      // short human line (imageUid · valueName · t · z)
}>()
const emit = defineEmits<{ (e: 'close'): void }>()

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
         own aspect on any wrap size. -->
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
    <div class="cvs-chip cc-fs-2xs">
      <i class="pi pi-camera cvs-chip-icon" />
      <span class="cvs-chip-label">Viewing shared frame</span>
      <span v-if="addressLine" class="cvs-chip-addr cc-muted">{{ addressLine }}</span>
      <button v-if="claudePaintables.length" class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              @click="dismissAllClaudeMarks" v-tooltip.bottom="'Clear pointer marks'">
        <i class="pi pi-eraser" />
      </button>
      <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro"
              @click="emit('close')" v-tooltip.bottom="'Return to live viewer'">
        <i class="pi pi-times" />
      </button>
    </div>
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
