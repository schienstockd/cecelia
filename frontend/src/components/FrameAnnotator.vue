<!--
  FrameAnnotator — the SHARED "frozen frame + DrawSurface on top" surface.
  Two callers use it:
    • `CaptureViewSurface.vue` — re-annotating an existing capture (view frozen at share-time).
    • `components/canvas/SummaryCanvas.vue` — annotating a fresh multi-panel plot composite
      before the first Save.
  Both need the same thing: a still image the user can draw over with DrawSurface's tools +
  palette + per-mark labels, then Save (composite marks into a PNG) or Cancel. Coupling either
  surface to its own copy of the plumbing would let them drift — the viewer already had a
  bespoke draw path that we straightened out in PR #1040; this component keeps that discipline.

  What this component OWNS.
    • Displaying a frozen frame (`<img>`) so DrawSurface has something visible under it.
    • Mounting DrawSurface with `:visible=true` and threading its `save` / `cancel` back up.
    • Compositing the drawn marks onto the frame at Save time and handing the caller a
      ready-to-POST PNG data URL, so no caller needs to know about `composeImageWithOverlay`.
  What it does NOT own (deliberately).
    • The POST — each caller has its own address / envelope shape (a viewer re-annotate needs
      `previousCaptureId`; a canvas share needs `panels[]`).
    • The "Claude marks that came back from the assistant" overlay — that only makes sense on a
      capture that already exists; canvas share is a fresh capture. `CaptureViewSurface` paints
      those itself, separately.
    • The lifecycle of the surrounding surface (`CaptureViewSurface`'s chip / close, canvas
      Share's toolbar) — this component only concerns itself with the annotation loop.

  Coord frame: DrawSurface is agnostic to what's underneath. The `<img>` sizes itself with
  `object-fit: contain` so any composite aspect fits, and DrawSurface's SVG covers the full box
  — marks come out in [0,1] frame-relative coords, and `composeImageWithOverlay` bakes them
  at the image's natural pixel size regardless of the on-screen box.
-->
<script setup lang="ts">
import { ref } from 'vue'
import DrawSurface from './DrawSurface.vue'
import type { OverlayMark } from '../utils/captureAddress'
import { composeImageWithOverlay } from '../utils/overlayCompose'

defineProps<{
  /** The frozen frame — a PNG data URL. The caller composited whatever it wanted first (viewer
   *  frame, multi-panel plot composite, …); we treat it as opaque pixels. */
  frameDataUrl: string
  /** Short orientation label for DrawSurface's toolbar — e.g. `"behaviour · plot canvas"`. */
  addressLine?: string
  /** Parent-driven busy flag (mid-POST) that disables DrawSurface's Save button — a fast
   *  double-click otherwise sends two captures. */
  busy?: boolean
}>()

const emit = defineEmits<{
  /** DrawSurface Save fired. Payload:
   *   overlay      — the vector marks, unchanged. Store these in the envelope; Claude reads the
   *                  same shape it would read from a viewer capture.
   *   composedPng  — the frame with marks baked in (or the bare frame on any compose failure —
   *                  matches `composeFrameWithOverlay`'s degrade). Ready to POST as `frames[0].png`. */
  (e: 'save', payload: { overlay: OverlayMark[]; composedPng: string }): void
  /** DrawSurface Cancel — the caller decides whether that means "close the whole surface" or
   *  "back to a prior state". */
  (e: 'cancel'): void
}>()

// The `<img>` ref is what `composeImageWithOverlay` reads from — a same-origin data URL image
// is safe to draw onto a 2D canvas without CORS taint. If the load hasn't landed by Save time
// (e.g. a tiny data URL that queued behind a busy event loop), the compose returns null and we
// fall back to the bare frame — the caller still gets a valid PNG.
const frameImg = ref<HTMLImageElement | null>(null)

function onDrawSave(payload: { overlay: OverlayMark[] }) {
  const img = frameImg.value
  // The empty-marks case is legitimate ("share the frame with no annotations"); fall through with
  // the original PNG. `composeImageWithOverlay` returns null when the source image hasn't
  // decoded, which we treat the same as an empty overlay — keep the frame, drop the annotations.
  let composedPng = ''
  if (!img) composedPng = ''
  else if (payload.overlay.length === 0) composedPng = ''
  else composedPng = composeImageWithOverlay(img, payload.overlay) ?? ''
  emit('save', {
    overlay: payload.overlay,
    composedPng: composedPng || '',   // caller fills with bare frameDataUrl when this is empty
  })
}
</script>

<template>
  <div class="fa-root">
    <img ref="frameImg" :src="frameDataUrl" class="fa-frame" alt="Shared frame" />
    <DrawSurface :visible="true" :address-line="addressLine" :busy="busy"
                 @save="onDrawSave" @cancel="emit('cancel')" />
  </div>
</template>

<style scoped>
.fa-root {
  position: absolute; inset: 0;
  background: #000;                /* letterbox around any non-matching aspect */
  /* Above CanvasPanel's `.panel` (z-index 10) and CanvasSelectionOverlay (z-index 30) so the
     frozen composite and DrawSurface both paint on top of the live panels — an earlier version
     let the panels show through the letterbox around the composite, which read as "why is my
     annotation drawing behind the plots". */
  z-index: 40;
}
.fa-frame {
  position: absolute; inset: 0;
  width: 100%; height: 100%; object-fit: contain;
  display: block;
}
</style>
