<!--
  The DOM shell every free-floating plot canvas repeats: a `position: relative` outer box that
  holds a scrolling viewport, and inside it a zoomable workspace the panels sit in. Was hand-rolled
  three times (SummaryCanvas / GatingPlots / ClusterPlots) as `.sc-* / .gp-* / .cp-*` triples with
  identical CSS and structure.

  Pair with the `useFloatingCanvas` composable, which owns the two DOM refs, all three underlying
  composables (`useCanvasPanels` / `useCanvasZoom` / `useCanvasWorkspace`), and the
  `provide(CANVAS_ZOOM_KEY, …)` the panels rely on. The caller passes the refs and `workspaceStyle`
  through as props; this component binds them to the inner elements. Refs stay in the caller so
  Vue's `provide` still lands on the caller's instance (a `provide` inside this component would
  target this component's descendants only, and slot content is not a descendant for injection).

  Slots:
    default   — rendered inside the zoomable workspace (the panels + share overlays that must
                move/scale with the workspace).
    overlay   — rendered inside the outer `.floating-canvas` (`position: relative`) but OUTSIDE
                the zoom transform, for chips/toasts that must not scale with the panels
                (SummaryCanvas's `.sc-share-chip` is what motivated the split).
-->
<script setup lang="ts">
import type { CSSProperties } from 'vue'

// The bind callbacks match Vue's VNodeRef shape (Element | ComponentPublicInstance | null); this
// component only attaches them to native `<div>`s so the underlying assignment discards anything
// that isn't an Element. `useFloatingCanvas` produces the callbacks; parents forward them here.
type BindTarget = Element | { $el?: unknown } | null
defineProps<{
  bindCanvas: (el: BindTarget) => void
  bindZoom: (el: BindTarget) => void
  workspaceStyle: CSSProperties
}>()
</script>

<template>
  <div class="floating-canvas">
    <div :ref="bindCanvas" class="floating-canvas-scroll">
      <div :ref="bindZoom" class="floating-canvas-zoom" :style="workspaceStyle">
        <slot />
      </div>
    </div>
    <slot name="overlay" />
  </div>
</template>

<style scoped>
/* Outer positioned box. Panels' floating chrome is `position: absolute`; overlay-slot chips also
   anchor here (not `.floating-canvas-zoom`) so the workspace's CSS transform doesn't scale them. */
.floating-canvas { position: relative; flex: 1; min-height: 70vh; }
/* Measured viewport: the workspace it holds can be taller than this box (useCanvasWorkspace grows
   it to fit the plots), so overflow scrolls here rather than escaping the canvas. */
.floating-canvas-scroll { position: absolute; inset: 0; overflow: auto; }
/* Scaled workspace: the panels zoom together. Size + transform come from useCanvasWorkspace via the
   `workspaceStyle` prop. `min-width/min-height: 100%` keeps the workspace filling the viewport even
   before JS lands a measured size — else a 0-sized workspace pins panel drag to the top-left. */
.floating-canvas-zoom { position: absolute; top: 0; left: 0; min-width: 100%; min-height: 100%; }
</style>
