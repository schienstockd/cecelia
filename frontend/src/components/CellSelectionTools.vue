<!--
  Spatial cell-selection toolset for module pages that do gating (Gate / Cluster / Tracking / …).
  One control cluster:
    • Pencil — toggles the WebGPU viewer's SELECT MODE (`settings.viewerSelectMode`). When ON, a
      click in the viewer picks a cell; shift = add, alt = toggle. When OFF, the viewer stays in
      pan/rotate mode and clicks do nothing (Dominik, 2026-08-26: "clear. pan around mode. and
      clear selection mode. otherwise i'm confused what mode i'm in").
    • Clear (×) — empties any transient cell selection (through the gating store's
      `clearSelection`, which POSTs `/api/viewer/pick-clear` and re-broadcasts the tree).
    • Z scope — whole stack (default) vs current z-slice ± N. Read by `ViewerWindow`'s
      `pickRectAt`: when 'slice', the rect POST carries `zLo`/`zHi` and the reader spans that
      inclusive z-range; when 'stack' the endpoint reads just the viewer's current z-plane.

  Why a shared component: the same buttons need to appear on every gating-capable module page —
  Gate, Cluster, Tracking, and whatever's added next. Duplicating four `<button>` blocks per page
  is how three of them would drift the day the endpoint changes. Dominik, 2026-08-26: "make sure
  the wiring is a communal component and that every module page that does gating consumes it."

  Track pops have nothing to spatially select (a track is not a pixel), so the caller passes
  `:show="!isTrack"` — the whole cluster hides in one place.
-->
<script setup lang="ts">
import { useSettingsStore } from '../stores/settings'
import { useGatingStore } from '../stores/gating'

defineProps<{
  /** False hides the whole toolbar — set by track-pop pages where spatial selection is meaningless. */
  show?: boolean
}>()

const settings = useSettingsStore()
const g = useGatingStore()
</script>

<template>
  <div v-if="show ?? true" class="cc-btn-group">
    <button class="cc-btn cc-btn-bare cc-btn-icon"
            :class="{ 'cc-btn-on cc-btn-on-tint': settings.viewerSelectMode === 'select' }"
            v-tooltip.bottom="settings.viewerSelectMode === 'select'
              ? 'In selection mode — click to exit'
              : 'Enter selection mode'"
            @click="settings.viewerSelectMode = settings.viewerSelectMode === 'select' ? 'off' : 'select'">
      <i class="pi pi-pencil" />
    </button>
    <button class="cc-btn cc-btn-bare cc-btn-icon"
            v-tooltip.bottom="'Clear the current cell selection'"
            @click="g.clearSelection"><i class="pi pi-times" /></button>
    <button class="cc-btn cc-btn-bare"
            :class="{ 'cc-btn-on cc-btn-on-tint': g.pickZMode === 'slice' }"
            v-tooltip.bottom="g.pickZMode === 'slice'
              ? `Selecting cells from the current z-slice ±${g.pickZWindow} — click for the whole stack`
              : 'Selecting cells across the whole z-stack — click to restrict to the current z-slice'"
            @click="g.pickZMode = g.pickZMode === 'slice' ? 'stack' : 'slice'">
      <i class="pi pi-clone" /> Z
    </button>
  </div>
  <label v-if="(show ?? true) && g.pickZMode === 'slice'" class="zwin"
         v-tooltip.bottom="'Include cells within ± this many z-slices (0 = current only)'">
    ±<input type="number" min="0" max="50" step="1" v-model.number="g.pickZWindow" />
  </label>
</template>
