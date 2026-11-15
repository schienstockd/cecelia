<!--
  The viewer window's "How it looks" box: the controls that used to be the docked right sidebar,
  floating top right on the image, level with "On the image" (VwOnImageOverlay) and folding like it. The pair
  splits the work the same way the sidebar and that box always did — On the image picks WHAT is drawn,
  this box sets HOW it looks (channels, contrast, sizes, colours). Floating, not docked, so the image
  keeps the whole window: half a laptop screen left the image 480 px and cut the sidebar off.
  docs/todo/GUI_LAYOUT_PLAN.md → *Narrow windows*.

  The `controls` slot (what a click does, 2D / 3D, reset, ⚙) stays visible when folded, so the mode
  switches are one click away; the default slot (the sections) folds. Folded state is the old
  sidebar's `settings.viewerWindowSideCollapsed`, width the old panel's `viewerWindowSide` key — a
  user's saved choice carries over. The left edge drags to resize, as the panel did.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useSettingsStore } from '../../stores/settings'
import { usePanelResize } from '../../composables/usePanelResize'

const settings = useSettingsStore()
const { widthStyle, onResizeStart } = usePanelResize({ min: 240, max: 480, default: 290,
  storageKey: 'viewerWindowSide', edge: 'left' })
const folded = computed<boolean>({
  get: () => settings.viewerWindowSideCollapsed,
  set: v => { settings.viewerWindowSideCollapsed = v },
})
</script>

<template>
  <div class="cc-card vw-look" :class="{ 'vw-look-folded': folded }" :style="folded ? undefined : widthStyle"
       data-guide="viewer.look">
    <div v-if="!folded" class="vw-look-resizer" @mousedown="onResizeStart" v-tooltip.top="'Drag to resize'" />
    <div class="vw-look-head">
      <span class="cc-eyebrow cc-fs-2xs">How it looks</span>
      <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" :aria-expanded="!folded"
              :aria-label="folded ? 'Show how the image looks' : 'Fold how the image looks'"
              v-tooltip.bottom="folded ? 'Channels, contrast, sizes and colours' : 'Fold to the controls'"
              @click="folded = !folded">
        <i class="pi" :class="folded ? 'pi-chevron-left' : 'pi-chevron-right'" />
      </button>
    </div>
    <div class="vw-look-controls"><slot name="controls" /></div>
    <!-- v-show, not v-if: folding must not tear the sections down (and refetch on reopen) -->
    <div v-show="!folded" class="vw-look-body"><slot /></div>
  </div>
</template>

<style scoped>
.vw-look {
  position: relative; display: flex; flex-direction: column;
  min-height: 0; flex: 0 1 auto;          /* shrinks to the corner column; the body scrolls */
  max-width: calc(100vw - 1.5rem);
  padding: 0.3rem 0.5rem 0.45rem;
  box-shadow: 0 2px 10px var(--cc-shadow);
}
.vw-look-folded { padding-bottom: 0.35rem; }
.vw-look-resizer {
  position: absolute; left: -3px; top: 0; bottom: 0; width: 6px; cursor: col-resize;
  border-radius: var(--cc-radius-xs); transition: background 0.12s;
}
.vw-look-resizer:hover { background: var(--cc-accent); }
.vw-look-head { display: flex; align-items: center; gap: 0.4rem; flex-shrink: 0; }
.vw-look-head .cc-eyebrow { flex: 1; white-space: nowrap; }
.vw-look-controls { flex-shrink: 0; padding-top: 0.25rem; }
.vw-look-body {
  /* the sections' own column (`.vw-side` in ViewerWindow) is the scroller; this only bounds it */
  min-height: 0; flex: 1 1 auto; display: flex; flex-direction: column;
  margin-top: 0.3rem; padding-top: 0.35rem; border-top: 1px solid var(--cc-border);
}
</style>
