<!--
  The viewer window's playback bar (docs/todo/GUI_LAYOUT_PLAN.md, V2; mock frame 4): one floating
  strip at the bottom centre of the image for time, z and level. A shell: the window fills it (the
  controls stay bound to the window's state), so this owns only the placement, the look and the
  collapse. It floats above the bottom strip of the image, where the scale bar (bottom right, part of
  what Save image composites) and the status chips (bottom left) sit, so it never covers either.

  Collapsed (per profile, `cc.vw.playbackBar`) it shrinks to a pill holding the `collapsed` slot —
  play and the readouts — rather than vanishing: the image keeps saying it has time. Playback runs on
  regardless; the state lives in the window.
-->
<script setup lang="ts">
import { computed } from 'vue'
import { useStoredSectionOpen } from '../../utils/sectionOpen'

const open = useStoredSectionOpen('cc.vw.playbackBar', true)
const collapsed = computed({ get: () => !open.value, set: v => { open.value = !v } })
</script>

<template>
  <div class="vw-pbar" :class="{ 'vw-pbar-collapsed': collapsed }" role="toolbar" aria-label="Playback">
    <slot v-if="!collapsed" />
    <slot v-else name="collapsed" />
    <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" :aria-expanded="!collapsed"
            :aria-label="collapsed ? 'Show the playback bar' : 'Collapse the playback bar'"
            v-tooltip.top="collapsed ? 'Show the playback bar' : 'Collapse to play and position'"
            @click="collapsed = !collapsed">
      <i class="pi" :class="collapsed ? 'pi-chevron-up' : 'pi-chevron-down'" />
    </button>
  </div>
</template>

<style scoped>
.vw-pbar {
  position: absolute; left: 50%; bottom: 3rem; transform: translateX(-50%);
  max-width: calc(100% - 1.5rem); overflow-x: auto;
  display: flex; align-items: center; gap: 0.4rem; padding: 0.3rem 0.6rem;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-sm); box-shadow: 0 2px 10px var(--cc-shadow);
  white-space: nowrap; z-index: 2;
}
.vw-pbar-collapsed { gap: 0.25rem; padding: 0.15rem 0.35rem; border-radius: var(--cc-radius-pill); }
</style>
