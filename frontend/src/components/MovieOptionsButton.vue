<!--
  The gear that holds a movie's OPTIONS — frame rate, size, file name, title card. This owns the
  CHROME only: the button, its tooltip, the popover, and the stack the control blocks sit in. The
  blocks come in through the slot, because the two sites feed them different props (the viewer also
  owns the viewer's baked overlays and the z range; the Animation page has neither).

  Shared rather than written twice: the viewer had this inline and the Animation page needed the same
  thing, which is how one surface ends up with a different tooltip, icon or popover width. Same split
  as TeleportPopover itself — the component owns the shell, the caller owns the content.
-->
<script setup lang="ts">
import { ref } from 'vue'
import TeleportPopover from './TeleportPopover.vue'

// two root nodes (the anchor and the teleported popover), so a caller's `class` has to be routed to
// the button explicitly rather than auto-inherited onto a fragment
defineOptions({ inheritAttrs: false })

const open = ref(false)
const anchor = ref<HTMLElement | null>(null)
</script>

<template>
  <button ref="anchor" v-bind="$attrs" class="cc-btn cc-btn-ghost cc-btn-icon"
          :class="{ 'cc-btn-on cc-btn-on-tint': open }"
          @click="open = !open"
          v-tooltip.bottom="'Movie options: frame rate, size, file name, title card'">
    <i class="pi pi-cog" />
  </button>
  <TeleportPopover v-model="open" :anchor="anchor" placement="bottom-end">
    <div class="mob-body"><slot /></div>
  </TeleportPopover>
</template>

<style scoped>
/* The popover IS the movie block, so it takes the width the blocks inside are capped to. */
.mob-body { display: flex; flex-direction: column; gap: 0.45rem;
  width: var(--cc-movie-block); max-width: 80vw; }
</style>
