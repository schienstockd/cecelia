<!--
  One image thumbnail in a fixed square slot — the image table's row thumbnail. The slot is the same
  box whatever the image's aspect (portrait, landscape, a 20:1 strip), and whether it is loaded,
  loading or missing, so a row never changes height or width as thumbnails arrive. The image sits
  centred and contained on the media ground (`--cc-media-bg`, black in both schemes: what is under a
  fluorescence image is not the page), inside a hairline border so a black image keeps its edge.

  It is a button: the caller's `@click` (open the image) and `tip` (its hover preview) ride on it, so
  it takes keyboard focus like any other control.

  `src`: a data URL → the picture; `''` → loading; null / undefined → nothing to show.
-->
<script setup lang="ts">
import type { TooltipBinding } from '../utils/tooltipOptions'

withDefaults(defineProps<{
  src: string | null | undefined
  alt: string
  tip?: TooltipBinding
  /** Edge of the square, px. */
  size?: number
}>(), { tip: undefined, size: 80 })
</script>

<template>
  <button type="button" class="cc-btn cc-btn-bare thumb-slot" :style="{ '--thumb-slot': `${size}px` }"
    :aria-label="alt" v-tooltip.right="tip">
    <img v-if="src" :src="src" :alt="alt" class="thumb-slot-img" decoding="async" />
    <i v-else-if="src === ''" class="pi pi-spin pi-spinner cc-muted cc-fs-xs" />
    <i v-else class="pi pi-image thumb-slot-none" />
  </button>
</template>

<style scoped>
.cc-btn.thumb-slot {
  width: var(--thumb-slot); height: var(--thumb-slot); flex: none; padding: 0;
  justify-content: center; overflow: hidden;
  background: var(--cc-media-bg); border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs);
}
.cc-btn.thumb-slot:hover:not(:disabled) { border-color: var(--cc-text-dim); }
.thumb-slot-img { width: 100%; height: 100%; object-fit: contain; display: block; }
.thumb-slot-none { color: var(--cc-text-dim); opacity: 0.5; }
</style>
