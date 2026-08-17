<!--
  The arrange/close button group in a plot canvas's toolbar — Tile, Cascade, Close all.

  Every plot canvas (`SummaryCanvas`, `GatingPlots`, `ClusterPlots`, `FlowPlots`) drives the same
  `useCanvasPanels` workspace, and each carried a byte-identical copy of the Tile/Cascade group; the
  fourth copy is what made this a component rather than a fifth. The rest of each toolbar genuinely
  differs (pool toggle, z-window, manager toggle, zoom), so only this group is shared — extracting the
  whole toolbar would force four unlike things into one shape.

  CLOSE ALL is the bulk half of the per-panel X in `CanvasPanel`: closing fifteen plots one at a time
  is the only alternative. It is destructive and unrecoverable (panel state + persisted geometry), so
  it arms first via `ConfirmButton` — the canonical arm→confirm primitive, never `window.confirm`
  (docs/UI.md → *No native browser dialogs*). This mirrors `TabbedCanvas`'s "Close board" exactly,
  including `needs-confirm` being FALSE when there is nothing to lose: arming an empty canvas would be
  a confirmation step protecting nothing.

  The buttons live in THIS template using the GLOBAL `cc-btn*` utilities (style.css), not host-scoped
  classes — which is why they can move out of the hosts without losing their styling.
-->
<script setup lang="ts">
import ConfirmButton from '../ConfirmButton.vue'

const props = defineProps<{
  /** how many plots are open — drives the confirm copy, and whether arming is needed at all */
  count: number
}>()

defineEmits<{ tile: []; cascade: []; closeAll: [] }>()

const plural = () => `${props.count} plot${props.count === 1 ? '' : 's'}`
</script>

<template>
  <!-- no group tip: the buttons carry their own, and a container tip fires on top of them -->
  <div class="cc-btn-group">
    <button class="cc-btn cc-btn-bare cc-btn-icon" type="button" v-tooltip.bottom="'Tile in a grid'"
            @click="$emit('tile')"><i class="pi pi-th-large" /></button>
    <button class="cc-btn cc-btn-bare cc-btn-icon" type="button" v-tooltip.bottom="'Cascade windows'"
            @click="$emit('cascade')"><i class="pi pi-clone" /></button>
    <ConfirmButton :needs-confirm="count > 0" @confirm="$emit('closeAll')"
                   v-slot="{ armed, arm, confirm, cancel }">
      <button v-if="!armed" class="cc-btn cc-btn-bare cc-btn-icon" type="button" :disabled="!count"
              @click="arm" v-tooltip.bottom="'Close all plots'" aria-label="Close all plots">
        <i class="pi pi-times" />
      </button>
      <template v-else>
        <button class="cc-btn cc-btn-bare cc-btn-icon ca-danger" type="button" @click="confirm"
                v-tooltip.bottom="`Confirm — close all ${plural()}`"><i class="pi pi-check" /></button>
        <button class="cc-btn cc-btn-bare cc-btn-icon" type="button" @click="cancel"
                v-tooltip.bottom="'Keep plots'"><i class="pi pi-undo" /></button>
      </template>
    </ConfirmButton>
  </div>
</template>

<style scoped>
/* the armed confirm reads as destructive — same cue as CanvasPanel's remove button */
.ca-danger:hover { color: #f87171; border-color: #f87171; }
</style>
