<!--
  The arrange/close button group in a plot canvas's toolbar — Tile, Cascade, Close all.

  Every plot canvas (`SummaryCanvas`, `GatingPlots`, `ClusterPlots`, `FlowPlots`) drives the same
  `useCanvasPanels` workspace, and each carried a byte-identical copy of the Tile/Cascade group; the
  fourth copy is what made this a component rather than a fifth. The rest of each toolbar genuinely
  differs (pool toggle, z-window, manager toggle, zoom), so only this group is shared — extracting the
  whole toolbar would force four unlike things into one shape.

  TILE OPTIONS live in a `TeleportPopover` on the Tile button — today just the Columns knob, so that
  a narrow (or briefly unmeasured) workspace can be tiled multi-column instead of silently collapsing
  to one. Auto = the sqrt-shape default; a number pins the count. The popover pattern is the same one
  `SummaryPanel` uses for its plot-options ⚙ — escapes the toolbar's clipping, one place.

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
import { ref, useTemplateRef } from 'vue'
import ConfirmButton from '../ConfirmButton.vue'
import TeleportPopover from '../TeleportPopover.vue'

const props = defineProps<{
  /** how many plots are open — drives the confirm copy, and whether arming is needed at all */
  count: number
  /** current Columns knob value (0 = Auto). Persisted by the host in its canvas `shared` bag. */
  cols?: number
}>()

const emit = defineEmits<{ tile: []; cascade: []; closeAll: []; 'update:cols': [number] }>()

const plural = () => `${props.count} plot${props.count === 1 ? '' : 's'}`

const optsOpen = ref(false)
const optsBtn = useTemplateRef<HTMLElement>('optsBtn')
// small fixed set — the useful shapes for 1..~16 plots on a canvas; anything larger just goes to Auto
// (which flows into the width). Kept short so the popover is a glance, not a scroll.
const COLS_OPTIONS = [0, 1, 2, 3, 4, 5, 6] as const
const colsLabel = (v: number) => v ? String(v) : 'Auto'
function setCols(v: number) { emit('update:cols', v) }
</script>

<template>
  <!-- no group tip: the buttons carry their own, and a container tip fires on top of them -->
  <div class="cc-btn-group">
    <button class="cc-btn cc-btn-bare cc-btn-icon" type="button" v-tooltip.bottom="'Tile in a grid'"
            @click="emit('tile')"><i class="pi pi-th-large" /></button>
    <!-- Tile options: Columns knob. The trigger is a small chevron next to Tile so it never intercepts
         a Tile click; the popover is teleported so the toolbar's clipping cannot cut it off. -->
    <button ref="optsBtn" class="cc-btn cc-btn-bare cc-btn-icon" :class="{ 'cc-btn-on': optsOpen }"
            type="button" v-tooltip.bottom="`Tile columns: ${colsLabel(cols ?? 0)}`"
            @click.stop="optsOpen = !optsOpen">
      <i class="pi pi-chevron-down" />
    </button>
    <TeleportPopover v-model="optsOpen" :anchor="optsBtn" placement="bottom-start">
      <div class="cab-opts" @click.stop>
        <div class="cab-opts-hd cc-muted cc-fs-xs">Columns</div>
        <div class="cc-row cc-row-tight cab-opts-row">
          <button v-for="v in COLS_OPTIONS" :key="v" type="button" class="cc-btn cc-btn-dense"
                  :class="{ 'cc-btn-on cc-btn-on-tint': (cols ?? 0) === v }"
                  v-tooltip.bottom="v ? `Tile in ${v} column${v === 1 ? '' : 's'}` : 'Auto — columns from the workspace width'"
                  @click="setCols(v); emit('tile'); optsOpen = false">{{ colsLabel(v) }}</button>
        </div>
      </div>
    </TeleportPopover>
    <button class="cc-btn cc-btn-bare cc-btn-icon" type="button" v-tooltip.bottom="'Cascade windows'"
            @click="emit('cascade')"><i class="pi pi-clone" /></button>
    <ConfirmButton :needs-confirm="count > 0" @confirm="emit('closeAll')"
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

/* Columns popover — the popover itself owns surface/border/shadow/position (TeleportPopover); we only
   lay out the header + the row of chips inside. The row uses `.cc-row.cc-row-tight`; only the
   per-column min-width is scoped here so the seven column buttons line up as a chip strip. */
.cab-opts { min-width: 12rem; display: flex; flex-direction: column; gap: 6px; padding: 4px; }
.cab-opts-hd { padding: 0 2px; }
.cab-opts-row .cc-btn { min-width: 2rem; }
</style>
