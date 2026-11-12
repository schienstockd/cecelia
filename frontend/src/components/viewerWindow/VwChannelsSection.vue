<!--
  The viewer window's Channels section (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer): the
  section-wide row (all on, distinct colours, one at a time; auto / reset contrast, auto tuning,
  compact all) and one card per channel (colour, visibility, contrast window). The window owns the
  channel state and pushes it to the renderer; this renders it and reports each change.

  The list scrolls inside the section (default max-height): a whole-slide image has 24+ channels and
  the sidebar's own scroll never engages.
-->
<script setup lang="ts">
import { ref } from 'vue'
import CollapsibleSection from '../CollapsibleSection.vue'
import CcToggle from '../CcToggle.vue'
import ChipSelect from '../ChipSelect.vue'
import ColourPicker from '../ColourPicker.vue'
import RangeSlider from '../RangeSlider.vue'
import TeleportPopover from '../TeleportPopover.vue'
import { useSettingsStore } from '../../stores/settings'
import { isCompact } from '../../utils/viewer/channelCards'
import { CHANNEL_COLORMAP_OPTIONS } from '../../utils/viewerColormap'
import { channelHexFrom } from '../../utils/viewerProps'
import { WHITE } from '../../plots/fixedColours'

defineProps<{
  /** the channels shown (the window caps how many) */
  channels: { name: string; visible: boolean; lo: number; hi: number; lut: number[][] }[]
  /** the image's channel count — more than `channels.length` says the list is clipped */
  nC: number
  clipped: boolean
  allVisible: boolean
  distinct: boolean
  solo: boolean
  /** cards compact by default, and the ones flipped from that default */
  compact: boolean
  flips: Set<number>
  allCompact: boolean
  /** the AF-preview badge text for a channel, '' when it reads the source */
  afBadge: (c: number) => string
  /** channels whose AF preview is switched off for an A/B */
  afSuspended: Set<number>
  chMax: number[]
  hiCeiling: number[]
  samples: (Uint16Array | Uint8Array)[] | null
}>()
const emit = defineEmits<{
  setAll: [visible: boolean]
  'update:distinct': [on: boolean]
  'update:solo': [on: boolean]
  autoAll: []
  resetAll: []
  toggleAllCards: []
  flip: [c: number]
  afToggle: [c: number]
  colour: [c: number, hex: string]
  visible: [c: number, visible: boolean]
  auto: [c: number]
  reset: [c: number]
  range: [c: number, lo: number, hi: number]
}>()
const settings = useSettingsStore()

// Auto-contrast tuning popover; not persisted, because a popover that reopens on reload is its own bug.
const autoTuneOpen = ref(false)
const autoTuneTrigger = ref<HTMLElement | null>(null)

/** The auto-contrast percentile, by what it does to the picture rather than what the number is:
 *  Tight = trim more, Balanced = middle, Wide = keep more bright pixels visible (the default). */
const AUTO_CONTRAST_OPTIONS = [
  { value: '99', label: 'Tight', tip: 'p99 — trims the bright tail, saturates rare cells' },
  { value: '99.8', label: 'Balanced', tip: 'p99.8 — smooth on dense signal' },
  { value: '99.99', label: 'Wide', tip: 'p99.99 — the default; keeps sparse bright signal' },
]
/** A picked colour is a two-stop black→colour LUT (exact for a channel colormap); the swatch shows
 *  the LUT's top stop. */
const CHANNEL_PALETTE = [...new Set(CHANNEL_COLORMAP_OPTIONS.map(o => o.hex))]
const channelHex = (ch: { lut: number[][] }): string => channelHexFrom(ch.lut ?? []) || WHITE
</script>

<template>
<CollapsibleSection label="Channels" tip="Colour and contrast per channel"
                      accordion="cc.vw.section" section="channels"
                      fill>
    <!-- ONE icon row for the section-wide controls: what is shown on the left (all on, distinct
         colours, one at a time — its stepper is the chip on the canvas), contrast and card layout on the right. Pressed
         state matches the View row at the top (ghost icon, `.cc-btn-on-tint` engaged) — icons rather than
         labelled `CcToggle` rows, so the section header is one line. Each side is a non-wrapping
         `.cc-row-group`, so a narrow sidebar breaks the row BETWEEN the two clusters (the right
         one keeps its right edge) instead of stranding an icon or two on a second line. -->
    <div class="cc-row cc-row-tight vw-ch-master">
      <div class="cc-row-group">
      <button :class="['cc-btn cc-btn-ghost cc-btn-icon', { 'cc-btn-on cc-btn-on-tint': allVisible }]"
              @click="emit('setAll', !allVisible)" :aria-pressed="allVisible"
              v-tooltip.right="allVisible ? 'Hide every channel' : 'Show every channel'"
              aria-label="Toggle every channel">
        <i :class="['pi', allVisible ? 'pi-eye' : 'pi-eye-slash']" />
      </button>
      <button :class="['cc-btn cc-btn-ghost cc-btn-icon', { 'cc-btn-on cc-btn-on-tint': distinct }]"
              @click="emit('update:distinct', !distinct)" :aria-pressed="distinct"
              v-tooltip.right="'Distinct colours — a separate hue per channel'"
              aria-label="Assign a distinct colour to each channel">
        <i class="pi pi-palette" />
      </button>
      <button :class="['cc-btn cc-btn-ghost cc-btn-icon', { 'cc-btn-on cc-btn-on-tint': solo }]"
              @click="emit('update:solo', !solo)" :aria-pressed="solo"
              v-tooltip.right="'One at a time — showing a channel hides the others'"
              aria-label="Show one channel at a time">
        <i class="pi pi-dot-circle" />
      </button>
      </div>
      <div class="cc-row-group vw-ch-master-end">
      <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="emit('autoAll')"
              v-tooltip.left="'Auto contrast on every channel'"
              aria-label="Auto contrast every channel">
        <i class="pi pi-bolt" />
      </button>
      <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="emit('resetAll')"
              v-tooltip.left="'Reset every channel to the full bit range'"
              aria-label="Reset every channel contrast">
        <i class="pi pi-arrow-right-arrow-left" />
      </button>
      <!-- Auto-contrast tuning popover, anchored beside the Auto button so the two read together. -->
      <button ref="autoTuneTrigger" class="cc-btn cc-btn-ghost cc-btn-icon"
              @click="autoTuneOpen = !autoTuneOpen"
              v-tooltip.left="'Auto tuning'" aria-label="Auto-contrast tuning">
        <i class="pi pi-sliders-h" />
      </button>
      <TeleportPopover v-model="autoTuneOpen" :anchor="autoTuneTrigger" placement="bottom-end">
        <div class="cc-eyebrow cc-fs-2xs">Auto range</div>
        <div class="cc-row cc-row-tight">
          <ChipSelect
            :options="AUTO_CONTRAST_OPTIONS"
            :model-value="String(settings.viewerAutoContrastPercent)"
            variant="segmented" aria-label="Auto contrast percentile"
            @update:model-value="v => (settings.viewerAutoContrastPercent = Number(v))"
          />
        </div>
        <div class="cc-muted cc-fs-3xs vw-adv-note">
          How far Auto pushes the top of the window.
        </div>
      </TeleportPopover>
      <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="emit('toggleAllCards')"
              v-tooltip.left="allCompact ? 'Expand every channel' : 'Compact every channel'"
              :aria-label="allCompact ? 'Expand every channel' : 'Compact every channel'">
        <i :class="['pi', allCompact ? 'pi-angle-double-down' : 'pi-angle-double-up']" />
      </button>
      </div>
    </div>
    <div v-for="(ch, c) in channels" :key="c"
         :class="['vw-ch cc-card cc-card-2', { 'vw-ch-compact': isCompact(compact, flips, c) }]">
      <div class="cc-row cc-row-tight">
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro vw-ch-fold"
                @click="emit('flip', c)"
                v-tooltip.right="isCompact(compact, flips, c) ? 'Show contrast' : 'Hide contrast'"
                :aria-label="(isCompact(compact, flips, c) ? 'Expand ' : 'Compact ') + ch.name">
          <i :class="['pi', isCompact(compact, flips, c) ? 'pi-chevron-right' : 'pi-chevron-down']" />
        </button>
        <span class="vw-ch-name cc-fs-xs"
              v-tooltip.right="'Show this channel in the composite'">{{ ch.name }}</span>
        <!-- P7.1: says which channels are reading from the AF preview scratch store rather than
             the source image, so a corrected/uncorrected mixup is not silent. Click to A/B:
             suspend the swap on THIS channel (badge dims, channel reads source) while the
             other corrected channels stay swapped; click again to re-arm. Rendered whenever an
             AF correction exists for this channel, so a dimmed badge tells the user WHY the
             channel looks uncorrected. Resets on every new AF run. -->
        <button v-if="afBadge(c)"
                :class="['vw-ch-af-badge cc-fs-3xs', { 'vw-ch-af-badge-off': afSuspended.has(c) }]"
                @click="emit('afToggle', c)"
                v-tooltip.top="afSuspended.has(c)
                  ? 'Reading SOURCE pixels — click to switch back to the preview'
                  : 'Reading previewed pixels — click to compare against the source'">{{ afBadge(c) }}</button>
        <ColourPicker
          :model-value="channelHex(ch)" :palette="CHANNEL_PALETTE" :tip="'Colour for ' + ch.name"
          @update:model-value="v => emit('colour', c, v)"
        />
        <CcToggle :model-value="ch.visible" :aria-label="'Show ' + ch.name"
                  @update:model-value="v => emit('visible', c, v)" />
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="emit('auto', c)"
                v-tooltip.left="'Auto contrast — window on the loaded pixels (ImageJ-style)'">
          <i class="pi pi-bolt" />
        </button>
        <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="emit('reset', c)"
                v-tooltip.left="'Reset to the full bit range of the image'">
          <i class="pi pi-arrow-right-arrow-left" />
        </button>
      </div>
      <!-- RangeSlider is a flex-ROW item by construction (`flex: 1`, i.e. `flex-basis: 0`), so in a
           column it collapses to no height and its absolutely-positioned thumbs escape the card.
           Every other consumer wraps it in a row with a readout beside it; so does this one. -->
      <div v-if="!isCompact(compact, flips, c)" class="cc-row cc-row-tight">
        <RangeSlider
          v-tooltip.top="'Contrast window — values outside it clip'"
          :lo="ch.lo" :hi="ch.hi" :min="0" :max="Math.max(chMax[c] ?? 1, hiCeiling[c] ?? 0, 1)" :step="1"
          scale="log" :samples="samples?.[c] ?? null"
          @update:lo="v => emit('range', c, v, ch.hi)"
          @update:hi="v => emit('range', c, ch.lo, v)"
        />
        <span class="cc-readout cc-fs-3xs vw-ch-val">{{ ch.lo }}–{{ ch.hi }}</span>
      </div>
    </div>
    <div v-if="clipped" class="cc-muted-warn cc-fs-2xs">
      Showing {{ channels.length }} of {{ nC }} channels
    </div>

  </CollapsibleSection>
</template>

<style scoped>
.vw-ch { padding: 0.35rem 0.4rem; display: flex; flex-direction: column; gap: 0.2rem;
  /* any content that outgrows the card (a stray-wide readout, a wrapping row) is clipped rather than
     poking past the card border */
  min-width: 0; overflow: clip; }
.vw-ch :deep(.rs) { min-width: 0; }
.vw-ch-compact { padding-top: 0.15rem; padding-bottom: 0.15rem; }
/* The fold chevron sits in the card's left padding so names line up compact or not. */
.vw-ch-fold { margin-left: -0.45rem; }
.vw-ch-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vw-ch-af-badge { flex: none; padding: 0 0.35rem; border-radius: var(--cc-radius-pill);
  border: 1px solid var(--cc-accent); background: transparent; color: var(--cc-accent);
  font-weight: 600; letter-spacing: 0.02em; cursor: pointer; line-height: 1.2; }
.vw-ch-af-badge:hover { background: color-mix(in srgb, var(--cc-accent) 15%, transparent); }
.vw-ch-af-badge-off { border-color: var(--cc-border); color: var(--cc-text-dim); text-decoration: line-through; }
.vw-ch-af-badge-off:hover { background: color-mix(in srgb, var(--cc-text-dim) 12%, transparent); }
/* The thumbs are centred on their value, so half of one overhangs at either end of the rail. Room for
   that, or they sit on the card's border. */
.vw-ch { padding-left: 0.7rem; padding-right: 0.7rem; }
.vw-ch-master { margin-bottom: 0.35rem; }
.vw-ch-master-end { margin-left: auto; }
/* sized for the widest reading ("0–65535"), so the slider beside it does not breathe as `hi` drops */
.vw-ch-val { flex: none; white-space: nowrap; min-width: 3rem; text-align: right; }
.vw-adv-note { padding: 0.2rem 0 0.1rem; margin-left: calc(var(--cc-lbl-col) + 0.4rem); }
</style>
