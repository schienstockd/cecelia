<!--
  THE colour picker: a swatch you click to open a palette, plus the native picker for anything outside
  it. Use this wherever a user chooses an arbitrary colour for a thing.

  Extracted from `canvas/PopulationManager.vue` (2026-08-24), which had the only copy, when the browser
  volume viewer needed the same control for channel colours — asked for by name, because the alternative
  (`SwatchSelect`) spells the option out in text and a channel row has no width to spare for that.
  `SwatchSelect` is still the right control for a NAMED set of colormaps with labels worth reading (the
  batch-movie channel picker); this one is for "any colour", where the swatch IS the label.

  It is one component per swatch rather than one popover shared by a list. That was the shape the pop
  manager used — a single popover plus `colourAnchor`/`colourPop` refs tracking which row opened it — and
  it costs nothing to drop: `TeleportPopover` renders nothing at all while closed, so N closed pickers
  are N buttons, and the anchor bookkeeping disappears.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import { PALETTES } from '../plots/plot'
import TeleportPopover from './TeleportPopover.vue'

const model = defineModel<string>({ required: true })
const props = withDefaults(defineProps<{
  /** Offered as chips. Defaults to the house palette, which is what a caller almost always wants. */
  palette?: string[]
  disabled?: boolean
  /** Hover help for the swatch. A colour has no text, so without one the control is unnamed. */
  tip?: string
}>(), { palette: () => PALETTES.cecelia, disabled: false, tip: 'Colour' })

const open = ref(false)
const anchor = ref<HTMLElement | null>(null)
// Case-insensitive: a palette entry and a value that came back from a native picker differ in case.
const isCurrent = (c: string) => c.toLowerCase() === (model.value ?? '').toLowerCase()
const current = computed(() => model.value ?? '#ffffff')

function toggle(e: MouseEvent) {
  if (props.disabled) return
  anchor.value = e.currentTarget as HTMLElement
  open.value = !open.value
}
/** `close` is false for the native picker, which fires `change` while the user is still in its dialog. */
function pick(c: string, close = true) {
  model.value = c
  if (close) open.value = false
}
</script>

<template>
  <button
    type="button" class="cp-swatch" :style="{ background: current }" :disabled="disabled"
    v-tooltip.left="disabled ? '' : tip" @click.stop="toggle"
  />
  <TeleportPopover v-model="open" :anchor="anchor" placement="bottom-start">
    <div class="cp-body">
      <div class="cp-grid">
        <button
          v-for="c in palette" :key="c" type="button" class="cp-chip"
          :class="{ on: isCurrent(c) }" :style="{ background: c }"
          v-tooltip.top="c" @click="pick(c)"
        />
      </div>
      <label class="cp-custom cc-muted cc-fs-xs">
        <span>custom</span>
        <input
          type="color" :value="current" v-tooltip.top="'Select a colour outside the palette'"
          @change="pick(($event.target as HTMLInputElement).value, false)"
        >
      </label>
    </div>
  </TeleportPopover>
</template>

<style scoped>
.cp-swatch {
  width: 16px; height: 16px; padding: 0; border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs); cursor: pointer; flex-shrink: 0;
}
.cp-swatch:disabled { cursor: default; opacity: 0.7; }
/* TeleportPopover supplies the surface and border; this is only the contents. */
.cp-body { display: flex; flex-direction: column; gap: 8px; padding: 8px; }
.cp-grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 5px; }
.cp-chip {
  width: 22px; height: 22px; border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs); cursor: pointer; padding: 0;
}
.cp-chip:hover { transform: scale(1.08); }
.cp-chip.on { outline: 2px solid var(--cc-text); outline-offset: 1px; }
/* Layout only — the muted look is composed in the markup (`.cc-muted .cc-fs-xs`). Re-declaring it here
   would outrank the global rule on specificity and silently stop tracking it. */
.cp-custom { display: flex; align-items: center; justify-content: space-between; gap: 8px; }
.cp-custom input { width: 28px; height: 20px; padding: 0; border: none; background: none; cursor: pointer; }
</style>
