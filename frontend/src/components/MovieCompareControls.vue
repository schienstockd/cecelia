<!--
  Which image VERSIONS a movie records — the one control for both movie surfaces (the viewer's
  recorder and the batch panel), replacing the batch's old single-version `<select>`.

  The selection IS the mode: nothing = the active version, one = that version, two or more = a
  side-by-side comparison in chip order (drag to reorder). So there is no separate compare switch that
  could disagree with the list. Layout + contrast only appear once there is something to compare.

  Renders nothing at all for an image with a single version — the common case gets smaller, not bigger.
  Pure logic lives in utils/movieCompare.ts. See docs/todo/MOVIE_COMPARE_PLAN.md.
-->
<script setup lang="ts">
import { computed } from 'vue'
import ChipSelect, { type ChipOption } from './ChipSelect.vue'
import { isComparison, type CompareLayout, type CompareContrast } from '../utils/movieCompare'

const props = defineProps<{
  versions: string[]          // the picked versions, in column order
  available: string[]         // the versions this image has
  layout: CompareLayout
  contrast: CompareContrast
}>()
const emit = defineEmits<{
  (e: 'update:versions', v: string[]): void
  (e: 'update:layout', v: CompareLayout): void
  (e: 'update:contrast', v: CompareContrast): void
}>()

// No per-option `tip` on any row here. A chip row already carries ONE tooltip for the whole control,
// and a second one anchored on the individual chip renders on top of it — two overlapping boxes over
// the thing you were about to click (Dominik, 2026-08-07). So the group tooltip has to say what the
// per-option tips would have, and the icon rows below get labels a tooltip can name.
const options = computed<ChipOption[]>(() =>
  props.available.map(v => ({ value: v, label: v })))

const comparing = computed(() => isComparison(props.versions))

const LAYOUT_OPTIONS: ChipOption[] = [
  { value: 'row', label: '', icon: 'pi pi-pause' },
  { value: 'column', label: '', icon: 'pi pi-equals' },
]
const CONTRAST_OPTIONS: ChipOption[] = [
  { value: 'reference', label: 'matched' },
  { value: 'version', label: 'own' },
]
</script>

<template>
  <div v-if="available.length > 1" class="mc">
    <ChipSelect :options="options" :model-value="versions" multiple reorderable
                aria-label="Image versions to record"
                v-tooltip.bottom="'Select versions to record; two or more compare them'"
                @update:model-value="emit('update:versions', $event as string[])" />
    <div v-if="comparing" class="mc-row cc-row">
      <ChipSelect variant="segmented" :options="LAYOUT_OPTIONS" :model-value="layout"
                  aria-label="Comparison layout" v-tooltip.bottom="'Columns side by side or stacked'"
                  @update:model-value="emit('update:layout', $event as CompareLayout)" />
      <ChipSelect variant="segmented" :options="CONTRAST_OPTIONS" :model-value="contrast"
                  aria-label="Comparison contrast"
                  v-tooltip.bottom="'Contrast matched to the first version, or each version\'s own'"
                  @update:model-value="emit('update:contrast', $event as CompareContrast)" />
    </div>
  </div>
</template>

<style scoped>
.mc { display: flex; flex-direction: column; gap: 0.3rem; min-width: 0; }
/* + .cc-row (wrap/align/gap). Nothing else shares this row: a hint line here pushed the second
   control onto its own line in the viewer panel, and the cost it stated now rides the action
   button's tooltip, where the user is actually committing to it. */
.mc-row { gap: 0.35rem; }
</style>
