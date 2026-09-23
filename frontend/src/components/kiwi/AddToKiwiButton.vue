<script setup lang="ts">
// "Add to Kiwi" — THE one affordance that attaches an app object to the next Kiwi question
// (KIWI_ASSISTANT_PLAN Phase 5). Every surface that has a KiwiRef (a plot panel, the viewer, a task
// page, a capture row, a Blackboard entry, a population, an image) uses this button; none builds its
// own. `kiwiRef` may be a getter, so a surface whose ref depends on live state (the viewer's t / z,
// its current selection) is read at click time. A getter returning null disables nothing — the click
// is simply a no-op, because the state it reads can change between render and click.
import { useKiwiStore } from '../../stores/kiwi'
import type { KiwiRef } from '../../utils/kiwiRef'

const props = withDefaults(defineProps<{
  kiwiRef: KiwiRef | (() => KiwiRef | null)
  /** Only for a window with no open project of its own (the pop-out viewer). */
  projectUid?: string
  size?: 'micro' | 'dense'
  tip?: string
}>(), { size: 'micro', tip: 'Add to Kiwi' })

const kiwi = useKiwiStore()
function add() {
  const r = typeof props.kiwiRef === 'function' ? props.kiwiRef() : props.kiwiRef
  if (r) kiwi.addRef(r, props.projectUid || undefined)
}
</script>

<template>
  <button class="cc-btn cc-btn-bare cc-btn-icon kiwi-add" :class="`cc-btn-${size}`"
          @mousedown.stop @click.stop="add" v-tooltip.bottom="tip">
    <i class="pi pi-at" />
  </button>
</template>

<style scoped>
.kiwi-add:hover { color: var(--cc-kiwi); }
</style>
