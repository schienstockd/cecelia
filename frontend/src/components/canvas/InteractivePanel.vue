<!--
  Generic interactive-plot panel: the CanvasPanel chrome (drag/resize/active/remove) wrapping a
  registered interactive VIEW (interactiveViews.ts) resolved by `view` key. Knows nothing about any
  specific plot — it spreads `context` (the generic plot context: project/images/popType/…) and the
  panel's persisted per-panel `state` onto the view. The view owns its data fetch, rendering, and its
  own controls. This is the interactive counterpart of SummaryPanel (which renders summary specs via
  PlotChart). Shared infra: any canvas (cluster now, universal later) hosts interactive plots with it.
-->
<script setup lang="ts">
import { computed } from 'vue'
import CanvasPanel from './CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import { INTERACTIVE_VIEWS } from './interactiveViews'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  view: string
  context: Record<string, unknown>
  state: Record<string, unknown>
}>()
const emit = defineEmits<{ activate: [number]; remove: [] }>()
const entry = computed(() => INTERACTIVE_VIEWS[props.view])
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey"
               :title="entry?.label ?? view"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <component v-if="entry" :is="entry.component" v-bind="context" :state="state" />
    <div v-else class="ip-missing">Unknown interactive plot “{{ view }}”.</div>
  </CanvasPanel>
</template>

<style scoped>
.ip-missing { padding: 1rem; color: var(--cc-text-dim); font-size: 0.85rem; }
</style>
