<!--
  Generic interactive-plot panel: the CanvasPanel chrome (drag/resize/active/remove) wrapping a
  registered interactive VIEW (interactiveViews.ts) resolved by `view` key. Knows nothing about any
  specific plot — it spreads `context` (the generic plot context: project/images/popType/…) and the
  panel's persisted per-panel `state` onto the view. The view owns its data fetch, rendering, and its
  own controls. This is the interactive counterpart of SummaryPanel (which renders summary specs via
  PlotChart). Shared infra: any canvas (cluster now, universal later) hosts interactive plots with it.
-->
<script setup lang="ts">
import { computed, useTemplateRef } from 'vue'
import CanvasPanel from './CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import { INTERACTIVE_VIEWS } from './interactiveViews'

const props = defineProps<{
  index: number; active: boolean; arrange?: ArrangeCmd | null; persistKey?: string
  view: string
  context: Record<string, unknown>
  state: Record<string, unknown>
  duplicable?: boolean            // show a footer Duplicate button (host wires @duplicate)
  docked?: boolean                // fill a grid slot (Analysis board) instead of free-floating
}>()
const emit = defineEmits<{ activate: [number]; remove: []; duplicate: [] }>()
const entry = computed(() => INTERACTIVE_VIEWS[props.view])

// a view MAY expose `exportFormats: string[]` + `exportAs(kind)` (defineExpose) — if so, the footer
// shows an Export dropdown that delegates to it. Views without export just get the Duplicate button.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const viewRef = useTemplateRef<any>('viewRef')
const exportFormats = computed<string[]>(() => viewRef.value?.exportFormats ?? [])
const FMT_LABEL: Record<string, string> = { png: 'Image (PNG)', svg: 'Image (SVG)', csv: 'Data (CSV)' }
function onExport(kind: string) { if (kind) viewRef.value?.exportAs?.(kind) }
// plot-only, light-theme PNG for the PDF export — delegated to the view if it implements exportImage
async function exportImage(): Promise<string | null> { return (await viewRef.value?.exportImage?.()) ?? null }
defineExpose({ exportImage })
</script>

<template>
  <CanvasPanel :index="index" :active="active" :arrange="arrange" :persist-key="persistKey" :docked="docked"
               :title="entry?.label ?? view"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <component v-if="entry" :is="entry.component" ref="viewRef" v-bind="context" :state="state" />
    <div v-else class="ip-missing">Unknown interactive plot “{{ view }}”.</div>
    <template v-if="duplicable || exportFormats.length" #footer>
      <button v-if="duplicable" class="ip-iconbtn" type="button" @click="emit('duplicate')"
              v-tooltip.top="'Duplicate this plot (same settings) to tweak one thing'"><i class="pi pi-copy" /></button>
      <select v-if="exportFormats.length && !docked" class="ip-export" v-tooltip.top="'Export the shown plot'"
              @change="onExport(($event.target as HTMLSelectElement).value); ($event.target as HTMLSelectElement).value = ''">
        <option value="">⤓ Export</option>
        <option v-for="f in exportFormats" :key="f" :value="f">{{ FMT_LABEL[f] ?? f }}</option>
      </select>
    </template>
  </CanvasPanel>
</template>

<style scoped>
.ip-missing { padding: 1rem; color: var(--cc-text-dim); font-size: 0.85rem; }
.ip-iconbtn { display: inline-flex; align-items: center; justify-content: center; width: 1.5rem; height: 1.5rem;
  border: 1px solid var(--cc-border); border-radius: 0.3rem; background: var(--cc-surface-1);
  color: var(--cc-text-dim); cursor: pointer; font-size: 0.7rem; }
.ip-iconbtn:hover { color: var(--cc-text); border-color: #484f58; }
.ip-export { font-size: 12px; max-width: 7rem; }
</style>
