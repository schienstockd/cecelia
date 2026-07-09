<!--
  A read-only "channel pairs" plot for the Gate / Tracking pages — the single gate plot's X/Y
  generalised to a LIST of channels, rendered as the full N×N matrix of every channel-vs-channel
  scatter (R pairs()). A sibling of GatePlotPanel: same free-floating CanvasPanel chrome, same page
  (added via GatingPlots' "+ Pairs" button, so BOTH flow and track gating get it). It does NOT draw
  gates — but it DOES show the gates that define the displayed population's children, and it honours the
  SAME highlight pipeline as the normal plot: the manager's "eye" populations AND the transient napari
  cell-selection light up across every tile (linked brushing). Rendering/fetching is the shared
  GateMontage (feedback_use_existing_framework); the tiles come from the pure buildPairDefs.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef, onMounted, onUnmounted } from 'vue'
import { useGatingStore } from '../../stores/gating'
import CanvasPanel from '../../components/canvas/CanvasPanel.vue'
import type { ArrangeCmd } from '../../composables/useFloatingPanel'
import GateMontage from '../../components/plots/GateMontage.vue'
import { buildPairDefs } from '../../plots/pairsMatrix'
import { downloadDataUrl } from '../../plots/export'

type Kind = 'linear' | 'log' | 'asinh' | 'logicle'
const TRANSFORMS: Kind[] = ['linear', 'log', 'asinh', 'logicle']
const MAX_CHANNELS = 8            // N×N tiles grow fast — cap the selection and say so (no silent cut)

const props = defineProps<{
  index: number; active: boolean; parent: string; highlight: string[]
  gateLineWidth: number; gateLabels: boolean; axisFromZero: boolean
  // persisted per-plot config (owned by GatingPlots' PlotState): the channel list, the one shared
  // transform, and the render mode. Read/written directly so they survive navigation.
  ui: { channels?: string[]; xt?: Kind; renderMode?: 'points' | 'contour' }
  arrange?: ArrangeCmd | null
  persistKey?: string
}>()
const emit = defineEmits<{ activate: [number]; 'update:parent': [string]; remove: [] }>()
const g = useGatingStore()

// track properties → linear by default; flow intensities → logicle (FlowJo) — same rule as GatePlotPanel
const defaultTransform: Kind = g.popType === 'track' ? 'linear' : 'logicle'
const channels = computed<string[]>({ get: () => props.ui.channels ?? [], set: v => { props.ui.channels = v } })
const transform = computed<Kind>({ get: () => props.ui.xt ?? defaultTransform, set: v => { props.ui.xt = v } })
const renderMode = computed<'points' | 'contour'>({ get: () => props.ui.renderMode ?? 'points', set: v => { props.ui.renderMode = v } })
const parent = computed({ get: () => props.parent, set: v => emit('update:parent', v) })
const parentOptions = computed(() => ['root', ...g.flat.map(p => p.path)])

// the displayed population's directly-gated children → their outlines appear on whichever tile matches
// their channel-pair (buildPairDefs orients them). Mirrors GatePlotPanel's currentGates intent.
const parentChildren = computed(() => g.flat.filter(p => p.parent === parent.value && p.gate))
const defs = computed(() => buildPairDefs(channels.value, parent.value, transform.value, parentChildren.value))

// coloured overlays: the pops highlighted for THIS panel (manager eye + napari transient), with colours
// resolved from the store (GateMontage is store-agnostic).
const highlightPops = computed(() => (props.highlight ?? []).map(path =>
  ({ path, colour: g.flat.find(p => p.path === path)?.colour ?? '#22d3ee' })))
// force a point refresh when membership moves without the tiles changing (ancestor gate edit, napari
// selection re-evaluated) — the parent's / a highlighted pop's version bumps in the store.
const reloadKey = computed(() =>
  `${g.popVersion[parent.value] ?? 0}:${(props.highlight ?? []).map(p => g.popVersion[p] ?? 0).join(',')}`)

// channel picker (compact popover)
const pickOpen = ref(false)
const pickRef = useTemplateRef<HTMLElement>('pickRef')
function onDocClick(e: MouseEvent) { if (pickOpen.value && pickRef.value && !pickRef.value.contains(e.target as Node)) pickOpen.value = false }
onMounted(() => document.addEventListener('mousedown', onDocClick))
onUnmounted(() => document.removeEventListener('mousedown', onDocClick))
const atCap = computed(() => channels.value.length >= MAX_CHANNELS)
function toggleChannel(c: string) {
  const cur = channels.value
  if (cur.includes(c)) channels.value = cur.filter(x => x !== c)
  else if (!atCap.value) channels.value = [...cur, c]
}
function clearChannels() { channels.value = [] }

// seed a sensible default selection the first time (like GatePlotPanel.ensureChannels): the first few
// intensity channels for flow, else the first gateable columns for track.
function ensureChannels() {
  if (channels.value.length || !g.columns.length) return
  const src = g.channels.length ? g.channels : g.columns
  channels.value = src.slice(0, Math.min(4, src.length))
}
watch([() => g.columns, () => g.imageUid, () => g.valueName], ensureChannels, { immediate: true })

const montageRef = useTemplateRef<{ exportImage(bg?: string, light?: boolean): Promise<string | null> }>('montageRef')
function exportPng() {
  const stem = `pairs_${channels.value.map(c => g.colLabel(c)).join('_')}`.replace(/[^\w.-]+/g, '_').slice(0, 80)
  montageRef.value?.exportImage('#0d0b1a', false).then(url => url && downloadDataUrl(`${stem || 'pairs'}.png`, url))
}
</script>

<template>
  <CanvasPanel :index="index" :active="props.active" :arrange="props.arrange" :title="`Pairs ${channels.length}×${channels.length}`"
               :persist-key="props.persistKey"
               @activate="emit('activate', $event)" @remove="emit('remove')">
    <template #actions>
      <span class="ro-tag" v-tooltip.bottom="'Read-only — compare channels; draw gates on a single plot'">read-only</span>
      <span class="ctrl-sep" />
      <div class="seg" v-tooltip.bottom="'Render: pseudocolour points / density contour'">
        <button :class="{ on: renderMode === 'points' }" @click="renderMode = 'points'"><i class="pi pi-circle-fill" /></button>
        <button :class="{ on: renderMode === 'contour' }" @click="renderMode = 'contour'"><i class="pi pi-chart-line" /></button>
      </div>
    </template>
    <template #footer>
      <select class="gp-export" v-tooltip.top="'Export the matrix'" :disabled="!channels.length"
              @change="($event.target as HTMLSelectElement).value === 'png' && exportPng(); ($event.target as HTMLSelectElement).value = ''">
        <option value="">⤓ Export</option>
        <option value="png">Image (PNG)</option>
      </select>
    </template>

    <div class="panel-ctrl">
      <label class="ax-row"><span class="ax-lbl">pop</span>
        <select class="ax-chan" v-model="parent" v-tooltip.bottom="'Population to compare; its gates are shown'">
          <option v-for="p in parentOptions" :key="p" :value="p">{{ p }}</option></select>
      </label>
      <div class="ax-row">
        <span class="ax-lbl">chans</span>
        <div ref="pickRef" class="chan-pick">
          <button class="chan-btn" :class="{ on: pickOpen }" @click="pickOpen = !pickOpen"
                  v-tooltip.bottom="'Choose the channels to plot against each other'">
            {{ channels.length ? `${channels.length} selected` : 'select channels' }} <i class="pi pi-chevron-down" />
          </button>
          <div v-if="pickOpen" class="chan-pop">
            <div class="chan-pop-head">
              <span>{{ channels.length }}/{{ MAX_CHANNELS }}</span>
              <button class="chan-clear" :disabled="!channels.length" @click="clearChannels">clear</button>
            </div>
            <div class="chan-list">
              <label v-for="c in g.columns" :key="c" class="chan-item"
                     :class="{ disabled: !channels.includes(c) && atCap }">
                <input type="checkbox" :checked="channels.includes(c)"
                       :disabled="!channels.includes(c) && atCap" @change="toggleChannel(c)" />
                <span>{{ g.colLabel(c) }}</span>
              </label>
            </div>
            <div v-if="atCap" class="chan-cap">Max {{ MAX_CHANNELS }} channels ({{ MAX_CHANNELS * MAX_CHANNELS }} tiles). Clear one to swap.</div>
          </div>
        </div>
        <select class="tsel" v-model="transform" v-tooltip.bottom="'Axis transform (applied to every channel)'">
          <option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option>
        </select>
      </div>
    </div>

    <GateMontage ref="montageRef" :project-uid="g.projectUid()" :image-uid="g.imageUid ?? ''"
                 :value-name="g.valueName" :pop-type="g.popType" :defs="defs" :col-label="g.colLabel"
                 :render-mode="renderMode" :gate-labels="props.gateLabels" :gate-line-width="props.gateLineWidth"
                 :highlight="highlightPops" :cols="channels.length || 1" :reload-key="reloadKey">
      <template #empty>Pick channels above to compare them against each other.</template>
    </GateMontage>
  </CanvasPanel>
</template>

<style scoped>
.ro-tag { font-size: 10px; text-transform: uppercase; letter-spacing: 0.04em; color: var(--cc-text-dim);
  border: 1px solid var(--cc-border); border-radius: 3px; padding: 1px 5px; }
.ctrl-sep { width: 1px; align-self: stretch; background: var(--cc-border); margin: 2px 2px; }
.panel-ctrl { display: flex; flex-direction: column; gap: 6px; padding: 6px 8px;
  border-bottom: 1px solid var(--cc-border); font-size: 12px; }
.ax-row { display: flex; align-items: center; gap: 6px; }
.ax-lbl { width: 2.6rem; color: var(--cc-text-dim); flex-shrink: 0; }
.ax-chan { width: 12rem; flex: none; }
.tsel { width: 5.5rem; flex-shrink: 0; }
/* channel multiselect popover */
.chan-pick { position: relative; flex: 1; min-width: 0; }
.chan-btn { width: 100%; display: flex; align-items: center; justify-content: space-between; gap: 6px;
  background: var(--cc-surface-2); color: var(--cc-text); border: 1px solid var(--cc-border);
  border-radius: 5px; padding: 3px 8px; cursor: pointer; font-size: 12px; }
.chan-btn.on { border-color: var(--cc-accent); }
.chan-pop { position: absolute; top: calc(100% + 4px); left: 0; z-index: 30; width: 15rem; max-width: 80vw;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: 6px;
  box-shadow: 0 6px 18px rgba(0,0,0,0.35); padding: 6px; }
.chan-pop-head { display: flex; align-items: center; justify-content: space-between; padding: 2px 4px 6px;
  color: var(--cc-text-dim); font-size: 11px; border-bottom: 1px solid var(--cc-border); }
.chan-clear { background: none; border: none; color: var(--cc-accent); cursor: pointer; font-size: 11px; }
.chan-clear:disabled { color: var(--cc-text-dim); cursor: default; }
.chan-list { max-height: 220px; overflow-y: auto; padding: 4px 0; }
.chan-item { display: flex; align-items: center; gap: 6px; padding: 3px 4px; cursor: pointer; color: var(--cc-text); }
.chan-item:hover { background: var(--cc-surface-2); }
.chan-item.disabled { color: var(--cc-text-dim); cursor: default; }
.chan-cap { padding: 5px 4px 2px; font-size: 10px; color: var(--cc-text-dim); border-top: 1px solid var(--cc-border); }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 3px 8px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button.on { background: var(--cc-accent); color: #fff; }
.gp-export { font-size: 12px; max-width: 7rem; }
</style>
