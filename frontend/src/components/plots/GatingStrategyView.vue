<!--
  Gating-strategy (hierarchy) plot — an interactive VIEW for the Analysis board (registered in
  interactiveViews.ts). READ-ONLY (project_analysis_canvas_readonly): it visualises an existing gating
  scheme, never edits it. Ports the old R `.flowPlotGatedRaster`: walk the population tree, group each
  parent's children by their gate's channel-pair, and for each group render the PARENT's cells as a
  density scatter on those channels with the child gate outlines + "name  pct%" labels.

  Reuse (feedback_use_existing_framework): this owns ONLY the selectors + turning the population tree
  into montage tiles (`PanelDef[]`). The fetch + render + export of those tiles is the shared
  GateMontage (the SAME renderer the channel-pairs matrix uses), which in turn hosts the SAME read-only
  GateScatterCell as the Gate page. No second gate renderer, no store mutation.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef, onMounted, onUnmounted } from 'vue'
import type { GateSpec, TransformSpec, PopNode, PopTree } from '../../stores/gating'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { orientGate } from '../../plots/gateGeometry'
import type { PanelDef } from '../../plots/montage'
import type { VisProps } from '../../plots/plot'
import GateMontage from './GateMontage.vue'
import RenderModeToggle, { type RenderMode } from './RenderModeToggle.vue'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  vis?: VisProps
  state: { imageUid?: string; valueName?: string; popType?: string; rootPop?: string
    renderMode?: RenderMode; showHierarchy?: boolean }
}>()

// ── selectors (persisted in the panel state) ─────────────────────────────────────────────────────
const imageUid = computed(() => (props.state.imageUid && props.imageUids.includes(props.state.imageUid))
  ? props.state.imageUid : (props.imageUids[0] ?? ''))
const popType = computed({ get: () => props.state.popType ?? 'flow', set: v => (props.state.popType = v) })
const valueName = computed({ get: () => props.state.valueName ?? 'default', set: v => (props.state.valueName = v) })
const rootPop = computed({ get: () => props.state.rootPop ?? 'root', set: v => (props.state.rootPop = v) })
const renderMode = computed({ get: () => props.state.renderMode ?? 'points', set: v => (props.state.renderMode = v) })
// DEFAULT: a single plot for the selected population (its defining gate). Toggling "show hierarchy"
// walks the whole gating tree beneath the selected pop → the full montage (old .flowPlotGatedRaster).
const showHierarchy = computed({ get: () => props.state.showHierarchy ?? false, set: v => (props.state.showHierarchy = v) })
const setImageUid = (v: string) => (props.state.imageUid = v)

// size + the hierarchy toggle live in a ⚙ popover; close on an outside click.
const optsOpen = ref(false)
const optsRef = useTemplateRef<HTMLElement>('optsRef')
function onDocClick(e: MouseEvent) { if (optsOpen.value && optsRef.value && !optsRef.value.contains(e.target as Node)) optsOpen.value = false }
onMounted(() => document.addEventListener('mousedown', onDocClick))
onUnmounted(() => document.removeEventListener('mousedown', onDocClick))

const valueNames = ref<string[]>([])
// intensity columns + display names (aligned) → resolve raw axis keys (mean_intensity_2) to channel
// names (CD4), mirroring the gating store's colLabel so axes read the same as on the Gate page.
const channels = ref<string[]>([])
const channelNames = ref<string[]>([])
function colLabel(col: string): string {
  const i = channels.value.indexOf(col)
  return i >= 0 && channelNames.value[i] ? channelNames.value[i] : col
}
const tree = ref<PopTree | null>(null)

const idQ = () => `projectUid=${props.projectUid}&imageUid=${imageUid.value}&valueName=${encodeURIComponent(valueName.value)}&popType=${popType.value}`

async function loadChannels() {
  if (!props.projectUid || !imageUid.value) { valueNames.value = []; return }
  try {
    const d = await (await fetch(`/api/gating/channels?projectUid=${props.projectUid}&imageUid=${imageUid.value}&popType=${popType.value}`)).json()
    valueNames.value = d.valueNames ?? []
    channels.value = d.channels ?? []
    channelNames.value = d.channelNames ?? []
    if (valueNames.value.length && !valueNames.value.includes(valueName.value)) valueName.value = valueNames.value[0]
  } catch { valueNames.value = []; channels.value = []; channelNames.value = [] }
}
async function loadTree() {
  if (!props.projectUid || !imageUid.value) { tree.value = null; return }
  try {
    const d = await (await fetch(`/api/gating/popmap?${idQ()}`)).json() as { tree: PopTree }
    tree.value = d.tree ?? null
  } catch { tree.value = null }
}

// flat pop paths (for the "root population" selector)
const flatPaths = computed<string[]>(() => {
  const out: string[] = []
  const walk = (nodes: PopNode[], parent: string) => {
    for (const n of nodes) { const p = parent === 'root' ? `/${n.name}` : `${parent}/${n.name}`; out.push(p); walk(n.children ?? [], p) }
  }
  walk(tree.value?.populations ?? [], 'root')
  return out
})

// ── build the montage tiles: for each parent, group its gated children by gate channel-pair ────────
const pairKey = (a: string, b: string) => [a, b].sort().join('~~')
function childrenAt(root: string): { nodes: PopNode[]; name: string } {
  if (root === 'root') return { nodes: tree.value?.populations ?? [], name: 'all events' }
  let found: PopNode | null = null
  const walk = (nodes: PopNode[], parent: string) => {
    for (const n of nodes) { const p = parent === 'root' ? `/${n.name}` : `${parent}/${n.name}`; if (p === root) found = n; else walk(n.children ?? [], p) }
  }
  walk(tree.value?.populations ?? [], 'root')
  const f = found as PopNode | null
  return { nodes: f?.children ?? [], name: f?.name ?? root }
}
// one LEVEL: group a parent's directly-gated children by their gate channel-pair → one tile each
function groupsAt(parentPath: string, parentName: string, nodes: PopNode[]): PanelDef[] {
  const acc: PanelDef[] = []
  const groups = new Map<string, PopNode[]>()
  for (const n of nodes) if (n.gate) {
    const k = pairKey(n.gate.x_channel, n.gate.y_channel)
    ;(groups.get(k) ?? (groups.set(k, []), groups.get(k)!)).push(n)
  }
  for (const [k, group] of groups) {
    const g0 = group[0].gate!
    acc.push({
      key: `${parentPath}::${k}`, parentPath, parentName,
      xChan: g0.x_channel, yChan: g0.y_channel, xt: g0.x_transform, yt: g0.y_transform,
      children: group.map(n => {
        const path = parentPath === 'root' ? `/${n.name}` : `${parentPath}/${n.name}`
        return { path, name: n.name, colour: n.colour, gate: orientGate(n.gate!, g0.x_channel, g0.y_channel) ?? n.gate! }
      }),
    })
  }
  return acc
}
// locate a node by path AND its parent context (for the single defining-gate plot)
function nodeWithParent(target: string): { node: PopNode; parentPath: string; parentName: string } | null {
  let res: { node: PopNode; parentPath: string; parentName: string } | null = null
  const walk = (nodes: PopNode[], parentPath: string, parentName: string) => {
    for (const n of nodes) {
      const p = parentPath === 'root' ? `/${n.name}` : `${parentPath}/${n.name}`
      if (p === target) res = { node: n, parentPath, parentName }
      else walk(n.children ?? [], p, n.name)
    }
  }
  walk(tree.value?.populations ?? [], 'root', 'all events')
  return res
}
// FULL montage: recurse the tree beneath the selected pop, a tile per parent×channel-pair
const hierarchyDefs = computed<PanelDef[]>(() => {
  const acc: PanelDef[] = []
  const collect = (parentPath: string, parentName: string, nodes: PopNode[]) => {
    acc.push(...groupsAt(parentPath, parentName, nodes))
    for (const n of nodes) {
      const path = parentPath === 'root' ? `/${n.name}` : `${parentPath}/${n.name}`
      collect(path, n.name, n.children ?? [])
    }
  }
  const start = childrenAt(rootPop.value)
  collect(rootPop.value, start.name, start.nodes)
  return acc
})
// SINGLE plot: the plot that DEFINES the selected pop (its parent's density + this pop's own gate).
const singleDef = computed<PanelDef | null>(() => {
  const rp = rootPop.value
  if (rp !== 'root') {
    const nw = nodeWithParent(rp)
    if (nw?.node.gate) {
      const g0 = nw.node.gate as GateSpec
      return {
        key: `single::${rp}`, parentPath: nw.parentPath, parentName: nw.parentName,
        xChan: g0.x_channel, yChan: g0.y_channel, xt: g0.x_transform as TransformSpec, yt: g0.y_transform as TransformSpec,
        children: [{ path: rp, name: nw.node.name, colour: nw.node.colour,
                     gate: orientGate(g0, g0.x_channel, g0.y_channel) ?? g0 }],
      }
    }
  }
  const start = childrenAt(rp)
  return groupsAt(rp, start.name, start.nodes)[0] ?? null
})
const panelDefs = computed<PanelDef[]>(() =>
  showHierarchy.value ? hierarchyDefs.value : (singleDef.value ? [singleDef.value] : []))

watch([imageUid, popType], () => { loadChannels().then(loadTree) }, { immediate: true })
watch([valueName], loadTree)
// a task finishing on THIS image → gates/stats may have changed; reload the tree (cascades to tiles)
useDataRefresh(() => [imageUid.value], () => { loadChannels().then(loadTree) })

// PDF export: delegate to the shared montage renderer (single cell hi-res, or the whole grid on white).
const montageRef = useTemplateRef<{ exportImage(bg?: string, light?: boolean): Promise<string | null> }>('montageRef')
async function exportImage(): Promise<string | null> { return (await montageRef.value?.exportImage('#ffffff', true)) ?? null }
defineExpose({ exportImage })
</script>

<template>
  <div class="gs-view">
    <div class="gs-bar cc-panel-controls">
      <select v-if="imageUids.length > 1" :value="imageUid" @change="setImageUid(($event.target as HTMLSelectElement).value)"
              v-tooltip.bottom="'Image'">
        <option v-for="u in imageUids" :key="u" :value="u">{{ u }}</option>
      </select>
      <select v-model="popType" v-tooltip.bottom="'Population type'">
        <option value="flow">flow</option>
        <option value="live">live</option>
      </select>
      <select v-if="valueNames.length" v-model="valueName" v-tooltip.bottom="'Segmentation'">
        <option v-for="v in valueNames" :key="v" :value="v">{{ v }}</option>
      </select>
      <select v-model="rootPop" v-tooltip.bottom="'Start from this population'">
        <option value="root">root</option>
        <option v-for="p in flatPaths" :key="p" :value="p">{{ p }}</option>
      </select>
      <RenderModeToggle v-model="renderMode" />
      <div ref="optsRef" class="gs-opts">
        <button class="gs-gear" :class="{ on: optsOpen }" @click="optsOpen = !optsOpen"
                v-tooltip.bottom="'Plot size & hierarchy'"><i class="pi pi-cog" /></button>
        <div v-if="optsOpen" class="gs-pop">
          <label class="gs-check"><input type="checkbox" :checked="showHierarchy"
                 @change="showHierarchy = ($event.target as HTMLInputElement).checked" />
            show gating hierarchy</label>
        </div>
      </div>
    </div>

    <GateMontage ref="montageRef" :project-uid="projectUid" :image-uid="imageUid" :value-name="valueName"
                 :pop-type="popType" :defs="panelDefs" :col-label="colLabel" :render-mode="renderMode"
                 :gate-labels="true" :font-size="vis?.fontSize ?? 11">
      <template #empty>
        No gate to show for “{{ rootPop }}”.
        {{ showHierarchy ? 'No gated populations beneath it — draw gates on the Gate page first.'
                         : 'Select a gated population, or draw gates on the Gate page first.' }}
      </template>
    </GateMontage>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .gs-bar (.cc-panel-controls) anchors to the plot box, not the panel */
.gs-view { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.gs-bar { display: flex; align-items: center; gap: 6px; padding: 6px 8px; flex-wrap: wrap;
  font-size: 12px; }
.gs-bar select { font-size: 12px; max-width: 9rem; }
/* ⚙ options popover (hierarchy toggle). margin-left:auto pins the gear to the far right so the popover —
   anchored right:0 — always opens LEFTWARD into the panel and is never clipped at the left edge. */
.gs-opts { position: relative; display: inline-flex; margin-left: auto; }
.gs-gear { background: var(--cc-surface-2); color: var(--cc-text-dim); border: 1px solid var(--cc-border);
  border-radius: 5px; padding: 4px 8px; cursor: pointer; font-size: 12px; }
.gs-gear.on { background: var(--cc-accent); color: #fff; border-color: var(--cc-accent); }
.gs-pop { position: absolute; top: calc(100% + 4px); right: 0; z-index: 20; width: 13rem;
  display: flex; flex-direction: column; gap: 8px; padding: 10px; background: var(--cc-surface-1);
  border: 1px solid var(--cc-border); border-radius: 6px; box-shadow: 0 6px 18px rgba(0,0,0,0.35); }
.gs-check { display: flex; align-items: center; gap: 6px; color: var(--cc-text); font-size: 12px; }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 4px 8px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button.on { background: var(--cc-accent); color: #fff; }
</style>
