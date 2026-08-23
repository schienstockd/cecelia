<!--
  Gating-strategy (hierarchy) plot — an interactive VIEW for the Analysis board (registered in
  interactiveViews.ts). READ-ONLY (project_analysis_canvas_readonly): it visualises an existing gating
  scheme, never edits it. Ports the old R `.flowPlotGatedRaster`: walk the population tree, group each
  parent's children by their gate's channel-pair, and for each group render the PARENT's cells as a
  density scatter on those channels with the child gate outlines + "name  pct%" labels.

  It also offers COLOUR BY (docs/POPULATION.md → Colour by a third measure): a third measure painted onto
  the dots, chosen in the ⚙ options and persisted in the panel state like every other selector here, so a
  saved board reopens with the same figure. The montage draws ONE colour bar for the grid.

  Reuse (feedback_use_existing_framework): this owns ONLY the selectors + turning the population tree
  into montage tiles (`PanelDef[]`). The fetch + render + export of those tiles is the shared
  GateMontage (the SAME renderer the channel-pairs matrix uses), which in turn hosts the SAME read-only
  GateScatterCell as the Gate page. No second gate renderer, no store mutation.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef } from 'vue'
import TeleportPopover from '../TeleportPopover.vue'
import type { GateSpec, TransformSpec, PopNode, PopTree } from '../../stores/gating'
import { useDataRefresh } from '../../composables/useDataRefresh'
import { orientGate } from '../../plots/gateGeometry'
import { defaultTransformForCol } from '../../utils/gatingAxes'
import { measureGroups } from '../../utils/measureGroups'
import { pairTransform } from '../../plots/pairsMatrix'
import type { ColourBy, PanelDef } from '../../plots/montage'
import type { VisProps } from '../../plots/plot'
import GateMontage from './GateMontage.vue'
import RenderModeToggle, { type RenderMode } from './RenderModeToggle.vue'
import CcToggle from '../CcToggle.vue'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  vis?: VisProps
  state: { imageUid?: string; valueName?: string; popType?: string; rootPop?: string
    renderMode?: RenderMode; showHierarchy?: boolean
    // colour-by: the third measure painted as the dot colour ('' = density) + its ramp scale
    z?: string; zt?: 'linear' | 'log' | 'asinh' | 'logicle' }
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

// size + the hierarchy toggle live in a ⚙ popover (shared TeleportPopover — no clip, self-dismissing).
const optsOpen = ref(false)
const gearBtn = useTemplateRef<HTMLElement>('gearBtn')

const valueNames = ref<string[]>([])
// intensity columns + display names (aligned) → resolve raw axis keys (mean_intensity_2) to channel
// names (CD4), mirroring the gating store's colLabel so axes read the same as on the Gate page.
const channels = ref<string[]>([])
const channelNames = ref<string[]>([])
// the rest of the measure universe, for the colour-by picker — grouped by family through the SAME
// `measureGroups` every other picker uses, so the board's list reads like the Gate page's
const columns = ref<string[]>([])
const obsColumns = ref<string[]>([])
const spatialAxes = ref<string[]>([])
const colourGroups = computed(() => measureGroups({
  columns: columns.value, channels: channels.value, spatialAxes: spatialAxes.value,
  obsColumns: obsColumns.value, popType: popType.value }))
// COLOUR BY a third measure. No gating store here (this view is store-agnostic), so the default scale
// comes from the shared pure rule with THIS segmentation's spatial axes — same rule the Gate page uses.
type Kind = 'linear' | 'log' | 'asinh' | 'logicle'
const TRANSFORMS: Kind[] = ['linear', 'log', 'asinh', 'logicle']
const zDefault = (col: string): Kind =>
  defaultTransformForCol(col, { spatialAxes: spatialAxes.value, popType: popType.value })
const zChan = computed({ get: () => props.state.z ?? '',
                         set: v => { props.state.z = v; props.state.zt = v ? zDefault(v) : undefined } })
const zt = computed<Kind>({ get: () => props.state.zt ?? zDefault(zChan.value), set: v => { props.state.zt = v } })
const colourBy = computed<ColourBy | null>(() =>
  zChan.value ? { col: zChan.value, t: pairTransform(zt.value) } : null)
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
    columns.value = d.columns ?? []
    obsColumns.value = d.obsColumns ?? []
    spatialAxes.value = [...(d.spatialColumns ?? []), ...(d.temporalColumns ?? [])]
    if (valueNames.value.length && !valueNames.value.includes(valueName.value)) valueName.value = valueNames.value[0]
  } catch {
    valueNames.value = []; channels.value = []; channelNames.value = []
    columns.value = []; obsColumns.value = []; spatialAxes.value = []
  }
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
const montageRef = useTemplateRef<{
  exportImage(bg?: string, light?: boolean): Promise<string | null>
  exportSvg(bg?: string, light?: boolean): string
}>('montageRef')
async function exportImage(): Promise<string | null> { return (await montageRef.value?.exportImage('#ffffff', true)) ?? null }
// full vector <svg> for the board→SVG export — the montage stitches its read-only tiles (Phase A)
function exportSvg(): string | null { return montageRef.value?.exportSvg('#ffffff', true) || null }
defineExpose({ exportImage, exportSvg })
</script>

<template>
  <div class="gs-view">
    <div class="gs-bar cc-row cc-panel-controls">
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
      <div class="gs-opts">
        <button ref="gearBtn" class="gs-gear cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on': optsOpen }" @click="optsOpen = !optsOpen"
                v-tooltip.bottom="'Colour, size & hierarchy'"><i class="pi pi-cog" /></button>
        <TeleportPopover v-model="optsOpen" :anchor="gearBtn" placement="bottom-end">
          <div class="gs-pop">
            <CcToggle class="gs-check" label="show gating hierarchy"
                   v-tooltip.bottom="'Draw the parent-child tree beside the plots'"
                   :model-value="showHierarchy" @update:model-value="showHierarchy = $event" />
            <label class="gs-row"><span class="gs-lbl">colour</span>
              <select v-model="zChan" v-tooltip.bottom="'Colour the dots by a third measure (points mode)'">
                <option value="">density</option>
                <optgroup v-for="grp in colourGroups" :key="grp.title" :label="grp.title">
                  <option v-for="c in grp.cols" :key="c" :value="c">{{ colLabel(c) }}</option>
                </optgroup>
              </select>
            </label>
            <label class="gs-row"><span class="gs-lbl">scale</span>
              <select v-model="zt" :disabled="!zChan" v-tooltip.bottom="'Colour scale'">
                <option v-for="t in TRANSFORMS" :key="t" :value="t">{{ t }}</option>
              </select>
            </label>
          </div>
        </TeleportPopover>
      </div>
    </div>

    <GateMontage ref="montageRef" :project-uid="projectUid" :image-uid="imageUid" :value-name="valueName"
                 :pop-type="popType" :defs="panelDefs" :col-label="colLabel" :render-mode="renderMode"
                 :colour-by="colourBy" :gate-labels="true" :font-size="vis?.fontSize ?? 11">
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
.gs-bar { padding: 6px 8px; font-size: var(--cc-fs-sm); }
.gs-bar select { max-width: 9rem; }
/* ⚙ options popover (hierarchy toggle). margin-left:auto pins the gear to the far right so the popover —
   anchored right:0 — always opens LEFTWARD into the panel and is never clipped at the left edge. */
.gs-opts { position: relative; display: inline-flex; margin-left: auto; }
/* .gs-gear → cc-btn cc-btn-ghost cc-btn-icon */
/* inner layout only — TeleportPopover provides surface/border/shadow/position */
.gs-pop { width: 13rem; display: flex; flex-direction: column; gap: 8px; }   /* padding: TeleportPopover */
.gs-check { display: flex; align-items: center; gap: 6px; color: var(--cc-text); font-size: var(--cc-fs-sm); }
/* colour-by rows in the options popover: label + select, one per line so a long measure name doesn't
   push the select out of the popover */
.gs-row { display: flex; align-items: center; gap: 6px; font-size: var(--cc-fs-sm); }
.gs-row select { flex: 1; min-width: 0; }
.gs-lbl { width: 3rem; flex: none; color: var(--cc-text-dim); }
</style>
