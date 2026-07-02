<!--
  Gating-strategy (hierarchy) plot — an interactive VIEW for the Analysis board (registered in
  interactiveViews.ts). READ-ONLY (project_analysis_canvas_readonly): it visualises an existing gating
  scheme, never edits it. Ports the old R `.flowPlotGatedRaster`: walk the population tree, group each
  parent's children by their gate's channel-pair, and for each group render the PARENT's cells as a
  density scatter on those channels with the child gate outlines + "name  pct%" labels — a montage of
  the whole gating strategy.

  Reuse (feedback_use_existing_framework): each sub-panel is a read-only GateScatterCell (`mode='off'`)
  — the SAME renderer as the Gate page — fed by the SAME stateless gating routes GatePlotPanel uses
  (popmap / plotmeta / plotdata / stats). No second gate renderer, no store mutation.
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef, onMounted, onUnmounted } from 'vue'
import type { GateSpec, TransformSpec, PopNode, PopTree } from '../../stores/gating'
import { orientGate } from '../../plots/gateGeometry'
import { plotHostToImageURL } from '../../plots/export'
import type { VisProps } from '../../plots/plot'
import GateScatterCell from './GateScatterCell.vue'

const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  vis?: VisProps
  state: { imageUid?: string; valueName?: string; popType?: string; rootPop?: string
    renderMode?: 'points' | 'contour'; showHierarchy?: boolean }
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
// Put a gating-strategy view in a 1×1 plate and turn this on to render the full strategy in one slot.
const showHierarchy = computed({ get: () => props.state.showHierarchy ?? false, set: v => (props.state.showHierarchy = v) })
// no size control — fit to the slot: the single plot fills it; the montage lays out responsive squares
// that fit the slot width and wrap (grid columns set in CSS, so nothing to size manually).
const single = computed(() => panelDefs.value.length === 1)
const titleFor = (parentPath: string) => (parentPath === 'root' ? 'all events (root)' : parentPath)
const setImageUid = (v: string) => (props.state.imageUid = v)

// size + the hierarchy toggle live in a ⚙ popover (like the heatmap/state-plot panels) so the bar
// never widens; close on an outside click.
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
const loading = ref(false)
const err = ref('')

// axis-transform → query params (mirrors GatePlotPanel.axisQ; logicle carries its shape)
function axisQ(p: 'x' | 'y', ts: TransformSpec): string {
  let q = `&${p}t=${ts.kind}`
  if (ts.kind === 'logicle') q += `&${p}T=${ts.T ?? 262144}&${p}W=${ts.W ?? 0.5}&${p}M=${ts.M ?? 4.5}&${p}A=${ts.A ?? 0}`
  return q
}
const idQ = () => `projectUid=${props.projectUid}&imageUid=${imageUid.value}&valueName=${encodeURIComponent(valueName.value)}&popType=${popType.value}`
// x0=1&y0=1 → fixed whole-dataset axis per side, so the parent density + child gates align across panels
function plotQ(pop: string, xc: string, yc: string, xt: TransformSpec, yt: TransformSpec) {
  return `${idQ()}&x=${encodeURIComponent(xc)}&y=${encodeURIComponent(yc)}&pop=${encodeURIComponent(pop)}${axisQ('x', xt)}${axisQ('y', yt)}&x0=1&y0=1`
}

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

// ── build the panel defs: for each parent, group its gated children by gate channel-pair ──────────
interface PanelDef { key: string; parentPath: string; parentName: string; xChan: string; yChan: string
  xt: TransformSpec; yt: TransformSpec; children: { path: string; name: string; colour: string; gate: GateSpec }[] }
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
// one LEVEL: group a parent's directly-gated children by their gate channel-pair → one panel each
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
// FULL montage: recurse the tree beneath the selected pop, a panel per parent×channel-pair
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
// For root / an ungated pop, fall back to the first gated group directly beneath it.
const singleDef = computed<PanelDef | null>(() => {
  const rp = rootPop.value
  if (rp !== 'root') {
    const nw = nodeWithParent(rp)
    if (nw?.node.gate) {
      const g0 = nw.node.gate
      return {
        key: `single::${rp}`, parentPath: nw.parentPath, parentName: nw.parentName,
        xChan: g0.x_channel, yChan: g0.y_channel, xt: g0.x_transform, yt: g0.y_transform,
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

// ── fetch parent density + child stats per panel ─────────────────────────────────────────────────
type Ext = { xMin: number; xMax: number; yMin: number; yMax: number }
interface PanelData { points: Float32Array; extents: Ext; xTicks: { pos: number; label: string }[]
  yTicks: { pos: number; label: string }[]; gates: { path: string; colour: string; gate: GateSpec; label: string }[] }
const panelData = ref<Record<string, PanelData>>({})
let loadTok = 0

async function loadPanels() {
  const defs = panelDefs.value
  if (!defs.length) { panelData.value = {}; return }
  const tok = ++loadTok
  loading.value = true; err.value = ''
  try {
    const entries = await Promise.all(defs.map(async d => {
      const q = plotQ(d.parentPath, d.xChan, d.yChan, d.xt, d.yt)
      const meta = await (await fetch(`/api/gating/plotmeta?${q}`)).json() as {
        xExtent: [number, number]; yExtent: [number, number]
        xTicks: { pos: number; label: string }[]; yTicks: { pos: number; label: string }[] }
      const points = new Float32Array(await (await fetch(`/api/gating/plotdata?${q}`)).arrayBuffer())
      const gates = await Promise.all(d.children.map(async c => {
        let label = c.name
        try {
          const s = await (await fetch(`/api/gating/stats?${idQ()}&pop=${encodeURIComponent(c.path)}`)).json() as { pctParent?: number }
          // pctParent is ALREADY a percentage (backend pop_stats: 100 * cnt / pcnt) — don't ×100 again
          if (typeof s.pctParent === 'number') label = `${c.name}  ${s.pctParent.toFixed(1)}%`
        } catch { /* keep bare name */ }
        return { path: c.path, colour: c.colour, gate: c.gate, label }
      }))
      const extents: Ext = { xMin: meta.xExtent[0], xMax: meta.xExtent[1], yMin: meta.yExtent[0], yMax: meta.yExtent[1] }
      return [d.key, { points, extents, xTicks: meta.xTicks, yTicks: meta.yTicks, gates }] as const
    }))
    if (tok === loadTok) panelData.value = Object.fromEntries(entries)
  } catch (e) {
    if (tok === loadTok) { err.value = e instanceof Error ? e.message : String(e); panelData.value = {} }
  } finally { if (tok === loadTok) loading.value = false }
}

watch([imageUid, popType], () => { loadChannels().then(loadTree) }, { immediate: true })
watch([valueName], loadTree)
watch(panelDefs, loadPanels, { immediate: true })

// ── PDF export: a plot-only, LIGHT-theme image (no toolbar). Single plot → the one cell's hi-res
// composite on white; montage → snapshot the grid on white with dark ink (`cc-light` flips the vars).
const gsGridRef = useTemplateRef<HTMLElement>('gsGridRef')
type CellExport = {
  exportImage(bg?: string, light?: boolean): Promise<string | null>
  hiRes(cv: HTMLCanvasElement, scale: number): Promise<CanvasImageSource | null>
}
const cellRefs = new Map<string, CellExport>()
function setCellRef(key: string, el: unknown) { if (el) cellRefs.set(key, el as CellExport); else cellRefs.delete(key) }
async function exportImage(): Promise<string | null> {
  const defs = panelDefs.value
  if (defs.length === 1) return (await cellRefs.get(defs[0].key)?.exportImage('#ffffff', true)) ?? null
  // MONTAGE: capture the whole grid (titles + axes via the DOM overlay) but re-render each cell's canvases
  // at export scale — a combined resolver routes each canvas to its owning cell's hiRes (else the whole
  // montage would composite at screen resolution → soft).
  const el = gsGridRef.value
  if (!el) return null
  const cells = [...cellRefs.values()]
  const hiRes = async (cv: HTMLCanvasElement, scale: number) => {
    for (const c of cells) { const r = await c.hiRes?.(cv, scale); if (r) return r }
    return null
  }
  const scale = Math.min(8, Math.max(4, Math.ceil(2800 / (el.clientWidth || 500))))
  el.classList.add('cc-light')
  try { return await plotHostToImageURL(el, '#ffffff', { hiRes, scale }) } finally { el.classList.remove('cc-light') }
}
defineExpose({ exportImage })
</script>

<template>
  <div class="gs-view">
    <div class="gs-bar">
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
      <div class="seg" v-tooltip.bottom="'Render: points / density contour'">
        <button :class="{ on: renderMode === 'points' }" @click="renderMode = 'points'"><i class="pi pi-circle-fill" /></button>
        <button :class="{ on: renderMode === 'contour' }" @click="renderMode = 'contour'"><i class="pi pi-chart-line" /></button>
      </div>
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

    <div ref="gsGridRef" class="gs-grid" :class="{ single }">
      <div v-if="err" class="gs-msg">{{ err }}</div>
      <div v-else-if="!panelDefs.length" class="gs-msg">
        No gate to show for “{{ rootPop }}”.
        {{ showHierarchy ? 'No gated populations beneath it — draw gates on the Gate page first.'
                         : 'Select a gated population, or draw gates on the Gate page first.' }}
      </div>
      <div v-for="d in panelDefs" :key="d.key" class="gs-cell">
        <div class="gs-title" v-tooltip.top="`derived from ${titleFor(d.parentPath)}`">{{ titleFor(d.parentPath) }}</div>
        <GateScatterCell v-if="panelData[d.key]" class="gs-plot" :ref="el => setCellRef(d.key, el)"
                         :points="panelData[d.key].points" :extents="panelData[d.key].extents"
                         :view-extents="panelData[d.key].extents"
                         :x-ticks="panelData[d.key].xTicks" :y-ticks="panelData[d.key].yTicks"
                         :gates="panelData[d.key].gates" :x-label="colLabel(d.xChan)" :y-label="colLabel(d.yChan)"
                         :render-mode="renderMode" mode="off" :gate-labels="true" :compact="!single" :readonly="true" />
        <div v-else class="gs-loading">…</div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.gs-view { display: flex; flex-direction: column; height: 100%; min-height: 0; }
.gs-bar { display: flex; align-items: center; gap: 6px; padding: 6px 8px; flex-wrap: wrap; flex-shrink: 0;
  border-bottom: 1px solid var(--cc-border); font-size: 12px; }
.gs-bar select { font-size: 12px; max-width: 9rem; }
/* fit to the slot — no size control. Montage: responsive squares that fill the slot width and wrap
   (scroll if many). Single: one cell that fills the whole slot. */
.gs-grid { flex: 1; min-height: 0; overflow-y: auto; padding: 6px; display: grid; gap: 8px;
  grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); align-content: start; }
.gs-grid.single { grid-template-columns: 1fr; grid-template-rows: 1fr; overflow: hidden; }
/* light theme for PDF export: flip the ink/border vars so titles + axes read dark on white */
.gs-grid.cc-light { --cc-text: #111; --cc-text-dim: #555; --cc-border: #c9ccd1; --cc-bg: #fff; --cc-surface-2: #f0f0f3; }
.gs-cell { display: flex; flex-direction: column; min-height: 0; border: 1px solid var(--cc-border);
  border-radius: 5px; overflow: hidden; background: var(--cc-bg); }
/* montage: the plot area is SQUARE. Cap the width so a wide column doesn't make the square so TALL that
   its bottom (the x-axis label) drops past the slot; centred when narrower than the column. */
.gs-plot { flex: none; width: 100%; max-width: 320px; aspect-ratio: 1; margin: 0 auto; }
/* single: keep the square aspect ratio but fit the slot — the largest square that fits the cell's
   min(width, height − title), centred horizontally (title bar stays full-width). */
.gs-grid.single .gs-cell { container-type: size; }
/* min-height:0 overrides GateScatterCell's 218px floor so the square can shrink to fit a small slot */
.gs-grid.single .gs-plot { flex: none; aspect-ratio: 1; min-height: 0; max-width: none;
  width: min(100cqw, calc(100cqh - 26px)); margin: 0 auto; }
/* ⚙ options popover (size + hierarchy toggle). margin-left:auto pins the gear to the far right of the
   toolbar (even when it wraps to its own row), so the popover — anchored right:0 — always opens LEFTWARD
   into the panel and is never clipped at the left edge. */
.gs-opts { position: relative; display: inline-flex; margin-left: auto; }
.gs-gear { background: var(--cc-surface-2); color: var(--cc-text-dim); border: 1px solid var(--cc-border);
  border-radius: 5px; padding: 4px 8px; cursor: pointer; font-size: 12px; }
.gs-gear.on { background: var(--cc-accent); color: #fff; border-color: var(--cc-accent); }
/* opens leftward into the panel from the (right-pinned) gear; kept narrow so it fits a small slot */
.gs-pop { position: absolute; top: calc(100% + 4px); right: 0; z-index: 20; width: 13rem;
  display: flex; flex-direction: column; gap: 8px; padding: 10px; background: var(--cc-surface-1);
  border: 1px solid var(--cc-border); border-radius: 6px; box-shadow: 0 6px 18px rgba(0,0,0,0.35); }
.gs-check { display: flex; align-items: center; gap: 6px; color: var(--cc-text); font-size: 12px; }
/* flex-shrink:0 so the title bar can never be squeezed to nothing by the plot in a short/constrained
   cell (it's a flex-column child alongside the plot) */
.gs-title { flex-shrink: 0; font-size: 11px; font-weight: 700; padding: 3px 6px; color: var(--cc-text-dim);
  border-bottom: 1px solid var(--cc-border); background: var(--cc-surface-2); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.gs-loading { flex: 1; display: flex; align-items: center; justify-content: center; color: var(--cc-text-dim); }
.gs-msg { grid-column: 1 / -1; padding: 16px; color: var(--cc-text-dim); font-size: 13px; }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 4px 8px; cursor: pointer; font-size: 12px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button.on { background: var(--cc-accent); color: #fff; }
</style>
