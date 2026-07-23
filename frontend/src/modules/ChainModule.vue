<script setup lang="ts">
import { ref, computed, watch, onMounted, onActivated, markRaw, nextTick } from 'vue'
defineOptions({ name: 'ChainModule' })
import {
  VueFlow, useVueFlow,
  type Node, type Edge,
  type NodeMouseEvent,
  type NodeTypesObject,
} from '@vue-flow/core'
import { Background } from '@vue-flow/background'
import '@vue-flow/core/dist/style.css'
import '@vue-flow/core/dist/theme-default.css'
import ChainTaskNode from '../components/ChainTaskNode.vue'
import ChainStartNode from '../components/ChainStartNode.vue'
import ChainPicnicNode from '../components/ChainPicnicNode.vue'
import ChainLiveNode from '../components/ChainLiveNode.vue'
import ChainLiveLabel from '../components/ChainLiveLabel.vue'
import ChainQcNode from '../components/ChainQcNode.vue'
import SummaryCanvas from '../components/canvas/SummaryCanvas.vue'
import ConfirmDeleteButton from '../components/ConfirmDeleteButton.vue'
import ParamRenderer from '../tasks/ParamRenderer.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import TeleportPopover from '../components/TeleportPopover.vue'
import PoolThrottle from '../components/PoolThrottle.vue'
import ChipSelect, { type ChipOption } from '../components/ChipSelect.vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useProjectStore } from '../stores/project'
import { useTaskStore, type TaskStatus } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useLogStore } from '../stores/log'
import type { TaskDef, ChainTemplate } from '../tasks/types'
import { isExcluded, includedUids } from '../utils/inclusion'
import { START_ID, startTargetsOf, touchesStart, buildStartGraph } from '../utils/startDot'

// ── Stores & composables ─────────────────────────────────────────────────────

const projectMeta = useProjectMetaStore()
const project     = useProjectStore()
const taskStore   = useTaskStore()
const ws          = useWsStore()
const log         = useLogStore()

// ── Tab: "edit" | "live" ─────────────────────────────────────────────────────
const activeTab = ref<'edit' | 'live'>('edit')
const tabOptions = computed<ChipOption[]>(() => {
  const running = chainTasks.value.filter(t => t.status === 'running').length
  return [
    { value: 'edit', label: 'Edit', icon: 'pi pi-pencil' },
    { value: 'live', label: 'Live', icon: 'pi pi-bolt', badge: running || undefined },
  ]
})

// live scheduler throttle — same PoolThrottle popover as the Task Manager / module pages,
// off the live toolbar so pool limits can be nudged while a chain run is in flight
const throttleBtn  = ref<HTMLElement | null>(null)
const throttleOpen = ref(false)

const {
  nodes, edges,
  addNodes, addEdges, removeNodes, removeEdges,
  findNode, updateNode, toObject,
  onConnect, onNodeClick, onEdgeDoubleClick,
  screenToFlowCoordinate, setCenter,
} = useVueFlow({ id: 'chain-whiteboard' })

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const nodeTypes: NodeTypesObject = {
  task:      markRaw(ChainTaskNode)   as any,
  start:     markRaw(ChainStartNode)  as any,
  picnic:    markRaw(ChainPicnicNode) as any,
  live:      markRaw(ChainLiveNode)   as any,
  liveLabel: markRaw(ChainLiveLabel)  as any,
  qc:        markRaw(ChainQcNode)      as any,
}

// ── Live view ─────────────────────────────────────────────────────────────────

const selectedRunId = ref<string>('')

// A run's tasks come from ONE of two sources: the in-memory task store (a run happening/just
// happened this session) OR a persisted run.json loaded from disk (a past run — survives reload).
// Both are normalised to this shape so the layout/nodes/edges code is source-agnostic.
interface LiveTaskLike {
  id: string; chainRunId: string; chainNodeId: string; imageUid: string
  funName: string; label?: string; status: TaskStatus
  startedAt?: number; finishedAt?: number; chainName?: string
}

// Live tasks (from WS events) for this project.
const chainTasks = computed(() =>
  taskStore.tasks.filter(t => !!t.chainRunId && t.projectUid === projectMeta.current?.uid)
)
const liveRunIds = computed(() => [...new Set(chainTasks.value.map(t => t.chainRunId!))])

// Persisted runs listed from disk (GET /api/chains/runs) + a cache of fully-loaded ones.
interface RunMeta { runId: string; chainName: string; createdAt: number; imageCount: number }
interface LoadedRun { chainName: string; createdAt: number; nodes: LiveTemplateNode[]; edges: { from: string; to: string }[]; tasks: LiveTaskLike[] }
const persistedRuns = ref<RunMeta[]>([])
const loadedRuns = ref<Map<string, LoadedRun>>(new Map())

async function loadRunList() {
  const uid = projectMeta.current?.uid
  if (!uid) { persistedRuns.value = []; return }
  try {
    const res = await fetch(`/api/chains/runs?projectUid=${uid}`)
    if (res.ok) persistedRuns.value = ((await res.json()).runs ?? []) as RunMeta[]
  } catch { /* non-critical */ }
}

// Load a persisted run's frozen template + per-node status, synthesising task-like entries.
async function loadRun(runId: string) {
  if (loadedRuns.value.has(runId)) return
  const uid = projectMeta.current?.uid
  if (!uid) return
  try {
    const res = await fetch(`/api/chains/run?projectUid=${uid}&runId=${encodeURIComponent(runId)}`)
    if (!res.ok) return
    const r = await res.json() as { chainName: string; createdAt: number
      nodes: LiveTemplateNode[]; edges: { from: string; to: string }[]
      imageStates: Record<string, Record<string, string>> }
    const nodeFn = new Map(r.nodes.map(n => [n.id, n.fn]))
    const labelOf = (fn: string) => allTaskDefs.value.find(d => d.fun_name === fn)?.label ?? fn.split('.').pop() ?? fn
    const tasks: LiveTaskLike[] = []
    for (const [uid2, nm] of Object.entries(r.imageStates)) {
      for (const [nid, status] of Object.entries(nm)) {
        const fn = nodeFn.get(nid) ?? nid
        tasks.push({ id: `${runId}:${nid}:${uid2}`, chainRunId: runId, chainNodeId: nid,
          imageUid: uid2, funName: fn, label: labelOf(fn), status: status as TaskStatus, chainName: r.chainName })
      }
    }
    const m = new Map(loadedRuns.value)
    m.set(runId, { chainName: r.chainName, createdAt: r.createdAt, nodes: r.nodes, edges: r.edges, tasks })
    loadedRuns.value = m
  } catch { /* non-critical */ }
}

// Dropdown options: persisted runs ∪ live runs, newest first, each with a timestamp for context.
interface RunOption { runId: string; chainName: string; createdAt: number; live: boolean }
const runOptions = computed<RunOption[]>(() => {
  const map = new Map<string, RunOption>()
  for (const r of persistedRuns.value)
    map.set(r.runId, { runId: r.runId, chainName: r.chainName, createdAt: r.createdAt, live: false })
  for (const id of liveRunIds.value) {
    const t = chainTasks.value.find(t => t.chainRunId === id)
    const created = map.get(id)?.createdAt ?? (t?.startedAt ? t.startedAt.getTime() / 1000 : 0)
    map.set(id, { runId: id, chainName: t?.chainName ?? map.get(id)?.chainName ?? '', createdAt: created, live: true })
  }
  return [...map.values()].sort((a, b) => b.createdAt - a.createdAt)
})

function fmtRunTime(sec: number): string {
  if (!sec) return ''
  return new Date(sec * 1000).toLocaleString(undefined,
    { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' })
}
function runLabel(o: RunOption): string {
  const base = o.chainName ? `${o.chainName} / ${o.runId}` : o.runId
  const ts = fmtRunTime(o.createdAt)
  return `${base}${ts ? ` · ${ts}` : ''}${o.live ? ' · live' : ''}`
}

// Tasks for the selected run. Persisted (run.json) is the full frozen graph; live (task store) is
// this session's in-flight nodes. On a RESUME both exist: only the re-run nodes emit live events, so
// we OVERLAY live status onto the persisted snapshot (by node+image) rather than replacing it — else
// the skipped :done nodes would vanish from the graph while the resumed section runs.
const selectedRunTasks = computed<LiveTaskLike[]>(() => {
  const rid = selectedRunId.value
  if (!rid) return []
  const live = chainTasks.value.filter(t => t.chainRunId === rid).map(t => ({
    id: t.id, chainRunId: t.chainRunId!, chainNodeId: t.chainNodeId!, imageUid: t.imageUid,
    funName: t.funName, label: t.label, status: t.status,
    startedAt: t.startedAt?.getTime(), finishedAt: t.finishedAt?.getTime(), chainName: t.chainName,
  } as LiveTaskLike))
  const persisted = loadedRuns.value.get(rid)?.tasks ?? []
  if (!live.length) return persisted
  if (!persisted.length) return live
  const key = (t: LiveTaskLike) => `${t.chainNodeId}::${t.imageUid}`
  const liveByKey = new Map(live.map(t => [key(t), t]))
  const persistedKeys = new Set(persisted.map(key))
  const merged = persisted.map(t => liveByKey.get(key(t)) ?? t)     // live wins where present
  for (const t of live) if (!persistedKeys.has(key(t))) merged.push(t)   // + any brand-new node
  return merged
})

// Auto-select: focus a newly-appeared live run; else keep a valid selection, else newest.
watch(runOptions, (opts, old) => {
  if (!opts.length) return
  const fresh = opts.find(o => o.live && !(old ?? []).some(p => p.runId === o.runId))
  if (fresh) { selectedRunId.value = fresh.runId; return }
  if (!opts.some(o => o.runId === selectedRunId.value)) selectedRunId.value = opts[0].runId
}, { immediate: true })

// ── Live-run layout: rows = images, columns = tasks in execution order ──────────
// The run reads left→right along the chain (import → … → segment), one row per image, with edges
// linking each row's tasks so fan-out (one node → two branches) is visible. Node order and edges
// come from the run's chain template; fetched by name (falls back to a task-derived layout if the
// template is gone). Copy run ID lets you reference a run in logs / REPL (load_chain_run).

interface LiveTemplateNode { id: string; fn: string; params?: Record<string, unknown> }
interface LiveTemplate { nodes: LiveTemplateNode[]; edges: { from: string; to: string }[] }
const liveTemplate = ref<LiveTemplate | null>(null)

// Topological order of node ids (Kahn) — the execution order used for the columns.
function topoOrder(nodes: { id: string }[], edges: { from: string; to: string }[]): string[] {
  const indeg = new Map(nodes.map(n => [n.id, 0]))
  const succ  = new Map(nodes.map(n => [n.id, [] as string[]]))
  for (const e of edges) {
    if (!indeg.has(e.to) || !succ.has(e.from)) continue
    indeg.set(e.to, (indeg.get(e.to) ?? 0) + 1)
    succ.get(e.from)!.push(e.to)
  }
  const q = nodes.filter(n => (indeg.get(n.id) ?? 0) === 0).map(n => n.id)
  const out: string[] = []
  while (q.length) {
    const id = q.shift()!
    out.push(id)
    for (const c of succ.get(id) ?? []) {
      indeg.set(c, indeg.get(c)! - 1)
      if (indeg.get(c) === 0) q.push(c)
    }
  }
  for (const n of nodes) if (!out.includes(n.id)) out.push(n.id)  // cycle safety
  return out
}

// Template (column order + edges) for the selected run. A persisted run carries its own FROZEN
// template (nodes/edges from run.json); a live run fetches the current template by chain name.
watch(selectedRunId, async (runId) => {
  liveTemplate.value = null
  if (!runId) return
  const uid = projectMeta.current?.uid
  if (!uid) return
  const isLive = liveRunIds.value.includes(runId)
  if (!isLive) await loadRun(runId)                 // persisted → load template + states from disk
  const loaded = loadedRuns.value.get(runId)
  if (loaded) { liveTemplate.value = { nodes: loaded.nodes, edges: loaded.edges }; return }
  const chain = chainTasks.value.find(t => t.chainRunId === runId)?.chainName
  if (!chain) return
  try {
    const res = await fetch(`/api/chains/get?projectUid=${uid}&name=${encodeURIComponent(chain)}`)
    if (!res.ok) return
    const t = await res.json() as LiveTemplate
    liveTemplate.value = { nodes: t.nodes ?? [], edges: t.edges ?? [] }
  } catch { /* fall back to task-derived layout (no edges) */ }
}, { immediate: true })

const LIVE = { colW: 190, laneH: 84, bandGap: 30, padX: 150, padY: 40 }

function imageName(uid: string): string {
  for (const s of project.sets) {
    const img = s.images.find(i => i.uid === uid)
    if (img) return img.name
  }
  return uid.slice(0, 8)
}

// A distinguishing suffix for a node — the value_name it produces (or consumes), so the two
// branches of a fan-out (same fn, different output like "T" vs "default") are told apart.
function nodeVariant(nodeId: string): string {
  const p = liveTemplate.value?.nodes.find(n => n.id === nodeId)?.params ?? {}
  const v = (p as Record<string, unknown>).outputValueName ?? (p as Record<string, unknown>).valueName
  return v ? String(v) : ''
}

// Layered layout of the run's DAG (shared by nodes + edges):
//   layer = longest path from a root  → the X column (execution depth)
//   lane  = index of the node within its layer → the Y offset inside an image's band
// Fan-out siblings share a layer (same column) but get different lanes, so a branch visibly
// splits into parallel tracks. Each image is one band; a band is `bandLanes` tall.
const liveLayout = computed(() => {
  const tasks = selectedRunTasks.value
  const taskNodeIds = new Set(tasks.map(t => t.chainNodeId))
  const tmpl = liveTemplate.value
  let nodes: LiveTemplateNode[] = tmpl?.nodes.filter(n => taskNodeIds.has(n.id)) ?? []
  let edges = tmpl?.edges.filter(e => taskNodeIds.has(e.from) && taskNodeIds.has(e.to)) ?? []
  if (!nodes.length) {   // fallback: no template → linear, no edges
    nodes = [...taskNodeIds].map(id => ({ id, fn: '' }))
    edges = []
  }
  const order = topoOrder(nodes, edges)
  const preds = new Map(nodes.map(n => [n.id, [] as string[]]))
  for (const e of edges) preds.get(e.to)?.push(e.from)
  const layer = new Map<string, number>()
  for (const id of order) {
    const ps = preds.get(id) ?? []
    layer.set(id, ps.length ? Math.max(...ps.map(p => layer.get(p) ?? 0)) + 1 : 0)
  }
  const perLayer = new Map<number, number>()
  const lane = new Map<string, number>()
  for (const id of order) {
    const L = layer.get(id)!
    const k = perLayer.get(L) ?? 0
    lane.set(id, k)
    perLayer.set(L, k + 1)
  }
  const bandLanes = Math.max(1, ...perLayer.values())
  const imageIds = [...new Set(tasks.map(t => t.imageUid))]
    .sort((a, b) => imageName(a).localeCompare(imageName(b)))
  return { tasks, edges, layer, lane, bandLanes, imageIds }
})

const liveNodes = computed<Node[]>(() => {
  const { tasks, layer, lane, bandLanes, imageIds } = liveLayout.value
  if (!tasks.length) return []
  const rowOf   = new Map(imageIds.map((id, i) => [id, i]))
  const bandH   = bandLanes * LIVE.laneH
  const gridTop = LIVE.padY + qcBandH.value        // leave room for the QC band above the grid
  const bandY   = (r: number) => gridTop + r * (bandH + LIVE.bandGap)
  const nodes: Node[] = []

  // QC band: for each QC-producing column, one thumbnail per value_name (B, T, …) stacked vertically,
  // aligned to the column, above the grid.
  if (showQc.value) {
    for (const col of qcColumns.value) {
      qcValueNames.value.forEach((vn, i) => {
        const s = qcData.value.get(qcKey(col.nodeId, vn)) ?? {}
        nodes.push({
          id: `qc:${col.nodeId}::${vn}`, type: 'qc',
          position: { x: LIVE.padX + col.layer * LIVE.colW, y: 10 + i * QC_THUMB_H },
          data: { label: col.label, valueName: vn,
                  total: s.total, values: s.values, imageCount: s.imageCount, loading: s.loading },
          draggable: false, selectable: false, connectable: false,
        })
      })
    }
  }

  // Row header (image name), vertically centred in the band.
  imageIds.forEach(uid => nodes.push({
    id: `row:${uid}`, type: 'liveLabel',
    position: { x: 8, y: bandY(rowOf.get(uid)!) + (bandH - 20) / 2 },
    data: { text: imageName(uid), sub: uid.slice(0, 6), kind: 'row' },
    draggable: false, selectable: false, connectable: false,
  }))
  // Task nodes, placed at (layer → x, lane → y within band).
  for (const t of tasks) {
    const L = layer.get(t.chainNodeId)
    const K = lane.get(t.chainNodeId)
    const r = rowOf.get(t.imageUid)
    if (L === undefined || K === undefined || r === undefined) continue
    nodes.push({
      id: t.id, type: 'live',
      position: { x: LIVE.padX + L * LIVE.colW, y: bandY(r) + K * LIVE.laneH },
      data: {
        fn: t.funName, label: t.label, variant: nodeVariant(t.chainNodeId),
        imageUid: t.imageUid, status: t.status,
        startedAt: t.startedAt, finishedAt: t.finishedAt,
        nodeId: t.chainNodeId,                       // template node id — for "resume from here"
        restart: t.chainNodeId === restartNodeId.value ? 'start'
               : rerunNodeIds.value.has(t.chainNodeId) ? 'rerun' : undefined,
      },
      draggable: false, selectable: false, connectable: false,
    })
  }
  return nodes
})

// Edges: each DAG edge replicated per image band, linking that band's task nodes. A fan-out node
// (afDriftCorrect → two segmentations) has two outgoing edges to two lanes → a visible split.
const liveEdges = computed<Edge[]>(() => {
  const { tasks, edges, imageIds } = liveLayout.value
  if (!edges.length || !tasks.length) return []
  const idOf = new Map(tasks.map(t => [`${t.chainNodeId}::${t.imageUid}`, t.id]))
  const out: Edge[] = []
  for (const uid of imageIds) {
    for (const e of edges) {
      const s = idOf.get(`${e.from}::${uid}`)
      const d = idOf.get(`${e.to}::${uid}`)
      if (s && d) out.push({
        id: `${s}->${d}`, source: s, target: d,
        style: { stroke: 'var(--cc-border, #3f3f46)', strokeWidth: 1.5 },
      })
    }
  }
  return out
})

function copyRunId() {
  if (!selectedRunId.value) return
  navigator.clipboard?.writeText(selectedRunId.value)
    .then(() => log.info(`Copied run ID ${selectedRunId.value}`, { source: 'whiteboard' }))
    .catch(() => { /* clipboard blocked — non-critical */ })
}

// ── Live QC row ────────────────────────────────────────────────────────────────
// A task whose def declares `qcPlot` gets an aggregate QC thumbnail in a band above the grid,
// aligned to its column (segmentation → cell count over the run's images). Toggle to show/hide;
// click a thumbnail to expand the full QC panel. Refreshes as images clear the stage (incremental).
const showQc = ref(true)
interface QcSummary { total?: number; values?: number[]; imageCount?: number; loading?: boolean }
const qcData = ref<Map<string, QcSummary>>(new Map())          // keyed by `${nodeId}::${valueName}`
const qcExpand = ref<{ label: string; valueName: string; imageUids: string[] } | null>(null)
const qcKey = (nodeId: string, vn: string) => `${nodeId}::${vn}`

function defFor(fn: string) { return allTaskDefs.value.find(d => d.fun_name === fn) }

// The segmentations (value_names) produced in this run, discovered from the canonical populations
// picker (popType=labels → one entry per value_name). Segmentations are shared across the run's
// images (same pipeline), so the first image is representative. Drives one QC thumbnail per value_name.
const qcValueNames = ref<string[]>([])
async function loadQcValueNames() {
  const uid = projectMeta.current?.uid
  const first = liveLayout.value.imageIds[0]
  if (!uid || !first) { qcValueNames.value = []; return }
  try {
    const q = `projectUid=${uid}&imageUid=${first}&popType=labels`
    const res = await fetch(`/api/plots/populations?${q}`)
    const groups = res.ok ? (await res.json() as { valueName: string }[]) : []
    qcValueNames.value = groups.map(g => g.valueName)
  } catch { qcValueNames.value = [] }
}

// QC-producing columns in the selected run: distinct nodes whose task declares a qcPlot.
const qcColumns = computed(() => {
  const { tasks, layer } = liveLayout.value
  const seen = new Map<string, { nodeId: string; label: string; layer: number }>()
  for (const t of tasks) {
    if (seen.has(t.chainNodeId)) continue
    const def = defFor(t.funName)
    if (def?.qcPlot) seen.set(t.chainNodeId, {
      nodeId: t.chainNodeId, label: def.label ?? t.funName, layer: layer.get(t.chainNodeId) ?? 0,
    })
  }
  return [...seen.values()]
})
// Each QC column shows one thumbnail per value_name (B, T, …), stacked vertically. Band height fits
// the tallest stack.
const QC_THUMB_H = 92
const qcBandH = computed(() =>
  (showQc.value && qcColumns.value.length && qcValueNames.value.length)
    ? qcValueNames.value.length * QC_THUMB_H : 0)

// Fetch the QC cell count per image for each (QC column × value_name) of the selected run, via the
// canonical /api/plot_data (popType=labels, chartType=count) — one request per image (no set handle
// here), so `values` is the per-image count series and `total` their sum. Debounced; re-runs as tasks
// complete so the count fills in during a live run.
let qcFetchTimer: ReturnType<typeof setTimeout> | null = null
function scheduleQcFetch() {
  if (qcFetchTimer) clearTimeout(qcFetchTimer)
  qcFetchTimer = setTimeout(fetchQcData, 250)
}
async function fetchQcData() {
  const uid = projectMeta.current?.uid
  const { imageIds } = liveLayout.value
  if (!uid || !showQc.value || !qcColumns.value.length || !imageIds.length || !qcValueNames.value.length) return
  for (const col of qcColumns.value) {
    for (const vn of qcValueNames.value) {
      const key = qcKey(col.nodeId, vn)
      const cur = new Map(qcData.value)
      cur.set(key, { ...(cur.get(key) ?? {}), loading: true })
      qcData.value = cur
      try {
        const counts = await Promise.all(imageIds.map(async imageUid => {
          const res = await fetch('/api/plot_data', {
            method: 'POST', headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              projectUid: uid, imageUid, popType: 'labels', chartType: 'count',
              series: [{ valueName: vn, pop: '/labels' }],
            }),
          })
          if (!res.ok) return 0
          const r = await res.json() as { series?: { value?: number }[] }
          return (r.series ?? []).reduce((a, s) => a + Number(s.value ?? 0), 0)
        }))
        const summary: QcSummary = {
          loading: false, imageCount: imageIds.length,
          values: counts, total: counts.reduce((a, b) => a + b, 0),
        }
        const m = new Map(qcData.value); m.set(key, summary); qcData.value = m
      } catch {
        const m = new Map(qcData.value); m.set(key, { loading: false }); qcData.value = m
      }
    }
  }
}

// Refresh when the run, its completed-task count, the toggle, or the discovered value_names change.
const doneCount = computed(() => selectedRunTasks.value.filter(t => t.status === 'done').length)
watch([selectedRunId, () => liveLayout.value.imageIds.join(',')], loadQcValueNames, { immediate: true })
watch([selectedRunId, showQc, doneCount, qcColumns, qcValueNames],
      () => { qcData.value = new Map(); scheduleQcFetch() })

// A live-graph node click does one of two things by node type:
//  • a QC thumbnail (`qc:${nodeId}::${valueName}`) → expand the full segmentation QC canvas;
//  • a task node → pick/unpick it as the RESUME START NODE (re-run from here). The picked node and
//    everything downstream are then highlighted so it's obvious what a Resume will re-run.
function onLiveNodeClick(node: { id: string; data?: Record<string, unknown> }) {
  if (node.id.startsWith('qc:')) {
    const [, vn] = node.id.slice(3).split('::')
    if (!vn) return
    qcExpand.value = { label: 'Segmentation QC', valueName: vn, imageUids: liveLayout.value.imageIds }
    return
  }
  const nid = node.data?.nodeId as string | undefined
  if (!nid || resumeBusy.value) return                          // no picking a start node mid-run
  restartNodeId.value = restartNodeId.value === nid ? null : nid
}

// ── Resume ───────────────────────────────────────────────────────────────────
// The chosen "resume from here" node (a chain template node id), and everything downstream of it —
// what a Resume with a start node will re-run. Used to highlight the graph and label the button.
const restartNodeId = ref<string | null>(null)
const rerunNodeIds = computed<Set<string>>(() => {
  const start = restartNodeId.value
  const edges = liveTemplate.value?.edges
  if (!start || !edges) return new Set()
  const succ = new Map<string, string[]>()
  for (const e of edges) (succ.get(e.from) ?? succ.set(e.from, []).get(e.from)!).push(e.to)
  const out = new Set<string>([start]); const q = [start]
  while (q.length) { for (const c of succ.get(q.shift()!) ?? []) if (!out.has(c)) { out.add(c); q.push(c) } }
  return out
})
const restartLabel = computed(() => {
  if (!restartNodeId.value) return ''
  const n = liveTemplate.value?.nodes.find(nn => nn.id === restartNodeId.value)
  return n ? (allTaskDefs.value.find(d => d.fun_name === n.fn)?.label ?? n.fn.split('.').pop() ?? n.fn) : ''
})
// A run is busy (can't resume) while any of its nodes are running/queued.
const resumeBusy = computed(() =>
  selectedRunTasks.value.some(t => t.status === 'running' || t.status === 'queued'))
// clear the start-node pick when switching runs (it refers to the selected run's template)
watch(selectedRunId, () => { restartNodeId.value = null })

function resumeRun() {
  const uid = projectMeta.current?.uid
  const rid = selectedRunId.value
  if (!uid || !rid || resumeBusy.value) return
  ws.send({
    type:       'chain:run',
    projectUid: uid,
    runId:      rid,
    ...(restartNodeId.value ? { startNode: restartNodeId.value } : {}),
  })
  restartNodeId.value = null
}

// ── Chain list & selection ───────────────────────────────────────────────────

const chainNames   = ref<string[]>([])
const activeChain  = ref<string>('')
const newChainName = ref<string>('')
const showNewInput = ref(false)
const saving       = ref(false)

async function loadChainList() {
  const uid = projectMeta.current?.uid
  if (!uid) { chainNames.value = []; return }
  try {
    const res = await fetch(`/api/chains?projectUid=${uid}`)
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    const data = await res.json() as { chains: string[] }
    chainNames.value = data.chains ?? []
    if (chainNames.value.length && !chainNames.value.includes(activeChain.value)) {
      await switchChain(chainNames.value[0])
    }
  } catch (e) {
    log.warn(`Could not load chain list: ${e}`, { source: 'whiteboard' })
  }
}

async function switchChain(name: string) {
  if (!name) return
  activeChain.value = name
  await loadChain(name)
}

async function loadChain(name: string) {
  const uid = projectMeta.current?.uid
  if (!uid) return
  try {
    const res = await fetch(`/api/chains/get?projectUid=${uid}&name=${encodeURIComponent(name)}`)
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    const tmpl = await res.json() as ChainTemplate & { positions?: Record<string, {x:number; y:number}> }
    applyTemplate(tmpl, tmpl.positions ?? {})
  } catch (e) {
    log.warn(`Could not load chain "${name}": ${e}`, { source: 'whiteboard' })
  }
}

// the single UML start dot (an initial node, not a task) — START_ID + pure round-trip in utils/startDot
const hasStartNode = computed(() => nodes.value.some(n => n.id === START_ID))

// Add the start dot once, near the top-left; the user drags it and links it to the first task(s).
function addStartNode() {
  if (hasStartNode.value) return
  addNodes([{ id: START_ID, type: 'start', position: { x: 20, y: 40 }, data: {} }])
}

function applyTemplate(
  tmpl: ChainTemplate,
  positions: Record<string, { x: number; y: number }>,
) {
  // Clear canvas
  removeNodes(nodes.value.map(n => n.id))
  removeEdges(edges.value.map(e => e.id))
  selectedNodeId.value = null

  const newNodes: Node[] = tmpl.nodes.map((n, i) => {
    const def = allTaskDefs.value.find(d => d.fun_name === n.fn)
    // Merge saved params over task-def defaults so nodes created before a param was added
    // (or with empty params) still get sensible defaults rather than running with nothing.
    const defaults: Record<string, unknown> = {}
    for (const p of def?.params ?? []) {
      if (p.default !== undefined) defaults[p.key] = p.default
    }
    return {
      id:       n.id,
      type:     n.scope === 'set' ? 'picnic' : 'task',
      position: positions[n.id] ?? { x: 80 + i * 220, y: 120 },
      data: {
        fn:              n.fn,
        scope:           n.scope,
        params:          { ...defaults, ...n.params },
        barrier_policy:  n.barrier_policy,
        resource_pool:   n.resource_pool || def?.resource_pool || 'default',
        label:           def?.label ?? n.fn.split('.').pop() ?? n.fn,
      },
    }
  })

  const newEdges: Edge[] = tmpl.edges.map(e => ({
    id:     `${e.from}->${e.to}`,
    source: e.from,
    target: e.to,
    style:  { stroke: 'var(--cc-accent, #a78bfa)' },
  }))

  // Reconstruct the UML start dot + its edges (pure logic in utils/startDot): links restored from the
  // persisted startTargets (dropping any since-deleted), position from the reserved '__start__' key —
  // and the dot is kept even when unlinked if a position was saved (so the default dot survives reload).
  const start = buildStartGraph(
    tmpl.startTargets,
    new Set(tmpl.nodes.map(n => n.id)),
    positions[START_ID] ?? { x: 20, y: 40 },
    START_ID in positions,
  )
  if (start) { newNodes.push(start.node); newEdges.push(...start.edges) }

  addNodes(newNodes)
  addEdges(newEdges)
}

function currentTemplate(): ChainTemplate & { positions: Record<string, {x:number; y:number}> } {
  const obj = toObject()
  // The UML start dot is not a task: exclude it from nodes/edges and instead record the tasks it
  // links to as `startTargets` (pure logic in utils/startDot). Its position is still persisted
  // (positions['__start__']) so it reappears where the user left it — even when unlinked.
  return {
    name:  activeChain.value,
    nodes: obj.nodes.filter(n => n.id !== START_ID).map(n => ({
      id:             n.id,
      fn:             n.data.fn,
      scope:          n.data.scope,
      params:         n.data.params,
      barrier_policy: n.data.barrier_policy,
      resource_pool:  n.data.resource_pool,
    })),
    edges: obj.edges
      .filter(e => !touchesStart(e))
      .map(e => ({ from: e.source, to: e.target })),
    startTargets: startTargetsOf(obj.edges),
    positions: Object.fromEntries(obj.nodes.map(n => [n.id, n.position])),
  }
}

async function saveChain() {
  const uid = projectMeta.current?.uid
  if (!uid || !activeChain.value) return
  saving.value = true
  try {
    const tmpl = currentTemplate()
    const res = await fetch('/api/chains/save', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: uid, template: tmpl }),
    })
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    log.info(`Chain "${activeChain.value}" saved.`, { source: 'whiteboard' })
  } catch (e) {
    log.error(`Save failed: ${e}`, { source: 'whiteboard' })
  } finally {
    saving.value = false
  }
}

async function removeChain() {
  const name = activeChain.value
  const uid  = projectMeta.current?.uid
  if (!name || !uid) return
  try {
    const res = await fetch('/api/chains/delete', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: uid, name }),
    })
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    chainNames.value = chainNames.value.filter(n => n !== name)
    removeNodes(nodes.value.map(n => n.id))
    removeEdges(edges.value.map(e => e.id))
    selectedNodeId.value = null
    if (chainNames.value.length) {
      await switchChain(chainNames.value[0])
    } else {
      activeChain.value = ''
    }
    log.info(`Chain "${name}" deleted.`, { source: 'whiteboard' })
  } catch (e) {
    log.error(`Delete failed: ${e}`, { source: 'whiteboard' })
  }
}

async function createChain() {
  const name = newChainName.value.trim()
  if (!name) return
  const uid = projectMeta.current?.uid
  if (!uid) return
  // Save an empty template immediately so it appears in the list.
  try {
    const res = await fetch('/api/chains/save', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        projectUid: uid,
        template: { name, nodes: [], edges: [] },
      }),
    })
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    if (!chainNames.value.includes(name)) chainNames.value.push(name)
    chainNames.value.sort()
    activeChain.value = name
    removeNodes(nodes.value.map(n => n.id))
    removeEdges(edges.value.map(e => e.id))
    selectedNodeId.value = null
    addStartNode()                       // new chains get a UML start dot by default — link it to the first task
    newChainName.value = ''
    showNewInput.value = false
    // Center + zoom on the start dot (which sits at ~20,40) so it's obviously visible on an otherwise
    // empty canvas — "here's the start, drop your first task to the right" — instead of parked
    // off-screen at the origin. Offset right so there's room for tasks.
    await nextTick()
    setCenter(230, 60, { zoom: 1, duration: 350 })
    log.info(`Chain "${name}" created.`, { source: 'whiteboard' })
  } catch (e) {
    log.error(`Create failed: ${e}`, { source: 'whiteboard' })
  }
}

// ── Resource pools ────────────────────────────────────────────────────────────

interface PoolInfo { name: string; limit: number }
const pools = ref<PoolInfo[]>([])

async function loadPools() {
  try {
    const res = await fetch('/api/pools')
    if (res.ok) pools.value = await res.json() as PoolInfo[]
  } catch { /* non-critical */ }
}

// ── Task defs — palette ───────────────────────────────────────────────────────

const allTaskDefs = ref<TaskDef[]>([])

async function loadAllTaskDefs() {
  try {
    const res = await fetch('/api/tasks/definitions')
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    const data = await res.json() as Record<string, TaskDef[]>
    const flat: TaskDef[] = []
    for (const cat of Object.keys(data).sort()) {
      for (const def of data[cat]) flat.push({ ...def, category: cat })
    }
    allTaskDefs.value = flat
  } catch (e) {
    log.warn(`Could not load task definitions: ${e}`, { source: 'whiteboard' })
  }
}

const paletteCategories = computed(() => {
  const byCategory: Record<string, TaskDef[]> = {}
  for (const def of allTaskDefs.value) {
    const cat = def.category || def.fun_name.split('.')[0]
    ;(byCategory[cat] ??= []).push(def)
  }
  return Object.entries(byCategory).sort(([a], [b]) => a.localeCompare(b))
    .map(([name, defs]) => ({ name, defs }))
})

// ── Drag-and-drop from palette ─────────────────────────────────────────────

const canvasEl = ref<HTMLElement | null>(null)
let dragDef: TaskDef | null = null

function onPaletteDragStart(event: DragEvent, def: TaskDef) {
  dragDef = def
  event.dataTransfer!.effectAllowed = 'copy'
  event.dataTransfer!.setData('text/plain', def.fun_name)
}

function onCanvasDrop(event: DragEvent) {
  event.preventDefault()
  if (!dragDef || !canvasEl.value) return
  const bounds = canvasEl.value.getBoundingClientRect()
  const pos = screenToFlowCoordinate({
    x: event.clientX - bounds.left,
    y: event.clientY - bounds.top,
  })
  const def = dragDef
  dragDef = null
  const id = `${def.fun_name}.${Date.now()}`
  // Scope defaults from the task spec: a set-scope task (behaviour.hmm, clustTracks.cluster)
  // drops in as a picnic node. The user can still change it in the config panel.
  const scope = def.scope ?? 'image'
  const initialParams: Record<string, unknown> = {}
  for (const p of def.params) {
    if (p.default !== undefined) initialParams[p.key] = p.default
  }
  addNodes([{
    id,
    type: scope === 'set' ? 'picnic' : 'task',
    position: pos,
    data: {
      fn:             def.fun_name,
      scope,
      params:         initialParams,
      barrier_policy: 'all',
      resource_pool:  def.resource_pool ?? 'default',
      label:          def.label,
    },
  }])
}

// ── Value-name propagation ────────────────────────────────────────────────────
// A processing task's output value_name (e.g. cellposeCorrect → "cpCorrected") only exists on
// disk after the chain runs, so a downstream node's `valueNameSelection` can't offer it from the
// image. Instead we read the producer's declared output (TaskDef.outputValueName, or an
// `outputValueName` param for tasks whose output name is user-set) and, when an edge is drawn,
// prefill the downstream node's input valueName with it — auto-populated but still editable.

function taskDefFor(fn: string): TaskDef | undefined {
  return allTaskDefs.value.find(d => d.fun_name === fn)
}

// filepath vs labels — the two image fields a value_name can live under. Consumer params tag this
// via `field` ('labels' | 'filepath' | 'imFilepath'); anything not 'labels' is a filepath.
function normField(f: string | undefined): 'filepath' | 'labels' {
  return f === 'labels' ? 'labels' : 'filepath'
}

// The value_name a node produces, or null if it declares none (import, plots, …).
function nodeOutputValueName(node: Node): { name: string; field: 'filepath' | 'labels' } | null {
  const def = taskDefFor(node.data.fn)
  if (!def) return null
  if (def.outputValueName)
    return { name: def.outputValueName, field: normField(def.outputField) }
  // Output name is a user-set param (e.g. segment.cellpose "outputValueName") → labels output.
  const outParam = def.params?.find(p => p.key === 'outputValueName')
  if (outParam) {
    const v = (node.data.params?.outputValueName ?? outParam.default) as unknown
    if (v) return { name: String(v), field: normField(outParam.field) || 'labels' }
  }
  return null
}

// Prefill every field-compatible valueNameSelection param on `target` with `source`'s output.
function propagateValueName(sourceId: string, targetId: string) {
  const source = findNode(sourceId)
  const target = findNode(targetId)
  if (!source || !target) return
  const out = nodeOutputValueName(source)
  if (!out) return
  const def = taskDefFor(target.data.fn)
  if (!def) return
  const patch: Record<string, unknown> = {}
  for (const p of def.params ?? []) {
    if (p.type === 'valueNameSelection' && normField(p.field) === out.field)
      patch[p.key] = out.name
  }
  if (Object.keys(patch).length) {
    updateNode(targetId, {
      data: { ...target.data, params: { ...target.data.params, ...patch } },
    })
  }
}

// ── Edge connections ─────────────────────────────────────────────────────────

onConnect((params) => {
  const fromStart = params.source === START_ID
  addEdges([{
    ...params,
    id: `${params.source}->${params.target}`,
    // start-dot edges are dashed (they mark the entry, not a data dependency)
    style: { stroke: 'var(--cc-accent, #a78bfa)', ...(fromStart ? { strokeDasharray: '4 3' } : {}) },
  }])
  // value-name propagation is a task→task concern; the start dot has no output, so skip it there
  if (params.source && params.target && !fromStart) propagateValueName(params.source, params.target)
})

// Double-click an edge to remove it.
onEdgeDoubleClick(({ edge }) => {
  removeEdges([edge.id])
})

// ── Node selection & config panel ────────────────────────────────────────────

const selectedNodeId = ref<string | null>(null)

const selectedNode = computed(() => {
  if (!selectedNodeId.value) return null
  return findNode(selectedNodeId.value) ?? null
})

const selectedTaskDef = computed<TaskDef | null>(() => {
  if (!selectedNode.value) return null
  return allTaskDefs.value.find(d => d.fun_name === selectedNode.value!.data.fn) ?? null
})

onNodeClick(({ node }: NodeMouseEvent) => {
  selectedNodeId.value = selectedNodeId.value === node.id ? null : node.id
})

function updateSelectedNodeData(patch: Record<string, unknown>) {
  if (!selectedNodeId.value) return
  const node = findNode(selectedNodeId.value)
  if (!node) return
  const newData = { ...node.data, ...patch }
  updateNode(selectedNodeId.value, { data: newData })
  // Sync visual type when scope changes
  if ('scope' in patch) {
    updateNode(selectedNodeId.value, { type: patch.scope === 'set' ? 'picnic' : 'task' })
  }
}

function updateParam(key: string, value: unknown) {
  if (!selectedNodeId.value) return
  const node = findNode(selectedNodeId.value)
  if (!node) return
  updateNode(selectedNodeId.value, {
    data: { ...node.data, params: { ...node.data.params, [key]: value } },
  })
}

function deleteSelectedNode() {
  if (!selectedNodeId.value) return
  // Remove edges connected to this node
  const toRemove = edges.value
    .filter(e => e.source === selectedNodeId.value || e.target === selectedNodeId.value)
    .map(e => e.id)
  removeEdges(toRemove)
  removeNodes([selectedNodeId.value])
  selectedNodeId.value = null
}

// ── Run table ────────────────────────────────────────────────────────────────

const runSetUid       = ref('')
const runSelectedUids = ref<string[]>([])
const chainRunning    = ref(false)

const runImages = computed(() =>
  project.sets.find(s => s.uid === runSetUid.value)?.images ?? []
)
// Excluded images aren't runnable — select-all and the default selection use the included subset.
const includedRunUids = computed(() => includedUids(runImages.value))

// Context for ParamRenderer — use selected run images so channelSelection/valueNameSelection
// widgets reflect the actual images that will be run.
const paramContext = computed(() => {
  const imgs = runSelectedUids.value.length
    ? runImages.value.filter(i => runSelectedUids.value.includes(i.uid))
    : runImages.value
  // Value_names produced by nodes feeding the selected node — offered in its valueNameSelection
  // dropdowns even though they don't exist on the image until the chain runs.
  const extraValueNames = selectedNodeId.value
    ? edges.value
        .filter(e => e.target === selectedNodeId.value)
        .map(e => { const s = findNode(e.source); return s ? nodeOutputValueName(s)?.name : null })
        .filter((n): n is string => !!n)
    : []
  return { images: imgs, extraValueNames }
})

const runAllSelected = computed(() =>
  includedRunUids.value.length > 0 &&
  includedRunUids.value.every(u => runSelectedUids.value.includes(u))
)

const runSomeSelected = computed(() =>
  runSelectedUids.value.length > 0 && !runAllSelected.value
)

// Auto-select all INCLUDED images when set changes (excluded ones start unselected)
watch(runSetUid, () => {
  runSelectedUids.value = includedRunUids.value
})

// Seed runSetUid from first available set
watch(() => project.sets, (sets) => {
  if (!runSetUid.value && sets.length) runSetUid.value = sets[0].uid
}, { immediate: true })

function toggleRunImage(uid: string) {
  const img = runImages.value.find(i => i.uid === uid)
  if (img && isExcluded(img)) return    // excluded images can't be run
  runSelectedUids.value = runSelectedUids.value.includes(uid)
    ? runSelectedUids.value.filter(u => u !== uid)
    : [...runSelectedUids.value, uid]
}

function toggleRunAll() {
  runSelectedUids.value = runAllSelected.value ? [] : includedRunUids.value
}

const runChainTip = computed(() =>
  !activeChain.value        ? 'Select or create a chain first.' :
  !runSelectedUids.value.length ? 'Select at least one image.' :
  `Run chain "${activeChain.value}" on ${runSelectedUids.value.length} image(s).`
)

async function runChain() {
  const uid = projectMeta.current?.uid
  if (!uid || !activeChain.value || !runSelectedUids.value.length || chainRunning.value) return
  chainRunning.value = true
  // Save current params (defaults merged by applyTemplate) before backend loads the file.
  await saveChain()
  ws.send({
    type:       'chain:run',
    projectUid: uid,
    chain:      activeChain.value,
    imageUids:  runSelectedUids.value,
  })
  activeTab.value = 'live'
  setTimeout(() => { chainRunning.value = false }, 500)
}

// ── Lifecycle ────────────────────────────────────────────────────────────────

// Project switch: reload templates AND persisted run history (clear the loaded-run cache so a stale
// project's runs don't linger).
watch(() => projectMeta.current?.uid, () => { loadedRuns.value = new Map(); loadChainList(); loadRunList() }, { immediate: true })

// Refresh the run list when opening the Live tab (a run may have finished since last look).
watch(activeTab, tab => { if (tab === 'live') loadRunList() })

onMounted(async () => {
  await Promise.all([loadAllTaskDefs(), loadChainList(), loadPools(), loadRunList()])
})

// onActivated fires when KeepAlive restores the component. Retry loading defs
// if the first mount failed (server wasn't ready yet).
onActivated(async () => {
  if (!allTaskDefs.value.length) await loadAllTaskDefs()
})
</script>

<template>
  <div class="chain-module">

    <!-- ── Tab bar ─────────────────────────────────────────────────────────── -->
    <div class="chain-tabs">
      <ChipSelect
        variant="segmented"
        :options="tabOptions"
        :model-value="activeTab"
        aria-label="Chain view"
        @update:model-value="v => activeTab = v as 'edit' | 'live'"
      />
    </div>

    <!-- ── Live view ──────────────────────────────────────────────────────── -->
    <div v-if="activeTab === 'live'" class="chain-live">
      <div class="live-toolbar">
        <label class="live-label">Run</label>
        <select
          v-if="runOptions.length"
          class="chain-select live-run-select"
          :value="selectedRunId"
          @change="selectedRunId = ($event.target as HTMLSelectElement).value"
        >
          <option v-for="o in runOptions" :key="o.runId" :value="o.runId">{{ runLabel(o) }}</option>
        </select>
        <button
          v-if="selectedRunId"
          class="wb-btn live-resume-btn"
          :disabled="resumeBusy"
          @click="resumeRun"
          v-tooltip.bottom="resumeBusy
            ? 'Run is still in progress'
            : restartLabel
              ? `Re-run from “${restartLabel}” and everything downstream (upstream stays done)`
              : 'Resume: re-run failed / unfinished / changed nodes. Click a node to re-run from there instead.'"
        >
          <i class="pi pi-play" />
        </button>
        <button
          v-if="restartNodeId"
          class="wb-btn live-copy-btn"
          @click="restartNodeId = null"
          v-tooltip.bottom="'Clear the resume-from node'"
        >
          <i class="pi pi-times" />
        </button>
        <button
          v-if="selectedRunId"
          class="wb-btn live-copy-btn"
          @click="copyRunId"
          v-tooltip.bottom="`Copy run ID (${selectedRunId}) — e.g. for load_chain_run in the REPL`"
        >
          <i class="pi pi-copy" />
        </button>
        <button
          class="wb-btn live-copy-btn"
          @click="loadRunList"
          v-tooltip.bottom="'Reload run history from disk'"
        >
          <i class="pi pi-refresh" />
        </button>
        <button
          class="wb-btn qc-toggle"
          :class="{ 'qc-on': showQc }"
          @click="showQc = !showQc"
          v-tooltip.bottom="'Show/hide the segmentation QC row'"
        >
          <i class="pi pi-chart-bar" />
        </button>
        <button
          ref="throttleBtn"
          class="wb-btn"
          :class="{ 'qc-on': throttleOpen }"
          @click="throttleOpen = !throttleOpen"
          v-tooltip.bottom="'Throttle — how many tasks of each kind run at once'"
        >
          <i class="pi pi-sliders-h" />
        </button>
        <TeleportPopover v-model="throttleOpen" :anchor="throttleBtn" placement="bottom-end">
          <PoolThrottle />
        </TeleportPopover>
        <span v-if="!runOptions.length" class="live-hint">No runs yet — start a chain run to see progress.</span>
      </div>

      <div v-if="liveNodes.length" class="live-canvas-wrap">
        <VueFlow
          id="chain-live"
          :nodes="liveNodes"
          :edges="liveEdges"
          :node-types="nodeTypes"
          :nodes-draggable="false"
          :edges-updatable="false"
          :zoom-on-scroll="true"
          :pan-on-drag="true"
          :min-zoom="0.2"
          :max-zoom="2"
          fit-view-on-init
          class="vue-flow-canvas"
          @node-click="onLiveNodeClick(($event as NodeMouseEvent).node)"
        >
          <Background pattern-color="#2a2742" :gap="20" />
        </VueFlow>

        <!-- QC expand overlay -->
        <div v-if="qcExpand" class="qc-expand-overlay" @click.self="qcExpand = null">
          <div class="qc-expand-card">
            <div class="qc-expand-head">
              <span>Segmentation QC · {{ qcExpand.valueName }}</span>
              <button class="wb-btn" @click="qcExpand = null"><i class="pi pi-times" /></button>
            </div>
            <div class="qc-expand-body">
              <SummaryCanvas :image-uids="qcExpand.imageUids" module="segment" />
            </div>
          </div>
        </div>
      </div>
      <div v-else class="live-empty">
        <i class="pi pi-hourglass" style="font-size:2rem; opacity:0.2" />
        <p>No nodes for this run yet.</p>
      </div>
    </div>

    <!-- ── Edit content: v-show so VueFlow instance survives tab switches ──── -->
    <div v-show="activeTab === 'edit'" class="edit-content">
    <aside class="wb-palette">

      <!-- Chain selector -->
      <div class="chain-bar">
        <div class="chain-bar-select">
          <select
            v-if="chainNames.length"
            class="chain-select"
            :value="activeChain"
            @change="switchChain(($event.target as HTMLSelectElement).value)"
            v-tooltip.right="'Select a chain to edit.'"
          >
            <option v-for="name in chainNames" :key="name" :value="name">{{ name }}</option>
          </select>
          <span v-else class="no-chains-hint">No chains yet</span>
        </div>
        <div class="chain-bar-actions">
          <button
            class="wb-btn"
            @click="showNewInput = !showNewInput"
            v-tooltip.right="'Create a new chain template.'"
          >
            <i class="pi pi-plus" />
          </button>
          <ConfirmDeleteButton :disabled="!activeChain"
            title="Delete this chain template from disk."
            armed-title="Click again to permanently delete this chain"
            @confirm="removeChain" />
          <button
            class="wb-btn"
            :disabled="!activeChain || hasStartNode"
            @click="addStartNode"
            v-tooltip.right="'Add a start node — link it to the task(s) a run begins from. Only tasks reachable from it run; the rest stay as drafts.'"
          >
            <i class="pi pi-circle-fill" />
          </button>
          <button
            class="wb-btn"
            :disabled="!activeChain"
            @click="loadChain(activeChain)"
            v-tooltip.right="'Reload chain from disk — discards unsaved edits.'"
          >
            <i class="pi pi-refresh" />
          </button>
          <button
            class="wb-btn wb-btn-save"
            :disabled="!activeChain || saving"
            @click="saveChain"
            v-tooltip.right="'Save the current chain to disk.'"
          >
            <i :class="['pi', saving ? 'pi-spinner pi-spin' : 'pi-save']" />
          </button>
        </div>
      </div>

      <!-- New chain input -->
      <div v-if="showNewInput" class="new-chain-form">
        <input
          v-model="newChainName"
          class="new-chain-input"
          placeholder="chain name…"
          @keydown.enter="createChain"
          @keydown.esc="showNewInput = false; newChainName = ''"
          autofocus
        />
        <button class="wb-btn wb-btn-save" @click="createChain" :disabled="!newChainName.trim()">
          <i class="pi pi-check" />
        </button>
      </div>

      <div v-if="!projectMeta.hasProject" class="palette-hint">
        Open a project first.
      </div>

      <!-- Task palette -->
      <template v-else>
        <!-- Module functions (collapsible) — drag onto the canvas to add nodes -->
        <CollapsibleSection label="Module functions" max-height="50vh">
          <div class="palette-scroll">
            <div
              v-for="cat in paletteCategories"
              :key="cat.name"
              class="palette-category"
            >
              <div class="palette-cat-heading">{{ cat.name }}</div>
              <div
                v-for="def in cat.defs"
                :key="def.fun_name"
                class="palette-item"
                draggable="true"
                @dragstart="onPaletteDragStart($event, def)"
                v-tooltip.right="`Drag to canvas to add a ${def.label} node.`"
              >
                <i class="pi pi-grip-vertical drag-grip" />
                <span class="palette-item-label">{{ def.label }}</span>
              </div>
            </div>

            <div v-if="!paletteCategories.length" class="palette-hint palette-hint-retry">
              No task definitions found.
              <button class="wb-btn palette-retry-btn" @click="loadAllTaskDefs"
                v-tooltip.right="'Retry loading task definitions from the server.'">
                <i class="pi pi-refresh" />
              </button>
            </div>
          </div>
        </CollapsibleSection>

        <!-- Plots (collapsible) — drag plot nodes onto the canvas; not built yet -->
        <CollapsibleSection label="Plots" :default-open="false" max-height="50vh">
          <div class="palette-soon">
            Plot nodes — drop summary plots into the chain — coming soon.
          </div>
        </CollapsibleSection>

        <!-- ── Run table ──────────────────────────────────────────────────── -->
        <div class="run-table-section">
          <div class="run-section-heading">Run</div>

          <select
            v-if="project.sets.length"
            class="chain-select run-set-select"
            v-model="runSetUid"
            v-tooltip.right="'Select which set to run the chain on.'"
          >
            <option v-for="s in project.sets" :key="s.uid" :value="s.uid">{{ s.name }}</option>
          </select>
          <span v-else class="palette-hint">No sets in project.</span>

          <!-- Image list -->
          <div v-if="runImages.length" class="run-image-list">
            <!-- select-all row -->
            <div
              class="run-row run-row-all"
              @click.stop="toggleRunAll"
              v-tooltip.right="runAllSelected ? 'Deselect all' : 'Select all images'"
            >
              <span class="run-check-icon">
                <i :class="[
                  'pi',
                  runAllSelected   ? 'pi-check-square' :
                  runSomeSelected  ? 'pi-minus-circle' : 'pi-stop'
                ]" />
              </span>
              <span class="run-all-label">All ({{ includedRunUids.length }})</span>
              <span class="run-sel-count" v-if="runSomeSelected">{{ runSelectedUids.length }}</span>
            </div>

            <!-- per-image rows (excluded images greyed + not selectable) -->
            <div
              v-for="img in runImages"
              :key="img.uid"
              class="run-row"
              :class="{ active: runSelectedUids.includes(img.uid), excluded: isExcluded(img) }"
              @click.stop="toggleRunImage(img.uid)"
              v-tooltip.right="isExcluded(img) ? (img.note ? `Excluded: ${img.note}` : 'Excluded from processing') : img.uid"
            >
              <span class="run-check-icon">
                <i :class="['pi', isExcluded(img) ? 'pi-ban' : runSelectedUids.includes(img.uid) ? 'pi-check-square' : 'pi-stop']" />
              </span>
              <span class="run-img-name">{{ img.name }}</span>
            </div>
          </div>

          <button
            class="run-chain-btn"
            :disabled="!activeChain || !runSelectedUids.length || chainRunning"
            @click="runChain"
            v-tooltip.right="runChainTip"
          >
            <i :class="['pi', chainRunning ? 'pi-spin pi-spinner' : 'pi-play']" />
            {{ chainRunning ? 'Starting…' : 'Run chain' }}
          </button>
        </div>
      </template>

    </aside>

    <!-- ── Center: canvas ───────────────────────────────────────────────────── -->
    <div
      ref="canvasEl"
      class="wb-canvas"
      tabindex="0"
      @dragover.prevent
      @drop="onCanvasDrop"
      @keydown.delete="deleteSelectedNode"
      @keydown.backspace="deleteSelectedNode"
    >
      <VueFlow
        id="chain-whiteboard"
        :node-types="nodeTypes"
        :default-viewport="{ zoom: 0.85, x: 60, y: 60 }"
        :min-zoom="0.2"
        :max-zoom="2"
        class="vue-flow-canvas"
      >
        <Background pattern-color="#2a2742" :gap="20" />

        <!-- Empty state -->
        <template v-if="!activeChain && projectMeta.hasProject" #empty>
          <div class="canvas-empty">
            <i class="pi pi-sitemap" />
            <p>Select a chain or create one to start editing.</p>
          </div>
        </template>
      </VueFlow>

      <div class="canvas-hints">
        Drag to pan · Scroll to zoom · Double-click edge to remove
      </div>
    </div>

    <!-- ── Right: node config ───────────────────────────────────────────────── -->
    <aside class="wb-config" :class="{ open: !!selectedNode }">
      <template v-if="selectedNode">

        <div class="config-header">
          <div class="config-title">{{ selectedNode.type === 'start' ? 'Start node' : 'Node' }}</div>
          <ConfirmDeleteButton
            title="Remove this node and its connections from the chain."
            armed-title="Click again to remove this node"
            @confirm="deleteSelectedNode" />
        </div>

        <div v-if="selectedNode.type === 'start'" class="config-section">
          <p class="no-params-hint">Link this to the task(s) a run should begin from. Only tasks
          reachable from it will run — the rest stay in the editor as drafts.</p>
        </div>

        <template v-else>
        <!-- Identity -->
        <div class="config-section">
          <label class="config-label">ID</label>
          <div class="config-value mono">{{ selectedNode.id }}</div>
          <label class="config-label" style="margin-top:0.5rem">Function</label>
          <div class="config-value mono">{{ selectedNode.data.fn }}</div>
        </div>

        <!-- Scope -->
        <div class="config-section">
          <label class="config-label">Scope</label>
          <select
            class="config-select"
            :value="selectedNode.data.scope"
            @change="updateSelectedNodeData({ scope: ($event.target as HTMLSelectElement).value })"
            v-tooltip.left="'image: runs once per image in parallel. set: synchronises all images (picnic node). incremental: event-driven plot watcher.'"
          >
            <option value="image">image</option>
            <option value="set">set (picnic)</option>
            <option value="incremental">incremental</option>
          </select>

          <template v-if="selectedNode.data.scope === 'set'">
            <label class="config-label" style="margin-top:0.5rem">Barrier policy</label>
            <select
              class="config-select"
              :value="selectedNode.data.barrier_policy"
              @change="updateSelectedNodeData({ barrier_policy: ($event.target as HTMLSelectElement).value })"
              v-tooltip.left="'all: run regardless of upstream failures. require_all: abort if any image failed. successful_only: skip failed images.'"
            >
              <option value="all">all</option>
              <option value="require_all">require_all</option>
              <option value="successful_only">successful_only</option>
            </select>
          </template>

          <label class="config-label" style="margin-top:0.5rem">Resource pool</label>
          <select
            class="config-select"
            :value="selectedNode.data.resource_pool"
            @change="updateSelectedNodeData({ resource_pool: ($event.target as HTMLSelectElement).value })"
            v-tooltip.left="'Limits how many nodes share a concurrency slot. GPU tasks should use the gpu pool (limit: 1) to avoid running multiple models at once.'"
          >
            <option value="">— none (unbounded) —</option>
            <option v-for="p in pools" :key="p.name" :value="p.name">
              {{ p.name }} (max {{ p.limit }} concurrent)
            </option>
          </select>
        </div>

        <!-- Params -->
        <div class="config-section" v-if="selectedTaskDef && selectedTaskDef.params.length">
          <div class="config-section-heading">Parameters</div>
          <div class="config-params">
            <ParamRenderer
              v-for="p in selectedTaskDef.params"
              :key="p.key"
              :param="p"
              :modelValue="selectedNode.data.params[p.key]"
              :context="paramContext"
              @update:modelValue="updateParam(p.key, $event)"
            />
          </div>
        </div>

        <div class="config-section" v-else-if="selectedTaskDef && !selectedTaskDef.params.length">
          <span class="no-params-hint">No parameters for this function.</span>
        </div>

        <div class="config-section" v-else>
          <span class="no-params-hint">Function not found in task definitions.</span>
        </div>
        </template>

      </template>

      <div v-else class="config-placeholder">
        <i class="pi pi-mouse-pointer" style="font-size:1.4rem; opacity:0.3" />
        <p>Click a node to configure it.</p>
      </div>
    </aside>
    </div><!-- end edit-content -->

  </div>
</template>

<style scoped>
.chain-module {
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow: hidden;
  background: var(--cc-bg);
}

/* ── Tab bar ──────────────────────────────────────────────────────────────── */
.chain-tabs {
  display: flex;
  gap: 0;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  flex-shrink: 0;
}

/* ── Live view ────────────────────────────────────────────────────────────── */
.chain-live {
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.live-toolbar {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  padding: 0.4rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  background: var(--cc-surface-1);
  flex-shrink: 0;
}

.live-label {
  font-size: 0.68rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--cc-text-dim);
  flex-shrink: 0;
}

/* Live toolbar: keep the run selector tight (it otherwise stretches full width via .chain-select
   flex:1) with the copy button right beside it. */
.live-run-select {
  flex: 0 0 auto;
  width: auto;
  max-width: 240px;
}

.live-copy-btn { flex: 0 0 auto; color: var(--cc-text-dim); }
.live-copy-btn:hover:not(:disabled) { color: var(--cc-text); }

/* QC row toggle — a square icon button like the others; accent tint when on */
.qc-toggle { color: var(--cc-text-dim); }
.qc-toggle:hover:not(:disabled) { color: var(--cc-text); }
.qc-toggle.qc-on { color: var(--cc-accent); border-color: var(--cc-accent); }

/* Resume — a square icon button (.wb-btn) accent-tinted as the Live tab's primary action.
   The play icon carries it; the "resume from X" detail lives in the tooltip. */
.live-resume-btn {
  color: var(--cc-accent);
  border-color: var(--cc-accent);
}
.live-resume-btn:hover:not(:disabled) { background: var(--cc-accent); color: #fff; }

.qc-expand-overlay {
  position: absolute; inset: 0; z-index: 20;
  background: rgba(0, 0, 0, 0.55);
  display: flex; align-items: center; justify-content: center;
}
.qc-expand-card {
  width: 96%; height: 94%;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: 0.5rem;
  display: flex; flex-direction: column; overflow: hidden;
}
.qc-expand-head {
  display: flex; align-items: center; justify-content: space-between;
  padding: 0.5rem 0.75rem; border-bottom: 1px solid var(--cc-border);
  font-size: 0.8rem; font-weight: 600; color: var(--cc-text);
}
.qc-expand-body { flex: 1; min-height: 0; overflow: auto; }

.live-hint {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  font-style: italic;
}

.live-canvas-wrap {
  flex: 1;
  overflow: hidden;
  position: relative;
}

.live-empty {
  flex: 1;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
  color: var(--cc-text-dim);
  font-size: 0.8rem;
  font-style: italic;
}

/* Edit tab: flex row layout ─────────────────────────────────────────────── */
.edit-content {
  flex: 1;
  display: flex;
  overflow: hidden;
}


/* ── Palette ──────────────────────────────────────────────────────────────── */
.wb-palette {
  width: 190px;
  flex-shrink: 0;
  background: var(--cc-surface-1);
  border-right: 1px solid var(--cc-border);
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.chain-bar {
  display: flex;
  flex-direction: column;
  gap: 0.3rem;
  padding: 0.45rem 0.55rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}

.chain-bar-select {
  display: flex;
  align-items: center;
}
.chain-bar-select .chain-select,
.chain-bar-select .no-chains-hint {
  flex: 1;
  width: 100%;
}

.chain-bar-actions {
  display: flex;
  align-items: center;
  gap: 0.3rem;
}

.chain-select {
  flex: 1;
  min-width: 0;
  font-size: 0.78rem;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: 0.3rem;
  color: var(--cc-text);
  padding: 0.25rem 0.4rem;
  cursor: pointer;
}
.chain-select:focus { outline: 1px solid var(--cc-accent); }

.no-chains-hint {
  flex: 1;
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  font-style: italic;
}

.wb-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  width: 26px; height: 26px;
  border-radius: 0.3rem;
  border: 1px solid var(--cc-border);
  background: var(--cc-surface-2);
  color: var(--cc-text-dim);
  cursor: pointer;
  font-size: 0.7rem;
  flex-shrink: 0;
  transition: background 0.1s, color 0.1s;
}
.wb-btn:hover:not(:disabled) { background: var(--cc-surface-2); color: var(--cc-text); border-color: var(--cc-accent); }
.wb-btn:disabled { opacity: 0.4; cursor: not-allowed; }
.wb-btn-save { color: var(--cc-accent); border-color: var(--cc-accent); }
.wb-btn-save:hover:not(:disabled) { background: var(--cc-accent); color: #fff; }
.wb-btn-danger { color: #f87171; border-color: #f8717155; }
.wb-btn-danger:hover:not(:disabled) { background: #f87171; color: #fff; border-color: #f87171; }

.new-chain-form {
  display: flex;
  gap: 0.3rem;
  padding: 0.4rem 0.55rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.new-chain-input {
  flex: 1;
  min-width: 0;
  font-size: 0.78rem;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-accent);
  border-radius: 0.3rem;
  color: var(--cc-text);
  padding: 0.2rem 0.4rem;
  outline: none;
}

.palette-scroll {
  flex: 1;
  overflow-y: auto;
  padding: 0.25rem 0;
}

.palette-category { margin-bottom: 0.25rem; }

.palette-cat-heading {
  font-size: 0.6rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--cc-text-dim);
  padding: 0.5rem 0.65rem 0.2rem;
}

.palette-item {
  display: flex;
  align-items: center;
  gap: 0.35rem;
  padding: 0.3rem 0.65rem;
  font-size: 0.78rem;
  color: var(--cc-text-dim);
  cursor: grab;
  border-radius: 0.3rem;
  margin: 0 0.3rem;
  transition: background 0.1s, color 0.1s;
  user-select: none;
}
.palette-item:hover {
  background: var(--cc-surface-2);
  color: var(--cc-text);
}
.palette-item:active { cursor: grabbing; }

.drag-grip {
  font-size: 0.65rem;
  opacity: 0.4;
  flex-shrink: 0;
}
.palette-item-label {
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}

.palette-hint {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  font-style: italic;
  padding: 0.75rem 0.65rem;
}
.palette-soon {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  font-style: italic;
  padding: 0.75rem 0.65rem;
  opacity: 0.7;
}
.palette-hint-retry {
  display: flex;
  align-items: center;
  gap: 0.4rem;
}
.palette-retry-btn {
  font-size: 0.7rem;
  padding: 0.1rem 0.3rem;
  flex-shrink: 0;
}

/* ── Canvas ───────────────────────────────────────────────────────────────── */
.wb-canvas {
  flex: 1;
  min-width: 0;
  position: relative;
  background: var(--cc-bg);
}

.vue-flow-canvas {
  width: 100%;
  height: 100%;
}

.canvas-empty {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem;
  color: var(--cc-text-dim);
  font-size: 0.8rem;
  font-style: italic;
}
.canvas-empty i { font-size: 2rem; opacity: 0.25; }

.canvas-hints {
  position: absolute;
  bottom: 0.5rem;
  left: 50%;
  transform: translateX(-50%);
  font-size: 0.65rem;
  color: var(--cc-text-dim);
  opacity: 0.5;
  pointer-events: none;
  white-space: nowrap;
}

/* ── Config panel ─────────────────────────────────────────────────────────── */
.wb-config {
  width: 0;
  overflow: hidden;
  flex-shrink: 0;
  background: var(--cc-surface-1);
  border-left: 1px solid var(--cc-border);
  transition: width 0.18s ease;
  display: flex;
  flex-direction: column;
}
.wb-config.open {
  width: 260px;
  overflow-y: auto;
}

.config-placeholder {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 0.5rem;
  padding: 2rem 1rem;
  color: var(--cc-text-dim);
  font-size: 0.78rem;
  font-style: italic;
  text-align: center;
}

.config-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0.6rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
  flex-shrink: 0;
}
.config-title {
  font-size: 0.68rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--cc-text-dim);
}

.config-section {
  padding: 0.65rem 0.75rem;
  border-bottom: 1px solid var(--cc-border);
}
.config-label {
  display: block;
  font-size: 0.68rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  color: var(--cc-text-dim);
  margin-bottom: 0.25rem;
}
.config-value {
  font-size: 0.75rem;
  color: var(--cc-text);
  word-break: break-all;
}
.config-value.mono { font-family: var(--cc-mono, monospace); font-size: 0.7rem; }

.config-select, .config-input {
  width: 100%;
  font-size: 0.78rem;
  background: var(--cc-surface-2);
  border: 1px solid var(--cc-border);
  border-radius: 0.3rem;
  color: var(--cc-text);
  padding: 0.28rem 0.5rem;
}
.config-select:focus, .config-input:focus { outline: 1px solid var(--cc-accent); border-color: var(--cc-accent); }
.config-input::placeholder { color: var(--cc-text-dim); font-style: italic; }

.config-section-heading {
  font-size: 0.65rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.07em;
  color: var(--cc-text-dim);
  margin-bottom: 0.35rem;
}
.config-params { display: flex; flex-direction: column; }
.no-params-hint {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  font-style: italic;
}

/* ── Run table ────────────────────────────────────────────────────────────── */
.run-table-section {
  flex-shrink: 0;
  border-top: 1px solid var(--cc-border);
  display: flex;
  flex-direction: column;
  gap: 0;
  background: var(--cc-bg);
}

.run-section-heading {
  font-size: 0.6rem;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--cc-text-dim);
  padding: 0.45rem 0.65rem 0.2rem;
}

.run-set-select {
  margin: 0 0.45rem 0.3rem;
  width: calc(100% - 0.9rem);
}

.run-image-list {
  max-height: 160px;
  overflow-y: auto;
  border-top: 1px solid var(--cc-border);
  border-bottom: 1px solid var(--cc-border);
  margin-bottom: 0.4rem;
}

.run-row {
  display: flex;
  align-items: center;
  gap: 0.3rem;
  padding: 0.22rem 0.65rem;
  cursor: pointer;
  user-select: none;
  transition: background 0.07s;
}
.run-row:hover { background: var(--cc-surface-2); }
.run-row.active .run-img-name { color: var(--cc-text); }
/* excluded from processing — greyed, not clickable */
.run-row.excluded { opacity: 0.45; cursor: default; }
.run-row.excluded:hover { opacity: 0.7; }
.run-row.excluded .run-check-icon { color: #fca5a5; }

.run-row-all {
  background: var(--cc-surface-1);
  border-bottom: 1px solid var(--cc-border);
  position: sticky;
  top: 0;
  z-index: 1;
}
.run-row-all:hover { background: var(--cc-surface-2); }

.run-check-icon {
  font-size: 0.7rem;
  color: var(--cc-accent);
  flex-shrink: 0;
  width: 14px;
  text-align: center;
}
.run-row:not(.active) .run-check-icon { color: var(--cc-border); }
.run-row-all .run-check-icon { color: var(--cc-accent); }

.run-all-label {
  font-size: 0.72rem;
  font-weight: 600;
  color: var(--cc-text-dim);
  flex: 1;
}
.run-sel-count {
  font-size: 0.65rem;
  font-family: var(--cc-mono);
  color: var(--cc-accent);
}

.run-img-name {
  font-size: 0.72rem;
  color: var(--cc-text-dim);
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  max-width: 130px;
}

.run-chain-btn {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.35rem;
  margin: 0 0.45rem 0.5rem;
  width: calc(100% - 0.9rem);
  padding: 0.35rem 0.5rem;
  font-size: 0.78rem;
  font-weight: 600;
  border-radius: 0.3rem;
  border: 1px solid #10b981;
  background: #0c1a0e;
  color: #6ee7b7;
  cursor: pointer;
  transition: background 0.1s, color 0.1s;
}
.run-chain-btn:hover:not(:disabled) { background: #10b981; color: #fff; }
.run-chain-btn:disabled { opacity: 0.35; cursor: not-allowed; }
</style>
