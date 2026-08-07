// Layered layout for a chain DAG — THE one place that turns (nodes, edges) into columns and rows.
//
// Two consumers, one geometry, on purpose:
//   • the Live tab's run grid (execution depth left→right, one band per image)
//   • the Edit whiteboard, when a template arrives with no saved `positions`
//
// That second case is why this is a util rather than SFC-local logic. `positions` is whiteboard-only
// sidecar data, so a template authored anywhere else — the REPL, or Claude via the MCP `create_chain`
// — has none, and `applyTemplate` used to fall back to `{x: 80 + i * 220, y: 120}`: every node in one
// horizontal row, in template order. A linear chain survives that; a branching one lands as a straight
// line with crossing edges, which HIDES the topology the user is being asked to review. Reviewing the
// graph is the whole safety model for an authored chain, so the layout has to show the shape.
//
// Deliberately simple: a layered sweep, no edge routing or crossing minimisation. Chains are 5–10
// nodes; the layering carries the meaning and anything cleverer is unjustified here.

export interface DagNode { id: string }
export interface DagEdge { from: string; to: string }
export interface Point { x: number; y: number }

/**
 * Topological order (Kahn). Nodes in a cycle — or unreachable from any root — are appended at the end
 * rather than dropped, so a malformed template still renders instead of vanishing. Edges naming an
 * unknown node are ignored.
 */
export function topoOrder(nodes: DagNode[], edges: DagEdge[]): string[] {
  const indeg = new Map(nodes.map(n => [n.id, 0]))
  const succ = new Map(nodes.map(n => [n.id, [] as string[]]))
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

export interface LayerLanes {
  order: string[]
  /** longest path from a root → the column (execution depth) */
  layer: Map<string, number>
  /** index of the node within its column → the row */
  lane: Map<string, number>
  /** widest column, i.e. how many lanes a band needs */
  maxLane: number
  /** how many nodes sit in each layer — used to centre a narrow layer against a wide one */
  countByLayer: Map<number, number>
}

/**
 * Assign each node a layer (column) and a lane (row within that column).
 *
 * Layer is the LONGEST path from a root, not the shortest: a node must sit to the right of everything
 * that feeds it, so a late join renders after its slowest input rather than overlapping it. Fan-out
 * siblings share a layer and get separate lanes, which is what makes a branch visibly split.
 */
export function layerLanes(nodes: DagNode[], edges: DagEdge[]): LayerLanes {
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

  return {
    order, layer, lane,
    maxLane: Math.max(1, ...perLayer.values()),
    countByLayer: perLayer,
  }
}

/**
 * Which way execution flows. `LR` puts depth on X (the Live tab's convention, and the better fit for a
 * chain that fans out); `TB` puts it on Y, which keeps a long linear pipeline on screen instead of
 * running it off the right edge.
 */
export type FlowDirection = 'LR' | 'TB'

export interface GridSpec {
  /** spacing along the flow axis (between depth steps) */
  depth: number
  /** spacing across the flow axis (between siblings in one layer) */
  across: number
  originX: number
  originY: number
}

/** Editor default: wide enough that a node's box plus its label doesn't touch the next step. */
export const EDITOR_GRID: GridSpec = { depth: 220, across: 120, originX: 80, originY: 120 }

/**
 * Compact: fits more of a long chain on screen.
 *
 * The two axes are NOT equally compressible. A task node is up to ~182px wide (`ChainTaskNode`:
 * min-width 140, label max-width 160, +22px padding), so `depth` cannot go far below 190 in `LR`
 * without boxes touching — most of the saving has to come from `across`, where a node is only ~50px
 * tall. Hence the asymmetric trim rather than one scale factor: a uniform 0.6× would have overlapped
 * horizontally while still wasting vertical space.
 */
export const COMPACT_GRID: GridSpec = { depth: 190, across: 72, originX: 60, originY: 90 }

export interface LayoutVariant {
  id: string
  direction: FlowDirection
  spec: GridSpec
  label: string
  icon: string
}

/**
 * The variants offered in the whiteboard's layout menu, in menu order.
 *
 * Direction × spacing is listed FLAT rather than as a direction menu plus a compact toggle: every
 * combination is then one click with no hidden state to remember, and four short rows is still a small
 * menu.
 */
export const LAYOUT_VARIANTS: LayoutVariant[] = [
  { id: 'LR',         direction: 'LR', spec: EDITOR_GRID,  label: 'Left to right',           icon: 'pi-arrow-right' },
  { id: 'LR-compact', direction: 'LR', spec: COMPACT_GRID, label: 'Left to right, compact',  icon: 'pi-arrow-right' },
  { id: 'TB',         direction: 'TB', spec: EDITOR_GRID,  label: 'Top to bottom',           icon: 'pi-arrow-down' },
  { id: 'TB-compact', direction: 'TB', spec: COMPACT_GRID, label: 'Top to bottom, compact',  icon: 'pi-arrow-down' },
]

/**
 * Positions keyed by node id.
 *
 * Siblings in a layer are CENTRED against the widest layer, so the single node feeding a fan-out sits
 * level with the middle of the branches rather than level with the first one. Top-aligning made a
 * two-way split look like the trunk belonged to the upper branch.
 *
 * Note what this does to a LINEAR chain in `LR`: every node is lane 0 in a maxLane-1 graph, so it lays
 * out as one horizontal row — identical to the old hardcoded fallback. That is correct, and it's why
 * this is invisible for a simple pipeline and only shows up where the old behaviour actually lied.
 */
export function layoutDag(
  nodes: DagNode[],
  edges: DagEdge[],
  direction: FlowDirection = 'LR',
  spec: GridSpec = EDITOR_GRID,
): Record<string, Point> {
  const { layer, lane, maxLane, countByLayer } = layerLanes(nodes, edges)
  const out: Record<string, Point> = {}
  for (const n of nodes) {
    const L = layer.get(n.id) ?? 0
    const centred = (lane.get(n.id) ?? 0) + (maxLane - (countByLayer.get(L) ?? 1)) / 2
    out[n.id] = direction === 'LR'
      ? { x: spec.originX + L * spec.depth,   y: spec.originY + centred * spec.across }
      : { x: spec.originX + centred * spec.depth, y: spec.originY + L * spec.across }
  }
  return out
}
