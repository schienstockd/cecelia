// Pure round-trip logic for the chain whiteboard's UML start dot (see ChainModule / docs/UI.md).
// Kept out of the .vue SFC so it's unit-testable (utils/startDot.test.ts) — the save side extracts
// the dot's links as `startTargets`; the load side rebuilds the dot node + edges from them. The dot
// itself is never a template task node; only its position persists (under the reserved id below).

export const START_ID = '__start__'

export interface EdgeLite { source: string; target: string }

/** SAVE: the task ids the start dot links to = the targets of edges out of the start node. */
export function startTargetsOf(edges: EdgeLite[]): string[] {
  return edges.filter(e => e.source === START_ID).map(e => e.target)
}

/** True for the start node / any edge touching it — used to exclude it from the saved template. */
export const isStartId = (id: string): boolean => id === START_ID
export const touchesStart = (e: EdgeLite): boolean => e.source === START_ID || e.target === START_ID

/** Where the dot sits when nothing better is known — a template with no targets AND no layout. */
export const DEFAULT_START_POS = { x: 20, y: 40 }

/**
 * LOAD: where to park the dot when no position was persisted — which is every template authored
 * outside the whiteboard (the REPL, or Claude via the MCP `create_chain`; `positions` is canvas-only
 * sidecar data).
 *
 * The dot is not a template node, so `layoutDag` never sees it. That left the two placed by
 * independent mechanisms: the tasks on the layout grid, the dot at a hardcoded constant — and with
 * `EDITOR_GRID.originY` at 120 against the dot's y of 40, an authored chain always opened with the dot
 * parked off in the corner and a long dashed swoop to the first task. It reads as "the start node is
 * not connected", which is the one thing a reviewer must be able to see at a glance, and it was
 * wrong: the edge was there all along.
 *
 * One depth step left of the first target, same lane, so it reads as the node that feeds it.
 */
export function startDotPosition(
  targets: string[] | undefined,
  layout: Record<string, { x: number; y: number }>,
  depth: number,
): { x: number; y: number } {
  const anchor = (targets ?? []).map(t => layout[t]).find(Boolean)
  return anchor ? { x: anchor.x - depth, y: anchor.y } : DEFAULT_START_POS
}

export interface StartNodeSpec { id: string; type: 'start'; position: { x: number; y: number }; data: Record<string, never> }
export interface StartEdgeSpec { id: string; source: string; target: string; style: Record<string, string> }

/**
 * LOAD: rebuild the start dot + its (dashed) edges to add to the canvas.
 * Reconstructs when the dot links to at least one still-existing target OR a position was persisted
 * for it (so an unlinked-but-placed dot — e.g. the default on a new chain — survives save/reload).
 * Returns null when there's nothing to add. Targets not in `existingIds` (deleted nodes) are dropped.
 */
export function buildStartGraph(
  targets: string[] | undefined,
  existingIds: Set<string>,
  pos: { x: number; y: number },
  hasPersistedPos: boolean,
): { node: StartNodeSpec; edges: StartEdgeSpec[] } | null {
  const valid = (targets ?? []).filter(t => existingIds.has(t))
  if (!valid.length && !hasPersistedPos) return null
  return {
    node: { id: START_ID, type: 'start', position: pos, data: {} },
    edges: valid.map(t => ({
      id: `${START_ID}->${t}`, source: START_ID, target: t,
      style: { stroke: 'var(--cc-accent, #a78bfa)', strokeDasharray: '4 3' },
    })),
  }
}
