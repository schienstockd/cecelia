// Per-image provenance graph — which STORED VERSION was produced FROM which — reconstructed from the
// automatic run log. Each successful in-place run reads an input version (`valueName`) and writes an
// output version (`outputValueName`, derived server-side from the task spec's `namespace`, see
// `api/src/routes.jl` → `_enriched_run_log`), which is the one edge shape this needs.
//
// Only `done` entries count: a `failed`/`cancelled`/`interrupted` run wrote nothing, so it must not
// appear as an edge — the resulting graph would claim a version was produced by a task that never
// finished. Running entries are treated the same: no output yet.
//
// When multiple `done` runs share an output (a re-segmentation), the MOST RECENT one wins — the
// current parent on disk is the one that most recently wrote it. Entries arrive oldest→newest, so a
// later entry naturally overwrites an earlier one.

import type { RunLogEntry } from './runLog'
import { runStatus } from './runLog'

export interface LineageEdge {
  parent: string           // the version READ as input
  fun: string              // the fun_name that produced the child (e.g. 'cleanupImages.driftCorrect')
  at: string               // the run's start timestamp (`yyyy-mm-ddTHH:MM:SS`)
}

// One node in the version tree: a stored version name plus its inbound edge (if any).
export interface LineageNode {
  version: string
  edge?: LineageEdge       // undefined ⇒ a root (imported, or pre-runlog, or produced by a
                            // truncated/removed run)
  children: LineageNode[]
}

// Given ALL stored version names (the keys of `img.filepaths`) and the run log, return the map
// `version → inbound edge`. Versions with no producing run are absent from the map — the caller
// treats them as roots.
export function buildLineageEdges(
  versions: string[],
  runLog?: RunLogEntry[] | null,
): Map<string, LineageEdge> {
  const known = new Set(versions)
  const edges = new Map<string, LineageEdge>()
  for (const e of runLog ?? []) {
    // A run only produces a version when it FINISHED successfully — see file header.
    if (runStatus(e) !== 'done') continue
    const child = String(e.outputValueName ?? '').trim()
    const parent = String(e.valueName ?? '').trim()
    // Self-edge (`smoothed → smoothed` from a re-run on itself) is a legitimate re-run: it doesn't
    // add a new node, and treating it as a parent would loop the tree. Skip.
    if (!child || !parent || child === parent) continue
    // Only edges INTO an actually-stored version are useful — a run that wrote a version later
    // deleted or renamed would otherwise contribute a stale node the modal can't render.
    if (!known.has(child)) continue
    edges.set(child, { parent, fun: String(e.fun ?? ''), at: String(e.at ?? '') })
  }
  return edges
}

// Build the version forest — one tree per root, children nested under their parent. Order across
// siblings is by earliest inbound edge, so a re-run doesn't reshuffle the layout.
export function buildLineageForest(
  versions: string[],
  runLog?: RunLogEntry[] | null,
): LineageNode[] {
  const edges = buildLineageEdges(versions, runLog)
  const nodes = new Map<string, LineageNode>()
  for (const v of versions) nodes.set(v, { version: v, edge: edges.get(v), children: [] })

  const roots: LineageNode[] = []
  for (const v of versions) {
    const n = nodes.get(v)!
    // A parent named by an edge might itself not be a stored version any more — treat as a root so
    // the child still appears. Same for missing edges.
    const parent = n.edge && nodes.get(n.edge.parent)
    if (parent) parent.children.push(n)
    else roots.push(n)
  }
  // Deterministic order: by inbound `at` for children, alphabetical for roots.
  roots.sort((a, b) => a.version.localeCompare(b.version))
  for (const n of nodes.values()) {
    n.children.sort((a, b) => (a.edge?.at ?? '').localeCompare(b.edge?.at ?? ''))
  }
  return roots
}

// Flatten the forest for a rendering that walks node-by-node with a depth. Preorder, so a parent
// prints before its children.
export interface LineageRow { node: LineageNode; depth: number }
export function flattenLineage(forest: LineageNode[]): LineageRow[] {
  const out: LineageRow[] = []
  const walk = (n: LineageNode, depth: number) => {
    out.push({ node: n, depth })
    for (const c of n.children) walk(c, depth + 1)
  }
  for (const r of forest) walk(r, 0)
  return out
}
