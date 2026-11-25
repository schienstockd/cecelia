// Refine runs — gating with clustering (docs/todo/CLUSTER_REFINE_PLAN.md). A coarse clustering run gives
// the first populations; a run restricted to ONE cluster population (usually at a higher resolution)
// gives its children. The server records each run's params and, for a refine run, what it refines
// (`/api/gating/channels` → `clusterRuns`); these helpers turn that into the run tree the cluster page
// shows and the form the Refine action fills.

/** One run as `clusterRuns[suffix]` reports it. */
export interface ClusterRun {
  params?: Record<string, unknown>
  refines?: { pop: string; run: string }   // the cluster population (path) and run it refines
  stale?: string                           // why it no longer matches its source
}

/** The runs in tree order: each run, then the runs that refine one of its populations, indented. */
export function runTreeOrder(suffixes: string[], runs: Record<string, ClusterRun>): { suffix: string; depth: number }[] {
  const have = new Set(suffixes)
  const parentOf = (s: string) => {
    const r = runs[s]?.refines?.run
    return r && have.has(r) && r !== s ? r : null
  }
  const out: { suffix: string; depth: number }[] = []
  const seen = new Set<string>()
  const walk = (s: string, depth: number) => {
    if (seen.has(s)) return
    seen.add(s)
    out.push({ suffix: s, depth })
    for (const c of suffixes) if (parentOf(c) === s) walk(c, depth + 1)
  }
  for (const s of suffixes) if (parentOf(s) === null) walk(s, 0)
  for (const s of suffixes) walk(s, 0)            // a cycle has no root — never drop a run
  return out
}

/** "lymphoid · res 2", or just the suffix for a run recorded before params were. */
export function runLabel(suffix: string, run?: ClusterRun): string {
  const res = run?.params?.resolution
  return typeof res === 'number' ? `${suffix} · res ${+res.toFixed(2)}` : suffix
}

/** The option text of the run dropdown: indented by depth, ⚠ on a stale refine run. */
export function runOptionText(suffix: string, depth: number, run?: ClusterRun): string {
  return `${'  '.repeat(depth)}${depth ? '↳ ' : ''}${runLabel(suffix, run)}${run?.stale ? ' ⚠' : ''}`
}

/** The refine runs of each population: pop path → run suffixes. */
export function refinedBy(runs: Record<string, ClusterRun>): Record<string, string[]> {
  const out: Record<string, string[]> = {}
  for (const [s, r] of Object.entries(runs)) if (r.refines) (out[r.refines.pop] ??= []).push(s)
  for (const k of Object.keys(out)) out[k].sort()
  return out
}

/** The ancestors of `paths` that are not in `paths` themselves — the context rows above a refine run's
 *  populations (its source population and up). */
export function contextAncestors(flat: { path: string; parent: string }[], paths: string[]): Set<string> {
  const byPath = new Map(flat.map(p => [p.path, p]))
  const own = new Set(paths)
  const out = new Set<string>()
  for (const path of paths) {
    let parent = byPath.get(path)?.parent
    while (parent && parent !== 'root' && !own.has(parent) && !out.has(parent)) {
      out.add(parent)
      parent = byPath.get(parent)?.parent
    }
  }
  return out
}

/** A free run name from a population name: "Lymphoid, mixed" → "lymphoid_mixed" (then _2, _3, …). */
export function refineSuffix(popName: string, taken: string[]): string {
  const base = popName.toLowerCase().replace(/[^a-z0-9]+/g, '_').replace(/^_+|_+$/g, '') || 'refine'
  if (!taken.includes(base)) return base
  let i = 2
  while (taken.includes(`${base}_${i}`)) i++
  return `${base}_${i}`
}

/** One step finer than the source run: double its resolution (2 when it is unknown), within the task's
 *  range (0.05–5, step 0.05). */
export function nextResolution(res: unknown): number {
  if (typeof res !== 'number' || !isFinite(res) || res <= 0) return 2
  return Math.min(5, Math.max(0.05, Math.round((res * 2) / 0.05) * 0.05))
}

/** The segmentations a run clustered, from its `popsToCluster` refs (`"B/qc"` → `B`). */
export function runValueNames(popsToCluster: unknown, fallback: string): string[] {
  const refs = Array.isArray(popsToCluster) ? popsToCluster.filter((r): r is string => typeof r === 'string') : []
  const vns = [...new Set(refs.filter(r => !r.startsWith('/') && r.includes('/')).map(r => r.slice(0, r.indexOf('/'))))]
  return vns.length ? vns : [fallback]
}

/** The Cluster cells form for refining population `popPath` of a run: the source run's settings
 *  (`base` — its saved params, or what `clusterRuns` + the feature list recorded) with the population as
 *  the only input on every segmentation the run covered, a finer resolution and a new run name. */
export function refineFormValues(base: Record<string, unknown>, o: {
  popPath: string; popName: string; fallbackVn: string; suffixes: string[]
}): Record<string, unknown> {
  const vns = runValueNames(base.popsToCluster, o.fallbackVn)
  return {
    ...base,
    popsToCluster: vns.map(v => `${v}${o.popPath}`),
    resolution: nextResolution(base.resolution),
    valueNameSuffix: refineSuffix(o.popName, o.suffixes),
  }
}
