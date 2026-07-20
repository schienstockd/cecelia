export interface ParamDef {
  key: string
  label: string
  type: 'int' | 'float' | 'bool' | 'text' | 'select'
       | 'channelSelection' | 'valueNameSelection'
       | 'popSelection' | 'labelPropsSelection' | 'labelPropsColsSelection'
       | 'motionDimsSelection'
       | 'group' | 'section'
  tip?: string
  trimPrefix?: string   // labelPropsColsSelection: strip this prefix from option labels (display only)
  acrossSegmentations?: boolean  // popSelection: list populations across ALL segmentations (value_name-prefixed)
  includeRoot?: boolean          // popSelection (across, legacy popType path): also offer each segmentation's whole population ("<seg> · all")
  popScope?: 'cells' | 'tracks'  // popSelection: the module-function object scope — cell pops vs tracked pops (backend resolves sources + cell/track filtering); preferred over raw popType
  includeClusters?: boolean      // popSelection (popScope): also offer clustering-derived pops (clust/trackclust); default true
  accepts?: string[]             // popSelection: explicit pop_type allow-list (Decision 14) — the exact types this function takes (any of 'live'/'flow','clust','region','track','trackclust'); supersedes popScope, enables cells+tracks in one picker (e.g. region-clustering basis)
  // int / float
  min?: number
  max?: number
  step?: number
  default?: unknown
  // select / valueNameSelection
  options?: { label: string; value: string }[]
  multiple?: boolean
  field?: string        // valueNameSelection: which image field to read names from ('filepath' | 'labels')
  popType?: string      // popSelection: which population type to list ('flow' | 'live' | 'clust')
  // group / section
  repeatable?: boolean
  sortable?: boolean
  collapsed?: boolean
  params?: ParamDef[]
  labelKey?: string    // group: param key whose value is shown in the entry header
}

export interface TaskDef {
  fun_name: string      // canonical "category.task" identifier, e.g. "importImages.omezarr"
  task: string
  label: string
  category: string
  env: string[]
  params: ParamDef[]
  resource_pool?: string  // default resource profile for this task
  scope?: string          // "image" (default) | "set" — set-scope runs once over all selected images
  outputValueName?: string // the value_name this task produces (e.g. "cpCorrected"); read by the
                           // whiteboard to prefill a downstream node's input valueName. Absent when
                           // the output name is a user-set param instead (segment.cellpose).
  outputField?: string    // which image field the output lands in ('filepath' | 'labels'); default 'filepath'
  qcPlot?: string         // plotDefinitions id of this task's default QC plot (e.g. "segmentation_qc"); if set,
                          // the whiteboard Live view auto-shows a QC thumbnail linked to this node
}

export type ParamValues = Record<string, unknown>

// ── Chain template types (shared with backend chain.jl format) ────────────────

export interface ChainNodeSpec {
  id: string
  fn: string            // "category.task" — matches TaskDef.fun_name
  scope: string         // "image" | "set" | "incremental"
  params: Record<string, unknown>
  barrier_policy: string
  resource_pool: string
}

export interface ChainEdgeSpec {
  from: string
  to: string
}

export interface ChainTemplate {
  name: string
  nodes: ChainNodeSpec[]
  edges: ChainEdgeSpec[]
  // UML start-dot targets: node ids the start dot links to. When set, a run executes only the nodes
  // reachable from these (rest are drafts). Persisted verbatim; the whiteboard also stores the dot's
  // position under positions['__start__'].
  startTargets?: string[]
  positions?: Record<string, { x: number; y: number }>
}
