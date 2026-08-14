export interface ParamDef {
  key: string
  label: string
  type: 'int' | 'float' | 'bool' | 'text' | 'dirPath' | 'select' | 'chipSelect'
       | 'channelSelection' | 'valueNameSelection'
       | 'popSelection' | 'labelPropsSelection' | 'labelPropsColsSelection'
       | 'motionDimsSelection'
       | 'group' | 'section'
  tip?: string
  placeholder?: string  // text / dirPath: shown when empty — for dirPath, the default destination
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
  // `help` is per-OPTION guidance — "when would I pick this one" — shown under the select once that
  // option is chosen. Distinct from `tip`, which describes the PARAM: a label like "Gated" says
  // nothing on its own and the answer differs per option, so one param-level tip cannot carry it.
  options?: { label: string; value: string; help?: string }[]
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
  requires?: { axes?: string[] }  // task-applicability gate: axis codes the image must carry (e.g. ["T"]);
                                  // absent = applies to any image. See utils/taskGating.ts + docs/MODULES.md.
  hidden?: boolean        // keep this task OUT of the module page's function list, while leaving it
                          // registered, runnable from the REPL, and available as a chain node. For a
                          // task whose job a purpose-built UI now does better — `importImages.remove`
                          // is the Delete modal's versions scope. Filtered in `useTaskDefs` (the
                          // module-page path) and NOWHERE else on purpose: the chain whiteboard and
                          // the label store read the same route and must still see it.
  previewable?: boolean   // can the task preview run this task's compute over the visible region?
                          // DECLARED in Julia (`task_previewable`, tasks/task.jl) and stamped onto the
                          // spec by the definitions route — never inferred from the params here, which
                          // is what this replaced: sniffing for a cellpose-shaped `models` bag was
                          // right about cellpose and silently wrong about every other backend.
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
