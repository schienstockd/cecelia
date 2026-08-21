export interface ParamDef {
  key: string
  label: string
  type: 'int' | 'float' | 'bool' | 'text' | 'dirPath' | 'filePath' | 'select' | 'chipSelect'
       | 'channelSelection' | 'valueNameSelection' | 'valueNameInput'
       | 'popSelection' | 'labelPropsColsSelection'
       | 'motionDimsSelection'
       | 'group' | 'section'
  tip?: string
  placeholder?: string  // text / dirPath / filePath: shown when empty — for dirPath, the default destination
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
  // When true, editing THIS param re-resolves the task's options against the current form — for a
  // param whose value other params' options are derived from (an importer's file path, whose columns
  // become the mapping fields' suggestions). The refetch is debounced at the sink; see TaskRunner.
  // Options obtained this way are SUGGESTIONS only: validation never depends on the form, so a field
  // fed this way must stay valid on its own (`_inject_dynamic_options!` in app/src/tasks/task.jl).
  triggersOptions?: boolean
  // Set by the SERVER (`_inject_dynamic_options!`), never authored in a spec file: this param does
  // not apply to the form as it currently stands, so it renders nowhere. A spec-file `hidden: true`
  // would just be a param nobody can ever set — delete it instead. Chosen over a declarative
  // `showIf` because the condition is often not expressible in the form alone: "the file you picked
  // is an XML export, which has no columns" needs the file read.
  hidden?: boolean
  /** select: `'chips'` renders the same closed set as a segmented ChipSelect instead of a dropdown. */
  variant?: 'chips'
  // chipSelect: draw one chip per ENTRY of the named repeatable group instead of from `options`, so
  // the control grows and shrinks with the group. The value is the ordered list of entry keys that
  // are switched ON — picking is "include this entry in the run", dragging is "run it in this
  // order". Generic on purpose: any group-built control gets ordering by declaring this, rather than
  // each one growing its own reorder widget. `reorderable` is what makes the picked chips draggable.
  optionsFromGroup?: string
  reorderable?: boolean
  // chipSelect + optionsFromGroup: which field of an entry labels its chip (falls back to its
  // position). Same idea as a group's own `labelKey`, which names the entry's header.
  optionLabelKey?: string
  // Show this param only while the form satisfies these conditions: `{ "mode": "attach" }`, or
  // `{ "method": ["gaussian", "bilateral"] }` for one-of. Keys AND, values within a key OR, compared
  // as strings (a spec is JSON; a control's value is a string). This is the DECLARATIVE half of
  // conditional visibility — the half that can be decided from the form alone. A condition needing a
  // file read stays a server hook setting `hidden`. See `showIfSatisfied` in paramValues.ts.
  showIf?: Record<string, string | number | boolean | (string | number | boolean)[]
                        | { endsWith?: string | string[]; notEndsWith?: string | string[] }>
  // Refuse the run with a readable error when the value is missing or empty. Enforced SERVER-side in
  // `validate_params`, so it holds for a chain and the REPL too — not only for a form that drew it.
  required?: boolean
  // The sentence shown when `required` is unmet. "Required param 'pops' is missing" is a key, not a
  // sentence — the tasks that hand-rolled this check said things like "select at least 2
  // populations", which is the part worth keeping.
  requiredMessage?: string
  /** filePath: pickable suffixes for the Browse dialog (e.g. ['.xml', '.csv']). Empty = any file. */
  extensions?: string[]
  multiple?: boolean
  field?: string        // valueNameSelection: which image field to read names from ('filepaths' | 'labels' | 'spatialGraphs')
  // valueNameInput: which storage namespace this param NAMES INTO — the registry entry that makes
  // "the name this task writes under" one greppable concept across six different key spellings
  // (`outputValueName`, `valueNameSuffix`, `graphSuffix`, `statsSuffix`, `colName`, `modelName`).
  // Read by `utils/taskOutput.taskOutput` for the suggestion list, chain propagation and the preview
  // layer stem. See docs/todo/VALUE_NAME_INPUT_PLAN.md → D1.
  namespace?: 'filepaths' | 'labels' | 'spatialGraphs' | 'tracks' | 'branches'
            | 'clusters' | 'regions' | 'stats' | 'models' | 'obsCols'
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
  outputField?: string    // which image field the output lands in ('filepath' | 'labels'); default 'filepath'.
                          // NOTE the singular spelling — consumer params say 'filepaths'. Normalise
                          // both through `utils/taskOutput.normaliseField`, never compare them raw.
  outputNamespace?: string // the namespace a FIXED output lands in; supersedes outputField. Absent on
                           // every spec today — outputField still answers it for image versions/labels.
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
