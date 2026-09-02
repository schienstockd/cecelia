export interface ParamDef {
  key: string
  label: string
  type: 'int' | 'float' | 'bool' | 'text' | 'dirPath' | 'filePath' | 'select' | 'chipSelect'
       | 'channelSelection' | 'valueNameSelection' | 'valueNameInput'
       | 'popSelection' | 'labelPropsColsSelection'
       | 'motionDimsSelection'
       | 'group' | 'section'
  tip?: string
  // A tip whose applicable text depends on the IMAGE (in practice, whether it carries T). Ordered
  // list, first-with-satisfied-`requires` wins; an entry without `requires` matches anything, so it
  // is the fallback and belongs LAST. Absent tips (nothing matched, or the entry was T-only and the
  // image is static) render no info icon — better than showing prose that does not apply.
  // Declarative on purpose: the alternative was a Vue computed sniffing image axes and switching
  // strings, which is exactly the "hand-rolled visibility" this exists to delete.
  tips?: Array<{ text: string; requires?: { axes?: string[] } }>
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
  // Show this param only while the form satisfies these conditions: `{ "mode": "attach" }`, or
  // `{ "method": ["gaussian", "bilateral"] }` for one-of. Keys AND, values within a key OR, compared
  // as strings (a spec is JSON; a control's value is a string). This is the DECLARATIVE half of
  // conditional visibility — the half that can be decided from the form alone. A condition needing a
  // file read stays a server hook setting `hidden`. See `showIfSatisfied` in paramValues.ts.
  // Relabel a `chipSelect`'s options with a physical quantity derived from the SELECTED IMAGES,
  // leaving the values untouched. `frameDuration` reads each option as a frame lag and appends what
  // it spans at the finest frame interval in the set — `"4"` → `"4 · 60s"`. The one case today is
  // `opticalFlow.train`'s temporal scales, where a lag is not a displacement until you know the rate
  // and the whole point of the seconds mode is that the DURATIONS define the model. See
  // `utils/frameDuration.ts`; a value with no rate to read is left exactly as the spec wrote it.
  labelUnit?: 'frameDuration'
  showIf?: Record<string, string | number | boolean | (string | number | boolean)[]
                        | { endsWith?: string | string[]; notEndsWith?: string | string[] }>
  // Gate this param on the IMAGE, not the form — `showIf`'s image-side counterpart. Same shape as
  // `TaskDef.requires`: `axes` lists axis codes the image must carry (e.g. `["T"]`). A param that
  // fails renders nowhere AND is filtered out of the effective run server-side, so the handler's
  // `get(params, key, default)` returns the "off" default. Used when a task's math applies to any
  // image but a subset of controls is temporal-only (smooth: spatial sigma applies to a still,
  // temporal window does not) — splitting the whole task on `TaskDef.requires` would refuse the run
  // for a case that actually works. See `paramApplies` in paramValues.ts.
  requires?: { axes?: string[] }
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
  labelKey?: string    // group: param key whose value names the entry — its header, and its chip in the order row
  // group + repeatable: starting values for the Nth entry, overlaid on the sub-params' own defaults
  // when that entry is ADDED. Index 0 is the first entry, so it is normally omitted — the plain
  // defaults already describe it.
  //
  // This exists because entries of a repeatable group are not interchangeable. Coastal's `models`
  // group is applied in order and each entry fills only what an earlier one left, so a second entry
  // born as a COPY of the first is the one configuration it must never have: two passes that do the
  // same thing, at twice the cost, where the second contributes almost nothing. See
  // docs/SEGMENTATION.md → *Two passes = two model groups*.
  entryDefaults?: Array<Record<string, unknown>>
  // group + repeatable: HOW the entries combine, in one line, shown as soon as there are two of
  // them. Not hardcoded in the renderer because the rule is per task: coastal's and cellpose's
  // entries are applied in order and each labels only what an earlier one left, while the AF spec's
  // channel combinations are independent and order means nothing. The renderer used to state
  // coastal's rule for all of them.
  entriesTip?: string
  // How this param is DRAWN in the strip beside a repeatable group (`paramVis.ts`). A role, not a
  // key match: matching on `seedSize` inside the renderer would make the picture a second, silently
  // divergent description of the form. Omit it and the param is not drawn.
  //   diameter - a circle of that size (a seed window)
  //   blur     - a soft ring (a gaussian sigma)
  //   distance - a short span (a merge search radius)
  //   area     - a disc whose AREA is the value (a size floor, in um^2)
  //   fraction - a marker on a 0-1 track (a threshold, a weight)
  vis?: 'diameter' | 'blur' | 'distance' | 'area' | 'fraction'
  // A figure offered BESIDE this param, by name — `tasks/paramFigures.ts` owns what each name builds.
  // For a choice whose options differ in what they DO rather than in any number on the form, where
  // `vis` has nothing to draw. Named in the spec rather than matched on the key in the renderer, for
  // the same reason `vis` is a role: a picture keyed off `temporalStat` inside a Vue file is a second
  // description of the form, and the next task that wants one would have to edit the renderer.
  figure?: string
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
  // Task-applicability gate. `axes`: axis codes the image must carry (e.g. ["T"]). `scale`: physical
  // scales it must RECORD (["xy"], ["xy","t"]) — declared by anything computing in microns, because
  // a missing pixel size falls back to 1.0 and reports pixels as microns. Absent = applies to any
  // image. See utils/taskGating.ts + docs/MODULES.md.
  requires?: { axes?: string[]; scale?: string[] }
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
