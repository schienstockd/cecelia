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
}
