// Function-picker sub-headings (docs/todo/FUNCTION_GROUPS_PLAN.md). A task spec's optional `group`
// ("Segment", "Measure", "Correct", …) says what the function is FOR; this helper turns a module's
// defs into ordered, titled groups so the picker reads as sections instead of one alphabetised block.
// Presentational only — selection stays keyed on `task` / `fun_name`. Distinct from `category`, which
// is the module and is read by the lab-log digest.

import type { TaskDef } from '../tasks/types'

export interface TaskGroup { title: string; defs: TaskDef[] }

/** Title for defs that carry no `group` in a picker where others do (a hand-dropped plugin task). */
export const UNGROUPED = 'Other'

// Workflow order. Only a page's own groups render, so this need only be consistent within a module
// (Segment → Measure → Correct → Correction tools; Autofluorescence → Align → Denoise). Groups not
// listed follow, alphabetically; UNGROUPED is always last.
const GROUP_ORDER = [
  'Import', 'Export', 'Manage',
  'Reshape', 'Project', 'Convert', 'Register',
  'Autofluorescence', 'Align', 'Denoise',
  'Segment', 'Track', 'Measure', 'Correct', 'Correction tools',
  'Contacts', 'Aggregates', 'Neighbourhood',
  'HMM', 'Motifs',
]

function rank(title: string): number {
  if (title === UNGROUPED) return Number.MAX_SAFE_INTEGER
  const i = GROUP_ORDER.indexOf(title)
  return i === -1 ? GROUP_ORDER.length : i
}

/**
 * Group defs under their `group` heading. Returns `null` when headings would not help — fewer than
 * two distinct groups — so the caller renders the flat list it already has. Within a group, defs keep
 * label order.
 */
export function groupTaskDefs(defs: TaskDef[]): TaskGroup[] | null {
  const byTitle = new Map<string, TaskDef[]>()
  for (const d of defs) {
    const title = d.group || UNGROUPED
    if (!byTitle.has(title)) byTitle.set(title, [])
    byTitle.get(title)!.push(d)
  }
  if (byTitle.size < 2) return null
  return [...byTitle.entries()]
    .sort(([a], [b]) => rank(a) - rank(b) || a.localeCompare(b))
    .map(([title, ds]) => ({ title, defs: [...ds].sort((x, y) => x.label.localeCompare(y.label)) }))
}
