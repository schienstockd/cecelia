// Function-picker sub-headings (docs/todo/FUNCTION_GROUPS_PLAN.md). A task spec's optional `group`
// ("Segment", "Measure", "Correct", …) says what the function is FOR; this helper turns a module's
// defs into ordered, titled groups so the picker reads as sections instead of one alphabetised block.
// Presentational only — selection stays keyed on `task` / `fun_name`. Distinct from `category`, which
// is the module and is read by the lab-log digest.

import type { TaskDef } from '../tasks/types'

export interface TaskGroup { title: string; defs: TaskDef[] }

/** Title for defs that carry no `group` in a picker where others do (a hand-dropped plugin task). */
export const UNGROUPED = 'Other'

// Workflow order. Only a page's own groups render, so this need only be consistent within a module.
// The page opens on the first group's first function (TaskRunner), so each page's FIRST group is its
// recipe's first step (docs/todo/GUI_CLARITY_PLAN.md, Decision 12): Segment → Measure → Correct →
// Correction tools → Import (labels made elsewhere are the exception, not the start); Align (drift)
// → Autofluorescence → Denoise, because the AF task expects a drift-corrected store. On Manage images
// (import + export tasks) importing IS the first step: Import → Manage → Export. Groups not listed
// follow, alphabetically; UNGROUPED is always last.
const GROUP_ORDER = [
  'Reshape', 'Project', 'Convert', 'Register',
  'Align', 'Autofluorescence', 'Denoise',
  'Segment', 'Track', 'Measure', 'Correct', 'Correction tools',
  'Import', 'Manage', 'Export',
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

