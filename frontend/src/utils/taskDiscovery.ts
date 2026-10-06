// Task discovery — what each task is for, and when it is the wrong step (docs/todo/TASK_DISCOVERY_PLAN.md).
// The text lives in the task specs (`purpose` / `useWhen` / `notWhen`, docs/MODULES.md); this only
// shapes it for the two GUI surfaces: the hover under TaskRunner's function picker, and the Guides
// panel's "Which step?" view. Pure, so it is unit-tested; no copy is written here.
import type { TaskDef } from '../tasks/types'
import { escapeHtml } from '../lib/qc'
import { NAV_GROUPS, navLabelFor } from '../lib/navGroups'
import { TASK_PAGES } from './kiwiTurn'

/** The function picker's hover: use-when and not-when as two short lists, HTML for v-tooltip's
 *  `escape: false` form (every line escaped). '' when the spec has neither. */
export function discoveryTooltipHtml(def: Pick<TaskDef, 'useWhen' | 'notWhen'>): string {
  const block = (cls: string, head: string, lines?: string[]) => lines?.length
    ? `<div class="td-head ${cls}">${head}</div>` + lines.map(l => `<div class="td-line">${escapeHtml(l)}</div>`).join('')
    : ''
  return block('td-use', 'Use when', def.useWhen) + block('td-not', 'Not when', def.notWhen)
}

export interface WhichStepTask {
  funName: string
  label: string
  purpose: string
  useWhen: string[]
  notWhen: string[]
  /** where the task runs, and what pre-selects it there: TaskRunner's `cc-fn:<module>` holds `def.task` */
  path: string
  module: string
  task: string
}

export interface WhichStepGroup { page: string; path: string; tasks: WhichStepTask[] }

// Pages in sidebar order — the pipeline order users already navigate by (lib/navGroups.ts).
const PAGE_ORDER = NAV_GROUPS.flatMap(g => g.items.map(i => i.to))

/** Every visible task that says what it is for, grouped by the page it runs on, pages in sidebar
 *  order. Hidden tasks, tasks on no known page and tasks without a `purpose` are left out. */
export function whichStepGroups(defs: readonly TaskDef[]): WhichStepGroup[] {
  const byPath = new Map<string, WhichStepTask[]>()
  for (const d of defs) {
    if (d.hidden || !d.purpose) continue
    const category = d.fun_name.split('.')[0]
    const page = TASK_PAGES[category]
    if (!page) continue
    const list = byPath.get(page.path) ?? byPath.set(page.path, []).get(page.path)!
    list.push({ funName: d.fun_name, label: d.label, purpose: d.purpose,
                useWhen: d.useWhen ?? [], notWhen: d.notWhen ?? [],
                path: page.path, module: page.module, task: d.task })
  }
  const rank = (p: string) => { const i = PAGE_ORDER.indexOf(p); return i < 0 ? PAGE_ORDER.length : i }
  return [...byPath.entries()]
    .sort(([a], [b]) => rank(a) - rank(b))
    .map(([path, tasks]) => ({ page: navLabelFor(NAV_GROUPS, path), path,
                               tasks: tasks.sort((a, b) => a.label.localeCompare(b.label)) }))
}
