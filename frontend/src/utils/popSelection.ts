// A multi-selection of populations in the population manager — which rows a ⋯-menu action reaches,
// and which actions can reach more than one.
//
// The click arithmetic (click / ctrl-click / shift-click) is NOT here: it is `fileManagerPick` in
// `tableSelection.ts`, the same one `SelectionTable` uses, so the manager's list picks exactly like
// every other table. This file is what is specific to populations: a selection is a set of SUBTREES,
// so a selected child of a selected parent travels with the parent (the server's `selection_roots`,
// `app/src/gating/popmanager/mutations.jl`, makes the same call), and a move target may not sit inside
// any of them. Pure ⇒ unit-tested (docs/DEV.md → Tests).
import { isInSubtree } from './popName'

/** The ⋯-menu actions of a population row. */
export type PopAction = 'kiwi' | 'plot' | 'convert' | 'refine' | 'rename' | 'move' | 'combine' | 'deleteBelow' | 'delete'

/**
 * The actions that apply to a multi-selection; every other one is single-only. Why each is where it
 * is: docs/UI.md → *Multi-selection* (the population manager).
 */
export const MULTI_ACTIONS: ReadonlySet<PopAction> = new Set<PopAction>(['kiwi', 'move', 'combine', 'delete'])

/** Hover help on a single-only action while more than one population is selected. */
export const SINGLE_ONLY_TIP = 'One population at a time'

export const actionEnabled = (action: PopAction, count: number) => count <= 1 || MULTI_ACTIONS.has(action)

interface PopLike { path: string; parent: string; transient?: boolean }

/** `paths` minus every path whose ancestor is also in it — what a bulk action acts on directly. */
export function selectionRoots(paths: readonly string[]): string[] {
  const uniq = [...new Set(paths)]
  return uniq.filter(p => !uniq.some(a => a !== p && isInSubtree(p, a)))
}

/**
 * The populations a ⋯-menu action on `menuPath` reaches: the whole selection when the menu was opened
 * on one of its rows (as in a file manager — acting on a selected item acts on the selection), else
 * just that row. Rows that are no longer listed (deleted, another run's) are dropped.
 */
export function actionTargets(selected: readonly string[], menuPath: string, listed: readonly string[]): string[] {
  const live = new Set(listed)
  const sel = selected.filter(p => live.has(p))
  return sel.length > 1 && sel.includes(menuPath) ? sel : [menuPath]
}

/**
 * Where a selection can be moved: all cells (`root`) or any persisted population outside every selected
 * subtree (inside one is a cycle — the server refuses it too). A parent EVERY selected root already
 * sits under is left out: moving there would move nothing.
 */
export function moveTargetsFor(pops: readonly PopLike[], selected: readonly string[]): string[] {
  const roots = selectionRoots(selected)
  const parents = new Set(pops.filter(p => roots.includes(p.path)).map(p => p.parent))
  const stay = parents.size === 1 ? [...parents][0] : null
  return ['root', ...pops
    .filter(t => !t.transient && !roots.some(r => isInSubtree(t.path, r)))
    .map(t => t.path)].filter(t => t !== stay)
}

/** The parent every selected population shares, else `root` — where a combination of them goes. */
export function sharedParent(pops: readonly PopLike[], selected: readonly string[]): string {
  const parents = new Set(pops.filter(p => selected.includes(p.path)).map(p => p.parent))
  return parents.size === 1 ? [...parents][0] : 'root'
}

/** How many populations deleting `selected` removes in all — each selected subtree, counted once. */
export function deleteCount(pops: readonly PopLike[], selected: readonly string[]): number {
  const roots = selectionRoots(selected)
  return pops.filter(p => !p.transient && roots.some(r => isInSubtree(p.path, r))).length
}
