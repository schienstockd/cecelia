// Per-module accent colours — the SINGLE source shared by the main task manager (TasksModule) and
// the image-table run tags, so a module reads the same colour everywhere. Keyed by the lowercase
// module id (moduleIdFromFun). Unknown modules fall back to grey.
import { funCategory } from './runLog'

export const MODULE_COLORS: Record<string, string> = {
  manageimages:    '#7c3aed',
  metadata:        '#0369a1',
  cleanup:         '#065f46',
  segment:         '#92400e',
  tracking:        '#9d174d',
  behaviour:       '#b45309',
  clustpops:       '#4d7c0f',
  clusttracks:     '#0f766e',
  clustregions:    '#7e22ce',
  spatialanalysis: '#be123c',
  edit:            '#525252',
}

export function moduleColor(m: string): string {
  return MODULE_COLORS[m] ?? '#52525b'
}

// Categories whose page key the suffix rule below can't reach. One page may host SEVERAL categories
// — Manage images runs both `importImages` and `exportImages` — so the rule alone would send an
// export to a module ('export') that has no page, and a running export would never adopt onto it.
const MODULE_OF_CATEGORY: Record<string, string> = {
  importImages: 'manageImages',
  exportImages: 'manageImages',
}

/**
 * The MODULE KEY for a fun_name — the same string a module page passes as `module=`, so a derived
 * task and its page agree ('cleanupImages.cellposeCorrect' → 'cleanup').
 *
 * The ONE derivation. It previously existed three times — here, in `runningTasks.moduleFromFun` and
 * inline in the tasks store — and all three lowercased, which quietly broke every multi-word page:
 * `clustPops` passes `module="clustPops"` and the derivation produced `clustpops`, so those never
 * matched. Case is preserved here precisely because the page keys are camelCase.
 */
export function moduleKeyFromFun(fun: string): string {
  const category = funCategory(fun)
  return MODULE_OF_CATEGORY[category]
      ?? (category.replace(/Images$/i, '').replace(/Tasks$/i, '') || 'chain')
}

// The module id used to look up a COLOUR — same derivation, lowercased, because MODULE_COLORS is
// keyed lowercase. Kept separate from the key so the colour map can't drift from the page identity.
export function moduleIdFromFun(fun: string): string {
  return moduleKeyFromFun(fun).toLowerCase()
}
