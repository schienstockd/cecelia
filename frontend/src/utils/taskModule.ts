// Per-module accent colours — the SINGLE source shared by the main task manager (TasksModule) and
// the image-table run tags, so a module reads the same colour everywhere. Keyed by the lowercase
// module id (moduleIdFromFun). Unknown modules fall back to grey.
import { funCategory } from './runLog'

export const MODULE_COLORS: Record<string, string> = {
  import:          '#7c3aed',
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

// The lowercase module id for a fun_name — matches the tasks store's derivation
// ('cleanupImages.cellposeCorrect' → 'cleanup'), so colours line up with the task manager.
export function moduleIdFromFun(fun: string): string {
  return funCategory(fun).replace(/Images$/i, '').replace(/Tasks$/i, '').toLowerCase()
}
