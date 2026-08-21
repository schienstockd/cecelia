// Per-module accent colours — the SINGLE source shared by the main task manager (TasksModule) and
// the image-table run tags, so a module reads the same colour everywhere. Keyed by the lowercase
// module id (moduleIdFromFun). Unknown modules fall back to grey.
import { funCategory } from './runLog'
import { composite, readableOn } from './colour'

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
  return MODULE_COLORS[m.toLowerCase()] ?? '#52525b'
}

//: The pill's fill opacity over whatever surface it sits on — kept as a number because the LABEL's
//: contrast has to be computed against the composited result, not against the `22` suffix.
const TAG_FILL_ALPHA = 0x22 / 255

//: The surface the LABEL's contrast is computed against — `--cc-surface-2`, the LIGHTEST surface a tag
//: sits on (the PrimeVue tooltip). The fill is translucent, so its painted colour depends on what is
//: behind it, and CSS cannot tell us; picking the lightest is the worst case for light label text, so a
//: label readable here is readable on `--cc-bg` and `--cc-surface-1` too.
const TAG_SURFACE = '#21262d'

/**
 * The inline colours for a `.cc-module-tag` — the ONE derivation of the pill's tint from the palette,
 * shared by the task manager, the image table's run tag and the QC tooltip's provenance badge.
 *
 * It existed twice with two different tints (`+'22'` with a `+'55'` border in ImageTable, a bare
 * `+'33'` in TasksModule), which is why the same module read as two slightly different pills on two
 * pages. Accepts either spelling of the module id — `moduleKeyFromFun` preserves camelCase for page
 * identity while `MODULE_COLORS` is keyed lowercase, and passing the key straight in silently returned
 * grey for every multi-word module (`clustPops`, `spatialAnalysis`, `manageImages`, …).
 *
 * **The LABEL is not the raw accent.** Using the accent as its own label text measured 1.84-2.70:1
 * against the pill's fill — every one of the twelve module colours below WCAG AA, worst for the dark
 * greens (`cleanup #065f46`). `readableOn` lifts it toward white by the least amount that reaches 4.5:1
 * (28-42% depending on the hue), which keeps the module recognisable — a lifted purple is still that
 * purple — where `--cc-text` would have thrown the identity away. The fill and border are untouched, so
 * the pill's shape and colour block look exactly as before.
 */
export function moduleTagStyle(module: string): Record<string, string> {
  const c = moduleColor(module)
  return {
    background: c + '22',
    color: readableOn(c, composite(c, TAG_SURFACE, TAG_FILL_ALPHA)),
    borderColor: c + '55',
  }
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
 * task and its page agree ('cleanupImages.smooth' → 'cleanup').
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
