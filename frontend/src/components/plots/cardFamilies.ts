/**
 * Card family registry — the config lookup `CardsPanelBase.vue` reads (BEHAVIOUR_CARDS_PLAN
 * Decision 2). Adding a new family (motifCards / hmmCards / …) later = one entry here plus a
 * ~30-line wrapper view that hands the family to `CardsPanelBase`.
 *
 * Every family shares filmstrip rendering, statScales, PDF export and the CanvasPanel wrap; the
 * config here supplies what varies — the endpoint, the request body shape, empty-state copy, the
 * footer stat-label transform. Detail-panel presentation (Cell / Motif / HMM), if any, is a slot
 * on the base — not part of the config.
 */

import type { CardFamily } from './cardsPanel'

// Cell cards — one card per trackclust pop the manager ticks. `valueName` is omitted so the server
// derives it from `co_clustered_value_names(suffix)` (established in cell_cards_api.jl — a Phase 2
// change that let the cluster panel skip plumbing valueName). The footer strips the `live.track.`
// prefix so the compact row reads "speed", "straightness" — not the fully-qualified name.
export const cellFamily: CardFamily = {
  id: 'cell',
  title: 'Cell cards',
  endpoint: '/api/cell_cards',
  buildRequestBody: ctx => ({
    projectUid: ctx.projectUid,
    rootUid: ctx.rootUid,
    suffix: ctx.suffix,
    pops: ctx.shownPops.map(p => ({ path: p.path, clusterIds: p.clusterIds })),
    maxPx: ctx.maxPx,
    padPx: ctx.padPx,
  }),
  emptyNoRoot: 'Select an image.',
  emptyNoSuffix: 'No clustering run in context.',
  emptyNoShownPops: 'Tick one or more track clusters in the panel on the right.',
  footerStatLabel: name => name.replace(/^live\.track\./, ''),
}

// Motif cards — one card per motif class discovered server-side in the image's cells h5ad. No rail
// picker today (BEHAVIOUR_CARDS_PLAN Decision 6 re-scoped 2026-09-20: motif classes are h5ad obs
// values, not populations). The panel exposes a segmentation picker whose options come back in
// `CardsResponse.availableValueNames`; the picked name is forwarded here as `ctx.valueName`, so a
// user with multiple motif-having segmentations (B, T on a co-imaged spleen) can switch.
// Footer stats: motif.speed / motif.angle medians + `motif.distance`. Label transform strips the
// `live.cell.` prefix so a compact footer row reads "speed" / "angle".
export const motifFamily: CardFamily = {
  id: 'motif',
  title: 'Motif cards',
  endpoint: '/api/motif_cards',
  requireSuffix: false,
  requireShownPops: false,
  buildRequestBody: ctx => ({
    projectUid: ctx.projectUid,
    rootUid: ctx.rootUid,
    ...(ctx.valueName ? { valueName: ctx.valueName } : {}),
    maxPx: ctx.maxPx,
    padPx: ctx.padPx,
  }),
  emptyNoRoot: 'Select an image.',
  footerStatLabel: name => name.replace(/^live\.cell\./, ''),
}

/** Every registered family, keyed by id. `CARD_FAMILIES.cell === cellFamily`. */
export const CARD_FAMILIES: Record<string, CardFamily> = {
  cell: cellFamily,
  motif: motifFamily,
}
