/**
 * The settings card's A4 pages (`AcquisitionCardSheet.vue`): which part of the card goes on which
 * page. Page 1 always opens with the header and the settings; the channel table runs on until the page
 * is full and continues on page 2 (a 40-marker PhenoCycler run), then the target, the fill-in fields
 * and the caveat. Heights are estimated in mm from the text (the sheet's fixed print units: 8.5 pt,
 * 186 mm wide), on the safe side — a page that overflows prints its tail onto a blank extra page.
 * The channel table splits between whole groups (a track, a sequence, a cycle), never inside one.
 */

import type { AcquisitionCard } from './acquisitionCard'

/** Printable height of A4 at the sheet's 12 mm margins, less a margin for estimation error. */
export const PAGE_MM = 262
const SHEET_MM = 186
const CHAR_MM = 1.75          // average advance of 8.5 pt DejaVu Sans
const LINE_MM = 4.4           // one table line: 8.5 pt × 1.25 + padding and border
const HEAD_MM = 30            // title box + legend
const CONT_HEAD_MM = 9        // a continued page's one-line header
const H2_MM = 8
const FILL_IN_MM = H2_MM + 3 * 6

export type CardBlock =
  | { kind: 'settings' }
  | { kind: 'channels'; from: number; to: number; cont: boolean }
  | { kind: 'targets'; from: number; to: number; cont: boolean }
  | { kind: 'fillIn' }
  | { kind: 'caveat' }
export interface CardPage { n: number; of: number; blocks: CardBlock[] }

/** Lines a row needs when its cells share the sheet width equally. */
function rowLines(cells: string[], extraCols = 0): number {
  const per = Math.max(4, Math.floor(SHEET_MM / (cells.length + extraCols) / CHAR_MM) - 2)
  return Math.max(1, ...cells.map(t => Math.ceil(t.length / per)))
}

/** The channel table's groups (the units it splits at), each with its estimated height, and the
 *  header's height. */
export function channelGroups(card: AcquisitionCard): { heights: number[]; headMm: number } {
  if (card.markers) {
    return { headMm: H2_MM + 2 * LINE_MM, heights: card.markers.map(cy => cy.rows.reduce((s, m) => s + LINE_MM *
      rowLines(['00', m.marker, m.clone.text, m.reagent.text, m.dye, m.band.text, m.cube.text, m.exposure.text]), 0)) }
  }
  if (card.camera) {
    return { headMm: H2_MM + 2 * LINE_MM, heights: card.camera.map(c => LINE_MM *
      rowLines([c.channel, c.cube.text, c.exposure.text, c.gain.text, c.lamp.text, ...(c.band ? [c.band.text] : [])])) }
  }
  const t = card.detectorTable
  if (!t) return { headMm: 0, heights: [] }
  const extra = (t.groupBy ? 1 : 0) + Object.values(t.cols).filter(Boolean).length
  return { headMm: H2_MM + 2 * LINE_MM, heights: t.groups.map(g => {
    const rows = g.rows.reduce((s, d) => s + LINE_MM * rowLines([d.channel, d.dye, d.detector, d.filter.text,
      d.hv?.text ?? '', d.gainOffset.text, ...(d.lines ? [d.lines.map(l => l.text).join(' ')] : [d.laser])], extra), 0)
    return Math.max(rows, LINE_MM * ((g.lines?.length ?? 0) + (g.splitters ? 2 : 0)))
  }) }
}

function settingsMm(card: AcquisitionCard): number {
  const per = Math.floor((SHEET_MM - 31) / CHAR_MM)
  return H2_MM + LINE_MM + card.settings.reduce((s, r) =>
    s + LINE_MM * r.items.reduce((k, i) => k + Math.ceil(i.text.length / per), 0), 0)
}

const targetRowMm = (card: AcquisitionCard) => LINE_MM * rowLines(['channel name', 'x', 'x',
  ...(card.targetCols.depth ? ['1,240 → 310'] : []), ...(card.targetCols.time ? ['1,240 → 310'] : []), 'x'])
const caveatMm = (card: AcquisitionCard) => 10 + LINE_MM * Math.ceil(card.caveat.length / Math.floor((SHEET_MM - 16) / CHAR_MM))

export function cardPages(card: AcquisitionCard): CardPage[] {
  const pages: CardBlock[][] = [[{ kind: 'settings' }]]
  let left = PAGE_MM - HEAD_MM - settingsMm(card)
  const newPage = () => { pages.push([]); left = PAGE_MM - CONT_HEAD_MM }
  const page = () => pages[pages.length - 1]

  // A table: as many units as fit, the rest continued on the next page under its header again.
  const place = (kind: 'channels' | 'targets', heights: number[], headMm: number, tailMm = 0) => {
    let i = 0
    while (i < heights.length) {
      if (left < headMm + heights[i]) newPage()
      let j = i, used = headMm
      while (j < heights.length && used + heights[j] + (j === heights.length - 1 ? tailMm : 0) <= left) used += heights[j++]
      if (j === i) j = i + 1                       // one unit taller than a page: it goes alone
      page().push({ kind, from: i, to: j, cont: i > 0 })
      left -= used
      i = j
    }
  }
  const ch = channelGroups(card)
  place('channels', ch.heights, ch.headMm)
  const notes = (card.targetsMissing ? LINE_MM : 0) + (card.tissue ? LINE_MM : 0)
  place('targets', card.targets.map(() => targetRowMm(card)), H2_MM + 2 * LINE_MM, notes)
  if (card.targets.length === 0) {
    if (left < H2_MM + 2 * LINE_MM + notes) newPage()
    page().push({ kind: 'targets', from: 0, to: 0, cont: false })
    left -= H2_MM + 2 * LINE_MM + notes
  }
  if (left < FILL_IN_MM + caveatMm(card)) newPage()
  page().push({ kind: 'fillIn' }, { kind: 'caveat' })
  return pages.map((blocks, i) => ({ n: i + 1, of: pages.length, blocks }))
}
