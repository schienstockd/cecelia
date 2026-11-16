// The shown mask coloured by population — the label colour TABLE the renderers' shared
// `labColour` reads when its row count is negative (`lib/webgpu/shaders/label_colour.wgsl`). Pure:
// the texture is in `lib/webgpu/labelColourTexture.ts`.
//
// The policy — the SAME rule a movie's population mask draws (`mask_pop_colours`,
// `api/src/overlay_author.jl`), held to it by a shared fixture (`maskPopTable.parity.json`, read by
// both test suites). A movie recorded from the viewer with *Colour* on pops carries it on its look
// (`maskAllCells: false`, `utils/viewer/viewerLook.ts`):
//  - **Which pops.** The ones whose eye is on in the viewer (`hidden` — the same authority the points
//    use, `buildPointBufferLayers`), on the segmentation the mask shows. The movie drops `show == false`.
//  - **Overlap: the deepest pop wins.** Pops are written in gate-tree depth order (path segments),
//    payload order within a depth, later overwriting earlier.
//  - **Parents pale.** A pop with a visible descendant (a path below it, same pop type) draws the cells
//    no descendant took at `MASK_PARENT_PALE` of the way to white — subsets read as the solid colour on
//    a washed-out parent. A pop whose children are all hidden is a leaf again and draws solid.
//  - **Cells in no visible pop** are `MASK_UNGATED_GREY` (`OVERLAY_GREY`), so the gate's complement
//    stays visible to check against the tissue. Ids outside the cell table (no centroid, so never
//    sent) and in no pop are not drawn.
//  - **Which pop family** is the mask's *Colour* choice (`MaskColour`): the cell pop types on, or the
//    segmentation's track clusters (each tracked cell in its track's cluster pop). The movie takes the
//    family as `maskPopTypes` (`mask_pop_scope`).
//  - **Colour by** paints every cell by the colour-by column (`colourByMaskLayer`), cells with no value
//    grey — the movie's `colourLabels` over all cells (`mask_id_colours`). `maskColourKeys` is the one
//    translation of the choice into a movie config, for the viewer's look and a batch alike.

import SHADER_CONSTANTS from '../lib/webgpu/shaders/constants.json'
import { OVERLAY_GREY, colourByValue, type OverlayPayload } from './viewerOverlays'
import { mix, parseHex, toHex } from './colour'
import { isInSubtree } from './popName'
import { WHITE } from '../plots/fixedColours'

/** Where the shown mask's colours come from: the per-id palette, the cell pops on (flow / clust /
 *  region), the segmentation's track clusters, or the colour-by column. `settings.viewerMaskColour`. */
export type MaskColour = 'id' | 'pops' | 'trackclust' | 'colourBy'
export const MASK_COLOURS: readonly MaskColour[] = ['id', 'pops', 'trackclust', 'colourBy']
export const isMaskColour = (v: unknown): v is MaskColour => MASK_COLOURS.includes(v as MaskColour)

/** The *Colour* choices, as the viewer's Segmentation section and the batch list them. Track clusters
 *  only on a timecourse — unless `current`, so a stored choice stays visible and switchable. */
export function maskColourOptions(timecourse: boolean, current: MaskColour):
  { value: MaskColour; label: string; tip: string }[] {
  return [
    { value: 'id', label: 'Cell id', tip: 'A colour per cell' },
    { value: 'pops', label: 'Pops', tip: 'Each cell by its deepest shown population' },
    ...(timecourse || current === 'trackclust'
      ? [{ value: 'trackclust' as const, label: 'Track clusters', tip: 'Each tracked cell by its track cluster' }] : []),
    { value: 'colourBy', label: 'Colour by', tip: 'Each cell by the colour-by measure' },
  ]
}

/** The movie config's mask keys for a *Colour* choice — what `mask_pop_scope` / `movie_mask` read.
 *  `popsOn`: some cell pop type is drawn (with none, *Pops* keeps the palette, as the viewer does);
 *  `colourBy`: the column (none = *Colour by* keeps the palette). `maskPopTypes` is always written, so
 *  a look patched over an older one never inherits its track clusters. */
export function maskColourKeys(c: MaskColour, popsOn: boolean, colourBy: string):
  { maskAllCells: boolean; maskPopTypes: string[]; colourLabels: boolean } {
  if (c === 'trackclust') return { maskAllCells: false, maskPopTypes: ['trackclust'], colourLabels: false }
  if (c === 'pops' && popsOn) return { maskAllCells: false, maskPopTypes: [], colourLabels: false }
  return { maskAllCells: true, maskPopTypes: [], colourLabels: c === 'colourBy' && !!colourBy }
}

/** Texels per row of a label colour table: id at `(id % W, id / W)` (`labColour`). One number for the
 *  viewer and the movie host (`wgpu_host.LABEL_TABLE_W`), from `shaders/constants.json`. */
export const LABEL_TABLE_W: number = SHADER_CONSTANTS.LABEL_TABLE_W

/** Row cap of a table: WebGPU's guaranteed `maxTextureDimension2D` (every adapter allows at least this
 *  many), so 4096 × 8192 = 33.5 M ids. A larger id is not drawn (the shader's out-of-table branch). */
export const LABEL_TABLE_MAX_ROWS = 8192

/** How far a parent's own cells move towards white — one number for the viewer and the movie
 *  (`shaders/constants.json`, read by `mask_pop_colours` in Julia). */
export const MASK_PARENT_PALE: number = SHADER_CONSTANTS.MASK_PARENT_PALE

/** Cells in no visible population — the movie's all-cells grey. */
export const MASK_UNGATED_GREY = OVERLAY_GREY

/** An `rgba8unorm` table `width` texels wide, `rows` high, row-major — `writeTexture` with
 *  `bytesPerRow = width * 4`. `truncated`: some id was past `width * rows` and is not drawn. */
export interface LabelTable {
  bytes: Uint8Array
  width: number
  rows: number
  truncated: boolean
}

/** One (segmentation, pop type) layer on the shown mask — `PointLayer`'s shape, narrowed to what the
 *  table reads. */
export interface MaskPopLayer {
  payload: {
    cells: { label?: readonly number[] }
    pops: readonly { path: string; colour: string; labels: readonly number[] }[]
  }
  hidden?: ReadonlySet<string>
}

/** The mask's layer when it is coloured by track clusters: the segmentation's trackclust pops, a pop
 *  shown by its eye — `eyeOn(path, show)`, the viewer's `popEyeOn` (a movie's mask takes a look's point
 *  eyes the same way, `_mask_pop_layer`); without one, the manager's `show`. */
export function trackclustMaskLayer(payload: {
  cells: { label?: readonly number[] }
  pops: readonly { path: string; colour: string; labels: readonly number[]; show?: boolean }[]
}, eyeOn: (path: string, show: boolean) => boolean = (_path, show) => show): MaskPopLayer {
  return { payload, hidden: new Set(payload.pops.filter(p => !eyeOn(p.path, p.show !== false)).map(p => p.path)) }
}

/** The mask's layer when it is coloured by the colour-by column: every cell with a value, in the
 *  points' colour for it (`colourByValue`), each colour one flat pop so none pales another; a cell
 *  with no value keeps the table's grey. The scale is this segmentation's own, as the movie's
 *  (`_cb_scale(img, [vn], …)` in `mask_id_colours`). No column on the payload = no pops. */
export function colourByMaskLayer(payload: OverlayPayload, palette: readonly string[] = []): MaskPopLayer {
  const labels = payload.cells.label ?? []
  const vals = payload.values ?? []
  const byValue = colourByValue(payload, palette)
  const groups = new Map<string, number[]>()
  if (byValue) {
    for (let i = 0; i < labels.length; i++) {
      const v = vals[i]
      if (v === null || v === undefined || (typeof v === 'number' && !Number.isFinite(v))) continue
      const hex = toHex(byValue(i).map(v => v * 255))
      const g = groups.get(hex)
      if (g) g.push(labels[i]); else groups.set(hex, [labels[i]])
    }
  }
  return { payload: { cells: payload.cells,
                      pops: [...groups].map(([colour, ls], k) => ({ path: `/${k}`, colour, labels: ls })) } }
}

/** Whether a layer draws any pop of its own — one not in `hidden` (the viewer's *No track clusters on
 *  this mask* note reads the layer it draws, not `show`). */
export function maskLayerShowsAny(layer: MaskPopLayer): boolean {
  return layer.payload.pops.some(p => !layer.hidden?.has(p.path))
}

const depthOf = (path: string) => path.split('/').filter(Boolean).length
/** A malformed colour draws white rather than vanishing — as the points do (`hexToUnit`). */
const bytesOf = (hex: string): [number, number, number] => parseHex(hex) ?? [255, 255, 255]

/** The table for the shown mask, or `null` when there are no cells to colour (the caller then keeps the
 *  per-id palette). Policy: the header above. */
export function maskPopTable(
  layers: readonly MaskPopLayer[], width = LABEL_TABLE_W, maxRows = LABEL_TABLE_MAX_ROWS,
): LabelTable | null {
  type Entry = { depth: number; order: number; rgb: [number, number, number]; labels: readonly number[] }
  const entries: Entry[] = []
  let maxId = 0
  for (const { payload, hidden } of layers) {
    for (const l of payload.cells.label ?? []) if (l > maxId) maxId = l
    const shown = payload.pops.filter(p => !hidden?.has(p.path))
    for (const p of shown) {
      for (const l of p.labels) if (l > maxId) maxId = l
      const pale = shown.some(q => q.path !== p.path && isInSubtree(q.path, p.path))
      entries.push({ depth: depthOf(p.path), order: entries.length,
                     rgb: bytesOf(pale ? mix(p.colour, WHITE, MASK_PARENT_PALE) : p.colour), labels: p.labels })
    }
  }
  if (maxId < 1) return null
  const rows = Math.min(Math.floor(maxId / width) + 1, maxRows)
  const cap = rows * width
  const bytes = new Uint8Array(cap * 4)
  const put = (id: number, rgb: readonly number[]) => {
    if (!(id > 0 && id < cap)) return
    const o = id * 4
    bytes[o] = rgb[0]; bytes[o + 1] = rgb[1]; bytes[o + 2] = rgb[2]; bytes[o + 3] = 255
  }
  const grey = bytesOf(MASK_UNGATED_GREY)
  for (const { payload } of layers) for (const l of payload.cells.label ?? []) put(l, grey)
  entries.sort((a, b) => a.depth - b.depth || a.order - b.order)
  for (const e of entries) for (const l of e.labels) put(l, e.rgb)
  return { bytes, width, rows, truncated: maxId >= cap }
}
