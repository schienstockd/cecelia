// The shown mask coloured by population — the label colour TABLE the renderers' shared
// `labColour` reads when its row count is negative (`lib/webgpu/shaders/label_colour.wgsl`). Pure:
// the texture is in `lib/webgpu/labelColourTexture.ts`.
//
// The policy, and how it lines up with a movie's population mask (`mask_id_colours`,
// `api/src/overlay_author.jl`):
//  - **Which pops.** The ones whose eye is on in the viewer (`hidden` — the same authority the points
//    use, `buildPointBufferLayers`), on the segmentation the mask shows. The movie drops `show == false`.
//  - **Overlap: the deepest pop wins.** Pops are written in gate-tree depth order (path segments),
//    payload order within a depth, later overwriting earlier. The movie writes in `resolve_pops` order
//    (the tree, parents before children) and the later one wins too, so a cell in /A and /A/B is /A/B's
//    colour on both sides; they can differ only for two pops of unequal depth in DIFFERENT branches.
//  - **Parents pale.** A pop with a visible descendant (a path below it, same pop type) draws the cells
//    no descendant took at `MASK_PARENT_PALE` of the way to white — subsets read as the solid colour on
//    a washed-out parent. A pop whose children are all hidden is a leaf again and draws solid. The movie
//    has no pale rule (yet): there a parent's left-over cells are its full colour.
//  - **Cells in no visible pop** are `MASK_UNGATED_GREY`, the movie's all-cells colour (`OVERLAY_GREY`),
//    so the gate's complement stays visible to check against the tissue. The movie's population mask
//    does not draw them at all. Ids outside the cell table (no centroid, so never sent) are not drawn.

import SHADER_CONSTANTS from '../lib/webgpu/shaders/constants.json'
import { OVERLAY_GREY } from './viewerOverlays'
import { mix, parseHex } from './colour'
import { isInSubtree } from './popName'

/** Texels per row of a label colour table: id at `(id % W, id / W)` (`labColour`). One number for the
 *  viewer and the movie host (`wgpu_host.LABEL_TABLE_W`), from `shaders/constants.json`. */
export const LABEL_TABLE_W: number = SHADER_CONSTANTS.LABEL_TABLE_W

/** Row cap of a table: WebGPU's guaranteed `maxTextureDimension2D` (every adapter allows at least this
 *  many), so 4096 × 8192 = 33.5 M ids. A larger id is not drawn (the shader's out-of-table branch). */
export const LABEL_TABLE_MAX_ROWS = 8192

/** How far a parent's own cells move towards white. */
export const MASK_PARENT_PALE = 0.6

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
                     rgb: bytesOf(pale ? mix(p.colour, '#ffffff', MASK_PARENT_PALE) : p.colour), labels: p.labels })
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
