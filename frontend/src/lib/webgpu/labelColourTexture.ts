// The label colour texture every renderer binds as `pal` (`shaders/label_colour.wgsl`): the per-id
// palette row by default, or a population colour table (`utils/maskPopTable.ts`). One builder, so the
// tile, flat-volume and brick renderers cannot disagree about what a row count means.

import { LABEL_PALETTE_N, labelPaletteBytes } from '../../utils/viewerLabels'
import type { LabelTable } from '../../utils/maskPopTable'

export interface LabelColourTexture {
  texture: GPUTexture
  /** The `lab.paletteRows` uniform the texture is read with: the palette's row count, or `-width`
   *  for a table (the shader's switch to `(id % W, id / W)`). */
  rows: number
}

/** `null` = the palette. A table taller than the device allows is cut to the rows it does — the ids
 *  past it fall into the shader's out-of-table branch and are not drawn. */
export function createLabelColourTexture(device: GPUDevice, table: LabelTable | null): LabelColourTexture {
  const width = table ? table.width : LABEL_PALETTE_N
  const height = table ? Math.min(table.rows, device.limits.maxTextureDimension2D) : 1
  const texture = device.createTexture({
    size: [width, height], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })
  const bytes = table ? table.bytes.subarray(0, width * height * 4) : labelPaletteBytes()
  device.queue.writeTexture({ texture }, bytes, { bytesPerRow: width * 4 }, [width, height])
  return { texture, rows: table ? -width : LABEL_PALETTE_N }
}
