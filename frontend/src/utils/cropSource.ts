// A crop's provenance (`cropSource` on the image payload, written by `editImages.cropImage`) as the
// one line the image-metadata dialog shows. Box bounds are full-res pixels, half-open; `-1` on an
// axis means it was kept whole, so that axis is left out of the line.

export interface CropBox {
  x0: number; x1: number; y0: number; y1: number
  z0: number; z1: number; t0: number; t1: number
}

export interface CropSource {
  uid: string               // source image uid
  valueName: string | null  // source version that was cropped
  box: CropBox
}

/** "Cropped from <name or uid> (<version>) · x 49–516 · y 279–747", or null when not a crop. */
export function cropSourceLine(cs: CropSource | null | undefined, sourceName?: string | null): string | null {
  if (!cs) return null
  const b = cs.box
  const axes = ([['x', b.x0, b.x1], ['y', b.y0, b.y1], ['z', b.z0, b.z1], ['t', b.t0, b.t1]] as const)
    .filter(([, lo, hi]) => lo !== -1 && hi !== -1)
    .map(([a, lo, hi]) => `${a} ${lo}–${hi}`)
  const head = `Cropped from ${sourceName || cs.uid}${cs.valueName ? ` (${cs.valueName})` : ''}`
  return [head, ...axes].join(' · ')
}
