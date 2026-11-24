// Tissue extent — how big the imaged volume is in physical units: X × Y × Z, the XY area and the
// volume. Pixel counts × pixel size. The pixel counts come from the ACTIVE version's store
// (`frameGeometry` → `/api/images/geometry`), never a stored per-image field: drift correction grows
// the canvas and a crop shrinks it, so the extent belongs to a version (see image_geometry.jl).
//
// Shared by the image table's "Tissue" column (short cell + popover) and the metadata modal, both
// through `TissueExtent.vue`, so the two read the same numbers.

import { lengthUnitUm } from './physicalSize'

export interface ExtentImage {
  sizeZ?: number | null
  physicalSizeX?: number | null
  physicalSizeY?: number | null
  physicalSizeZ?: number | null
  physicalSizeUnit?: string | null
  /** may carry a per-axis `PhysicalSizeZUnit` etc. that overrides `physicalSizeUnit` (as the backend's
   *  `_meta_axis_length_unit` does) */
  extraMeta?: Record<string, string | number | boolean>
}

export interface TissueExtent {
  /** pixels (X, Y) and planes (Z) of the version the geometry was read from */
  px: { x: number; y: number; z: number }
  /** physical lengths in µm; null when the axis has no pixel size (or Z is a single plane) */
  um: { x: number | null; y: number | null; z: number | null }
  /** µm² / µm³; null when an axis it needs is missing */
  areaUm2: number | null
  volumeUm3: number | null
}

const pos = (n: number | null | undefined): n is number => typeof n === 'number' && Number.isFinite(n) && n > 0

/** One axis' pixel size in µm, or null when absent or in a unit we can't convert — an unknown unit
 *  read as µm would print a length off by orders of magnitude, so it counts as uncalibrated. */
function axisUm(img: ExtentImage, axis: 'X' | 'Y' | 'Z', n: number | null | undefined): number | null {
  if (!pos(n)) return null
  const own = img.extraMeta?.[`PhysicalSize${axis}Unit`]
  const f = lengthUnitUm(typeof own === 'string' && own.trim() ? own : img.physicalSizeUnit)
  return f == null ? null : n * f
}

/** Extent of a frame `geom` (pixels) sampled at the image's pixel size. A single plane has no Z
 *  length and so no volume — the area is the answer for a 2D image. Z is planes × step. */
export function tissueExtent(geom: { sizeX: number; sizeY: number; sizeZ?: number | null },
                             img: ExtentImage): TissueExtent {
  const zPlanes = pos(geom.sizeZ) ? geom.sizeZ : (pos(img.sizeZ) ? img.sizeZ : 1)
  const sx = axisUm(img, 'X', img.physicalSizeX)
  const sy = axisUm(img, 'Y', img.physicalSizeY ?? img.physicalSizeX)
  const sz = axisUm(img, 'Z', img.physicalSizeZ)
  const x = pos(sx) ? geom.sizeX * sx : null
  const y = pos(sy) ? geom.sizeY * sy : null
  const z = zPlanes > 1 && pos(sz) ? zPlanes * sz : null
  const areaUm2 = x != null && y != null ? x * y : null
  return {
    px: { x: geom.sizeX, y: geom.sizeY, z: zPlanes },
    um: { x, y, z },
    areaUm2,
    volumeUm3: areaUm2 != null && z != null ? areaUm2 * z : null,
  }
}

/** Three significant figures, trailing zeros dropped: 2563.2 → "2560", 0.03912 → "0.0391". */
export function sig3(n: number): string {
  if (n === 0) return '0'
  return Number(n.toPrecision(3)).toString()
}

/** One length, in µm below 1 mm and in mm from there: "640 µm", "2.56 mm". */
export function formatLength(um: number): string {
  return um >= 1000 ? `${sig3(um / 1000)} mm` : `${sig3(um)} µm`
}

/** Area in mm² — the unit a tissue section is talked about in. */
export const formatArea = (um2: number): string => `${sig3(um2 / 1e6)} mm²`

/** Volume in mm³ from 0.01 mm³ up, µm³ below (a small stack reads as "3.68e6 µm³" otherwise). */
export function formatVolume(um3: number): string {
  const mm3 = um3 / 1e9
  if (mm3 >= 0.01) return `${sig3(mm3)} mm³`
  return `${sig3(um3 / 1e6)}×10⁶ µm³`
}

/** The table cell: "2.56 × 2.56 mm", "307 × 307 × 39 µm". ONE unit for the triple — picked by the
 *  largest axis — so the numbers compare at a glance. Uncalibrated → the pixel counts. */
export function extentShort(e: TissueExtent): string {
  const { x, y, z } = e.um
  if (x == null || y == null) {
    return e.px.z > 1 ? `${e.px.x} × ${e.px.y} × ${e.px.z} px` : `${e.px.x} × ${e.px.y} px`
  }
  const mm = Math.max(x, y, z ?? 0) >= 1000
  const f = (um: number) => sig3(mm ? um / 1000 : um)
  return `${f(x)} × ${f(y)}${z != null ? ` × ${f(z)}` : ''} ${mm ? 'mm' : 'µm'}`
}
