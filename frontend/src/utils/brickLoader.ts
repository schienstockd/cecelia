// ── Brick loader — VirtualBrick → slab URL → shape guard → payload ──────────────────
//
// The brick-atlas viewer's data source is `/api/viewer/slab` with the P0.5 `cTo` extension —
// one request per brick returns ALL channels stacked along Z (KILN_BRICK_PLAN.md → Decision 7).
// This module is the pure-logic half: URL construction and the shape guard, both testable
// without a fetch. The runtime `fetchBrick` at the bottom is a thin wrapper the caller drives.
//
// See docs/todo/KILN_BRICK_PLAN.md → Phase P3.

import { slabUrl, type ViewerMeta, type SlabQuery } from './volumeViewer'
import type { VirtualBrick } from './pageTable'

/** Shape of a `cTo`-carrying slab response, parsed from `X-Slab-Shape: nc,nz,ny,nx`. */
export interface BrickSlabShape {
  nc: number
  nz: number
  ny: number
  nx: number
}

/**
 * Parse the 4-tuple `X-Slab-Shape` header the server emits when `cTo` is present. Returns
 * `null` on any parse failure — an absent or malformed header means the request went to the
 * scalar-c code path (route bug on the caller's side), and the shape guard treats that as an
 * error rather than falling back to a 3-tuple parse silently.
 */
export function parseBrickSlabShape(header: string | null): BrickSlabShape | null {
  if (!header) return null
  const n = header.split(',').map(s => Number(s.trim()))
  if (n.length !== 4 || n.some(v => !Number.isFinite(v) || v <= 0)) return null
  return { nc: n[0], nz: n[1], ny: n[2], nx: n[3] }
}

/**
 * Voxel-space bounds of one brick at one level, given brick size in voxels. `bx`/`by`/`bz`
 * from `VirtualBrick` are in BRICK units — brick (2, 3, 0) at brick size [128, 128, 4] covers
 * voxels `x=256..383, y=384..511, z=0..3` at the LEVEL's resolution (not L0's).
 */
export interface BrickBounds {
  xLo: number
  xHi: number       // inclusive
  yLo: number
  yHi: number
  zLo: number
  zHi: number
}

export function brickBounds(
  brick: VirtualBrick,
  brickSizeVox: readonly [number, number, number],
): BrickBounds {
  const [bx, by, bz] = brickSizeVox
  return {
    xLo: brick.bx * bx,
    xHi: brick.bx * bx + bx - 1,
    yLo: brick.by * by,
    yHi: brick.by * by + by - 1,
    zLo: brick.bz * bz,
    zHi: brick.bz * bz + bz - 1,
  }
}

/**
 * Build a slab URL for one brick × all channels. `nC` comes from `meta.nC` — the caller passes
 * it explicitly because a store's channel count is fixed for the atlas's lifetime and passing
 * it every call catches a stale reference at the type level.
 *
 * The server clamps out-of-range bounds (e.g. an edge brick that hangs off the store), so an
 * atlas built for a padded grid still gets valid bytes back for the interior part of the edge
 * brick. The shape guard below then rejects a payload that came back smaller than expected —
 * which is the atlas's cue to write a shorter row and pad the rest with zeros.
 */
export function brickSlabQuery(
  base: { projectUid: string; imageUid: string; valueName?: string; enc?: 'identity' | 'zstd' },
  brick: VirtualBrick,
  nC: number,
  brickSizeVox: readonly [number, number, number],
): SlabQuery {
  const b = brickBounds(brick, brickSizeVox)
  return {
    projectUid: base.projectUid,
    imageUid: base.imageUid,
    valueName: base.valueName,
    enc: base.enc,
    t: brick.t,
    c: 0,
    cTo: nC - 1,
    x: b.xLo,
    xTo: b.xHi,
    y: b.yLo,
    yTo: b.yHi,
    z: b.zLo,
    zTo: b.zHi,
    level: brick.level,
  }
}

export function brickSlabUrl(
  base: { projectUid: string; imageUid: string; valueName?: string; enc?: 'identity' | 'zstd' },
  brick: VirtualBrick,
  nC: number,
  brickSizeVox: readonly [number, number, number],
): string {
  return slabUrl(brickSlabQuery(base, brick, nC, brickSizeVox))
}

/**
 * Why a response cannot be uploaded to the atlas, or `null` when it can. Same shape as
 * `slabShapeError` in `volumeViewer.ts` — a truncated body or mis-shaped header uploads
 * without complaint and renders a plausible-looking image of the wrong thing.
 *
 * `expectedNC` = `channelsPerBrick` from the atlas layout. `expectedBrickSize` = the layout's
 * `brickSizeVox`. Edge bricks are shorter than the interior — pass the ACTUAL size the server
 * clamped to via `expectedBrickSize`, not the layout's full size, if the caller knows.
 */
export function brickShapeError(
  header: string | null,
  byteLength: number,
  bytesPerVoxel: number,
  expectedNC: number,
  expectedBrickSize: readonly [number, number, number],
): string | null {
  const shape = parseBrickSlabShape(header)
  if (!shape) return 'Brick response carried no 4-tuple X-Slab-Shape header (need nc,nz,ny,nx)'
  const [ebx, eby, ebz] = expectedBrickSize
  if (shape.nc !== expectedNC || shape.nx !== ebx || shape.ny !== eby || shape.nz !== ebz) {
    return `Brick is ${shape.nc}x${shape.nz}x${shape.ny}x${shape.nx} (c,z,y,x) but ` +
           `${expectedNC}x${ebz}x${eby}x${ebx} was asked for`
  }
  const want = shape.nc * shape.nz * shape.ny * shape.nx * bytesPerVoxel
  if (byteLength !== want) return `Brick is ${byteLength} bytes, expected ${want}`
  return null
}

/**
 * Thin fetch wrapper — the runtime side of P3. The URL construction and shape guard above are
 * the parts worth testing; this is the plumbing that ties them to `fetch`. Returns the raw
 * `ArrayBuffer` on success or `null` on any failure (HTTP error, shape mismatch, abort).
 *
 * `signal` — the caller cancels an in-flight brick fetch when the atlas evicts the destination
 * slot or the viewport moves past the brick before it lands. `AbortController` on the caller
 * side; passed through to fetch verbatim.
 */
export async function fetchBrick(
  url: string,
  meta: ViewerMeta,
  expectedNC: number,
  expectedBrickSize: readonly [number, number, number],
  signal?: AbortSignal,
): Promise<{ bytes: ArrayBuffer; shape: BrickSlabShape } | null> {
  let res: Response
  try {
    res = await fetch(url, { signal })
  } catch {
    return null
  }
  if (!res.ok) return null
  const shape = parseBrickSlabShape(res.headers.get('X-Slab-Shape'))
  if (!shape) return null
  const bytes = await res.arrayBuffer()
  const err = brickShapeError(
    res.headers.get('X-Slab-Shape'), bytes.byteLength,
    meta.bytesPerVoxel, expectedNC, expectedBrickSize,
  )
  if (err !== null) return null
  return { bytes, shape }
}
