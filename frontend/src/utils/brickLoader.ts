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
  zOffset: number = 0,
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
    // zOffset is the FIRST plane the viewer is looking at — 0 in an uncropped volume view, but
    // in plane mode it's the currently-shown plane (`zPlane.value`), and in a cropped 3D view
    // it's `zRange[0]`. The brick's `bz` is relative to that origin, so a brick at bz=0 with
    // brickZ=1 in plane mode fetches ONE plane, the user's plane — not plane 0 of the store.
    z: b.zLo + zOffset,
    zTo: b.zHi + zOffset,
    level: brick.level,
  }
}

export function brickSlabUrl(
  base: { projectUid: string; imageUid: string; valueName?: string; enc?: 'identity' | 'zstd' },
  brick: VirtualBrick,
  nC: number,
  brickSizeVox: readonly [number, number, number],
  zOffset: number = 0,
): string {
  return slabUrl(brickSlabQuery(base, brick, nC, brickSizeVox, zOffset))
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
  // Edge bricks: the server clamps `xTo`/`yTo` to the store's bounds so an edge brick comes back
  // shorter than the interior on x and/or y. The caller pads the payload to full brick size before
  // writeTexture — the padded voxels are never sampled because the shader skips `vi.x >= p.dims.x`.
  // nc + nz must still match exactly (the atlas has no way to compensate for a wrong channel count
  // or a wrong z-thickness). shape.nx/ny must not EXCEED the expected — a server that returns MORE
  // than we asked for is either a route bug or a stale reply, both silent-corruption risks.
  if (shape.nc !== expectedNC || shape.nz !== ebz) {
    return `Brick is ${shape.nc}x${shape.nz}x${shape.ny}x${shape.nx} (c,z,y,x) but ` +
           `${expectedNC}x${ebz}x${eby}x${ebx} was asked for`
  }
  if (shape.nx > ebx || shape.ny > eby) {
    return `Brick nx/ny exceeds expected: got ${shape.nx}x${shape.ny}, max ${ebx}x${eby}`
  }
  const want = shape.nc * shape.nz * shape.ny * shape.nx * bytesPerVoxel
  if (byteLength !== want) return `Brick is ${byteLength} bytes, expected ${want}`
  return null
}

/**
 * Copy a `(nc, nz, actual.ny, actual.nx)` buffer into a full-size `(nc, nz, ebx, eby)` output,
 * leaving the padded voxels as zero. The layout is x-fastest → y → z → c (column-major, the shape
 * the server writes). Used ONLY when the server clamped x or y at a store edge; interior bricks
 * skip this call and use the response bytes directly.
 */
export function padBrickPayload(
  bytes: ArrayBuffer,
  actual: BrickSlabShape,
  expectedBrickSize: readonly [number, number, number],
  bytesPerVoxel: number,
): ArrayBuffer {
  const [ebx, eby, ebz] = expectedBrickSize
  const total = actual.nc * ebz * eby * ebx * bytesPerVoxel
  const out = new Uint8Array(total)
  const src = new Uint8Array(bytes)
  const rowBytes = actual.nx * bytesPerVoxel
  const dstRow = ebx * bytesPerVoxel
  // Per c → per z → per y: copy actual.nx*bpv bytes into the row's leading edge; the rest stays
  // zero, and the shader never samples there.
  for (let c = 0; c < actual.nc; c++) {
    for (let z = 0; z < actual.nz; z++) {
      for (let y = 0; y < actual.ny; y++) {
        const srcOff = ((c * actual.nz + z) * actual.ny + y) * rowBytes
        const dstOff = ((c * ebz + z) * eby + y) * dstRow
        out.set(src.subarray(srcOff, srcOff + rowBytes), dstOff)
      }
    }
  }
  return out.buffer
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
  const header = res.headers.get('X-Slab-Shape')
  const shape = parseBrickSlabShape(header)
  if (!shape) return null
  const bytes = await res.arrayBuffer()
  const err = brickShapeError(
    header, bytes.byteLength, meta.bytesPerVoxel, expectedNC, expectedBrickSize,
  )
  if (err !== null) return null
  return { bytes, shape }
}
