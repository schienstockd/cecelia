// ── Brick MAP_WRITE staging math ──────────────────────────────────────────────────
//
// Byte layout for the MAP_WRITE upload path used by `brickAtlasTexture.writeBrick`
// when Session D's MAP_WRITE mode is on. Kept pure so the tricky part — row-padding
// to WebGPU's 256-byte `bytesPerRow` requirement for `copyBufferToTexture` — is
// testable without a `GPUDevice`, the same discipline `brickAtlas.ts` uses for
// layout math.
//
// Why 256: WebGPU §Queue Copy — the `bytesPerRow` field of `copyBufferToTexture`'s
// buffer argument MUST be a multiple of 256. `writeTexture` has no such requirement
// because the driver repacks internally, which is the whole reason `writeBrick`'s
// current path can pass the tight `brickX * bytesPerVoxel` stride. Swapping in
// `copyBufferToTexture` forces us to lay out the payload row-padded to 256, so the
// memcpy from the wire payload into the staging buffer happens row-by-row rather
// than as one splat per channel.
//
// See docs/todo/WEB_VIEWER_PLAN.md → "Open, with a mechanism behind it".

/** Row stride in the staging buffer — the tight `bx * bpv` rounded up to 256. */
export function paddedBytesPerRow(brickX: number, bytesPerVoxel: number): number {
  const tight = brickX * bytesPerVoxel
  return Math.ceil(tight / 256) * 256
}

/** How many bytes one channel occupies in the staging buffer at the padded stride. */
export function paddedPerChannelBytes(
  brickX: number, brickY: number, brickZ: number, bytesPerVoxel: number,
): number {
  return paddedBytesPerRow(brickX, bytesPerVoxel) * brickY * brickZ
}

/** Total bytes the staging buffer must hold for one brick's full payload
 *  (all channels). Passed to `device.createBuffer({size:...})`. */
export function stagingBufferBytes(
  brickX: number, brickY: number, brickZ: number,
  bytesPerVoxel: number, channelsPerBrick: number,
): number {
  return paddedPerChannelBytes(brickX, brickY, brickZ, bytesPerVoxel) * channelsPerBrick
}

/** Row stride in the wire payload — no padding. */
export function sourceBytesPerRow(brickX: number, bytesPerVoxel: number): number {
  return brickX * bytesPerVoxel
}

/** How many bytes one channel occupies in the wire payload. From `writeBrick`'s
 *  doc: bytes for channel c are contiguous at offset `c × brickZ × brickY × brickX × bpv`. */
export function sourcePerChannelBytes(
  brickX: number, brickY: number, brickZ: number, bytesPerVoxel: number,
): number {
  return brickX * brickY * brickZ * bytesPerVoxel
}

/**
 * Copy the wire payload into a `MAP_WRITE`-mapped staging range at the padded row
 * stride. Splats row by row when `bx * bpv < 256` (the typical case — a 64×64
 * r8uint brick has a 64-byte row); does one big memcpy per channel when the tight
 * row is already 256-aligned (paddedBytesPerRow === tight → `set()` on the whole
 * channel is one memcpy).
 *
 * `dest` MUST be sized to at least `stagingBufferBytes(...)`. `source` MUST be at
 * least `sourcePerChannelBytes(...) * channelsPerBrick`. Neither is checked here —
 * the caller (`writeBrick`) already validates the payload length and refuses
 * truncated inputs.
 */
export function packStaging(
  dest: Uint8Array, source: Uint8Array,
  brickX: number, brickY: number, brickZ: number,
  bytesPerVoxel: number, channelsPerBrick: number,
): void {
  const tightRow = sourceBytesPerRow(brickX, bytesPerVoxel)
  const paddedRow = paddedBytesPerRow(brickX, bytesPerVoxel)
  const srcPerCh = sourcePerChannelBytes(brickX, brickY, brickZ, bytesPerVoxel)
  const dstPerCh = paddedPerChannelBytes(brickX, brickY, brickZ, bytesPerVoxel)
  const rowsPerChannel = brickY * brickZ
  // Fast path — no row padding, whole channel is one memcpy
  if (paddedRow === tightRow) {
    for (let c = 0; c < channelsPerBrick; c++) {
      dest.set(source.subarray(c * srcPerCh, (c + 1) * srcPerCh), c * dstPerCh)
    }
    return
  }
  // Slow path — row-by-row memcpy at the padded stride
  for (let c = 0; c < channelsPerBrick; c++) {
    const srcChOff = c * srcPerCh
    const dstChOff = c * dstPerCh
    for (let r = 0; r < rowsPerChannel; r++) {
      const srcOff = srcChOff + r * tightRow
      dest.set(source.subarray(srcOff, srcOff + tightRow), dstChOff + r * paddedRow)
    }
  }
}
