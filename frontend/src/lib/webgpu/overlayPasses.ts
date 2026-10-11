// The overlay passes' host side — population points and track tails — shared by all three renderers
// (flat MIP, brick, tile). The WGSL half is `shaders/overlay_quads.wgsl`.
//
// ONE COPY of the instance layouts, because an attribute offset that drifts from `POINT_STRIDE` /
// `SEG_STRIDE` mis-draws silently: the marker still renders, just from the wrong floats.

import { POINT_STRIDE, SEG_STRIDE } from '../../utils/viewerOverlays'

/** One point instance: centre (absolute image µm), rgb, z plane. */
export const POINT_VERTEX_LAYOUT: GPUVertexBufferLayout = {
  arrayStride: POINT_STRIDE * 4,
  stepMode: 'instance',
  attributes: [
    { shaderLocation: 0, offset: 0, format: 'float32x3' },    // centre, absolute image µm
    { shaderLocation: 1, offset: 12, format: 'float32x3' },   // rgb
    { shaderLocation: 2, offset: 24, format: 'float32' },     // z plane
  ],
}

/** One track-tail segment: from, to (absolute image µm), rgb, z plane of the segment's END. */
export const SEG_VERTEX_LAYOUT: GPUVertexBufferLayout = {
  arrayStride: SEG_STRIDE * 4,
  stepMode: 'instance',
  attributes: [
    { shaderLocation: 0, offset: 0, format: 'float32x3' },    // from, absolute image µm
    { shaderLocation: 1, offset: 12, format: 'float32x3' },   // to
    { shaderLocation: 2, offset: 24, format: 'float32x3' },   // rgb
    { shaderLocation: 3, offset: 36, format: 'float32' },     // z plane of the segment's END
  ],
}

/**
 * An overlay pipeline: six generated vertices per instance, alpha-blended over the finished image in
 * the SAME pass — no second attachment, no second clear. `layout` is the renderer's own, so the pass
 * shares its uniform buffer and therefore its camera.
 */
export function overlayPipelineDescriptor(
  module: GPUShaderModule, layout: GPUPipelineLayout, format: GPUTextureFormat, buffer: GPUVertexBufferLayout,
): GPURenderPipelineDescriptor {
  return {
    layout,
    vertex: { module, entryPoint: 'vs', buffers: [buffer] },
    fragment: {
      module, entryPoint: 'fs',
      targets: [{
        format,
        blend: {
          color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
          alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
        },
      }],
    },
    primitive: { topology: 'triangle-list' },
  }
}

/**
 * Write an instance array into a grow-only vertex buffer: reallocated only when too small, so
 * toggling a population off and on again does not churn VRAM. `minBytes` keeps a buffer allocated
 * for empty data. Returns the (possibly new) buffer and its capacity in bytes.
 */
export function writeInstances(
  device: GPUDevice, buf: GPUBuffer | null, capBytes: number, data: Float32Array, minBytes = 0,
): { buf: GPUBuffer; cap: number } {
  const need = Math.max(data.byteLength, minBytes)
  if (!buf || need > capBytes) {
    buf?.destroy()
    capBytes = need
    buf = device.createBuffer({ size: capBytes, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST })
  }
  if (data.byteLength > 0) device.queue.writeBuffer(buf, 0, data)
  return { buf, cap: capBytes }
}
