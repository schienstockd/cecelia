import { describe, it, expect } from 'vitest'
import { isViewerOom } from './gpuErrors'

describe('isViewerOom', () => {
  it('matches the Chromium/Dawn/Vulkan driver text — the message the user reported', () => {
    // The exact string is what the brick atlas surfaces via `onError` after `popErrorScope`.
    expect(isViewerOom(
      'Brick atlas: vkAllocateMemory failed with VK_ERROR_OUT_OF_DEVICE_MEMORY - ' +
      'While calling [Device].CreateTexture([TextureDescriptor]).')).toBe(true)
  })

  it("matches Metal's spelling", () => {
    expect(isViewerOom('Tile atlas: Out of memory')).toBe(true)
    expect(isViewerOom('Out Of Memory')).toBe(true)          // case-insensitive
  })

  it('matches D3D12 / WinRT wording', () => {
    expect(isViewerOom('E_OUTOFMEMORY')).toBe(true)
    expect(isViewerOom('Allocation failed: OUT_OF_MEMORY')).toBe(true)
  })

  it('matches the WebGPU error-scope name (spec text)', () => {
    expect(isViewerOom('renderer surfaced an out-of-memory error')).toBe(true)
  })

  it('leaves non-OOM errors alone — the swap must not fire on unrelated failures', () => {
    expect(isViewerOom('TextureView cannot be used with [Device]')).toBe(false)
    expect(isViewerOom('shader compile failed at line 42')).toBe(false)
    expect(isViewerOom('no layout fits budget 2000000000 bytes on this device')).toBe(false)
  })

  it('is safe on empty / whitespace input', () => {
    expect(isViewerOom('')).toBe(false)
    expect(isViewerOom('   ')).toBe(false)
  })
})
