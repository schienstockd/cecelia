// GPU error classification for the WebGPU viewer.
//
// A renderer's `onError` callback delivers the driver's own text — Vulkan on Chromium/Dawn is
// `vkAllocateMemory failed with VK_ERROR_OUT_OF_DEVICE_MEMORY - While calling [Device].CreateTexture`,
// Metal spells it `Out of memory`, D3D12 has `E_OUTOFMEMORY`. The viewer needs to tell an OOM apart
// from any other GPU error so it can auto-fall back to the memory-conservative brick renderer
// instead of leaving the canvas blank behind a static error chip.
//
// Pure logic — unit-testable, no Vue.

const OOM_MARKERS = [
  'out of memory',      // Metal, generic
  'out_of_memory',      // D3D12 code text
  'out-of-memory',      // WebGPU scope name (spec)
  'outofmemory',        // no-separator variant (E_OUTOFMEMORY / OutOfMemoryError)
  'vkallocatememory',   // Vulkan alloc failure — driver includes this in the message
]

/** Does this renderer error string describe a device-memory allocation failure? Case-insensitive
 *  substring match against the markers the three WebGPU backends emit. Broad on purpose — a false
 *  positive costs one wasted renderer swap, a false negative leaves the canvas blank. */
export function isViewerOom(msg: string): boolean {
  if (!msg) return false
  const s = msg.toLowerCase()
  for (const m of OOM_MARKERS) if (s.includes(m)) return true
  return false
}
