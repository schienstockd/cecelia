/**
 * Let the VRAM of just-destroyed textures actually go before the next allocation.
 *
 * Dawn frees a destroyed texture's memory only once a queue serial AFTER the destroy has retired, so
 * `onSubmittedWorkDone()` alone is not enough: after a bulk free nothing else submits (a renderer
 * with no atlas or bound slot skips its draw), the serial never advances, and allocating straight
 * after needs old + new resident at once — an out-of-memory scope firing on a cache that would fit.
 * Submit an empty command buffer to advance the serial, wait for it, and do it TWICE: the memory is
 * released on the device tick after the serial completes, which one wait can resolve ahead of.
 * Measured on an 8 GB NVIDIA (Vulkan) adapter, VRAM filled to the first OOM, then a reallocate and
 * one 192 MiB frame: no submit 12/12 false OOMs, one round 5/12, two rounds 0/12.
 */
export async function retireDestroyed(device: GPUDevice): Promise<void> {
  for (let i = 0; i < 2; i++) {
    device.queue.submit([device.createCommandEncoder().finish()])
    await device.queue.onSubmittedWorkDone()
  }
}
