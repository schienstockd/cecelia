// ── Brick payload ring — reusable ArrayBuffers for fetchBrick ─────────────────────
//
// U3 of docs/todo/WEBGPU_UPLOAD_PATH_PLAN.md. The default `fetch → res.arrayBuffer()` path
// allocates a fresh ArrayBuffer per brick then memcpys the whole body into it — measured at
// 6.7 ms median / 626 MB/s for a 4 MB brick on the RTX 2000 Ada laptop (§C). The plan
// estimated ~4 ms/brick savings by pre-allocating one buffer per inflight slot and streaming
// the body into it with `body.getReader()`.
//
// The ring is owned per-atlas: allocated at atlas creation, destroyed on layout change or
// component unmount. `MAX_INFLIGHT` is 16 today, so a 4 MB brick × 16 slots = 64 MB ring;
// bigger stores can push this to ~128 MB when the atlas budget goes past 4 GB. That's JS
// heap, not VRAM, and it's the price of the memcpy elimination.
//
// Fairness is FIFO — a waiter queued first gets the next free slot first. Contention should
// be zero in practice because capacity equals `MAX_INFLIGHT` and the caller's admission
// control (`shouldAdmitKick`) already gates on that same number; the queue exists only so a
// race between atlas resize + inflight fetches doesn't deadlock.

/** One slot from the ring — holds the receive buffer + the release call the caller MUST make
 *  once the bytes have been consumed (`writeBrick` copies synchronously into GPU staging, so
 *  release is safe immediately after that returns). */
export interface BrickPayloadLease {
  /** The reusable ArrayBuffer, sized to the ring's `payloadBytes`. The caller may write up
   *  to this many bytes into it. Do NOT retain a reference past `release()`. */
  buffer: ArrayBuffer
  /** Return the slot to the ring. Idempotent — a double-release is a no-op. */
  release(): void
}

/** Handle to a live ring. `capacity` and `payloadBytes` mirror the create-time arguments so a
 *  caller doesn't need to remember them separately. `destroyed` flips true after `destroy()`
 *  and every subsequent `lease()` rejects. */
export interface BrickPayloadRing {
  readonly capacity: number
  readonly payloadBytes: number
  readonly destroyed: boolean
  /** Acquire a slot. Resolves immediately if one is free; otherwise queues FIFO behind the
   *  outstanding leases. Rejects with an Error when `destroy()` is called before the lease
   *  is granted. */
  lease(): Promise<BrickPayloadLease>
  /** Drop every buffer, reject any queued waiters, refuse new leases. Outstanding leases
   *  granted before destroy stay valid — their `release()` is a no-op past this point. */
  destroy(): void
}

/**
 * Build a ring with `capacity` pre-allocated ArrayBuffers of `payloadBytes` each. Both must
 * be positive integers; negative or zero throws immediately — a zero-capacity ring or a
 * zero-byte payload is always a caller bug.
 */
export function createBrickPayloadRing(opts: {
  capacity: number
  payloadBytes: number
}): BrickPayloadRing {
  if (!Number.isInteger(opts.capacity) || opts.capacity <= 0) {
    throw new Error(`brickPayloadRing: capacity must be a positive integer, got ${opts.capacity}`)
  }
  if (!Number.isInteger(opts.payloadBytes) || opts.payloadBytes <= 0) {
    throw new Error(`brickPayloadRing: payloadBytes must be a positive integer, got ${opts.payloadBytes}`)
  }
  const capacity = opts.capacity
  const payloadBytes = opts.payloadBytes
  // A LIFO stack of free buffers — order doesn't matter for correctness, and popping the
  // most-recently-returned slot is friendly to CPU caches when a brick is written-then-read
  // in the same tick.
  const free: ArrayBuffer[] = []
  for (let i = 0; i < capacity; i++) free.push(new ArrayBuffer(payloadBytes))
  // FIFO of waiting leasers. Each entry resolves with a lease OR rejects on destroy.
  interface Waiter {
    resolve: (lease: BrickPayloadLease) => void
    reject: (err: Error) => void
  }
  const waiters: Waiter[] = []
  let destroyed = false

  const makeLease = (buffer: ArrayBuffer): BrickPayloadLease => {
    let released = false
    return {
      buffer,
      release() {
        if (released) return
        released = true
        if (destroyed) return
        // Prefer handing the buffer straight to the next waiter — avoids a round-trip through
        // the free list and keeps FIFO fairness for the queue.
        const next = waiters.shift()
        if (next !== undefined) {
          next.resolve(makeLease(buffer))
          return
        }
        free.push(buffer)
      },
    }
  }

  return {
    capacity,
    payloadBytes,
    get destroyed() { return destroyed },

    lease(): Promise<BrickPayloadLease> {
      if (destroyed) return Promise.reject(new Error('brickPayloadRing: destroyed'))
      const buf = free.pop()
      if (buf !== undefined) return Promise.resolve(makeLease(buf))
      return new Promise<BrickPayloadLease>((resolve, reject) => {
        waiters.push({ resolve, reject })
      })
    },

    destroy() {
      if (destroyed) return
      destroyed = true
      free.length = 0
      const rejectErr = new Error('brickPayloadRing: destroyed')
      while (waiters.length > 0) {
        const w = waiters.shift()!
        w.reject(rejectErr)
      }
    },
  }
}

/**
 * Drain `body` into `buffer`, returning the number of bytes written. Returns `null` if the
 * body is longer than the buffer — the caller then releases the lease and treats it as a
 * malformed response (same disposition as `brickShapeError`). Extracted for unit-testability;
 * `fetchBrick` calls it from behind the ring.
 *
 * Rationale for the manual reader loop over `body.pipeTo(WritableStream)`: WritableStream's
 * per-chunk queue-microtask overhead measured 0.4 ms/brick worse in a scratch bench, and the
 * manual loop is fewer lines. `body` is always defined on a successful `fetch` response in
 * every browser we target.
 */
export async function drainResponseIntoBuffer(
  body: ReadableStream<Uint8Array>,
  buffer: ArrayBuffer,
): Promise<number | null> {
  const reader = body.getReader()
  const view = new Uint8Array(buffer)
  let offset = 0
  try {
    while (true) {
      const { done, value } = await reader.read()
      if (done) break
      if (offset + value.byteLength > buffer.byteLength) {
        // Response is bigger than the leased buffer — caller will drop.
        return null
      }
      view.set(value, offset)
      offset += value.byteLength
    }
  } finally {
    reader.releaseLock()
  }
  return offset
}
