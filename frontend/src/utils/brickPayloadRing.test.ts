import { describe, it, expect } from 'vitest'
import { createBrickPayloadRing, drainResponseIntoBuffer } from './brickPayloadRing'

describe('createBrickPayloadRing', () => {
  it('rejects invalid capacity or payloadBytes upfront', () => {
    expect(() => createBrickPayloadRing({ capacity: 0, payloadBytes: 1024 })).toThrow(/capacity/)
    expect(() => createBrickPayloadRing({ capacity: -1, payloadBytes: 1024 })).toThrow(/capacity/)
    expect(() => createBrickPayloadRing({ capacity: 1.5, payloadBytes: 1024 })).toThrow(/capacity/)
    expect(() => createBrickPayloadRing({ capacity: 4, payloadBytes: 0 })).toThrow(/payloadBytes/)
    expect(() => createBrickPayloadRing({ capacity: 4, payloadBytes: -1 })).toThrow(/payloadBytes/)
  })

  it('grants leases up to capacity without blocking', async () => {
    const ring = createBrickPayloadRing({ capacity: 3, payloadBytes: 128 })
    const leases = await Promise.all([ring.lease(), ring.lease(), ring.lease()])
    for (const l of leases) {
      expect(l.buffer).toBeInstanceOf(ArrayBuffer)
      expect(l.buffer.byteLength).toBe(128)
    }
    // Buffers are distinct — the ring is a pool of N independent slots, not one shared buffer.
    const ids = new Set(leases.map(l => l.buffer))
    expect(ids.size).toBe(3)
  })

  it('queues waiters when exhausted and resumes on release (FIFO)', async () => {
    const ring = createBrickPayloadRing({ capacity: 1, payloadBytes: 64 })
    const first = await ring.lease()
    const order: number[] = []
    // Two waiters queue up. They should resume in the order they awaited, not in the order
    // release() picks them.
    const p1 = ring.lease().then(l => { order.push(1); return l })
    const p2 = ring.lease().then(l => { order.push(2); return l })
    // Give the microtask queue a tick — neither should have resolved yet.
    await Promise.resolve()
    expect(order).toEqual([])
    first.release()
    const l1 = await p1
    expect(order).toEqual([1])
    l1.release()
    const l2 = await p2
    expect(order).toEqual([1, 2])
    l2.release()
  })

  it('release is idempotent — a double-release does not hand out the same buffer twice', async () => {
    const ring = createBrickPayloadRing({ capacity: 1, payloadBytes: 64 })
    const l1 = await ring.lease()
    l1.release()
    l1.release()   // no-op
    const l2 = await ring.lease()
    // Only ONE additional lease should be grantable — a third must block.
    let resolved = false
    void ring.lease().then(() => { resolved = true })
    await Promise.resolve()
    expect(resolved).toBe(false)
    l2.release()
  })

  it('reuses the same underlying buffer across lease/release cycles', async () => {
    const ring = createBrickPayloadRing({ capacity: 1, payloadBytes: 64 })
    const first = await ring.lease()
    const buf = first.buffer
    // Write a byte so we can see it survive (which it should — nothing zeros the buffer).
    new Uint8Array(buf)[0] = 0xAB
    first.release()
    const second = await ring.lease()
    expect(second.buffer).toBe(buf)
    expect(new Uint8Array(second.buffer)[0]).toBe(0xAB)   // ring does not zero; caller overwrites
    second.release()
  })

  it('destroy rejects new leases and pending waiters', async () => {
    const ring = createBrickPayloadRing({ capacity: 1, payloadBytes: 32 })
    const held = await ring.lease()
    const waiter = ring.lease()
    ring.destroy()
    await expect(waiter).rejects.toThrow(/destroyed/)
    await expect(ring.lease()).rejects.toThrow(/destroyed/)
    expect(ring.destroyed).toBe(true)
    // Released after destroy is a no-op — never resurrects the ring.
    held.release()
    await expect(ring.lease()).rejects.toThrow(/destroyed/)
  })
})

describe('drainResponseIntoBuffer', () => {
  const makeStream = (chunks: Uint8Array[]): ReadableStream<Uint8Array> =>
    new ReadableStream<Uint8Array>({
      start(controller) {
        for (const c of chunks) controller.enqueue(c)
        controller.close()
      },
    })

  it('writes a single-chunk body into the buffer and returns the byte count', async () => {
    const buffer = new ArrayBuffer(16)
    const stream = makeStream([new Uint8Array([1, 2, 3, 4, 5])])
    const n = await drainResponseIntoBuffer(stream, buffer)
    expect(n).toBe(5)
    expect(Array.from(new Uint8Array(buffer, 0, 5))).toEqual([1, 2, 3, 4, 5])
  })

  it('concatenates multiple chunks contiguously', async () => {
    const buffer = new ArrayBuffer(16)
    const stream = makeStream([
      new Uint8Array([10, 20]),
      new Uint8Array([30]),
      new Uint8Array([40, 50, 60]),
    ])
    const n = await drainResponseIntoBuffer(stream, buffer)
    expect(n).toBe(6)
    expect(Array.from(new Uint8Array(buffer, 0, 6))).toEqual([10, 20, 30, 40, 50, 60])
  })

  it('returns null when the body overflows the buffer', async () => {
    const buffer = new ArrayBuffer(4)
    const stream = makeStream([
      new Uint8Array([1, 2, 3, 4, 5]),
    ])
    const n = await drainResponseIntoBuffer(stream, buffer)
    expect(n).toBeNull()
  })

  it('handles a zero-chunk body — returns 0', async () => {
    const buffer = new ArrayBuffer(4)
    const stream = makeStream([])
    const n = await drainResponseIntoBuffer(stream, buffer)
    expect(n).toBe(0)
  })
})
