import { describe, it, expect, vi } from 'vitest'
import { coalesceByKey } from './coalesce'

// A deferred promise + a counting worker, so we can assert exactly how many times the underlying work
// ran for a given interleaving of calls.
function deferred<T>() {
  let resolve!: (v: T) => void, reject!: (e: unknown) => void
  const promise = new Promise<T>((res, rej) => { resolve = res; reject = rej })
  return { promise, resolve, reject }
}

describe('coalesceByKey', () => {
  it('runs work once for concurrent calls with the same key and returns the same promise', async () => {
    const d = deferred<string>()
    const work = vi.fn((_key: string) => d.promise)
    const run = coalesceByKey(work)

    const a = run('k')
    const b = run('k')
    expect(work).toHaveBeenCalledTimes(1)
    expect(a).toBe(b)                     // both callers share the in-flight promise

    d.resolve('done')
    expect(await a).toBe('done')
    expect(await b).toBe('done')
  })

  it('runs work separately for concurrent calls with different keys', () => {
    const work = vi.fn((_key: string) => deferred<string>().promise)
    const run = coalesceByKey(work)
    run('k1'); run('k2')
    expect(work).toHaveBeenCalledTimes(2)
  })

  it('re-runs work for the same key once the previous call has settled', async () => {
    const d1 = deferred<string>()
    const work = vi.fn()
      .mockImplementationOnce(() => d1.promise)
      .mockImplementationOnce(() => Promise.resolve('second'))
    const run = coalesceByKey(work as (k: string) => Promise<string>)

    const first = run('k')
    d1.resolve('first')
    expect(await first).toBe('first')
    await Promise.resolve()               // let the .finally clear the slot

    const second = run('k')               // same key, but the first already settled → fresh work
    expect(work).toHaveBeenCalledTimes(2)
    expect(await second).toBe('second')
  })

  it('clears the slot on rejection so the key can be retried', async () => {
    const d1 = deferred<string>()
    const work = vi.fn()
      .mockImplementationOnce(() => d1.promise)
      .mockImplementationOnce(() => Promise.resolve('ok'))
    const run = coalesceByKey(work as (k: string) => Promise<string>)

    const first = run('k')
    d1.reject(new Error('boom'))
    await expect(first).rejects.toThrow('boom')
    await Promise.resolve()

    const retry = run('k')
    expect(work).toHaveBeenCalledTimes(2)
    expect(await retry).toBe('ok')
  })
})
