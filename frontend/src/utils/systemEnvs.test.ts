import { describe, it, expect, vi, afterEach } from 'vitest'
import { requestEnvInstall, onSystemEnvsChanged, invalidateSystemEnvs } from './systemEnvs'

function stubResponse(status: number, body: unknown) {
  vi.stubGlobal('fetch', vi.fn(async () => ({
    ok: status >= 200 && status < 300, status, json: async () => body,
  })))
}

afterEach(() => { vi.unstubAllGlobals() })

describe('requestEnvInstall', () => {
  it('a started job carries its id', async () => {
    stubResponse(202, { started: true, jobId: 'system-env-install:cellpose-v3', env: 'cellpose-v3' })
    expect(await requestEnvInstall('cellpose-v3'))
      .toEqual({ status: 'started', jobId: 'system-env-install:cellpose-v3' })
  })

  // No job, so no ws frame will ever arrive: the caller must not wait for one.
  it('an env that is already there resolves at once and notifies the listeners', async () => {
    stubResponse(200, { installed: true, alreadyPresent: true, env: 'cellpose-v3' })
    const cb = vi.fn()
    const off = onSystemEnvsChanged(cb)
    expect(await requestEnvInstall('cellpose-v3')).toEqual({ status: 'present' })
    expect(cb).toHaveBeenCalledOnce()
    off()
  })

  it('the server error text reaches the caller', async () => {
    stubResponse(500, { error: 'pixi not found — cannot install cellpose-v3 from within the app' })
    expect(await requestEnvInstall('cellpose-v3')).toEqual({
      status: 'error', error: 'pixi not found — cannot install cellpose-v3 from within the app' })
  })

  it('a body that is not JSON still gives an error', async () => {
    vi.stubGlobal('fetch', vi.fn(async () => ({
      ok: false, status: 502, json: async () => { throw new SyntaxError('bad') } })))
    expect(await requestEnvInstall('cellpose-v3')).toEqual({ status: 'error', error: 'HTTP 502' })
  })
})

describe('onSystemEnvsChanged', () => {
  it('stops after unsubscribe', () => {
    const cb = vi.fn()
    const off = onSystemEnvsChanged(cb)
    invalidateSystemEnvs()
    off()
    invalidateSystemEnvs()
    expect(cb).toHaveBeenCalledOnce()
  })
})
