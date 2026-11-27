import { describe, it, expect, beforeEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'

// An open viewer (a pop-out has its own, empty project store) reads the image's version list once at
// open. `updateImageMeta` is the one place the main window learns of a version added (a cleanup
// task's result), removed or reclaimed — so it is where the viewer gets told.
const publish = vi.hoisted(() => vi.fn())
vi.mock('../lib/viewerImageMetaChannel', () => ({ publishViewerImageMeta: publish }))

import { useProjectStore } from './project'
import { useWsStore } from './ws'

describe('updateImageMeta tells an open viewer when the version list changes', () => {
  beforeEach(() => {
    const mem = new Map<string, string>()
    vi.stubGlobal('localStorage', {
      getItem: (k: string) => mem.get(k) ?? null, setItem: (k: string, v: string) => { mem.set(k, v) },
      removeItem: (k: string) => { mem.delete(k) },
    })
    vi.stubGlobal('window', { addEventListener() {}, removeEventListener() {} })
    setActivePinia(createPinia())
    publish.mockClear()
    const project = useProjectStore()
    project.sets.push({ uid: 's', name: 's', images: [{ uid: 'a', name: 'a', activeValueName: 'default',
      filepaths: { default: 'ccidImage.ome.zarr' } }] } as never)
  })

  it('a new version (denoise on the open image) is announced', () => {
    const project = useProjectStore()
    project.updateImageMeta('a', { filepaths: { default: 'ccidImage.ome.zarr', denoised: 'ccidDenoised.ome.zarr' } })
    expect(publish).toHaveBeenCalledWith('a')
  })
  it('a removed version and a new active version are announced', () => {
    const project = useProjectStore()
    project.updateImageMeta('a', { filepaths: {} })
    expect(publish).toHaveBeenCalledTimes(1)
    project.updateImageMeta('a', { activeValueName: 'denoised' })
    expect(publish).toHaveBeenCalledTimes(2)
  })
  it('the same list re-sent on a task done is not', () => {
    const project = useProjectStore()
    project.updateImageMeta('a', { filepaths: { default: 'ccidImage.ome.zarr' }, activeValueName: 'default' })
    expect(publish).not.toHaveBeenCalled()
  })
})

describe('a task result writing a new image version', () => {
  it('reaches updateImageMeta, so the open viewer is told', () => {
    const mem = new Map<string, string>()
    vi.stubGlobal('localStorage', {
      getItem: (k: string) => mem.get(k) ?? null, setItem: (k: string, v: string) => { mem.set(k, v) },
      removeItem: (k: string) => { mem.delete(k) },
    })
    vi.stubGlobal('window', { addEventListener() {}, removeEventListener() {}, dispatchEvent() { return true } })
    vi.stubGlobal('location', { protocol: 'http:', host: 'x' })
    vi.stubGlobal('fetch', vi.fn(async () => new Response('{}', { status: 500 })))
    let sock: { onmessage?: (e: { data: string }) => void } = {}
    vi.stubGlobal('WebSocket', Object.assign(function (this: object) { sock = this as typeof sock },
      { OPEN: 1, CONNECTING: 0 }))
    setActivePinia(createPinia())
    publish.mockClear()
    const project = useProjectStore()
    project.sets.push({ uid: 's', name: 's', images: [{ uid: 'a', name: 'a', activeValueName: 'default',
      filepaths: { default: 'ccidImage.ome.zarr' } }] } as never)
    useWsStore().connect()
    sock.onmessage!({ data: JSON.stringify({ type: 'task:result', imageUid: 'a',
      meta: { valueName: 'denoised', filename: 'ccidDenoised.ome.zarr' } }) })
    expect(project.imageByUid('a')?.filepaths).toEqual({ default: 'ccidImage.ome.zarr', denoised: 'ccidDenoised.ome.zarr' })
    expect(publish).toHaveBeenCalledWith('a')
  })
})
