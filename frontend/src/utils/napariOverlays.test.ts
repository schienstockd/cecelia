import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { unionViewSnapshot, pushZView, pushLabelContour } from './napariOverlays'

// Only the pure helper is tested here; the fetch-backed capture/build functions are exercised live.
describe('unionViewSnapshot', () => {
  it('includes a layer visible in ANY keyframe, keeping a colormap from where it is shown', () => {
    const u = unionViewSnapshot([
      { layers: { gBT: { colormap: 'green', visible: true }, SHG: { colormap: 'gray', visible: false } } },
      { layers: { SHG: { colormap: 'gray', visible: true }, '(flow) segA/tcells': { visible: true } } },
    ])
    const layers = u.layers as Record<string, { colormap?: string; visible?: boolean }>
    expect(layers.gBT).toMatchObject({ visible: true, colormap: 'green' })
    expect(layers.SHG).toMatchObject({ visible: true, colormap: 'gray' })   // shown in kf2 → included
    expect(layers['(flow) segA/tcells'].visible).toBe(true)                 // overlay layer carried through
  })

  it('a layer hidden in every keyframe stays not-visible', () => {
    const u = unionViewSnapshot([
      { layers: { X: { colormap: 'red', visible: false } } },
      { layers: { X: { colormap: 'red', visible: false } } },
    ])
    expect((u.layers as Record<string, { visible?: boolean }>).X.visible).toBe(false)
  })

  it('tolerates empty / missing snapshots', () => {
    expect(unionViewSnapshot([undefined, null, {}]).layers).toEqual({})
  })
})

// The regression these guard is the reported one: dragging the movie z slider pushed a napari command
// per slider event, and the viewer kept stepping through slices long after the mouse was released.
// The bridge runs one command at a time, so what matters is the REQUEST COUNT, not just which reply wins.
describe('live view-property pushes are coalesced', () => {
  let fetchMock: ReturnType<typeof vi.fn>
  beforeEach(() => {
    vi.useFakeTimers()
    fetchMock = vi.fn().mockResolvedValue({ ok: true })
    vi.stubGlobal('fetch', fetchMock)
  })
  afterEach(() => { vi.useRealTimers(); vi.unstubAllGlobals() })

  const bodyOf = (call: unknown[]) => JSON.parse((call[1] as { body: string }).body)

  it('collapses a z-slider drag into ONE request carrying the last slice', async () => {
    for (let z = 0; z < 30; z++) pushZView(false, z)
    await vi.advanceTimersByTimeAsync(500)
    expect(fetchMock).toHaveBeenCalledTimes(1)
    expect(fetchMock.mock.calls[0][0]).toBe('/api/napari/set-z-view')
    expect(bodyOf(fetchMock.mock.calls[0])).toEqual({ show3D: false, zSlice: 29 })
  })

  it('does not start a second push while one is in flight, then sends the latest', async () => {
    // the schedulers are module-level singletons (one viewer → one scheduler), so every call this test
    // starts must be settled before it ends, or the next test inherits a `running` scheduler
    const pending: ((v: unknown) => void)[] = []
    fetchMock.mockImplementation(() => new Promise(r => { pending.push(r) }))
    const drain = async () => { pending.splice(0).forEach(r => r({ ok: true })); await vi.advanceTimersByTimeAsync(500) }

    pushZView(false, 1)
    await vi.advanceTimersByTimeAsync(200)
    expect(fetchMock).toHaveBeenCalledTimes(1)          // first push in flight
    for (let z = 2; z < 20; z++) pushZView(false, z)    // a whole drag arrives meanwhile
    await vi.advanceTimersByTimeAsync(200)
    expect(fetchMock).toHaveBeenCalledTimes(1)          // …and queues, it does not pile up
    await drain()
    expect(fetchMock).toHaveBeenCalledTimes(2)          // exactly one catch-up call…
    expect(bodyOf(fetchMock.mock.calls[1]).zSlice).toBe(19)   // …with the value the drag ended on
    await drain()
  })

  it('3D drops the slice, so a stale z cannot ride along', async () => {
    pushZView(true, 7)
    await vi.advanceTimersByTimeAsync(500)
    expect(bodyOf(fetchMock.mock.calls[0])).toEqual({ show3D: true, zSlice: null })
  })

  it('collapses an outline drag into one apply-view-state per visible mask layer', async () => {
    for (let w = 0; w < 12; w++) pushLabelContour(['A', 'B'], w)
    await vi.advanceTimersByTimeAsync(500)
    expect(fetchMock).toHaveBeenCalledTimes(1)
    expect(fetchMock.mock.calls[0][0]).toBe('/api/napari/apply-view-state')
    expect(bodyOf(fetchMock.mock.calls[0])).toEqual(
      { viewState: { layers: { '(A) Labels': { contour: 11 }, '(B) Labels': { contour: 11 } } } })
  })

  it('no visible mask layers → no request at all', async () => {
    pushLabelContour([], 3)
    await vi.advanceTimersByTimeAsync(500)
    expect(fetchMock).not.toHaveBeenCalled()
  })
})
