import { describe, it, expect, vi, afterEach } from 'vitest'
import { ref } from 'vue'
import { useVwPlayback } from './useVwPlayback'

// Tile-mode playback (the 2D view of a plane too wide for one texture). It used to advance every tick
// whatever had landed, and each advance aborted the frame still loading — so nothing was ever cached
// and every frame of every loop loaded anew. It now waits for the frame, like the flat renderer.
function setup(ready: (t: number) => boolean) {
  const t = ref(0), playing = ref(false), waitingFor = ref(-1)
  const scheduleTilePump = vi.fn()
  const gotoT = vi.fn((u: number) => { t.value = u })
  const { togglePlay, stopPlay } = useVwPlayback({
    playing, waitingFor, t,
    nT: () => 5,
    useTiles: () => true,
    bricksEnabled: () => false,
    renderer: () => null,
    loop: () => true,
    fps: () => 10,
    gotoT, schedulePump: vi.fn(), frameComplete: () => false,
    tileFrameReady: ready, scheduleTilePump,
    redraw: vi.fn(),
    playbackHint: () => null,
  })
  return { t, waitingFor, scheduleTilePump, gotoT, togglePlay, stopPlay }
}

describe('useVwPlayback — tile mode', () => {
  afterEach(() => { vi.useRealTimers() })

  it('waits on a frame whose tiles have not landed, and keeps the pump fetching it', () => {
    vi.useFakeTimers()
    const p = setup(() => false)
    p.togglePlay()
    vi.advanceTimersByTime(350)
    expect(p.t.value).toBe(0)
    expect(p.gotoT).not.toHaveBeenCalled()
    expect(p.waitingFor.value).toBe(1)
    expect(p.scheduleTilePump).toHaveBeenCalled()
    p.stopPlay()
  })

  it('re-pumps on stop, so the read-ahead fetches are dropped', () => {
    const p = setup(() => true)
    p.togglePlay()
    p.scheduleTilePump.mockClear()
    p.stopPlay()
    expect(p.scheduleTilePump).toHaveBeenCalledTimes(1)
  })

  it('advances once the frame is resident, and wraps to frame 0 on a loop', () => {
    vi.useFakeTimers()
    const p = setup(() => true)
    p.togglePlay()
    vi.advanceTimersByTime(450)   // four ticks at 10 fps
    expect(p.t.value).toBe(4)
    vi.advanceTimersByTime(100)
    expect(p.t.value).toBe(0)
    expect(p.waitingFor.value).toBe(-1)
    p.stopPlay()
  })
})
