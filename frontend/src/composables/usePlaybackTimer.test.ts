import { describe, it, expect, vi, afterEach } from 'vitest'
import { effectScope, ref } from 'vue'
import { usePlaybackTimer } from './usePlaybackTimer'

function setup(canPlay = true) {
  const playing = ref(false)
  const step = vi.fn((): 'ended' | void => undefined)
  const onStop = vi.fn()
  const scope = effectScope()
  const p = scope.run(() => usePlaybackTimer({ playing, fps: () => 10, canPlay: () => canPlay, step, onStop }))!
  return { playing, step, onStop, scope, ...p }
}

describe('usePlaybackTimer', () => {
  afterEach(() => { vi.useRealTimers() })

  it('steps once per frame interval until paused', () => {
    vi.useFakeTimers()
    const p = setup()
    p.togglePlay()
    expect(p.playing.value).toBe(true)
    vi.advanceTimersByTime(350)            // 10 fps: three ticks
    expect(p.step).toHaveBeenCalledTimes(3)
    p.togglePlay()
    expect(p.playing.value).toBe(false)
    expect(p.onStop).toHaveBeenCalledTimes(1)
    vi.advanceTimersByTime(1000)
    expect(p.step).toHaveBeenCalledTimes(3)
  })

  it('a step that ends playback stops the clock', () => {
    vi.useFakeTimers()
    const p = setup()
    p.step.mockReturnValueOnce('ended')
    p.togglePlay()
    vi.advanceTimersByTime(500)
    expect(p.step).toHaveBeenCalledTimes(1)
    expect(p.playing.value).toBe(false)
    expect(p.onStop).toHaveBeenCalledTimes(1)
  })

  it('nothing to play: play is a no-op', () => {
    vi.useFakeTimers()
    const p = setup(false)
    p.togglePlay()
    vi.advanceTimersByTime(500)
    expect(p.playing.value).toBe(false)
    expect(p.step).not.toHaveBeenCalled()
  })

  it('stops when its owner goes away (a panel unmounting), without the stop cleanup', () => {
    vi.useFakeTimers()
    const p = setup()
    p.togglePlay()
    vi.advanceTimersByTime(150)
    p.scope.stop()
    vi.advanceTimersByTime(1000)
    expect(p.step).toHaveBeenCalledTimes(1)
    expect(p.playing.value).toBe(false)
    expect(p.onStop).not.toHaveBeenCalled()
  })
})
