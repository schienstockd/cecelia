// The play/pause loop every timecourse player shares: the viewer window (`useVwPlayback`, which adds its
// cache waits and prefetch windows in `step`) and the population positions plot.
//
// A timer rather than rAF: the rate is a chosen frame rate, not the display's. The next tick is armed
// only after `step` returns, so a step that waits on a frame (`playbackAdvance` stalled) is asked again
// next tick — nothing queues up faster than it lands. Disposing the owning scope (a component
// unmounting) stops the timer.
import { getCurrentScope, onScopeDispose, type Ref } from 'vue'
import { playbackIntervalMs } from '../utils/volumeCache'

export interface PlaybackTimerDeps {
  playing: Ref<boolean>
  fps: () => number
  /** false = nothing to play (one frame) — play is a no-op */
  canPlay: () => boolean
  /** one tick: advance, or wait; 'ended' stops playback */
  step: () => 'ended' | void
  /** cleanup on an explicit stop (not on dispose — the owner is going away) */
  onStop?: () => void
}

export function usePlaybackTimer(d: PlaybackTimerDeps) {
  let timer: ReturnType<typeof setTimeout> | null = null
  const clear = () => { if (timer !== null) { clearTimeout(timer); timer = null } }

  function stopPlay() {
    d.playing.value = false
    clear()
    d.onStop?.()
  }

  function tick() {
    timer = setTimeout(() => {
      timer = null
      if (!d.playing.value) return
      if (d.step() === 'ended') { stopPlay(); return }
      if (d.playing.value) tick()
    }, playbackIntervalMs(d.fps()))
  }

  function togglePlay() {
    if (d.playing.value) { stopPlay(); return }
    if (!d.canPlay()) return
    d.playing.value = true
    tick()
  }

  if (getCurrentScope()) onScopeDispose(() => { d.playing.value = false; clear() })

  return { stopPlay, togglePlay }
}
