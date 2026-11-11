// The viewer window's playback loop (docs/todo/GUI_LAYOUT_PLAN.md, V0: split the viewer; the V2
// playback bar renders it).
//
// A timer rather than rAF: the rate is a chosen frame rate, not the display's. It WAITS for an
// uncached frame instead of skipping ahead — see `playbackAdvance` for why that is the honest choice.
//
// The window keeps owning its state (`playing`, `waitingFor`, `t` are read all over the render core);
// this takes them as refs and the rest as getters, so nothing here holds a second copy.
import type { Ref } from 'vue'
import type { VolumeRenderer } from '../lib/webgpu/volumeRenderer'
import type { PlaybackHint } from '../utils/pageTable'
import { playbackAdvance, playbackIntervalMs, prefetchWindow } from '../utils/volumeCache'

export interface VwPlaybackDeps {
  playing: Ref<boolean>
  /** the frame playback is waiting on, or -1 */
  waitingFor: Ref<number>
  t: Ref<number>
  nT: () => number
  /** tile mode (a whole-slide plane): advance at the requested rate regardless of residency */
  useTiles: () => boolean
  /** brick renderer: advance every tick; flat renderer: wait for a complete frame */
  bricksEnabled: () => boolean
  renderer: () => VolumeRenderer | null
  loop: () => boolean
  fps: () => number
  gotoT: (t: number) => void
  schedulePump: (t: number) => void
  frameComplete: (t: number) => boolean
  redraw: () => void
  playbackHint: (nT: number) => PlaybackHint | null
}

export function useVwPlayback(d: VwPlaybackDeps) {
  let playTimer: ReturnType<typeof setTimeout> | null = null

  function stopPlay() {
    d.playing.value = false
    d.waitingFor.value = -1
    if (playTimer !== null) { clearTimeout(playTimer); playTimer = null }
    // Retire the playback prefetch window. Playback sets `prefetchTs` to `[t, t±1, t±2]` so the
    // scheduler buffers ahead; leaving that in place after stop keeps `tickScheduler` kicking
    // fetches for those neighbour t's every tick with no benefit — ("doesn't
    // stop loading"). Idle prefetch is just the current t; the next `gotoT` will replace this.
    d.renderer()?.setPrefetchTimepoints?.([d.t.value])
  }

  function tick() {
    playTimer = setTimeout(() => {
      if (!d.playing.value) return
      if (d.useTiles()) {
        // Tile-mode playback: advance at the requested fps regardless of tile residency. The tile
        // pump chases behind `gotoT` — the loading chip appears while a fresh-t frame's tiles arrive
        // and clears the moment they do. Fetch-vs-frame-rate discipline (volume mode's residency
        // probe + wait-for-cached-frame path) needs measurement on real timelapses before it lands
        // here — see `docs/todo/VIEWER_TILES_PLAN.md` Phase F, decision "autoplay does NOT ship".
        const n = d.nT()
        if (n <= 0) { stopPlay(); return }
        const next = d.t.value + 1
        if (next >= n) {
          if (!d.loop()) { stopPlay(); return }
          d.waitingFor.value = -1
          d.gotoT(0)
        } else {
          d.waitingFor.value = -1
          d.gotoT(next)
        }
        tick()
        return
      }
      const r = d.renderer()
      // Brick renderer: advance every tick regardless of residency, so show(t) fires its
      // snap-to-boundT and the shader draws each frame with prev-t hole-fill for whatever hasn't
      // landed. Flat renderer: gate on cache residency (its frames are all-or-nothing).
      // A flat frame is ready with its mask: after a mask is ticked on mid-movie, the walk fetches each
      // frame's mask ahead of the playhead and playback never shows a frame without it (`frameComplete`).
      const readyProbe = d.bricksEnabled() ? () => true : d.frameComplete
      const nT = d.nT()
      const step = playbackAdvance(d.t.value, nT, d.loop(), readyProbe)
      if (step.ended) { stopPlay(); return }
      if (step.stalled) {
        // Pump around the frame we WANT, not the one we are on. At the end of a loop those are the one
        // pair that disagree, and a window centred on where we are fills backwards and never asks for
        // frame 0 — playback then waits forever for something nothing is fetching.
        d.waitingFor.value = step.next
        d.schedulePump(step.next)
        // Brick renderer only: schedulePump is a flat-renderer path — nothing on the brick side
        // hears it. Push step.next into the prefetch window so the brick tickScheduler starts
        // fetching for it now, or a stall never unstalls (nothing else is telling the scheduler
        // this t matters). Under playback cap=4 so the window covers the direction of travel.
        if (r?.setPrefetchTimepoints && nT > 0) {
          const dir = 1   // playback runs forward; `step.next - t` is negative at the loop wrap
          // Same atlas-size clamp as gotoT — a stalled playback step MUST NOT ask for more
          // prefetch than the atlas can hold, or the prefetch bricks LRU-evict the boundT bricks
          // the shader is trying to draw.
          const cap = r.maxSafePrefetchDepth?.(4) ?? 4
          r.setPrefetchTimepoints(prefetchWindow(step.next, dir, nT, cap), d.playbackHint(nT))
        }
        d.redraw()
      } else {
        d.waitingFor.value = -1
        d.gotoT(step.t)
      }
      tick()
    }, playbackIntervalMs(d.fps()))
  }

  function togglePlay() {
    if (d.playing.value) { stopPlay(); return }
    if (d.nT() <= 1) return
    d.playing.value = true
    tick()
  }

  return { stopPlay, togglePlay }
}
