<!--
  The in-browser volume viewer, in its own window — the "web eye" beside napari, and the first step of
  replacing it (docs/todo/WEB_VIEWER_PLAN.md → P1/P2). A WebGPU MIP raycast: drag to rotate, wheel to
  zoom, per-channel contrast and visibility, and a scrubbable/playable timecourse over a VRAM cache.

  WHY ITS OWN WINDOW. napari has one, so a side-by-side comparison needs a second one, and the two are
  meant to be looked at together while the browser side catches up. Same idiom as the console and the
  Task Manager: a named window (`lib/popout.ts`) opened from the ViewerPanel's ↗.

  WHAT IT IS TOLD, and why. A popup is a fresh app instance with no project open, so the image comes
  from the `?project=&image=&valueName=` seed written when the window was opened. Unlike the Task
  Manager it does NOT follow the main window's switches: it is a comparison surface, and a view you are
  measuring against napari must not move under you because someone clicked another row.

  WHERE THE CONTRAST COMES FROM, and why it is not persisted here. The server answers it — napari's own
  saved props file when the image has one, otherwise a percentile from a fixed (t, z) sample — so a
  reload restores the authoritative value rather than a stale local copy. That is the opposite of the
  usual "persist every user-settable option" case, whose bug is an option silently RESETTING; here the
  reset IS the correct value, and the alternative would be two disagreeing sources of truth for a
  window napari also writes. Which side owns it after napari is gone is P8's decision, not this
  file's. The options that are genuinely this window's — raycast steps, wire encoding, playback fps and
  loop, the VRAM budget — live in the settings store, so they survive a reload.

  SHOWING AND FETCHING ARE SEPARATE, and that is the design, not tidiness. `showT` is synchronous: it
  binds a cached timepoint and paints. `pump` is the async half that fills the prefetch window around
  it. Folding them together — awaiting the window fill before painting — is the obvious shape and it
  makes playback advance only once a whole window of up to `capacity` timepoints has loaded, i.e. once
  per several seconds instead of once per frame.
-->
<script setup lang="ts">
import { ref, computed, shallowRef, onMounted, onUnmounted } from 'vue'
import { useRoute } from 'vue-router'
import { useSettingsStore } from '../stores/settings'
import { usePlotResize } from '../composables/usePlotResize'
import { debouncedLatest } from '../utils/debouncedLatest'
import { createVolumeRenderer, WebGpuUnavailable, type VolumeRenderer } from '../lib/webgpu/volumeRenderer'
import {
  metaUrl, slabUrl, slabShapeError, extentUm, fitCamera, orbitDrag, orbitZoom, contrastFromSlab,
  slabMax, contrastCeiling, lutFromHex, MAX_CHANNELS, SAFE_CACHE_BYTES,
  type ViewerMeta, type OrbitCamera,
} from '../utils/volumeViewer'
import {
  prefetchWindow, stripCells, playbackAdvance, playbackIntervalMs,
} from '../utils/volumeCache'
import { toHex } from '../utils/colour'
import { CHANNEL_COLORMAP_OPTIONS } from '../utils/napariColormap'
import CcToggle from '../components/CcToggle.vue'
import ChipSelect from '../components/ChipSelect.vue'
import ColourPicker from '../components/ColourPicker.vue'
import RangeSlider from '../components/RangeSlider.vue'

const route = useRoute()
const settings = useSettingsStore()

const projectUid = String(route.query.project ?? '')
const imageUid = String(route.query.image ?? '')
const valueName = String(route.query.valueName ?? '') || undefined
const imageName = String(route.query.name ?? '')

const canvas = ref<HTMLCanvasElement | null>(null)
const renderer = shallowRef<VolumeRenderer | null>(null)
const meta = ref<ViewerMeta | null>(null)
const error = ref('')
const starting = ref('')
/** The timepoint asked for. `shownT` is what is actually on the canvas — they differ while loading. */
const t = ref(0)
const shownT = ref(-1)
const resident = ref<number[]>([])
const loadingT = ref<number[]>([])
const playing = ref(false)
/**
 * Brightest voxel SEEN SO FAR per channel, from the timepoints this client has actually loaded. The
 * slider's ceiling is derived from it (`contrastCeiling`) rather than being the dtype maximum, which
 * would put a real 0–545 range in the first 1% of the slider's travel.
 *
 * It only ever grows, and that is the fix rather than an optimisation: taken from the FIRST timepoint
 * and held, it clipped the slider below the data whenever later frames were brighter (Dominik,
 * 2026-08-24). It also deliberately survives a mode or plane change — a ceiling that dropped would
 * re-scale the slider under a value the user had already set.
 */
const seenMax = ref<number[]>([])
const chMax = computed(() =>
  seenMax.value.map(v => contrastCeiling(v, meta.value?.bytesPerVoxel ?? 2)))
/** Per-channel percentile window of the first timepoint loaded, behind the "Auto" button. Taken ONCE
 *  per image, like the server's: recomputed per timepoint, playback flickers as the window chases each
 *  frame's own distribution (WEB_VIEWER_PLAN.md decision 5). */
const autoWin = ref<{ lo: number; hi: number; max: number }[]>([])
const timing = ref<{ fetchMs: number; uploadMs: number; serverMs: number } | null>(null)

const cam = ref<OrbitCamera>({ yaw: 0, pitch: 0, dist: 1 })
const fitDist = ref(1)
/**
 * `plane` shows ONE z plane, `volume` the whole stack as a MIP. Plane is the default for anything with
 * a z axis, because it is what a timecourse is actually watched in — and it is the only one that plays:
 * on `Dml3RG` a plane timepoint is 8.8 MB against 326 MB, so the whole 181-frame movie is 1.59 GB and
 * fits in the budget, where the volume is 59 GB and never can.
 */
const mode = ref<'plane' | 'volume'>('plane')
const zPlane = ref(0)
/** Cache hits and misses since the last (re)allocation, and how long the last miss took end to end.
 *  The plan's headline scrub number was measured with capacity larger than the movie, i.e. with
 *  eviction impossible — so the shipped cache needs its OWN numbers, from real use. */
const hits = ref(0)
const misses = ref(0)
const lastMissMs = ref(0)
/** The frame playback is waiting on, or -1. A 3D loop advances at the speed of the server (~400 ms a
 *  timepoint), so without saying so on screen a working playback is indistinguishable from a hang. */
const waitingFor = ref(-1)
const lostDevice = ref(false)
const nChannels = computed(() => Math.min(meta.value?.nC ?? 0, MAX_CHANNELS))
const clipped = computed(() => (meta.value?.nC ?? 0) > MAX_CHANNELS)
const nT = computed(() => meta.value?.nT ?? 0)
const zDepth = computed(() => (mode.value === 'plane' ? 1 : (meta.value?.nZ ?? 1)))
/**
 * Channel colour, through the shared `ColourPicker` — the pop manager's design (a swatch you click,
 * not a labelled dropdown; `SwatchSelect` spells the option out in text and had squeezed the channel
 * names to one character each) with the CHANNEL colours rather than the population palette. Both halves
 * are Dominik's call (2026-08-24): the house palette is tuned for telling populations apart on a plot,
 * whereas these are the colormaps a channel is actually rendered in, and the batch-movie panel already
 * offers exactly this set.
 *
 * A picked colour becomes a two-stop black→colour LUT, which is EXACT for a channel: `image_render.jl`
 * verified every channel colormap is a linear ramp from black. It is a SESSION-LOCAL override — the
 * server still ships the real stops on load (`resolved_display_specs`, which handles the perceptual maps
 * a two-stop ramp cannot). Which side owns colour once napari is gone is P8's decision, like contrast.
 */
const CHANNEL_PALETTE = [...new Set(CHANNEL_COLORMAP_OPTIONS.map(o => o.hex))]
const channelHex = (ch: { lut: number[][] }): string => {
  const top = ch.lut?.[ch.lut.length - 1]
  return top ? toHex(top.map(v => v * 255)) : '#ffffff'
}
function setChannelColour(c: number, hex: string) {
  const m = meta.value
  if (!m) return
  m.channels[c].lut = lutFromHex(hex)
  pushChannels()
}

const MODES = [
  { value: 'plane', label: '2D', tip: 'One z plane — the only view that plays a whole timecourse' },
  { value: 'volume', label: '3D', tip: 'Max projection through the whole stack' },
]
const capacity = computed(() => renderer.value?.cache.capacity ?? 0)
const cells = computed(() => stripCells(
  nT.value, new Set(resident.value), new Set(loadingT.value), shownT.value))

// One paint per frame however fast the pointer or a slider fires, and no paint for a box that did not
// change — the canonical helper for exactly this (`usePlotResize`), not a second hand-rolled
// ResizeObserver. `redraw()` is the camera/contrast/timepoint path (the box is identical but the frame
// is not); `schedule()` is the resize path, which the size guard owns.
const frame = usePlotResize(canvas, () => {
  const r = renderer.value
  if (!r) return
  r.resize()
  r.setCamera(cam.value)
  r.setSteps(mode.value === 'plane' ? 1 : settings.viewerSteps)
  r.draw()
})

function pushChannels() {
  if (meta.value) renderer.value?.setChannels(meta.value.channels)
  frame.redraw()
}

// ── Fetching a timepoint ─────────────────────────────────────────────────────────
// One entry per in-flight timepoint, so a prefetch already running is joined rather than started
// again, and one that falls outside the window is aborted rather than left to finish into a cache that
// no longer wants it.
const inflight = new Map<number, Promise<boolean>>()
const aborts = new Map<number, AbortController>()
const syncCacheState = () => {
  resident.value = renderer.value?.residentTimepoints() ?? []
  loadingT.value = [...inflight.keys()]
}

function fetchTimepoint(tp: number): Promise<boolean> {
  const r = renderer.value, m = meta.value
  if (!r || !m) return Promise.resolve(false)
  if (r.hasTimepoint(tp)) { r.touch(tp); return Promise.resolve(true) }
  const missStart = performance.now()
  const running = inflight.get(tp)
  if (running) return running

  const ac = new AbortController()
  aborts.set(tp, ac)
  const enc = settings.viewerCompress ? 'zstd' : 'identity'
  const job = (async () => {
    const t0 = performance.now()
    let serverMs = 0
    // The channels go in parallel — independent reads on the server's thread pool, and serially they
    // would cost the sum rather than the max (~250 ms per channel on the real target). Timepoints go
    // one at a time (see `pump`), so the server is never asked for more than one volume at once.
    const bufs = await Promise.all(Array.from({ length: nChannels.value }, async (_, c) => {
      const z = mode.value === 'plane' ? zPlane.value : undefined
      const res = await fetch(slabUrl({ projectUid, imageUid, valueName, t: tp, c, z, enc }),
                              { cache: 'no-store', signal: ac.signal })
      if (!res.ok) throw new Error(`Slab ${c} failed: ${res.status}`)
      const buf = await res.arrayBuffer()
      // The guard, not a formality: a mismatched slab uploads fine and renders the wrong thing.
      const bad = slabShapeError(res.headers.get('X-Slab-Shape'), buf.byteLength, m, zDepth.value)
      if (bad) throw new Error(bad)
      serverMs = Math.max(serverMs, Number(res.headers.get('X-Server-Read-Ms')) || 0)
      return buf
    }))
    const fetchMs = performance.now() - t0

    // The auto WINDOW is taken from the first timepoint only and then held — recomputed per frame it
    // chases each frame's own distribution and playback flickers (decision 5). The slider's RANGE is
    // not the same question and does follow the data: a max-only pass, no sort, ~1 ms a channel.
    if (autoWin.value.length === 0) {
      autoWin.value = bufs.map(b => contrastFromSlab(new Uint16Array(b), m.nX))
    }
    seenMax.value = bufs.map((b, c) =>
      Math.max(seenMax.value[c] ?? 0, slabMax(new Uint16Array(b), m.nX)))

    const t1 = performance.now()
    await r.uploadTimepoint(tp, bufs, t.value)
    timing.value = {
      fetchMs: Math.round(fetchMs), uploadMs: Math.round(performance.now() - t1), serverMs,
    }
    lastMissMs.value = Math.round(performance.now() - missStart)
    return true
  })().catch((e: unknown) => {
    // An abort is the normal outcome for a prefetch the user scrubbed away from — not an error to show.
    if (e instanceof DOMException && e.name === 'AbortError') return false
    error.value = e instanceof Error ? e.message : String(e)
    return false
  }).finally(() => {
    inflight.delete(tp)
    aborts.delete(tp)
    syncCacheState()
  })

  inflight.set(tp, job)
  syncCacheState()
  return job
}

// ── Showing / pumping ────────────────────────────────────────────────────────────

/** Bind `tp` and paint if it is cached. Synchronous, and deliberately a no-op when it is not: keeping
 *  the previous frame on screen while the next loads reads far better than blanking to black. */
function showT(tp: number): boolean {
  t.value = tp
  const r = renderer.value
  if (!r || !r.show(tp)) { misses.value++; return false }
  hits.value++
  shownT.value = tp
  frame.redraw()
  return true
}

/**
 * Fill the cache around `tp`, in the direction of travel, one timepoint at a time.
 *
 * Through `debouncedLatest`, the canonical scheduler for a request, and it is load-bearing rather than
 * tidy. A slider fires per pixel of travel, and each position wants a DIFFERENT prefetch window — so a
 * hand-rolled version starts a fetch per event before it can notice it has been superseded. Dragging
 * across 100 timepoints then puts 100 concurrent volume fetches in flight (400 requests), which is
 * exactly the spam the rule exists to stop. The scheduler collapses a burst, runs one at a time, and
 * hands `isCurrent()` so a superseded walk stops at its next checkpoint instead of filling a cache
 * around a timepoint the user has left.
 *
 * `wait: 0` on purpose: there is nothing to tune. A burst inside one macrotask collapses to its last
 * position, and anything arriving during a run queues as the single latest — which is the whole
 * requirement, for a drag and for playback alike.
 */
let lastT = 0
/**
 * Where the walk should be centred RIGHT NOW — read on every iteration, not captured at entry.
 *
 * This is the load-bearing half, and it exists because `isCurrent()` does NOT go false while a newer
 * request merely SITS PENDING: the scheduler runs one call at a time, and the token it compares only
 * moves when a successor actually starts, which cannot happen until this one returns (pinned by
 * `debouncedLatest.test.ts` → "lets a superseded run discard its result"). A walk over a 170-frame
 * window therefore ran to completion no matter where the user went, which produced both of the bugs
 * Dominik reported: jumping to timepoint 90 waited for every frame before it, and playback simply
 * stopped — every tick queued a request that could not start, while the walk filled frames nobody was
 * waiting for.
 */
let pumpTarget = 0
const pump = debouncedLatest<number>(async (_tp, isCurrent) => {
  const r = renderer.value, m = meta.value
  if (!r || !m) return
  /** Attempted this walk. Without it a timepoint that fetches but fails to UPLOAD (an OOM texture) is
   *  chosen forever — the window still wants it and the cache still lacks it. */
  const tried = new Set<number>()
  let target = -1
  let dir = 1

  while (isCurrent()) {
    if (target !== pumpTarget) {
      // Follow, do not restart: the frames already fetched stay fetched, and the very next request is
      // the one the user is waiting for.
      target = pumpTarget
      dir = Math.sign(target - lastT) || dir
      lastT = target
    }
    const want = prefetchWindow(target, dir, m.nT, r.cache.capacity)
    // Everything resident in the window is worth keeping, whether or not this walk fetched it.
    for (const v of want) if (r.hasTimepoint(v)) r.touch(v)

    const u = want.find(v => !r.hasTimepoint(v) && !tried.has(v))
    if (u === undefined) return                  // the window is full — nothing left to do
    tried.add(u)
    const ok = await fetchTimepoint(u)
    if (!isCurrent()) return
    syncCacheState()
    if (!ok) return                              // aborted or failed; the next schedule restarts us
    if (u === t.value && shownT.value !== u) showT(u)
  }
}, { wait: 0, onError: e => { error.value = e instanceof Error ? e.message : String(e) } })

/**
 * Every request goes through here, so `pumpTarget` can never disagree with what was scheduled.
 *
 * It also abandons an in-flight fetch the new window has no use for. That has to happen HERE and not
 * inside the walk: the walk is awaiting that very fetch, so by the time it looks again the request has
 * already been paid for. It matters most where a request is dear — a 3D timepoint is ~400 ms and the
 * cache holds four of them, so a jump genuinely lands outside the window.
 */
function schedulePump(tp: number) {
  pumpTarget = tp
  const r = renderer.value, m = meta.value
  if (r && m) {
    const keep = new Set(prefetchWindow(tp, Math.sign(tp - lastT) || 1, m.nT, r.cache.capacity))
    for (const [k, ac] of [...aborts]) if (!keep.has(k)) ac.abort()
  }
  pump.schedule(tp)
}

/** Paint immediately, fetch on the scheduler — the documented split: a paint is coalesced per frame,
 *  a request is collapsed and serialised. */
function gotoT(tp: number) {
  showT(tp)
  schedulePump(tp)
}

// ── Playback ─────────────────────────────────────────────────────────────────────
// A timer rather than rAF: the rate is a chosen frame rate, not the display's. It WAITS for an
// uncached frame instead of skipping ahead — see `playbackAdvance` for why that is the honest choice.
let playTimer: ReturnType<typeof setTimeout> | null = null

function stopPlay() {
  playing.value = false
  waitingFor.value = -1
  if (playTimer !== null) { clearTimeout(playTimer); playTimer = null }
}

function tick() {
  playTimer = setTimeout(() => {
    if (!playing.value) return
    const r = renderer.value
    const step = playbackAdvance(t.value, nT.value, settings.viewerLoop,
                                 u => r?.hasTimepoint(u) ?? false)
    if (step.ended) { stopPlay(); return }
    if (step.stalled) {
      // Pump around the frame we WANT, not the one we are on. At the end of a loop those are the one
      // pair that disagree, and a window centred on where we are fills backwards and never asks for
      // frame 0 — playback then waits forever for something nothing is fetching.
      waitingFor.value = step.next
      schedulePump(step.next)
    } else {
      waitingFor.value = -1
      gotoT(step.t)
    }
    tick()
  }, playbackIntervalMs(settings.viewerFps))
}

function togglePlay() {
  if (playing.value) { stopPlay(); return }
  if (nT.value <= 1) return
  playing.value = true
  tick()
}

// ── Pointer ──────────────────────────────────────────────────────────────────────
let dragFrom: { x: number; y: number } | null = null
function onDown(e: PointerEvent) {
  dragFrom = { x: e.clientX, y: e.clientY }
  ;(e.target as HTMLElement).setPointerCapture?.(e.pointerId)
}
function onMove(e: PointerEvent) {
  if (!dragFrom || !canvas.value) return
  const dx = e.clientX - dragFrom.x, dy = e.clientY - dragFrom.y
  dragFrom = { x: e.clientX, y: e.clientY }
  cam.value = orbitDrag(cam.value, dx, dy, canvas.value.clientWidth)
  frame.redraw()
}
function onUp() { dragFrom = null }
function onWheel(e: WheelEvent) {
  e.preventDefault()
  cam.value = orbitZoom(cam.value, e.deltaY, fitDist.value)
  frame.redraw()
}
const canvasAspect = () => {
  const el = canvas.value
  return el && el.clientHeight > 0 ? el.clientWidth / el.clientHeight : 1
}
const reload = () => location.reload()
/** The framing for the CURRENT mode. Three callers needed the same three arguments, and the third —
 *  the projection — is the one that is easy to forget and shows up as a 3D reset that clips. */
const fitNow = (m: ViewerMeta) =>
  fitCamera(extentUm(m, zDepth.value), canvasAspect(), mode.value === 'volume')
function resetView() {
  cam.value = { ...fitNow(meta.value!) }
  frame.redraw()
}

/**
 * Re-allocate for the current mode and z, then reload. Every cached texture goes: at a different depth
 * they are a different shape, and at a different z they hold different pixels. That is a full refetch —
 * ~4 s for a 181-frame plane movie, ~90 s for the volume — which is the honest cost of the switch and
 * the reason the plane view is the default rather than something you opt into.
 */
function reallocate(refit = false) {
  const r = renderer.value, m = meta.value
  if (!r || !m) return
  pump.cancel()
  for (const ac of aborts.values()) ac.abort()
  aborts.clear(); inflight.clear()
  shownT.value = -1
  hits.value = 0; misses.value = 0
  autoWin.value = []                       // Auto windows on what is loaded, so re-derive per plane
  waitingFor.value = -1
  r.setImage(m, SAFE_CACHE_BYTES, zDepth.value)
  r.setCapacity(settings.viewerCacheFrames || m.nT)
  r.setOrthographic(mode.value === 'plane')
  r.setSteps(mode.value === 'plane' ? 1 : settings.viewerSteps)
  // Only re-frame when the BOX changed (a 2D/3D switch). Looking at a different plane of the same
  // image is not a reason to throw away a rotation or a zoom — that reads as the view jumping.
  const c = fitNow(m)
  fitDist.value = c.dist
  refit && (cam.value = c)
  syncCacheState()
  gotoT(t.value)
}

/** Window a channel on the percentiles of the first timepoint loaded — free, no refetch. */
function autoContrast(c: number) {
  const w = autoWin.value[c], m = meta.value
  if (!w || !m) return
  m.channels[c].lo = w.lo
  m.channels[c].hi = w.hi
  pushChannels()
}

// ── Lifecycle ────────────────────────────────────────────────────────────────────

onMounted(async () => {
  if (!projectUid || !imageUid) { error.value = 'No image — open this window from the viewer panel'; return }
  try {
    starting.value = 'Starting GPU'
    const r = await createVolumeRenderer(canvas.value!)
    renderer.value = r
    void r.lost.then(info => {
      stopPlay()
      // A lost device cannot be recovered in place — the canvas context goes with it — so the honest
      // offer is a reload rather than a setting to go and adjust.
      pump.cancel()
      for (const ac of aborts.values()) ac.abort()
      lostDevice.value = true
      error.value = 'The GPU dropped the connection: ' + (info?.message || 'unknown')
    })

    starting.value = 'Reading image'
    const res = await fetch(metaUrl({ projectUid, imageUid, valueName }))
    if (!res.ok) throw new Error((await res.json()).error ?? `Metadata failed: ${res.status}`)
    const m: ViewerMeta = await res.json()
    meta.value = m
    mode.value = m.nZ > 1 ? 'plane' : 'volume'
    zPlane.value = Math.floor(Math.max(m.nZ - 1, 0) / 2)
    r.setImage(m, SAFE_CACHE_BYTES, zDepth.value)
    r.setCapacity(settings.viewerCacheFrames || m.nT)
    r.setOrthographic(mode.value === 'plane')
    const c = fitNow(m)
    cam.value = c
    fitDist.value = c.dist
    r.resize()
    starting.value = ''
    gotoT(0)
  } catch (e) {
    error.value = e instanceof WebGpuUnavailable
      ? e.message + ' — the viewer needs WebGPU'
      : (e instanceof Error ? e.message : String(e))
    starting.value = ''
  }
})

onUnmounted(() => {
  stopPlay()
  pump.cancel()
  for (const ac of aborts.values()) ac.abort()
  renderer.value?.destroy()
})
</script>

<template>
  <div class="vw">
    <div class="vw-canvas-wrap">
      <canvas
        ref="canvas" class="vw-canvas"
        @pointerdown="onDown" @pointermove="onMove" @pointerup="onUp" @pointercancel="onUp"
        @wheel="onWheel"
      />
      <div v-if="starting" class="cc-empty cc-empty-overlay">{{ starting }}…</div>
      <div v-else-if="error" class="cc-empty cc-empty-overlay cc-muted-error">
        {{ error }}
        <button v-if="lostDevice" class="cc-btn cc-btn-ghost" @click="reload"
                v-tooltip.top="'Reopen the viewer'">Reload</button>
      </div>
      <!-- The timepoint ASKED FOR, not a literal 0: this overlay is not only the first load. A 2D/3D
           switch clears every texture, so it comes back at whatever timepoint the slider is on, and a
           hardcoded 0 there said the wrong thing (Dominik, 2026-08-24). -->
      <div v-else-if="shownT < 0" class="cc-empty cc-empty-overlay">Loading timepoint {{ t }}…</div>
    </div>

    <aside class="vw-side">
      <div class="vw-title cc-fs-sm">{{ imageName || imageUid }}</div>
      <div v-if="valueName" class="cc-muted cc-fs-2xs">{{ valueName }}</div>

      <div v-if="renderer && !renderer.adapter.looksDiscrete" class="cc-muted-warn cc-fs-2xs"
           v-tooltip.bottom="'The browser picked the integrated GPU — expect much slower frames'">
        Integrated GPU
      </div>

      <template v-if="meta">
        <div class="cc-eyebrow cc-fs-2xs">View</div>
        <ChipSelect
          :options="MODES" :model-value="mode" variant="segmented" aria-label="View mode"
          @update:model-value="v => { mode = v as 'plane' | 'volume'; reallocate(true) }"
        />
        <div v-if="mode === 'plane' && meta.nZ > 1" class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Plane</span>
          <input
            type="range" class="vw-grow" :min="0" :max="meta.nZ - 1" :step="1"
            v-model.number="zPlane" @change="reallocate()"
            v-tooltip.bottom="'Which z plane to show — changing it reloads the timecourse'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">{{ zPlane }} / {{ meta.nZ - 1 }}</span>
        </div>

        <div class="cc-eyebrow cc-fs-2xs">Timepoint</div>
        <div class="cc-row cc-row-tight">
          <button class="cc-btn cc-btn-ghost cc-btn-icon" :disabled="nT <= 1" @click="togglePlay"
                  v-tooltip.bottom="playing ? 'Pause' : 'Play through the timecourse'">
            <i class="pi" :class="playing ? 'pi-pause' : 'pi-play'" />
          </button>
          <input
            type="range" class="vw-grow" :min="0" :max="Math.max(nT - 1, 0)" :step="1"
            :value="t" @pointerdown="stopPlay()"
            @input="gotoT(Number(($event.target as HTMLInputElement).value))"
            v-tooltip.bottom="'Scrub the timecourse — cached timepoints are instant'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">{{ t }} / {{ Math.max(nT - 1, 0) }}</span>
        </div>

        <!-- Which timepoints are in VRAM: the answer to "will scrubbing there be instant". Bucketed,
             so a long movie does not put one element per frame in the DOM.
             The dot shares this row rather than earning its own: playback holds rather than skip an
             uncached frame (~400 ms each in 3D), so without a cue a working playback looks like a hang —
             but a line that appears and disappears above the strip shoves it up and down every tick,
             which is what read as the buffer trail jiggling. In-row, nothing can reflow. -->
        <div class="vw-striprow" v-tooltip.bottom="'Timepoints held in VRAM'">
          <span class="vw-dot" :class="{ 'is-waiting': waitingFor >= 0 }" />
          <div class="vw-strip">
            <span v-for="(c, i) in cells" :key="i" class="vw-cell" :class="'is-' + c.state" />
          </div>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Fps</span>
          <input
            type="range" class="vw-grow" :min="1" :max="30" :step="1"
            v-model.number="settings.viewerFps"
            v-tooltip.bottom="'Playback rate — it waits rather than skip an uncached frame'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerFps }}</span>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col"
                v-tooltip.right="'Restart from the first timepoint at the end'">Loop</span>
          <CcToggle v-model="settings.viewerLoop" aria-label="Loop playback" />
        </div>

        <div class="cc-eyebrow cc-fs-2xs">Channels</div>
        <div v-for="(ch, c) in meta.channels.slice(0, MAX_CHANNELS)" :key="c" class="vw-ch cc-card cc-card-2">
          <div class="cc-row cc-row-tight">
            <span class="vw-ch-name cc-fs-xs"
                  v-tooltip.right="'Show this channel in the composite'">{{ ch.name }}</span>
            <ColourPicker
              :model-value="channelHex(ch)" :palette="CHANNEL_PALETTE" :tip="'Colour for ' + ch.name"
              @update:model-value="v => setChannelColour(c, v)"
            />
            <CcToggle v-model="ch.visible" :aria-label="'Show ' + ch.name" @update:modelValue="pushChannels" />
            <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="autoContrast(c)"
                    v-tooltip.left="'Window this channel on the loaded voxels'">
              <i class="pi pi-sliders-h" />
            </button>
          </div>
          <!-- RangeSlider is a flex-ROW item by construction (`flex: 1`, i.e. `flex-basis: 0`), so in a
               column it collapses to no height and its absolutely-positioned thumbs escape the card.
               Every other consumer wraps it in a row with a readout beside it; so does this one. -->
          <div class="cc-row cc-row-tight">
            <RangeSlider
              v-tooltip.top="'Contrast window — values outside it clip'"
              :lo="ch.lo" :hi="ch.hi" :min="0" :max="chMax[c] ?? Math.max(ch.hi, 1)" :step="1"
              @update:lo="v => { ch.lo = v; pushChannels() }"
              @update:hi="v => { ch.hi = v; pushChannels() }"
            />
            <span class="cc-readout cc-fs-3xs vw-ch-val">{{ ch.lo }}–{{ ch.hi }}</span>
          </div>
        </div>
        <div v-if="clipped" class="cc-muted-warn cc-fs-2xs">
          Showing {{ MAX_CHANNELS }} of {{ meta.nC }} channels
        </div>

        <div class="cc-eyebrow cc-fs-2xs">Render</div>
        <!-- Ray steps mean nothing for a one-plane box: a single sample lands on the plane. -->
        <div class="cc-row cc-row-tight" v-if="mode === 'volume'">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Steps</span>
          <input
            type="range" class="vw-grow" :min="64" :max="512" :step="64"
            v-model.number="settings.viewerSteps" @input="frame.redraw()"
            v-tooltip.bottom="'Ray steps per pixel — higher is sharper and slower'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerSteps }}</span>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Keep</span>
          <input
            type="range" class="vw-grow" :min="0" :max="Math.max(nT, 1)" :step="1"
            v-model.number="settings.viewerCacheFrames" @change="reallocate()"
            v-tooltip.bottom="'How many timepoints stay instant — 0 keeps as many as fit'"
          >
          <span class="cc-readout cc-fs-2xs vw-num">
            {{ settings.viewerCacheFrames || 'all' }}
          </span>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col"
                v-tooltip.right="'Compress slabs on the wire — a win remotely, a cost locally'">Compress</span>
          <CcToggle v-model="settings.viewerCompress" aria-label="Compress slabs on the wire" />
        </div>
        <div class="cc-row cc-row-tight">
          <button class="cc-btn cc-btn-ghost" @click="resetView"
                  v-tooltip.top="'Face the volume square to the screen again'">Reset view</button>
        </div>

        <div class="cc-eyebrow cc-fs-2xs">Image</div>
        <div class="cc-muted cc-fs-3xs">
          {{ meta.nX }} × {{ meta.nY }} × {{ meta.nZ }} · {{ meta.nT }} t · {{ meta.nC }} ch<br>
          {{ (meta.slabBytes / 1e6 / (mode === 'plane' ? meta.nZ : 1)).toFixed(1) }} MB / channel ·
          contrast {{ meta.contrastSource }}<br>
          cache {{ resident.length }} / {{ capacity }}
          <template v-if="renderer?.vramCapped()">(GPU limit)</template>
          <template v-else-if="capacity >= nT && nT > 0">(whole movie fits)</template><br>
          {{ hits }} hit / {{ misses }} miss<template v-if="lastMissMs"> · last miss {{ lastMissMs }} ms</template><br>
          <template v-if="timing">
            fetch {{ timing.fetchMs }} ms (server {{ timing.serverMs }}) · upload {{ timing.uploadMs }} ms
          </template>
        </div>
      </template>
    </aside>
  </div>
</template>

<style scoped>
.vw { display: flex; height: 100vh; background: var(--cc-surface-1); }
.vw-canvas-wrap { position: relative; flex: 1; min-width: 0; }
/* No background: the renderer clears to black, and the overlay covers the pre-first-frame gap. */
.vw-canvas { display: block; width: 100%; height: 100%; cursor: grab; touch-action: none; }
.vw-canvas:active { cursor: grabbing; }
.vw-side {
  width: 15rem; flex: none; padding: 0.6rem; overflow-y: auto;
  border-left: 1px solid var(--cc-border); display: flex; flex-direction: column; gap: 0.35rem;
}
.vw-title { font-weight: 600; word-break: break-word; }
.vw-ch { padding: 0.35rem 0.4rem; display: flex; flex-direction: column; gap: 0.2rem; }
.vw-ch-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
/* The thumbs are centred on their value, so half of one overhangs at either end of the rail. Room for
   that, or they sit on the card's border. */
.vw-ch { padding-left: 0.7rem; padding-right: 0.7rem; }
.vw-ch-val { flex: none; white-space: nowrap; }
.vw-grow { flex: 1; min-width: 0; }
/* The numbers beside a slider change width as they count up (9 → 10 → 100), and the slider is `flex: 1`
   next to them — so without a fixed box the track resized on every frame of playback. That is the other
   half of what read as the panel jiggling. */
.vw-num { flex: none; min-width: 4.5rem; text-align: right; }
.vw-striprow { display: flex; align-items: center; gap: 0.3rem; }
.vw-strip { display: flex; gap: 1px; height: 0.35rem; flex: 1; min-width: 0; }
/* Always laid out, only ever recoloured — the space is reserved so nothing reflows when it lights up. */
.vw-dot {
  flex: none; width: 0.4rem; height: 0.4rem; border-radius: var(--cc-radius-pill);
  background: var(--cc-border);
}
.vw-dot.is-waiting { background: var(--cc-sev-warn); animation: vw-blink 0.9s ease-in-out infinite; }
@keyframes vw-blink { 0%, 100% { opacity: 1 } 50% { opacity: 0.15 } }
@media (prefers-reduced-motion: reduce) { .vw-dot.is-waiting { animation: none } }
.vw-cell { flex: 1; background: var(--cc-surface-2); border-radius: var(--cc-radius-xs); }
.vw-cell.is-resident { background: var(--cc-accent); }
.vw-cell.is-loading { background: var(--cc-accent-tint); }
.vw-cell.is-current { background: var(--cc-text); }
</style>
