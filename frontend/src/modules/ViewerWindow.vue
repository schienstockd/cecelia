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
  slabMax, contrastCeiling, slabZ, visibleExtentUm, lutFromHex,
  MAX_CHANNELS, SAFE_CACHE_BYTES,
  type ViewerMeta, type OrbitCamera,
} from '../utils/volumeViewer'
import {
  prefetchWindow, prefetchDepth, stripCells, playbackAdvance, playbackIntervalMs,
} from '../utils/volumeCache'
import { toHex } from '../utils/colour'
import { CHANNEL_COLORMAP_OPTIONS } from '../utils/napariColormap'
import {
  overlaysUrl, buildPointBuffer, timepointRange, overlaySummary, buildTrackBuffer, tailRange,
  type OverlayPayload, type PointBuffer, type SegmentBuffer,
} from '../utils/viewerOverlays'
import { heatUnit } from '../utils/viewerOverlays'
import { widenLabelSlab, labelBpv } from '../utils/viewerLabels'
import { toHex as rgbHex } from '../utils/colour'
import { PALETTES } from '../plots/plot'
import StillOverlay from '../components/StillOverlay.vue'
import { elapsedLabel } from '../utils/stillOverlay'
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

/**
 * The h5ad-derived overlays (P3): population points now, tracks next.
 *
 * ONE fetch for the whole movie, because it is small — measured at 2.0 MB for the largest cell table in
 * the dev projects and 0.13 MB for the typical one, against 8.8 MB for a single 2D slab. So there is no
 * request path here that a scrub can spam, and nothing to keep coherent with the timepoint cache.
 */
const overlays = ref<OverlayPayload | null>(null)
const overlaysErr = ref('')
/** Populations the USER has hidden, by path. The server's own `show` flag is honoured separately, so a
 *  pop hidden in the population manager stays hidden here without a second source of truth. */
const hiddenPops = ref<Set<string>>(new Set())
/** Which obs column shades the points, '' for the population colour. A REQUEST, not a display toggle:
 *  the values come from the server, so changing it refetches. */
const colourBy = ref('')
/**
 * Which segmentation's MASK is drawn, '' for none (P4).
 *
 * A REQUEST, and the most expensive kind: the mask rides each timepoint's slab and lives in that
 * timepoint's texture slot, so switching it reallocates and refetches the whole cache. That is the
 * price of the guarantee — a mask cached on its own can be a frame behind the pixels it outlines, and
 * an outline that is one frame stale still looks like an answer.
 *
 * ONE AT A TIME, where napari shows every segmentation at once as its own layer. A panel narrow enough
 * for one population list will not hold three, and the 2D view is the one people gate on.
 */
const labelName = ref('')
let points: PointBuffer = { data: new Float32Array(0), ranges: new Map(), count: 0 }
let segments: SegmentBuffer = {
  data: new Float32Array(0), firstAt: new Int32Array(1), endAt: new Int32Array(1), count: 0,
}
const pointCount = ref(0)
const segCount = ref(0)
const summary = computed(() => overlaySummary(overlays.value))
/** The ramp as a CSS gradient, from the same 256-entry lookup the points are shaded with — a legend
 *  built from a different set of stops would be a second answer about the same scale. */
const rampStyle = computed(() => {
  const stops = Array.from({ length: 12 }, (_, i) =>
    rgbHex(heatUnit(i / 11).map(v => v * 255)))
  return { background: `linear-gradient(to right, ${stops.join(', ')})` }
})

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
/**
 * Planes the 3D view actually loads, `[lo, hi]` inclusive — Dominik's suggestion (2026-08-24) and the
 * thing that makes the volume view usable at all. Every cost is linear in the count, so 8 of 41 planes
 * is a ~0.6 s fetch rather than ~5.8 s, and five times as many timepoints fit the VRAM budget.
 *
 * Defaults to the full stack: a MIP over part of a stack is a different picture, and silently
 * narrowing it would change what the view MEANS to make it fast.
 */
const zRange = ref<[number, number]>([0, 0])
const zDepth = computed(() =>
  mode.value === 'plane' ? 1 : Math.max(1, zRange.value[1] - zRange.value[0] + 1))
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
/**
 * The renderer's own numbers, SNAPSHOT into a ref rather than read through a computed.
 *
 * `computed(() => renderer.value?.cache.capacity)` looks equivalent and is not: the renderer is a
 * `shallowRef` and its capacity is a closure variable, so Vue has nothing to invalidate on when it
 * changes. The computed cached the boot value and the panel then reported `cache 3 / 169` for a cache
 * that actually held four — which is exactly the reading that made a stale READOUT look like a stale
 * geometry (2026-08-24). Updated in `syncCacheState`, which already runs at every moment these change.
 */
const gpu = ref({ capacity: 0, bytesPerTimepoint: 0, zDepth: 1, capped: false })
/**
 * Something is being fetched right now — the dot's whole job.
 *
 * It used to light up only while PLAYBACK was stalled, so a manual scrub or the 2D/3D switch showed
 * nothing at all, and those are the slow ones: "I'm left hanging, did it load? is it loading? I have no
 * idea" (Dominik, 2026-08-24). A timepoint in flight is the honest signal, whoever asked for it.
 */
const busy = computed(() => loadingT.value.length > 0 || waitingFor.value >= 0)
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
  // The scale bar is a function of the camera, so it is recomputed exactly where the frame is — not in
  // a watcher that could disagree with what is on screen.
  seen.value = visibleExtentUm(cam.value.dist, canvasAspect())
  r.setCamera(cam.value)
  r.setSteps(mode.value === 'plane' ? 1 : settings.viewerSteps)
  // The overlay slice for the frame ACTUALLY on screen, not the one asked for — same rule as the
  // timestamp. A range of `null` means nothing is drawn at this timepoint, which is not an error.
  const range = shownT.value >= 0 ? timepointRange(points, shownT.value) : null
  // The planes actually LOADED: one in the 2D view, the crop range in 3D. Not "-1 for 3D" — a view
  // cropped to eight planes would then draw the whole stack's cells against a box holding eight.
  const [pLo, pHi] = mode.value === 'plane'
    ? [zPlane.value, zPlane.value]
    : [zRange.value[0], zRange.value[1]]
  r.setOverlayDraw(range ? range[0] : 0, range ? range[1] : 0,
                   settings.viewerPointSize, pLo, pHi)
  // A tail of N frames ENDING at the frame on screen. Contiguous in the segment buffer by construction,
  // so this is two array reads rather than a per-frame filter.
  const tail = shownT.value >= 0 && settings.viewerTailLength > 0
    ? tailRange(segments, shownT.value, settings.viewerTailLength) : null
  r.setOverlaySegmentDraw(tail ? tail[0] : 0, tail ? tail[1] : 0, settings.viewerTailWidth)
  // Mask style, here for the same reason as the rest: it is display state, and a watcher that set it
  // elsewhere could disagree with the frame on screen. Opacity 0 with no segmentation picked is what
  // switches the shader's label path off — the placeholder texture stays bound, because a bind group
  // has to be complete.
  r.setLabelStyle(labelName.value ? settings.viewerLabelOpacity : 0, settings.viewerLabelContour)
  r.draw()
})

/**
 * Rebuild the instance buffer and hand it to the GPU. Called when the DATA or the visibility changes —
 * never per frame, and never per timepoint: the buffer is ordered by timepoint, so a frame is a range
 * inside it (see `buildPointBuffer`).
 */
function rebuildOverlays() {
  const r = renderer.value
  points = buildPointBuffer(overlays.value, meta.value, hiddenPops.value, PALETTES.cecelia)
  pointCount.value = points.count
  r?.setOverlayPoints(points.data)
  // Tails are coloured per TRACK, not per population, so they do not depend on which pops are visible —
  // but they are rebuilt together because both come from one payload and one rebuild is cheaper to
  // reason about than two lifetimes.
  segments = buildTrackBuffer(overlays.value, meta.value, PALETTES.cecelia)
  segCount.value = segments.count
  r?.setOverlaySegments(segments.data)
  frame.redraw()
}

async function loadOverlays() {
  if (!projectUid || !imageUid) return
  overlaysErr.value = ''
  try {
    // NO `valueName` — deliberately. The window's `valueName` is an IMAGE VERSION (the zarr the pixels
    // come from, e.g. "smoothed"); this route wants a labelProps key (a SEGMENTATION, e.g. "memTom").
    // They are different namespaces that happen to share a parameter name, and sending one for the
    // other resolves to the active segmentation by luck rather than by intent. The server picks the
    // active one and says which in `valueName`, so the panel can report it. A segmentation PICKER is
    // the next step — see the plan.
    const res = await fetch(overlaysUrl({ projectUid, imageUid, colourBy: colourBy.value }),
                            { cache: 'no-store' })
    const body = await res.json()
    if (!res.ok) throw new Error(body?.error ?? `Overlays failed: ${res.status}`)
    overlays.value = body as OverlayPayload
    rebuildOverlays()
  } catch (e) {
    // An overlay failure must not take the IMAGE down — the viewer's job is the pixels, and a missing
    // cell table is a normal state for an unsegmented image.
    overlaysErr.value = e instanceof Error ? e.message : String(e)
  }
}

function togglePop(path: string) {
  const next = new Set(hiddenPops.value)
  next.has(path) ? next.delete(path) : next.add(path)
  hiddenPops.value = next
  rebuildOverlays()
}

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
  const r = renderer.value
  resident.value = r?.residentTimepoints() ?? []
  loadingT.value = [...inflight.keys()]
  if (r) gpu.value = { ...r.cache, capped: r.vramCapped() }
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
    // Sized from the depth the texture ACTUALLY has, so a request can never be a shape the cache is
    // not holding — see `slabZ`. The view mode authors that depth (via `reallocate`); it does not get a
    // second say in what is fetched.
    const zd = r.cache.zDepth
    const zq = slabZ(zd, m.nZ, zPlane.value, zRange.value[0])
    // The MASK goes with the channels, in the same round trip and into the same texture slot. Fetching
    // it separately would let the two arrive apart, and an outline over the wrong frame is worse than
    // no outline: it still looks like an answer. `vn` is read once here so a picker change mid-flight
    // cannot label this response with a different segmentation's name.
    const vn = labelName.value
    const [bufs, labelBuf] = await Promise.all([
      Promise.all(Array.from({ length: nChannels.value }, async (_, c) => {
        const res = await fetch(slabUrl({ projectUid, imageUid, valueName, t: tp, c, ...zq, enc }),
                                { cache: 'no-store', signal: ac.signal })
        if (!res.ok) throw new Error(`Slab ${c} failed: ${res.status}`)
        const buf = await res.arrayBuffer()
        // The guard, not a formality: a mismatched slab uploads fine and renders the wrong thing.
        const bad = slabShapeError(res.headers.get('X-Slab-Shape'), buf.byteLength, m, zd)
        if (bad) throw new Error(bad)
        serverMs = Math.max(serverMs, Number(res.headers.get('X-Server-Read-Ms')) || 0)
        return buf
      })),
      (async () => {
        if (!vn) return null
        const res = await fetch(
          slabUrl({ projectUid, imageUid, valueName, t: tp, c: 0, ...zq, enc, labels: vn }),
          { cache: 'no-store', signal: ac.signal })
        if (!res.ok) throw new Error(`Mask failed: ${res.status}`)
        const buf = await res.arrayBuffer()
        // Same geometry as the image, its OWN dtype — so the guard is asked at the mask's width, which
        // the server reports. A store narrower than UInt32 is widened rather than refused: at half the
        // width it would render as a plausible mask of something else.
        const bpv = labelBpv(res.headers.get('X-Slab-Bpv'))
        const bad = slabShapeError(res.headers.get('X-Slab-Shape'), buf.byteLength, m, zd, bpv)
        if (bad) throw new Error('Mask: ' + bad)
        serverMs = Math.max(serverMs, Number(res.headers.get('X-Server-Read-Ms')) || 0)
        return widenLabelSlab(buf, bpv)
      })(),
    ])
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
    await r.uploadTimepoint(tp, bufs, t.value, labelBuf)
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
 * around a timepoint the user has left. That last part only became true when `debouncedLatest` was
 * fixed to treat a QUEUED request as superseding — before that a 170-frame walk ran to completion
 * wherever the user went, which is what made a jump wait for every frame before it and stopped
 * playback dead. This walk is why the hole was found; every other consumer had it too.
 *
 * `wait: 0` on purpose: there is nothing to tune. A burst inside one macrotask collapses to its last
 * position, and anything arriving during a run queues as the single latest — which is the whole
 * requirement, for a drag and for playback alike.
 */
let lastT = 0
/** How many timepoints ahead are worth pre-paying for — the whole window for a plane, only the frame
 *  asked for while a volume costs 1.5 s, unless playback needs the buffer. */
const depth = () => {
  const r = renderer.value
  return r ? prefetchDepth(r.cache.capacity, r.cache.bytesPerTimepoint, playing.value) : 1
}
const pump = debouncedLatest<number>(async (tp, isCurrent) => {
  const r = renderer.value, m = meta.value
  if (!r || !m) return
  const dir = Math.sign(tp - lastT) || 1
  lastT = tp

  const want = prefetchWindow(tp, dir, m.nT, depth())
  for (const u of want) {
    // The checkpoint. It is between fetches rather than inside one, so abandoning a window costs at
    // most the request already in flight — which is why `schedulePump` cuts that one short as well.
    if (!isCurrent()) return
    if (r.hasTimepoint(u)) { r.touch(u); continue }
    const ok = await fetchTimepoint(u)
    syncCacheState()
    if (!ok || !isCurrent()) return
    // Only paints when the walk is still the current one, so a frame the user has already left cannot
    // land on the canvas; playback's own tick paints whatever became resident meanwhile.
    if (u === t.value && shownT.value !== u) showT(u)
  }
}, { wait: 0, onError: e => { error.value = e instanceof Error ? e.message : String(e) } })

/**
 * Every request goes through here, because scheduling is not the only thing a new target has to do: it
 * also abandons an in-flight fetch the new window has no use for.
 *
 * That has to happen HERE and not inside the walk. The walk is *awaiting* that fetch, so by the time it
 * reaches its next checkpoint the request has already been paid for — aborting is what makes the
 * checkpoint arrive early. It matters most where a request is dear: a 3D timepoint is ~400 ms and the
 * cache holds four of them, so a jump genuinely lands outside the window.
 */
function schedulePump(tp: number) {
  const m = meta.value
  if (m) {
    const keep = new Set(prefetchWindow(tp, Math.sign(tp - lastT) || 1, m.nT, depth()))
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
/**
 * Scale bar + elapsed time, through the SAME component the captured stills and the animation timeline
 * use (`StillOverlay` / `elapsedLabel` / `niceScaleBar`) — napari draws both, and a fourth
 * implementation of a scale bar is how three of them end up disagreeing.
 *
 * The extent passed is what the camera can SEE, not the image, so the bar shrinks as you zoom in. The
 * component's SVG letterboxes its viewBox exactly as an `object-fit: contain` image would, which is a
 * no-op here because the visible extent has the canvas's own aspect — the bar lands where it is drawn.
 */
const seen = ref<[number, number]>([0, 0])
const overlayExtent = computed(() => {
  const m = meta.value
  if (!m || !m.calibrated.xy) return null          // uncalibrated: voxels, so there is no bar to draw
  return { x: seen.value[0], y: seen.value[1], unit: m.spaceUnit || 'µm' }
})
/**
 * In BOTH views and at any orientation — the rendered space is uniform in µm, so the bar is correct
 * wherever the camera is (see `visibleExtentUm`). `'clock'` because this overlay is replacing napari's,
 * which shows H:MM:SS.
 */
const timeLabel = computed(() => {
  const m = meta.value
  if (!m || m.nT <= 1) return ''
  // `shownT`, not `t`: anything drawn ON the image has to describe the PIXELS, and those two disagree
  // for as long as a load takes — deliberately, since keeping the previous frame up beats blanking to
  // black. Off `t` the clock jumped ahead of the picture the moment you scrubbed, which is a
  // mislabelled frame rather than a lag (Dominik, 2026-08-24). The slider's own readout stays on `t`,
  // where it describes the control instead.
  return elapsedLabel(shownT.value, m.frameIntervalMin, 'min', 'clock')
})

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
  r.setImage(m, SAFE_CACHE_BYTES, zDepth.value,
             mode.value === 'plane' ? zPlane.value : zRange.value[0], !!labelName.value)
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
    zRange.value = [0, Math.max(m.nZ - 1, 0)]
    r.setImage(m, SAFE_CACHE_BYTES, zDepth.value, zPlane.value)
    r.setCapacity(settings.viewerCacheFrames || m.nT)
    r.setOrthographic(mode.value === 'plane')
    const c = fitNow(m)
    cam.value = c
    fitDist.value = c.dist
    r.resize()
    starting.value = ''
    gotoT(0)
    // After the first frame is on its way: the overlays are a separate, small request and must not
    // delay the pixels.
    void loadOverlays()
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
      <!-- `chrome="fixed"`: on a full-bleed interactive canvas the still's proportional sizing renders a
           35 px label that also changes size as you zoom. The bar's LENGTH is physical either way. -->
      <StillOverlay
        v-if="meta && shownT >= 0" :extent-um="overlayExtent" :time-label="timeLabel" chrome="fixed"
        :show-scale-bar="settings.viewerScaleBar" :show-timestamp="settings.viewerTimestamp"
        :bar-font-px="settings.viewerScaleBarPx" :time-font-px="settings.viewerTimestampPx"
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
        <!-- The 3D view's own depth control. `@change`, not `@update:*`: the range reallocates every
             cached texture, so it commits on release rather than per pointer move. -->
        <div v-if="mode === 'volume' && meta.nZ > 1" class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Depth</span>
          <RangeSlider
            v-tooltip.top="'Planes to project — fewer is faster, in proportion'"
            :lo="zRange[0]" :hi="zRange[1]" :min="0" :max="Math.max(meta.nZ - 1, 0)" :step="1"
            @update:lo="v => (zRange = [v, zRange[1]])"
            @update:hi="v => (zRange = [zRange[0], v])"
            @change="reallocate()"
          />
          <span class="cc-readout cc-fs-2xs vw-num">{{ zRange[0] }}–{{ zRange[1] }}</span>
        </div>
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
        <div class="vw-striprow" v-tooltip.bottom="'Cached timepoints — the dot blinks while loading'">
          <span class="vw-dot" :class="{ 'is-waiting': busy }" />
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

        <!-- Segmentation mask. Only when a mask is actually ON DISK — `labelNames` is the server's
             directory check, not the label registry, so an imported track set with a table and no mask
             does not offer an empty option. -->
        <template v-if="meta.labelNames?.length">
          <div class="cc-eyebrow cc-fs-2xs">Segmentation</div>
          <div class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col">Mask</span>
            <select
              class="cc-select cc-fs-2xs vw-grow" :value="labelName"
              v-tooltip.bottom="'Draw a segmentation over the image — reloads the timecourse'"
              @change="e => { labelName = (e.target as HTMLSelectElement).value; reallocate() }"
            >
              <option value="">none</option>
              <option v-for="n in meta.labelNames" :key="n" :value="n">{{ n }}</option>
            </select>
          </div>
          <div v-if="labelName" class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col">Opacity</span>
            <input
              type="range" class="vw-grow" :min="0" :max="1" :step="0.05"
              v-model.number="settings.viewerLabelOpacity" @input="frame.redraw()"
              v-tooltip.bottom="'How strongly the mask covers the signal'"
            >
            <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerLabelOpacity.toFixed(2) }}</span>
          </div>
          <div v-if="labelName" class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col">Outline</span>
            <input
              type="range" class="vw-grow" :min="0" :max="5" :step="1"
              v-model.number="settings.viewerLabelContour" @input="frame.redraw()"
              v-tooltip.bottom="'Outline width in voxels — 0 fills each cell'"
            >
            <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerLabelContour || 'fill' }}</span>
          </div>
          <div v-if="labelName && mode === 'volume'" class="cc-muted cc-fs-3xs">
            3D shows the nearest mask surface
          </div>
        </template>

        <!-- Overlays. Only when there is something to say: an unsegmented image has no cell table and
             no populations, and an empty group would read as a broken feature rather than as an image
             that has not been through segmentation yet. -->
        <template v-if="summary.cells > 0 || overlaysErr">
          <div class="cc-eyebrow cc-fs-2xs">Overlays</div>
          <div v-if="overlaysErr" class="cc-muted-warn cc-fs-2xs">{{ overlaysErr }}</div>
          <!-- "cells but no populations" is a DIFFERENT state from "no cells", and they look identical
               on the canvas — so the panel names which one it is. -->
          <div v-else-if="summary.pops === 0" class="cc-muted cc-fs-2xs">
            {{ summary.cells }} cells, no populations gated
          </div>
          <template v-else>
            <div v-for="pop in overlays!.pops" :key="pop.path" class="cc-row cc-row-tight">
              <span class="vw-swatch" :style="{ background: pop.colour }" />
              <span class="cc-fs-2xs vw-pop-name" :title="pop.path">{{ pop.name }}</span>
              <span class="cc-readout cc-fs-3xs">{{ pop.labels.length }}</span>
              <CcToggle
                :model-value="pop.show && !hiddenPops.has(pop.path)" :disabled="!pop.show"
                :aria-label="'Show ' + pop.name" @update:modelValue="togglePop(pop.path)"
              />
            </div>
            <div class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Colour by</span>
              <select
                class="cc-select cc-fs-2xs vw-grow" :value="colourBy"
                v-tooltip.bottom="'Shade the points by a per-cell measure'"
                @change="e => { colourBy = (e.target as HTMLSelectElement).value; void loadOverlays() }"
              >
                <option value="">population</option>
                <option v-for="c in overlays!.colourColumns" :key="c" :value="c">{{ c }}</option>
              </select>
            </div>
            <!-- The legend says which SCALE is in use, because that is the server's decision (the same
                 rule the plots use) and the two kinds look nothing alike. -->
            <div v-if="overlays!.colourBy && overlays!.valueKind === 'numeric'"
                 class="cc-row cc-row-tight cc-fs-3xs">
              <span class="cc-muted">{{ (overlays!.valueRange?.[0] ?? 0).toPrecision(3) }}</span>
              <span class="vw-ramp" :style="rampStyle" />
              <span class="cc-muted">{{ (overlays!.valueRange?.[1] ?? 1).toPrecision(3) }}</span>
            </div>
            <div v-else-if="overlays!.colourBy && overlays!.valueKind === 'categorical'"
                 class="cc-muted cc-fs-3xs">
              {{ overlays!.valueLevels?.length ?? 0 }} levels
            </div>

            <div v-if="segCount > 0" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Tail</span>
              <input
                type="range" class="vw-grow" :min="0" :max="60" :step="1"
                v-model.number="settings.viewerTailLength" @input="frame.redraw()"
                v-tooltip.bottom="'Track history in frames — 0 hides the tails'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerTailLength }}</span>
            </div>
            <div v-if="segCount > 0 && settings.viewerTailLength > 0" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Tail width</span>
              <input
                type="range" class="vw-grow" :min="1" :max="12" :step="1"
                v-model.number="settings.viewerTailWidth" @input="frame.redraw()"
                v-tooltip.bottom="'Tail thickness on screen, not in µm'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerTailWidth }}</span>
            </div>
            <div class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Point size</span>
              <input
                type="range" class="vw-grow" :min="2" :max="24" :step="1"
                v-model.number="settings.viewerPointSize" @input="frame.redraw()"
                v-tooltip.bottom="'Marker size on screen, not in µm'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerPointSize }}</span>
            </div>
            <div class="cc-muted cc-fs-3xs">
              <template v-if="overlays!.valueName">{{ overlays!.valueName }} · </template>
              {{ pointCount }} drawn · {{ summary.cells }} cells
              <template v-if="summary.tracked">· {{ summary.tracked }} tracked</template>
              <template v-if="summary.dropped">· {{ summary.dropped }} without a centroid</template>
              <template v-if="mode === 'plane'">· this plane only</template>
            </div>
          </template>
        </template>

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
        <!-- Toggle and text size share a row: the size is only ever adjusted with the thing it sizes
             in front of you, and a separate row for each would double the group's height. -->
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col"
                v-tooltip.right="'Physical scale of the current zoom'">Scale bar</span>
          <CcToggle v-model="settings.viewerScaleBar" aria-label="Show the scale bar" />
          <input
            type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
            :disabled="!settings.viewerScaleBar" v-model.number="settings.viewerScaleBarPx"
            v-tooltip.bottom="'Scale-bar text size'" aria-label="Scale bar text size"
          >
          <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerScaleBarPx }}</span>
        </div>
        <div class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col"
                v-tooltip.right="'Elapsed time, or the frame index if uncalibrated'">Timestamp</span>
          <CcToggle v-model="settings.viewerTimestamp" aria-label="Show the timestamp" />
          <input
            type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
            :disabled="!settings.viewerTimestamp" v-model.number="settings.viewerTimestampPx"
            v-tooltip.bottom="'Timestamp text size'" aria-label="Timestamp text size"
          >
          <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerTimestampPx }}</span>
        </div>
        <div class="cc-row cc-row-tight">
          <button class="cc-btn cc-btn-ghost" @click="resetView"
                  v-tooltip.top="'Face the volume square to the screen again'">Reset view</button>
        </div>

        <div class="cc-eyebrow cc-fs-2xs">Image</div>
        <div class="cc-muted cc-fs-3xs">
          {{ meta.nX }} × {{ meta.nY }} × {{ meta.nZ }} · {{ meta.nT }} t · {{ meta.nC }} ch<br>
          {{ (meta.slabBytes / 1e6 / (meta.nZ / gpu.zDepth)).toFixed(1) }} MB / channel ·
          contrast {{ meta.contrastSource }}<br>
          cache {{ resident.length }} / {{ gpu.capacity }}
          <template v-if="gpu.capped">(GPU limit)</template>
          <template v-else-if="gpu.capacity >= nT && nT > 0">(whole movie fits)</template><br>
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
/* The size slider shares its row with a toggle and a readout, so it needs a floor it cannot be
   squeezed below — `flex: 1` alone collapses it to nothing in a narrow panel. */
.vw-px { min-width: 3.5rem; }
.vw-px-val { flex: none; min-width: 1.4rem; text-align: right; }
/* A population row: swatch, name that can shrink, count, toggle. The name is the only flexible part —
   letting the count or the toggle shrink is what made the channel rows overlap. */
.vw-swatch { flex: none; width: 0.7rem; height: 0.7rem; border-radius: var(--cc-radius-xs);
  border: 1px solid var(--cc-border); }
.vw-pop-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vw-ramp { flex: 1; min-width: 2rem; height: 0.5rem; border-radius: var(--cc-radius-xs); }
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
