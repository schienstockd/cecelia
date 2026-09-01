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
import { ref, computed, watch, shallowRef, onMounted, onUnmounted } from 'vue'
import { useRoute } from 'vue-router'
import { useSettingsStore } from '../stores/settings'
import { useViewerStore } from '../stores/viewer'
import { visibleRegion as computeVisibleRegion } from '../utils/viewer/visibleRegion'
import { buildViewState, applyViewStateToBrowser, type ViewerViewState } from '../utils/viewer/viewState'
import { usePlotResize } from '../composables/usePlotResize'
import { debouncedLatest } from '../utils/debouncedLatest'
import {
  createVolumeRenderer, WebGpuUnavailable,
  // Kiln brick renderer (KILN_BRICK_PLAN.md P5) — dev-flagged via `?bricks=1`. Same interface
  // as `createVolumeRenderer`; P5a's build is a proof-of-plumbing that clears the canvas
  // magenta so a screenshot proves the swap works before the shader lands.
  type VolumeRenderer, type UniformState, type FrameSample,
} from '../lib/webgpu/volumeRenderer'
import { createBrickVolumeRenderer } from '../lib/webgpu/brickVolumeRenderer'
import { createTileRenderer, type TileRenderer, type TileDraw } from '../lib/webgpu/tileRenderer'
import {
  tileKeyStr, tileFetchRect, tilesInHalo, tileEvictions, viewportCentreTile, levelMeta,
  tileGridDims,
  type TileKey, type ViewportL0,
} from '../utils/tileViewer'
import { publishUiLog } from '../lib/uiLogChannel'
import { sampleCanvas, type CanvasSample } from '../utils/canvasSample'
import { adapterNameText, probeWebGpu } from '../utils/webgpuProbe'
import { markViewerAttempt, clearViewerAttempt, viewerCrashedLastTime } from '../utils/viewerCrashGuard'
import {
  metaUrl, slabUrl, slabShapeError, extentUm, fitCamera, orbitDrag, panDrag, orbitZoom, contrastFromSlab,
  slabMax, slabView, contrastCeiling, slabZ, visibleExtentUm, lutFromHex, pickVolumeLevel, pickTileLevel,
  shouldUseBricks,
  VIEW_HALF_ANGLE, MAX_CHANNELS,
  type ViewerMeta, type OrbitCamera,
} from '../utils/volumeViewer'
import {
  prefetchWindow, prefetchDepth, stripCells, playbackAdvance, playbackIntervalMs,
} from '../utils/volumeCache'
import { toHex } from '../utils/colour'
import { CHANNEL_COLORMAP_OPTIONS } from '../utils/viewerColormap'
import { captureViewState, applyViewState, loadViewerProps, saveViewerProps } from '../utils/viewerProps'
import { screenToImagePx } from '../utils/viewerPick'
import { debouncedSave } from '../utils/debouncedSave'
import {
  overlaysUrl, buildPointBuffer, timepointRange, overlaySummary,
  buildMultiTrackBuffer, tailRange, filterPayloadByLabels,
  type OverlayPayload, type PointBuffer, type SegmentBuffer,
} from '../utils/viewerOverlays'
import { heatUnit } from '../utils/viewerOverlays'
import { widenLabelSlab, labelBpv } from '../utils/viewerLabels'
import {
  buildBlob as buildBenchBlob, benchFilename, summarize as summarizeBench,
  type BenchSample, type BenchMeta, type BenchVram, type BenchWriteSample,
  type GpuFrameSample,
} from '../utils/benchRecorder'
import { toHex as rgbHex } from '../utils/colour'
import { PALETTES, distinctColors } from '../plots/plot'
import { hslCssToRgb } from '../utils/viewerLabels'
import StillOverlay from '../components/StillOverlay.vue'
import { elapsedLabel } from '../utils/stillOverlay'
import CcToggle from '../components/CcToggle.vue'
import ChipSelect from '../components/ChipSelect.vue'
import ColourPicker from '../components/ColourPicker.vue'
import RangeSlider from '../components/RangeSlider.vue'
import CollapsibleSection from '../components/CollapsibleSection.vue'
import CollapsiblePanel from '../components/CollapsiblePanel.vue'
import TeleportPopover from '../components/TeleportPopover.vue'

const route = useRoute()
const settings = useSettingsStore()
// Task-preview integration (P7): publishes what this viewer has open + its visible region for the
// task-preview store to feed into `/api/preview/run`, and reads back the active preview labels flag
// so the labels slab URL flips to the scratch `<vn>__preview.ome.zarr` when a preview is showing.
const viewerStore = useViewerStore()

const projectUid = String(route.query.project ?? '')
const imageUid = String(route.query.image ?? '')
/**
 * `?bricks=1` — swap the flat-3D volume renderer for the brick-atlas one
 * (`docs/todo/KILN_BRICK_PLAN.md`). URL-scoped rather than a setting so the two paths can be
 * compared side-by-side by opening the same image in two windows. Read once on mount; the
 * renderer swap on mid-session flip needs a full destroy/recreate and isn't worth the
 * scaffolding for a dev-only flag.
 */
/** URL override for the auto-select — ephemeral, wins over the persisted setting. `?bricks=1`
 *  forces brick, `?bricks=0` forces flat, absent defers to `settings.viewerBricksMode` and then
 *  the predicate. Dev-only tool for A/B side-by-side comparisons. */
const bricksOverride = String(route.query.bricks ?? '')
/** Reactive on `meta` (server round-trip) AND `settings.viewerBricksMode` (user toggle). Read
 *  `.value` in script, auto-unwraps in template. Consumers that snapshot the value once (e.g.
 *  `ensureRenderer`) see the current classification because `reallocate` guards on `meta.value`
 *  being non-null first — and a setting flip fires the reallocate watcher below. */
const bricksEnabled = computed<boolean>(() => {
  if (bricksOverride === '1') return true
  if (bricksOverride === '0') return false
  const mode = settings.viewerBricksMode
  if (mode === 'brick') return true
  if (mode === 'flat') return false
  const m = meta.value
  return m !== null && shouldUseBricks(m)
})
/**
 * Brick LOD tuning knobs — URL params for interactive feel-testing. Applied ONCE at mount to
 * the renderer. Defaults reproduce the shipped behaviour; refresh with a new value to try
 * something else.
 * - `?brickThr=N`     — core intersect ceiling for the over-fetch guard (default 256).
 * - `?brickBias=N`    — added to the SSE-picked level (positive = coarser, negative = finer).
 * - `?brickHold=0|1`  — hold going-finer swaps until current level is stable (default 1).
 */
const parseNumQuery = (v: unknown, fallback: number): number => {
  // `Number('')` returns 0 (finite), so an unset URL param would silently override the fallback
  // with 0. Guard on the raw string being empty before converting.
  const s = String(v ?? '')
  if (s === '') return fallback
  const n = Number(s)
  return Number.isFinite(n) ? n : fallback
}
const brickKnobThr = parseNumQuery(route.query.brickThr, 256)
// URL param present? Only then does `?brickThr` win over the persisted quality tier — an unset
// param must NOT collapse to 256 and freeze the tier control, which is why the raw string is
// checked here rather than trusting `brickKnobThr`'s fallback.
const brickKnobThrFromUrl = String(route.query.brickThr ?? '') !== ''
const brickKnobBias = parseNumQuery(route.query.brickBias, 0)
const brickKnobHold = String(route.query.brickHold ?? '1') !== '0'
/**
 * `?bench=1` — turn on the debug bench harness. Records first-frame time, per-frame CPU
 * draw cost and bytes fetched via a `PerformanceObserver` on `/api/viewer/slab` responses.
 * User drives the workload (scrubbing, zooming); Save button downloads a JSON blob for
 * off-line comparison of flat vs brick on the five reference images.
 */
/** Bench harness state — initial value from the URL query, then toggleable from the Debug panel
 *  so a session can be turned into a saveable full-history capture without editing the URL. Not
 *  persisted: bench mode is a per-session diagnostic, not a preference. Reactive so the wiring
 *  and the Save/Reset chrome update as the toggle flips. */
const benchEnabled = ref(String(route.query.bench ?? '') === '1')
/** Ring size for perf recording (Debug panel Perf block) when Debug is open. Under `?bench=1`
 *  the arrays grow unbounded so a full session can be saved. Otherwise held at the last ~200
 *  samples — enough for p50/p95 to stabilise, small enough to be cheap. */
const PERF_RING = 200
const benchT0 = ref<number>(0)
const benchFirstFrameMs = ref<number | null>(null)
const benchFrames = shallowRef<BenchSample[]>([])
const benchBytes = ref(0)
/** Per-writeBrick timing samples, brick-only. Populated via `setOnBrickWritten` on the
 *  renderer. Times the atlas-upload path — durationMs is the CPU-side cost of one writeBrick. */
const benchWrites = shallowRef<BenchWriteSample[]>([])
/** Per-frame GPU + fine-grained CPU sub-frame samples, brick-only, best-effort. Populated
 *  asynchronously via `setOnFrameTimings` — GPU-side `gpuFrameMs` requires the adapter's
 *  `timestamp-query` feature; CPU-side buckets always populate. Not correlated 1:1 with
 *  `benchFrames`; the blob stores them as a parallel stream. */
const benchGpuFrames = shallowRef<GpuFrameSample[]>([])
/** Save-time live tally so the panel shows progress without allocating on every frame. */
const benchLive = computed(() => {
  const now = performance.now()
  const session = benchT0.value > 0 ? now - benchT0.value : 0
  return summarizeBench(benchFrames.value, session, benchGpuFrames.value)
})
/** Reset the bench counters. Called from setImage() so first-frame is measured from an honest
 *  boundary; also from the panel's Reset button when the user wants a clean segment. */
/** Rendererref that `wireFrameTimings` retargets when Debug opens/closes. Held as a module-scope
 *  reference so the watcher on `openSection` (declared later, once that ref exists) can flip the
 *  wiring without needing the renderer handle passed in. */
let lastWiredRenderer: VolumeRenderer | null = null
/** Wire or unwire the renderer's per-frame GPU/CPU sub-frame callback based on whether the
 *  Debug section is currently open (or `?bench=1` is set). Skipping the wire means the renderer
 *  never creates the query-set write into the render pass, so the whole timestamp path is
 *  zero-cost for a normal viewer session. Called from `ensureRenderer` (initial wire) and from
 *  a watch on `openSection` (state change). */
function wireFrameTimings(r: VolumeRenderer | null) {
  lastWiredRenderer = r
  if (r?.setOnFrameTimings === undefined) return
  const on = benchEnabled.value || openSection.value === 'debug'
  if (!on) { r.setOnFrameTimings(null); return }
  r.setOnFrameTimings(sample => {
    benchGpuFrames.value = benchEnabled.value
      ? [...benchGpuFrames.value, sample]
      : [...benchGpuFrames.value.slice(-(PERF_RING - 1)), sample]
  })
}
function benchReset() {
  benchT0.value = performance.now()
  benchFirstFrameMs.value = null
  benchFrames.value = []
  benchBytes.value = 0
  benchWrites.value = []
  benchGpuFrames.value = []
}
/** Save the current bench state as a JSON download. Filename encodes mode + image + iso date
 *  so a directory of them doesn't collide across images or renderers. */
function benchSave() {
  const m = meta.value
  if (!m) return
  const iso = new Date().toISOString()
  const benchMeta: BenchMeta = {
    projectUid, imageUid, valueName: valueName.value || '',
    nT: m.nT, nC: m.nC, nZ: m.nZ, nY: m.nY, nX: m.nX,
    nLevels: m.levels?.length ?? 1,
    bytesPerVoxel: m.bytesPerVoxel,
  }
  const r = renderer.value
  const br = r?.brickResidency?.()
  const vram: BenchVram | null = r ? {
    cacheCapacity: r.cache.capacity,
    cacheBytesPerTimepoint: r.cache.bytesPerTimepoint,
    cacheZDepth: r.cache.zDepth,
    residentTimepoints: r.residentTimepoints().length,
    brickCurrentLevel: br?.currentLevel ?? -1,
    residentBricks: br?.resident.length ?? 0,
    brickSizeVox: br?.brickSizeVox ?? [0, 0, 0],
  } : null
  // Full Debug-panel snapshot — everything else the panel currently shows, so the saved blob is
  // self-contained rather than "here are numbers, ask the user what they were looking at".
  const debug: Record<string, unknown> = {
    shader: shader.value,
    bricks: br ?? null,
    imageInfo: {
      slabLevel: slabLevel.value,
      renderNX: renderNX.value,
      renderNY: renderNY.value,
      resident: resident.value.length,
      capacity: gpu.value.capacity,
      capped: gpu.value.capped,
      hits: hits.value,
      misses: misses.value,
      lastMissMs: lastMissMs.value,
      lastTiming: timing.value,
      contrastSource: m.contrastSource ?? null,
    },
    knobs: {
      viewerSteps: settings.viewerSteps,
      viewerCacheFrames: settings.viewerCacheFrames,
      viewerCompress: settings.viewerCompress,
      opaqueCanvas: opaqueCanvas.value,
      testPattern: testPattern.value,
      brickKnobThr, brickKnobBias, brickKnobHold,
      brickKnobThrFromUrl, effectiveMaxIntersect: effectiveMaxIntersect.value,
      viewerBrickTier: settings.viewerBrickTier,
    },
  }
  const blob = buildBenchBlob({
    mode: bricksEnabled.value ? 'brick' : 'flat',
    meta: benchMeta,
    t0: benchT0.value,
    savedAt: performance.now(),
    isoDate: iso,
    firstFrameMs: benchFirstFrameMs.value,
    frames: benchFrames.value,
    bytesFetched: benchBytes.value,
    vram,
    writes: benchWrites.value,
    gpuFrames: benchGpuFrames.value,
    debug,
  })
  const json = JSON.stringify(blob, null, 2)
  const url = URL.createObjectURL(new Blob([json], { type: 'application/json' }))
  const a = document.createElement('a')
  a.href = url
  a.download = benchFilename(blob.mode, imageUid, iso)
  document.body.appendChild(a); a.click(); a.remove()
  // Free after the click has been dispatched — Firefox drops the download if we revoke sync.
  setTimeout(() => URL.revokeObjectURL(url), 1000)
  vlog('info', `Bench saved: ${a.download}`,
       `${blob.summary.nFrames} frames · ${(blob.bytesFetched / 1e6).toFixed(1)} MB · ` +
       `first ${blob.firstFrameMs?.toFixed(0) ?? '—'} ms`)
}
/**
 * Which VERSION of the image is on screen. The picker lives in the main-window ViewerPanel now
 * (VIEWER_CONTROLS_SPLIT_PLAN.md P3 extended). On mount, prefer the shared bag over the URL query —
 * the URL was frozen when the popup opened; the bag is what the panel has said since.
 *
 * Empty means "whatever the server resolves", which is the ACTIVE version (what a task would run
 * against). The meta response says which one that was.
 */
const valueName = ref(
  settings.getImageVersion(imageUid) || String(route.query.valueName ?? ''),
)
/**
 * The image's SET and display name are both server-owned (`/api/viewer/meta`, 2026-08-28). The URL
 * used to carry them so the pop-out had a title and per-set prefs before the first fetch; that
 * grew the query to five keys, and the server always knew both, so the round trip became the one
 * source. Both start empty and populate once `meta` resolves — one frame of window-local defaults
 * for per-set prefs before the real values land, which is not perceptible next to the slab fetch.
 *
 * Per-set preferences (point size, colour-by, which population type is shown) live in
 * `settings.getPointSize` and friends, keyed on `setUid.value`. Empty setUid = a viewer opened
 * without a set context (rare — export path) → window-local defaults, same fallback as before.
 */
const setUid = computed(() => meta.value?.setUid ?? '')
const imageName = computed(() => meta.value?.name ?? '')

/**
 * A second click on a DIFFERENT image's eye — the main window opens the viewer with the same window
 * NAME, so the browser reuses this popup and only updates its hash. `projectUid`/`imageUid` above are
 * `const`s captured at setup, so a hash change on its own leaves the viewer painting the FIRST image
 * for ever. We already have a full re-init pipeline — mounting — so hard-reload the popup when the
 * query names a new image and let it run.
 */
watch(() => route.query.image, next => {
  const nextUid = String(next ?? '')
  if (nextUid && nextUid !== imageUid) window.location.reload()
})

/**
 * Read a fetch response as JSON, but tell you WHICH request went wrong when the body is empty or not
 * JSON. `await res.json()` on a 500 with an empty body throws "Failed to execute 'json' on 'Response':
 * Unexpected end of JSON input", which reads as a client bug and hides the real status. Read as text
 * first, then try `JSON.parse` in a try/catch — a non-2xx becomes `Meta 500: <first 120 chars>`, an
 * empty 2xx becomes `Meta 200: empty body`.
 */
async function readJson<T = unknown>(res: Response, label: string): Promise<T> {
  const text = await res.text()
  if (!res.ok) {
    let msg = `${label} ${res.status}`
    if (text) {
      try { msg += `: ${(JSON.parse(text) as { error?: string }).error ?? text.slice(0, 120)}` }
      catch { msg += `: ${text.slice(0, 120)}` }
    }
    throw new Error(msg)
  }
  if (!text) throw new Error(`${label} ${res.status}: empty body`)
  try { return JSON.parse(text) as T }
  catch { throw new Error(`${label} ${res.status}: not JSON (${text.slice(0, 120)})`) }
}

const canvas = ref<HTMLCanvasElement | null>(null)
const renderer = shallowRef<VolumeRenderer | null>(null)
/**
 * The 2D pan/zoom viewer for whole-slide images (`docs/todo/VIEWER_TILES_PLAN.md` → Phase C).
 *
 * Lives alongside the volume renderer, NOT instead of it: swapping every code path over would break
 * the timecourse view that already works. Instead the tile pipeline turns on for the case it exists
 * to serve — a large plane whose whole-fetch does not fit — and the whole-plane MIP shader keeps the
 * timecourse and small-image cases untouched.
 *
 * A pop-out canvas has ONE WebGPU context, so `renderer` and `tileRenderer` are alternates: whichever
 * mode is active owns the canvas, and swapping mode destroys one before creating the other. Same cost
 * as a `reallocate` today — mode swap is already a full refetch.
 */
const tileRenderer = shallowRef<TileRenderer | null>(null)
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
/** The channels' `lo`/`hi` as the SERVER shipped them (napari-saved or first-load percentile) — the
 *  reset target. Snapshot once at meta load; a drag on the slider only changes the live values, so
 *  restoring these gets the user back to a picture they know worked. */
const initialContrast = ref<{ lo: number; hi: number }[]>([])
/** Same snapshot for the per-channel LUT (the colour), so the "Distinct colours" toggle can be
 *  turned OFF and put every channel back to the server default. Deep-copied — the toggle mutates
 *  `channels[c].lut` in place. */
const initialLUTs = ref<number[][][]>([])
/**
 * Assign each channel a hue from `distinctColors` — the house "N visually distinct colours"
 *  helper (`plots/plot.ts`, golden-angle rotation). Same idiom as `randomcoloR::distinctColorPalette`
 *  in the old R viewer. Toggle off restores the server-shipped colours.
 */
const distinctChannelColours = ref(false)
watch(distinctChannelColours, on => {
  const m = meta.value
  if (!m) return
  if (on) {
    const nch = Math.min(m.nC, MAX_CHANNELS)
    distinctColors(nch).forEach((hsl, c) => {
      const [r, g, b] = hslCssToRgb(hsl)
      m.channels[c].lut = lutFromHex(toHex([r, g, b]))
    })
  } else {
    m.channels.forEach((ch, c) => {
      const init = initialLUTs.value[c]
      if (init) ch.lut = init.map(stop => [...stop])
    })
  }
  pushChannels()
})
const timing = ref<{ fetchMs: number; uploadMs: number; serverMs: number } | null>(null)
/**
 * The uniform block as the shader last received it — Debug only.
 *
 * A frame that draws nothing has no other evidence to offer: the fetch reports bytes, the cache reports
 * residency, and both can be perfect while the box is the wrong size or the camera is somewhere else
 * entirely. This is the one readout that separates "the data never arrived" from "the data is there and
 * the camera is not looking at it".
 */
const shader = ref<UniformState | null>(null)

/**
 * Clear the canvas to magenta and draw nothing — Debug only, off by default.
 *
 * The one question the offscreen probe cannot answer: it renders into its own texture, so it proves
 * the shader works without proving anything reaches the SCREEN. This uses the real swap chain and the
 * simplest operation on it. Magenta appears → the canvas composites, and the fault is between the
 * draw and the swap-chain texture. Magenta does not appear → nothing this canvas draws is ever shown,
 * and the fault is below us: the browser, the compositor, or something filtering the page.
 */
const testPattern = ref(false)
/**
 * Composite the canvas as `opaque` instead of `premultiplied` — Debug only, off by default.
 *
 * The shader writes alpha 1 on every path, so the two are pixel-identical; they differ only in which
 * compositor path the browser takes. `opaque` is the mode that can be refused or mishandled, and on
 * this machine it showed nothing at all — including a magenta clear with no shader involved, and in a
 * standalone WebGPU page sharing only that one line. Kept as a switch so the comparison stays
 * available rather than becoming folklore.
 */
const opaqueCanvas = ref(false)

/**
 * With the test fill on, read the CANVAS ELEMENT back and say what it holds.
 *
 * This is the end of the line for the blank-viewer diagnosis. The fill clears the real swap-chain
 * texture to magenta with no pipeline, no bind group and no shader — the simplest thing WebGPU can be
 * asked to do. If the element then reads magenta while the screen is black, the canvas holds the
 * colour and is not being composited: nothing in this repo can fix that. If the element reads black
 * too, the clear never reached the swap chain, which IS ours.
 */
async function checkFill() {
  if (!canvas.value) return
  // Two frames of grace: the clear is submitted from a rAF and the snapshot must see the result of it,
  // not of the frame before.
  await new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r)))
  const c = await sampleCanvas(canvas.value)
  const pct = (v: number) => (v * 100).toFixed(1) + '%'
  vlog('warn',
       testPattern.value
         ? `Test fill ON — canvas element holds ${c ? `${pct(c.lit)} lit / max ${pct(c.max)}` : 'nothing readable'}`
         : `Test fill OFF — canvas element holds ${c ? `${pct(c.lit)} lit / max ${pct(c.max)}` : 'nothing readable'}`,
       testPattern.value
         ? 'The fill clears the swap-chain texture to magenta with no shader involved. Lit here + a ' +
           'black screen = the canvas is not being composited, which is below this app. Black here = ' +
           'the clear never reached the swap chain, which is ours.'
         : 'Baseline, for comparison with the fill.')
}

/**
 * The browser renders but does not DISPLAY — set when the shader produced an image and the canvas
 * element came back black.
 *
 * Not a debug field: it is the difference between a viewer that looks broken and one that says what is
 * wrong. This state is unfixable from here — the pixels exist, the canvas holds nothing, and no shader,
 * uniform or pipeline change moves it (Firefox 154 / Wayland / Mesa iris, both `alphaMode` paths, and a
 * standalone WebGPU page sharing none of this code: all black, 2026-08-25). What it needs is a
 * sentence, because a black rectangle reads as "this image is empty".
 */
const displayFault = ref(false)

/** What the shader last actually produced — see `sampleFrame`. Debug only. */
const probe = ref<FrameSample | null>(null)
/** What the CANVAS ELEMENT holds after that draw — see `utils/canvasSample.ts`. The pair is the whole
 *  diagnosis: shader lit + canvas black is a swap-chain problem, both lit is a compositing one. */
const canvasProbe = ref<CanvasSample | null>(null)
/**
 * Say it in the app's console, not just in this window.
 *
 * This is a pop-out — a second app instance with its own store and NO console rail (App.vue renders one
 * only for the shell) — so a `logStore` call here would go somewhere nobody can read. The channel puts
 * the line in the console you already watch napari in, which is the whole ask: "can't we print this to
 * the console" (Dominik, 2026-08-25).
 */
const vlog = (level: 'info' | 'warn' | 'error', message: string, detail?: string) =>
  publishUiLog({ level, message, detail, source: 'viewer' })
/** Announce the geometry once per box, not once per frame — set wherever the box is (re)built. */
const announce = ref(true)
/**
 * The last attempt at THIS image never reached a frame — see `utils/viewerCrashGuard.ts`.
 *
 * The probe below cannot see this case: the adapter answers, `r16uint` checks out, and the driver
 * segfaults later anyway, taking the browser with it before any handler runs. Reopening the window
 * lands on the same URL, so the next click is the same crash. We start held instead.
 */
const heldAfterCrash = ref(false)
/** The GPU verdict, shown WITH the hold. Sending someone to Settings → Diagnostics from a pop-out
 *  means finding another window; the probe is one call and the answer belongs next to the question. */
const heldProbe = ref('')

/**
 * The h5ad-derived overlays (P3): population points now, tracks next.
 *
 * ONE fetch for the whole movie, because it is small — measured at 2.0 MB for the largest cell table in
 * the dev projects and 0.13 MB for the typical one, against 8.8 MB for a single 2D slab. So there is no
 * request path here that a scrub can spam, and nothing to keep coherent with the timepoint cache.
 */
const overlays = ref<OverlayPayload | null>(null)
const overlaysErr = ref('')
/**
 * Track ribbons are drawn from PER-SEGMENTATION payloads, one per vn the panel's track eye has
 * ticked — so a user can see coastalFg's tracks AND coastalSm15's tracks at the same time, even
 * while the pop manager (which drives the main `overlays` fetch) is on a third, un-tracked vn like
 * `default`. Cached across renders so a repeat toggle is instant; refreshed when trackVisibility
 * changes or the viewer is pinged for an overlay update. Napari draws one Tracks layer per vn;
 * this is the WebGPU analogue.
 */
const trackPayloads = ref<Map<string, OverlayPayload>>(new Map())
/** Trackclust pops payload per ticked vn — a SECOND cache, keyed on the same vns but fetched with
 *  `popType=trackclust` so `pops` carries the track-cluster populations for that vn. Populated on
 *  loadTracks when the panel's Trackclust master toggle is on; used by rebuildOverlays to add one
 *  filtered-by-pop-labels source per trackclust pop with `pop.colour`. See
 *  VIEWER_CONTROLS_SPLIT_PLAN.md → P7 tail. */
const trackclustPayloads = ref<Map<string, OverlayPayload>>(new Map())
/** Per-source (per-vn) counts + palette hex from the last `buildMultiTrackBuffer` result. Feeds the
 *  Tracks legend so a viewer with several ticked eyes shows a swatch key rather than a rainbow with
 *  no reading. */
const trackSources = ref<{ vn: string; hex: string; count: number }[]>([])
/** Speed range in µm per hop (Δt = 1 frame), or null when the mode isn't speed. Feeds the ramp
 *  legend under the Tracks control block, same shape as the point colour-by numeric scale. */
const trackSpeedRange = ref<[number, number] | null>(null)
/** Whether the panel has this vn's popType turned on. Mirrors the same read `loadOverlays` uses
 *  when it decides whether to clear the payload's pops, so the summary line and the pop list agree.
 *  Reactive: `gatingCurrent` changes when the pop manager switches popType, `_setPrefs` updates via
 *  the storage bridge whenever the panel toggles the icon. */
const popsPanelOn = computed(() => {
  const pt = gatingCurrent.value.popType || 'flow'
  return setUid.value ? settings.getPopVisible(setUid.value, pt) : false
})
/** Track colour mode — persisted per set. Empty setUid = a viewer opened without a set context
 *  (rare); falls back to the default 'track'. */
const trackColorMode = computed<'track' | 'speed' | 'solid'>({
  get: () => setUid.value ? settings.getTrackColorMode(setUid.value) : 'track',
  set: (v) => { if (setUid.value) settings.setTrackColorMode(setUid.value, v); rebuildOverlays() },
})
/** Set the per-source override colour (Solid mode legend picker). No-op without a setUid, since the
 *  override is per set — a rare viewer opened without one just keeps the palette default. */
function setTrackSourceColour(vn: string, hex: string) {
  if (!setUid.value) return
  settings.setTrackSourceColour(setUid.value, vn, hex)
  rebuildOverlays()
}
/**
 * The pop manager's CURRENT (valueName, popType) for THIS image. Published to `cc.gatingCurrent`
 * by the gating store (main window) on selectImage + on any (valueName, popType) change, and
 * refreshed here on `storage` events. Read by `loadOverlays` so the viewer draws the pops the
 * user is authoring, not the pops of the "active segmentation" resolved server-side. Empty
 * strings = fall back to the server default (`_resolve_vn` + popType=flow) — the previous
 * behaviour, preserved for a viewer opened before the pop manager has selected anything.
 * Dominik, 2026-08-26: "why do you have flowtom as the only pop source for fXgbTl. it should
 * switch depending on the pop manager not depending on the segmentation being shown on the image".
 */
function readGatingCurrent(): { valueName: string; popType: string } {
  if (typeof localStorage === 'undefined' || !imageUid) return { valueName: '', popType: '' }
  try {
    const bag = JSON.parse(localStorage.getItem('cc.gatingCurrent') ?? '{}') as
                Record<string, { valueName?: string; popType?: string }>
    const e = bag[imageUid] ?? {}
    return { valueName: String(e.valueName ?? ''), popType: String(e.popType ?? '') }
  } catch { return { valueName: '', popType: '' } }
}
const gatingCurrent = ref(readGatingCurrent())

/**
 * `settings.viewerSelectMode` reactivity ACROSS windows. The popup viewer is its own Pinia
 * instance, so a change to `viewerSelectMode` in the main window's settings store does NOT
 * propagate — a localStorage `storage` event does. Same idiom as `cc.gatingCurrent` above and
 * `cc.viewerOverlaysTick` below. Kept as a local ref rather than reading `settings.viewerSelectMode`
 * so the storage listener has a single obvious target.
 */
const selectModeActive = ref<boolean>(
  (typeof localStorage !== 'undefined' && localStorage.getItem('cc.viewerSelectMode') === 'select')
  || settings.viewerSelectMode === 'select',
)
function readSelectMode(): boolean {
  if (typeof localStorage === 'undefined') return false
  return localStorage.getItem('cc.viewerSelectMode') === 'select'
}
/** Flip the mode from the viewer itself — mirrors what the gating toolbar's pencil does, so a user
 *  can stay in the viewer window without reaching back to the module page. Writes both the local
 *  ref (immediate) AND localStorage (the main-window's settings store watches this key). */
function toggleSelectMode() {
  const next = !selectModeActive.value
  selectModeActive.value = next
  try { localStorage.setItem('cc.viewerSelectMode', next ? 'select' : 'off') } catch { /* noop */ }
}
/** Populations the USER has hidden, by path. The server's own `show` flag is honoured separately, so a
 *  pop hidden in the population manager stays hidden here without a second source of truth. */
const hiddenPops = ref<Set<string>>(new Set())
/**
 * Track-layer visibility hides a pop's RIBBONS separately from its POINTS. A user showing points for
 * `/qc/CD169-/cells` and hiding its tracks is a valid state — collapsing this into `hiddenPops` would
 * silently link the two, which the plan (MULTI_POP_TRACKING_PLAN.md Decision 5) rejects.
 * Path-keyed, initialised empty on every overlays reload so a fresh payload's rows all start on.
 */
const hiddenTrackPops = ref<Set<string>>(new Set())
/**
 * Point size, SHARED with the panel when the set is known. `computed` with a setter so every read
 * and write goes to the one store the panel already uses — the same image cannot then look different
 * depending on which eye opened it. Falls back to the window's own setting when there is no set uid.
 */
const pointSize = computed({
  get: () => (setUid.value ? settings.getPointSize(setUid.value) : settings.viewerPointSize),
  set: (v: number) => setUid.value ? settings.setPointSize(setUid.value, v) : (settings.viewerPointSize = v),
})
// Screen-px width of the black outline drawn around every point. 0 disables it, so the default keeps
// the existing rendering pixel-identical. Shares `pointSize`'s per-set/global fallback.
const pointBorder = computed({
  get: () => (setUid.value ? settings.getPointBorder(setUid.value) : settings.viewerPointBorder),
  set: (v: number) => setUid.value ? settings.setPointBorder(setUid.value, v) : (settings.viewerPointBorder = v),
})
/**
 * Which obs column shades the points, '' for the population colour. **Read only** in the viewer —
 * locked decision 3: the viewer has no selectors. The CHOICE lives in `ViewerPanel`, keyed per set
 * in `settings.setColourBy`, and reaches this window via the P2 storage-event bridge. A change here
 * is a request for the server (the values come from disk), so a watch refetches the overlays.
 * See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P4.
 */
const colourBy = computed(() => setUid.value ? settings.getColourBy(setUid.value) : '')
watch(colourBy, () => { void loadOverlays() })
/**
 * Which segmentation's MASK is drawn, '' for none.
 *
 * Not a selector any more — the CHOICE lives in `ViewerPanel`, keyed per image in
 * `settings.getLabelVisibility` (P3, docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md). Locked decision 3:
 * the viewer has no selectors. Here we read whichever vn the panel has ticked on and render it;
 * the panel's write hits localStorage and reaches this window via the P2 storage-event bridge.
 *
 * STILL ONE AT A TIME. Multi-mask rendering (multi-texture bind group) is a later phase; if the
 * panel has more than one ticked, the first one wins deterministically. This is the visible
 * limitation the row hint below spells out.
 */
const labelName = computed(() => {
  const names = meta.value?.labelNames ?? []
  if (!imageUid || !names.length) return ''
  const vis = settings.getLabelVisibility(imageUid, names)
  return names.find(n => vis[n]) ?? ''
})
// A change of source-of-truth is a request for a new mask, and the mask rides each timepoint's slab
// — so a change here has to `reallocate()` for the same reason a `<select>` did.
watch(labelName, () => reallocate())
// Renderer swap when the user flips the bricks mode. `ensureRenderer` has an `if
// (renderer.value) return` short-circuit, so we have to destroy first — without this the
// toggle changed `bricksEnabled.value` but the OLD renderer kept drawing (Dominik
// 2026-08-29: "toggle doesn't do a full reload, need to reload the page").
watch(() => settings.viewerBricksMode, () => {
  if (renderer.value) { renderer.value.destroy(); renderer.value = null }
  reallocate()
})
// Cache size change → full reallocate (atlas / timepoint textures resize). Keep the renderer
// instance — only `setImage` needs to re-run at the new byte budget; the eviction/growth path is
// what `setImage` was written to do. Skips URL-override case since that's frozen at mount.
watch(() => settings.viewerCacheMB, () => {
  if (cacheMBFromUrl) return
  reallocate()
})
// 3D projection flip is a uniform write — cheap. No reallocate: the shader already contains both
// paths and `setOrthographic` just flips the `ortho` bit. In 2D the value is fixed to orthographic
// by `reallocate`, so this watcher is a no-op there.
watch(() => settings.viewerVolumeProjection, v => {
  if (mode.value !== 'volume') return
  const r = renderer.value
  if (!r) return
  r.setOrthographic(v === 'ortho')
  frame.redraw()
})
// P7: refetch whenever the preview labels for THIS image change — a fresh reply (plane change /
// param edit / toggle-on) or a toggle-off. Watch a NUMERIC key derived from `previewLabels`:
//   * `updateId` (>0) when the current preview matches this image
//   * `0`                otherwise
// A primitive watch is bulletproof against the object-identity subtleties of accessing a Pinia
// ref through the store proxy — earlier attempts (boolean, object reference) missed re-runs on
// the same image, so the mask on screen stayed pinned to the first run's plane.
const previewLabelsKey = computed(() => {
  const p = viewerStore.previewLabels
  return p && p.imageUid === imageUid ? p.updateId : 0
})
watch(previewLabelsKey, () => reallocate())

// P7.1: refetch whenever the AF preview for THIS image changes — a new AF run, a parameter edit or
// a toggle-off. Every entry in the array shares one `updateId` (the store stamps them together), so
// the key can key off any entry; use the first. Same primitive-watch discipline as previewLabelsKey.
// Which source channels are being swapped can change between runs (different `afCombinations`), so
// the shape of the set — not just the stamp — matters. Encode both as `"<updateId>:<c1>,<c2>,…"`.
const previewImagesKey = computed(() => {
  const arr = viewerStore.previewImages
  if (!arr || arr.length === 0) return ''
  const forThis = arr.filter(m => m.imageUid === imageUid)
  if (forThis.length === 0) return ''
  const chans = forThis.map(m => m.sourceChannel).sort((a, b) => a - b).join(',')
  return `${forThis[0].updateId}:${chans}`
})
watch(previewImagesKey, () => {
  // A new AF run (or a toggle-off) resets every "show original" suspension — the corrected channels
  // this run picked can be different from the last, so a stale entry could suspend a channel that
  // is not corrected at all now.
  afSuspended.value = new Set()
  reallocate()
})

/** Per-channel A/B toggle: click the AF badge to read the SOURCE bytes for that ONE channel while the
 *  other corrected channels stay swapped. Local to this viewer window — the state doesn't cross the
 *  cross-window bridge because it's a per-viewer view choice, not a preview-run output.
 *  `previewImagesKey`'s watch resets it whenever a new run lands. */
const afSuspended = ref<Set<number>>(new Set())
function toggleAfSuspended(c: number) {
  const next = new Set(afSuspended.value)
  next.has(c) ? next.delete(c) : next.add(c)
  afSuspended.value = next
  reallocate()
}

/** Fast lookup: source channel → the AF preview entry that overrides it for this image. Recomputed
 *  in the render loop, but the array is tiny (one entry per corrected channel, typically 1–4) so
 *  this stays a plain `.find` — no map cache needed. Returns null when the user has clicked the
 *  badge to suspend the swap on THIS channel (A/B). */
function previewImageFor(c: number) {
  if (afSuspended.value.has(c)) return null
  const arr = viewerStore.previewImages
  return arr?.find(m => m.imageUid === imageUid && m.sourceChannel === c) ?? null
}
/** Does an AF correction EXIST for this channel? Distinguishes an uncorrected channel (no badge)
 *  from a corrected-but-user-flipped-to-source channel (dim badge). */
function hasAfPreview(c: number) {
  const arr = viewerStore.previewImages
  return !!arr?.some(m => m.imageUid === imageUid && m.sourceChannel === c)
}
/** How many segmentations the panel has ticked on. Drives the "N ticked, showing one" hint below —
 *  when it's above 1, the visible limitation gets named rather than the extras silently dropping. */
const shownLabelCount = computed(() => {
  const names = meta.value?.labelNames ?? []
  if (!imageUid || !names.length) return 0
  const vis = settings.getLabelVisibility(imageUid, names)
  return names.filter(n => vis[n]).length
})
let points: PointBuffer = { data: new Float32Array(0), ranges: new Map(), count: 0 }
const EMPTY_SEGMENTS: SegmentBuffer = {
  data: new Float32Array(0), firstAt: new Int32Array(1), endAt: new Int32Array(1), count: 0,
}
let segments: SegmentBuffer = EMPTY_SEGMENTS
const pointCount = ref(0)
const segCount = ref(0)
const summary = computed(() => overlaySummary(overlays.value))
/**
 * Ribbon-drawable pops from the current overlays payload — pops the Tracks section enumerates as
 * per-pop layer rows (swatch/name/count/eye), mirroring the Populations section. A pop qualifies
 * when it was TYPED as a track pop (`isTrack`) OR when it currently holds cells with `track_id > 0`
 * (`hasTracks`, MULTI_POP_TRACKING_PLAN.md Decision 2). Sorted by path so the ordering is stable
 * across refetches. `count` is the pop's label count from the payload — a cell-level number that
 * matches what the Populations section shows for the same pop, so the two sections read the same.
 */
const trackDrawablePops = computed(() => {
  const pops = overlays.value?.pops ?? []
  return pops.filter(p => p.show && p.labels?.length && (p.isTrack || p.hasTracks))
             .map(p => ({ path: p.path, name: p.name, colour: p.colour, count: p.labels.length }))
             .sort((a, b) => a.path.localeCompare(b.path))
})
/** The ramp as a CSS gradient, from the same 256-entry lookup the points are shaded with — a legend
 *  built from a different set of stops would be a second answer about the same scale. */
const rampStyle = computed(() => {
  const stops = Array.from({ length: 12 }, (_, i) =>
    rgbHex(heatUnit(i / 11).map(v => v * 255)))
  return { background: `linear-gradient(to right, ${stops.join(', ')})` }
})

const cam = ref<OrbitCamera>({ yaw: 0, pitch: 0, dist: 1, panX: 0, panY: 0 })
const fitDist = ref(1)
/**
 * The level the current textures were ALLOCATED for. `slabLevel` is a derived value that reacts to
 * camera zoom; when the two disagree the level watch fires `reallocate(false)` (debounced), so a wheel
 * gesture that crosses two thresholds refetches once. Set inside `reallocate()` right after `setImage`
 * so the watch cannot chase a level that has already been picked up.
 */
const loadedLevel = ref(-1)
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
 * L0 image pixels per DEVICE pixel — the zoom the LOD picker takes. Derived from `cam.dist`: the
 * plane view is orthographic with visible height = `2 · dist · VIEW_HALF_ANGLE` µm at every depth
 * (`visibleExtentUm`), which converts to L0 pixels via `voxelUm[1]` and divides by device-pixel canvas
 * height. `zoom > 1` = one device pixel shows multiple L0 pixels (zoomed out — coarser level cheaper
 * and no detail lost); `zoom ≤ 1` = magnified past 1:1 (stay on L0).
 */
const camZoom = computed(() => {
  const c = canvas.value
  const m = meta.value
  if (!c || !m) return 1
  const visibleHeightUm = 2 * Math.max(cam.value.dist, 0) * VIEW_HALF_ANGLE
  const visibleL0Y = visibleHeightUm / (m.voxelUm[1] || 1)
  const devicePxY = Math.max(c.clientHeight * (window.devicePixelRatio || 1), 1)
  return visibleL0Y / devicePxY
})
/**
 * Pyramid level the current view fetches at. napari renders 3D at the coarsest level, and a full-res
 * volume of a wide-XY image exceeds WebGPU's `maxBufferSize` (`f8gzA2` needs 1.28 GB against a 256 MB
 * cap) — so the 3D view picks the deepest level by default, user-overridable via
 * `settings.viewerVolumeLevel`.
 *
 * The 2D plane view is ZOOM-DRIVEN — `pickTileLevel(camZoom, meta)` at every camera dist. That is the
 * whole point of the pyramid: at fit-to-window on a 20k×17k image, one device pixel already covers
 * ~20 L0 pixels, so fetching L0 ships pixels the screen cannot show — L4 is the coarsest level whose
 * native pixel is still ≤ one device pixel and it is 256× cheaper on the wire. As the user zooms in
 * past a `floor(log2(zoom))` threshold, `slabLevel` drops and the level watch reallocates. Level swaps
 * are debounced through `levelPump` so a wheel gesture that crosses two thresholds refetches once.
 * Still a whole-plane-per-level fetch (Phase B of `docs/todo/VIEWER_TILES_PLAN.md`); per-viewport tiles
 * come next.
 */
const slabLevel = computed(() => {
  const m = meta.value
  if (!m) return 0
  if (mode.value === 'plane') {
    const o = settings.viewerPlaneLevel
    if (o >= 0) return Math.max(0, Math.min((m.levels?.length ?? 1) - 1, Math.floor(o)))
    // Anchor on `loadedLevel` (what's on the GPU) so hysteresis biases against thrashing the
    // whole plane on a wobble around a `floor(log2(zoom))` boundary. `loadedLevel = -1` before
    // the first `reallocate` falls through to the classic floor picker — see `pickTileLevel`.
    return pickTileLevel(camZoom.value, m, loadedLevel.value)
  }
  const override = settings.viewerVolumeLevel
  return pickVolumeLevel(m, override < 0 ? undefined : override)
})
/** The XY dims actually being fetched — level-0's `meta.nX`/`nY`, or the coarser level's dims. */
const renderNX = computed(() =>
  meta.value?.levels?.[slabLevel.value]?.nX ?? meta.value?.nX ?? 0)
const renderNY = computed(() =>
  meta.value?.levels?.[slabLevel.value]?.nY ?? meta.value?.nY ?? 0)
/**
 * The tile pipeline turns on for the case it exists to serve — a plane whose L0 whole-fetch doesn't
 * fit in a comfortable single slab (VIEWER_TILES_PLAN.md → Phase C). Below the threshold, the volume
 * renderer's whole-plane path already works: a channel is a few MB, the plan movie plays, and the tile
 * atlas would add pipeline plumbing for no visible win. Above it — the `f8gzA2`-shape whole slide — a
 * whole plane cannot fit period, and the tile pipeline is the only way to interact.
 *
 * Timecourse whole-slide is a case we do not have (no store in the dev projects has both nT > 1 and a
 * plane over the threshold), so the tile path is gated on `nT ≤ 1` — the eviction ranker does not yet
 * penalise cross-timepoint distance, so caching tiles from several timepoints would fight for slots
 * without ordering them. Ship the whole-slide case first; if a timecourse tilescan turns up, add `t` to
 * the key.
 *
 * Threshold: 200 MB per channel-plane, chosen against Chromium/Dawn's 256 MB `maxBufferSize` — a whole
 * plane above this genuinely CANNOT be uploaded in one texture write. Not tuned.
 */
const PLANE_TILE_THRESHOLD_BYTES = 200e6
const needsTiling = computed(() => {
  const m = meta.value
  if (!m) return false
  const nch = Math.min(m.nC, MAX_CHANNELS)
  return m.nX * m.nY * m.bytesPerVoxel * nch > PLANE_TILE_THRESHOLD_BYTES
})
const useTiles = computed(() =>
  mode.value === 'plane' && needsTiling.value)
/** Whichever renderer is currently alive — for template reads that don't care which one. */
const activeAdapter = computed(() =>
  renderer.value?.adapter ?? tileRenderer.value?.adapter ?? null)
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
// 3D projection toggle — Imaris-style. Ortho is the default (matches the offline movie renderer and
// reads more head-on for intravital movies, Dominik 2026-09-01); Persp adds foreshortening depth cue.
const PROJECTIONS = [
  // Flat square = head-on / no depth cue; angled box = converging depth. Tooltips carry the
  // full name — the glyphs are the recognisable shortcut once you know which is which.
  { value: 'ortho', label: '', icon: 'pi pi-stop', tip: 'Orthographic — head-on, no foreshortening (matches movie output)' },
  { value: 'persp', label: '', icon: 'pi pi-box', tip: 'Perspective — closer geometry looks larger' },
]
/** 3D renderer selection. Auto uses `shouldUseBricks(meta)`; the two overrides are the safety
 *  valves for images the predicate gets wrong. Copy stays short — "Auto/Brick/Flat" reads faster
 *  than "Auto-select/Force brick/Force flat" in a segmented control. */
const BRICKS_MODES = [
  { value: 'auto', label: 'Auto', tip: 'Pick based on movie size vs cache' },
  { value: 'brick', label: 'Brick', tip: 'Force per-viewport streaming' },
  { value: 'flat', label: 'Flat', tip: 'Force per-timepoint cache' },
]
/** Brick-scheduler quality tier → `maxIntersect` (core-brick ceiling in the over-fetch guard).
 *  Balanced = `MAX_INTERSECT_BRICKS` shipped default. Quick halves it (wider viewports stay
 *  coarser but drop fewer bricks); Detailed doubles it — do NOT read that as a validated safe
 *  maximum, per KILN_BRICK_PLAN.md #705 the values were tuned against two reference images and
 *  the upper end is where f8gzA2's shader-scaling regression (200ms drawP95) lives for any large
 *  single-level volume routed through bricks. */
const BRICK_TIER_THRESHOLD: Record<'quick' | 'balanced' | 'detailed', number> = {
  quick: 128,
  balanced: 256,
  detailed: 512,
}
const BRICK_TIERS = [
  { value: 'quick', label: 'Quick', tip: 'Fewer bricks per view — faster on wide zooms, coarser' },
  { value: 'balanced', label: 'Balanced', tip: 'Shipped default' },
  { value: 'detailed', label: 'Detailed', tip: 'More bricks per view — finer at cost' },
]
/** Core-brick ceiling actually shipped to the scheduler. URL `?brickThr=N` wins so a dev
 *  measurement pass isn't overwritten by the tier control; otherwise the tier decides. */
const effectiveMaxIntersect = computed(() =>
  brickKnobThrFromUrl ? brickKnobThr : BRICK_TIER_THRESHOLD[settings.viewerBrickTier])
/** Cache size options — chips, not a spinner. Bytes are the honest currency (unlike Quality tier,
 *  where the safe range isn't measured), but a spinner offering arbitrary MB values implies
 *  "any value is fine" — same trap. */
/** Fallback for Auto when no adapter is known yet (renderer not constructed). Conservative;
 *  matches `webgpuProbe`'s pattern of "never claim a value you don't have." */
const AUTO_CACHE_MB_FALLBACK = 512
/** Auto derivation: integrated → 512 MB, discrete → 2 GB. Both capped at 70% of the browser's
 *  `maxBufferSize` — the atlas is one big 3D texture and can't exceed the biggest allocatable
 *  buffer. 0.7 is a safety margin against soft OOM from other tabs; tune when we've measured. */
const AUTO_CACHE_SAFETY = 0.7
const AUTO_CACHE_MB = computed(() => {
  const a = activeAdapter.value
  if (!a) return AUTO_CACHE_MB_FALLBACK
  const target = a.looksDiscrete ? 2048 : 512
  const cap = Math.floor((a.maxBufferSize * AUTO_CACHE_SAFETY) / (1024 * 1024))
  return Math.min(target, Math.max(128, cap))
})
// ChipSelect takes string values; the setting stores a number. Convert at the boundary.
// Options list is a computed — the numeric chips get `disabled` when they exceed what the
// browser's `maxBufferSize` will actually allocate (0.7 safety margin, same as `AUTO_CACHE_MB`),
// so a user on a 256-MB-buffer laptop can't pick 4 GB and hit the renderer's own OOM guard.
// Renderers self-guard: `volumeRenderer.ts:729` and `brickAtlasTexture.ts:87` both catch
// `out-of-memory` on the underlying texture allocation. Disabling oversized chips is UX (don't
// let the user pick a value that WILL trip that guard), not correctness.
const BASE_CACHE_OPTIONS: Array<{ value: string; label: string; mb: number; tip: string }> = [
  { value: 'auto', label: 'Auto', mb: 0, tip: 'The shipped default' },
  { value: '512', label: '512 MB', mb: 512, tip: 'Small — leaves VRAM for other apps' },
  { value: '1024', label: '1 GB', mb: 1024, tip: '' },
  { value: '2048', label: '2 GB', mb: 2048, tip: 'Large — more timepoints / bricks resident' },
  { value: '4096', label: '4 GB', mb: 4096, tip: 'Aggressive — only on discrete GPU with headroom' },
]
const cacheHardCapMB = computed(() => {
  const a = activeAdapter.value
  return a ? Math.floor((a.maxBufferSize * AUTO_CACHE_SAFETY) / (1024 * 1024)) : Infinity
})
const CACHE_MB_OPTIONS = computed(() => BASE_CACHE_OPTIONS.map(o => {
  const overCap = o.mb > 0 && o.mb > cacheHardCapMB.value
  return {
    value: o.value,
    label: o.label,
    disabled: overCap,
    tip: overCap
      ? `Beyond this GPU's buffer cap (${cacheHardCapMB.value} MB)`
      : o.tip,
  }
}))
/** Human-readable resolved values for the "Using: X" captions under the Advanced chips. Applies
 *  whether the user picked Auto or forced a value — a caption that only appears under Auto would
 *  jump the layout on every flip. */
const effectiveRendererLabel = computed(() => bricksEnabled.value ? 'Brick' : 'Flat')
const effectiveCacheMBLabel = computed(() =>
  `${Math.round(effectiveCacheBytes.value / (1024 * 1024))} MB`)
/** Colour class for the cache-size caption: green when the pick is comfortably below the GPU's
 *  buffer cap, amber when it's within the top half of the safe range. Chips above `cacheHardCapMB`
 *  are already disabled, so amber flags "you're picking large for this GPU", not "you'll crash". */
const cacheSeverity = computed(() => {
  const cap = cacheHardCapMB.value
  if (!Number.isFinite(cap)) return 'ok'
  const pickedMB = effectiveCacheBytes.value / (1024 * 1024)
  return pickedMB > cap * 0.5 ? 'warn' : 'ok'
})
const cacheMBAsString = computed(() =>
  settings.viewerCacheMB > 0 ? String(settings.viewerCacheMB) : 'auto')
/** URL override for the cache size. `?cacheMB=N` wins over the persisted setting (same shape as
 *  `?brickThr=`). Empty = defer to setting. */
const cacheMBFromUrl = String(route.query.cacheMB ?? '') !== ''
const cacheMBUrl = parseNumQuery(route.query.cacheMB, AUTO_CACHE_MB_FALLBACK)
/** Effective VRAM budget in BYTES for `setImage`. URL wins, then the persisted setting, then Auto. */
const effectiveCacheBytes = computed(() => {
  if (cacheMBFromUrl) return Math.max(1, cacheMBUrl) * 1024 * 1024
  const mb = settings.viewerCacheMB
  return (mb > 0 ? mb : AUTO_CACHE_MB.value) * 1024 * 1024
})
// Live-apply the tier without reallocating the renderer. Ignored on the flat renderer (setter
// is optional). `bias` stays URL-only, so it rides through unchanged. Nudge the frame pump so
// the new threshold takes effect on the next draw — otherwise `tickScheduler` doesn't re-run
// until the user moves the camera (2026-08-31 Dominik: "are you sure you're reloading").
watch(effectiveMaxIntersect, v => {
  renderer.value?.setSchedulerKnobs?.({ maxIntersect: v, bias: brickKnobBias })
  frame.redraw()
})
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
  // Tile mode owns the canvas when a whole-slide plane is active — see `useTiles`. Dispatched HERE
  // rather than upstream because every camera/pointer path funnels through this closure; a second
  // dispatch site would drift from this one.
  if (useTiles.value) { drawTiles(); return }
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
  // Widened by the z tolerance: a cell spans several planes, so drawing a marker only on the plane its
  // CENTROID falls on shows a handful of points against a mask layer full of cells — which reads as the
  // points being random rather than as a strict slice (Dominik, 2026-08-25). 0 is the strict reading and
  // is still available.
  const tol = Math.max(0, settings.viewerPointZTol)
  r.setOverlayDraw(range ? range[0] : 0, range ? range[1] : 0,
                   pointSize.value, pLo - tol, pHi + tol, pointBorder.value)
  // A tail of N frames ENDING at the frame on screen. Contiguous in the segment buffer by construction,
  // so this is two array reads rather than a per-frame filter.
  const tail = shownT.value >= 0 && settings.viewerTailLength > 0
    ? tailRange(segments, shownT.value, settings.viewerTailLength) : null
  // Track ribbons carry their OWN Z reach — usually wider than the points' (a track spans several
  // planes and reads best with slack). Same "negative lo = no filter" convention as the points.
  const trackTol = Math.max(0, settings.viewerTrackZTol)
  r.setOverlaySegmentDraw(tail ? tail[0] : 0, tail ? tail[1] : 0, settings.viewerTailWidth,
                          pLo - trackTol, pHi + trackTol)
  // Mask style, here for the same reason as the rest: it is display state, and a watcher that set it
  // elsewhere could disagree with the frame on screen. Opacity 0 with no segmentation picked is what
  // switches the shader's label path off — the placeholder texture stays bound, because a bind group
  // has to be complete.
  // P7: a task preview writes a labels-shaped scratch store for its output vn; render it even when the
  // user hasn't picked a labels layer (a first-time segmentation has no picker entry to select).
  const showLabels = !!labelName.value || (!!viewerStore.previewLabels &&
    viewerStore.previewLabels?.imageUid === imageUid)
  r.setLabelStyle(showLabels ? settings.viewerLabelOpacity : 0, settings.viewerLabelContour)
  r.setAlphaMode(opaqueCanvas.value ? 'opaque' : 'premultiplied')
  r.setTestPattern(testPattern.value)
  // Draw-time recording is gated on Debug being open (or `?bench=1` for the full-session
  // workflow). Debug is a diagnostic surface — no reason to pay per-frame `performance.now()`
  // + a GPU timestamp-query resolve when the panel isn't visible. Under `?bench=1` arrays grow
  // unbounded; otherwise they roll over PERF_RING.
  const recording = benchEnabled.value || openSection.value === 'debug'
  const drawT0 = recording ? performance.now() : 0
  r.draw()
  if (recording) {
    const drawT1 = performance.now()
    const sample: BenchSample = { atMs: drawT1, drawMs: drawT1 - drawT0 }
    // shallowRef on an array — replace with a new array so consumers reacting to it see the
    // change (mutating in place wouldn't trigger the computed).
    benchFrames.value = benchEnabled.value
      ? [...benchFrames.value, sample]
      : [...benchFrames.value.slice(-(PERF_RING - 1)), sample]
    if (benchFirstFrameMs.value === null && benchT0.value > 0) {
      benchFirstFrameMs.value = drawT1 - benchT0.value
    }
  }
  // Read back AFTER the draw, so Debug shows the numbers the frame on screen was rendered from rather
  // than the ones the next frame will use.
  const st = r.uniformState()
  shader.value = st
  // A frame is on screen, so whatever the driver was going to do to us, it did not. This is the only
  // place the breadcrumb is cleared: clearing it at device creation would clear it before the line
  // that crashes.
  if (shownT.value >= 0) clearViewerAttempt()
  // Once per box: the numbers only change when the mode, the plane or the crop does, and a line per
  // frame is not a log.
  if (announce.value && shownT.value >= 0) {
    announce.value = false
    vlog('info',
         `Viewer drawing ${meta.value?.nX}×${meta.value?.nY}×${meta.value?.nZ}, ` +
         `${st.nch} ch, ${mode.value === 'plane' ? 'plane ' + zPlane.value : '3D'}`,
         `box ${st.ext.map(v => v.toFixed(1)).join(' × ')} µm · camera ${st.dist.toFixed(0)} µm · ` +
         `pan ${st.pan[0].toFixed(0)},${st.pan[1].toFixed(0)} · ${st.steps} step(s) · ` +
         (st.ortho ? 'orthographic' : 'perspective') +
         ` · canvas ${st.canvas[0]}×${st.canvas[1]}`)
    // And what it actually PRODUCED. A blank viewer has two completely different causes — the shader
    // drew black, or it drew an image the screen never got — and nothing else told them apart: the
    // fetch reports bytes, the cache reports residency, the uniforms read back correct, and the canvas
    // is black either way.
    // THREE measurements, because there are three places the pixels can be lost and each pair of them
    // is ambiguous on its own: the volume alone, the volume WITH the overlays (which share the render
    // pass and can invalidate all of it), and the canvas element itself.
    void Promise.all([r.sampleFrame(false), r.sampleFrame(true), sampleCanvas(canvas.value!)])
      .then(([vol, full, el]) => {
        probe.value = full ?? vol
        canvasProbe.value = el
        const pct = (v: number) => (v * 100).toFixed(1) + '%'
        const say = (f: FrameSample | CanvasSample | null) =>
          f ? `${pct(f.lit)} lit / max ${pct(f.max)}` : 'not sampled'
        const [pts, tails] = r.overlayCounts()
        // The one reading that names a cause outright: the volume draws, and adding the overlays to
        // the same pass loses it. An invalid overlay pipeline or an instanced draw past the end of its
        // buffer discards the whole pass, including what was already in it.
        const overlaysKill = !!vol && vol.max > 0 && !!full && full.max === 0
        // Rendered but not displayed. Only claimed when BOTH probes answered: a null canvas read means
        // the browser would not snapshot the canvas, which is not the same as the canvas being black.
        displayFault.value = !!full && full.max > 0 && !!el && el.max === 0
        vlog(overlaysKill || (!!full && full.max > 0 && !!el && el.max === 0) ? 'warn' : 'info',
             `Viewer pixels — volume: ${say(vol)} · +overlays: ${say(full)} · canvas: ${say(el)}`,
             `${pts} point + ${tails} tail instances` +
             (overlaysKill
               ? ' — THE OVERLAYS ARE DISCARDING THE PASS: the volume renders and adding them to the' +
                 ' same pass loses everything in it.'
               : !!full && full.max > 0 && !!el && el.max === 0
                 ? ' — the draw is not reaching the swap-chain texture.'
                 : !!el && el.max > 0
                   ? ' — the canvas element holds the image, so anything blank on screen is below us.'
                   : ''))
      })
  }
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
  // Track ribbons are drawn from EVERY ticked vn's own payload (see `trackPayloads`), not from the
  // main `overlays` fetch. That way a user with the pop manager on a non-tracked vn (e.g. `default`)
  // still sees ribbons for whichever tracked vns they have the "directions" eye on, and can show
  // several vns at once — matching napari's one-Tracks-layer-per-segmentation model. P7 of
  // docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md.
  // Source list mixes THREE track kinds, in this order:
  //   1. Per-vn base tracks — from `trackPayloads`, the panel's "directions" eye per segmentation.
  //      Colour-cycled by track id (or per-source solid, or heat by speed — the mode picker).
  //   2. Gated-track pop ribbons — one filtered payload per gated track pop with pop.show &&
  //      pop.isTrack, from the POP MANAGER's active payload (`overlays.value`), not the per-vn
  //      track eye. That is the authoring surface for populations, so a viewer with the pop
  //      manager on vn A and no per-vn eye ticked still shows A's gated tracks. Gated by
  //      `settings.getShowGatedTracks(setUid)`.
  //   3. Trackclust ribbons — from `trackclustPayloads[popManagerVn]`, gated by
  //      `settings.getPopVisible(setUid, 'trackclust')`. Fetched with `popType=trackclust` in
  //      `loadTracks`; same filter-by-pop-labels treatment. See VIEWER_CONTROLS_SPLIT_PLAN.md → P7.
  const gatedOn = setUid.value ? settings.getShowGatedTracks(setUid.value) : false
  const trackclustOn = setUid.value ? settings.getPopVisible(setUid.value, 'trackclust') : false
  const overrides = setUid.value ? settings.getTrackSourceColours(setUid.value) : {}
  const sources: { vn: string; payload: OverlayPayload; colour?: string }[] = []
  for (const [vn, payload] of trackPayloads.value.entries()) {
    sources.push({ vn, payload, colour: overrides[vn] })
  }
  const popMgrPayload = overlays.value
  const popMgrVn = gatingCurrent.value.valueName || popMgrPayload?.valueName || ''
  if (gatedOn && popMgrPayload && popMgrVn) {
    for (const pop of popMgrPayload.pops ?? []) {
      // A pop is ribbon-drawable when it was TYPED as a track pop (`isTrack`) OR when its cells
      // actually hold `track_id > 0` (`hasTracks`) — data OR type, either qualifies. Legacy servers
      // omit `hasTracks`; the guard falls back to today's `isTrack`-only behaviour.
      // See docs/todo/MULTI_POP_TRACKING_PLAN.md Decision 2.
      if (!pop.show || !pop.labels?.length) continue
      if (!(pop.isTrack || pop.hasTracks)) continue
      if (hiddenTrackPops.value.has(pop.path)) continue
      const filtered = filterPayloadByLabels(popMgrPayload, new Set(pop.labels))
      if (!filtered.nCells) continue
      const key = `${popMgrVn}::${pop.path}`
      sources.push({ vn: key, payload: filtered, colour: overrides[key] ?? pop.colour })
    }
  }
  if (trackclustOn) {
    const tcPayload = popMgrVn ? trackclustPayloads.value.get(popMgrVn) : undefined
    if (tcPayload) {
      for (const pop of tcPayload.pops ?? []) {
        if (!pop.show || !pop.labels?.length) continue
        const filtered = filterPayloadByLabels(tcPayload, new Set(pop.labels))
        if (!filtered.nCells) continue
        const key = `${popMgrVn}::trackclust::${pop.path}`
        sources.push({ vn: key, payload: filtered, colour: overrides[key] ?? pop.colour })
      }
    }
  }
  if (sources.length) {
    const result = buildMultiTrackBuffer(sources, meta.value, PALETTES.cecelia, trackColorMode.value)
    segments = result.segments
    trackSources.value = result.sources
    trackSpeedRange.value = result.speedRange
  } else {
    segments = EMPTY_SEGMENTS
    trackSources.value = []
    trackSpeedRange.value = null
  }
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
    // Follow the pop manager's selection when it has published one. Empty strings fall back to the
    // server defaults (`_resolve_vn` + popType=flow) — matches the pre-P5 behaviour for a viewer
    // opened before the pop manager writes anything.
    const gc = gatingCurrent.value
    const res = await fetch(overlaysUrl({ projectUid, imageUid, colourBy: colourBy.value,
                                          valueName: gc.valueName, popType: gc.popType }),
                            { cache: 'no-store' })
    const p = await readJson<OverlayPayload>(res, 'Overlays')
    // Two layers of ground truth act on the pops in the payload:
    //   1. The panel's per-pop-TYPE gate ("Populations & tracks" icon row). If the popType the pop
    //      manager is currently on (flow / clust / …) is toggled off in the panel, the whole family
    //      is DROPPED from the overlays payload — not just hidden. The overlays panel then reads as
    //      "no populations gated" (Dominik, 2026-08-26: "there should be no pops in the overlays").
    //      That is what the panel toggle promises: no pops at all, not "pops listed but invisible".
    //   2. Per-pop `pop.show` — authored in the Population Manager, persisted in the gating JSON.
    //      The viewer's row-eye is a transient override for the SAME fetch; the next refetch resyncs.
    //      Trying to preserve local eye state across refetches was worse: PopManager pings this window
    //      on every write, so the override would be clobbered within a second anyway (Dominik,
    //      2026-08-25: "the toggles for pops and tracks dont do anything").
    // Empty `setUid` = a viewer opened without a set context (rare — export path). Default HIDDEN
    // to match `settings.getPopVisible` (line 429) and the panel's own `popVisible` fallback (both
    // false). Before this line defaulted to shown, which contradicted the panel — a viewer whose
    // meta.setUid came back empty showed pop dots while every icon in the panel read as off
    // (Dominik, 2026-08-31: "population dots still pop up despite the gating toggle being off").
    // Empty gating popType = the pop manager hasn't published yet; fall back to the server default
    // (`flow`) — matches the pre-P5 assumption so the pop-family gate stays meaningful.
    const currentPopType = gatingCurrent.value.popType || 'flow'
    const popTypeOn = setUid.value ? settings.getPopVisible(setUid.value, currentPopType) : false
    if (!popTypeOn) p.pops = []
    hiddenPops.value = new Set((p.pops ?? []).filter(x => !x.show).map(x => x.path))
    // Same reset for track-layer visibility: a refetch resyncs from the server's `pop.show` for
    // POINTS, and the ribbon toggle is a transient row-eye that starts CLEAR each payload (a stale
    // hide from an older payload would silently drop a ribbon the user thought they were seeing).
    hiddenTrackPops.value = new Set()
    overlays.value = p
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
// Ribbon eye for one pop — separate set from `hiddenPops` on purpose (see the field's docstring).
function toggleTrackPop(path: string) {
  const next = new Set(hiddenTrackPops.value)
  next.has(path) ? next.delete(path) : next.add(path)
  hiddenTrackPops.value = next
  rebuildOverlays()
}

/**
 * Fetch one overlays payload per ticked track vn — the panel's per-segmentation "directions" eye.
 * Cached across renders (`trackPayloads` map): a repeat toggle needs no refetch, and an un-ticked
 * vn drops from the map so a follow-up rebuild draws only what is still on. Fires alongside
 * `loadOverlays` and again whenever the panel writes trackVisibility (see the storage listener).
 *
 * The endpoint reuses `/api/viewer/overlays`; only `cells.{t,x,y,z,track}` is read here, so the
 * popType is a don't-care and the pops on that payload are ignored — this is a tracks-only path.
 * If the server payload has no `track` array (the vn has no tracked segmentation), we still cache
 * the payload so ticking that eye repeatedly doesn't retry the fetch every time.
 */
async function loadTracks() {
  if (!projectUid || !imageUid) return
  const names = meta.value?.labelNames ?? []
  const vis = settings.getTrackVisibility(imageUid, names)
  const wantVns = names.filter(vn => vis[vn])
  // Drop cached vns no longer ticked
  const next = new Map<string, OverlayPayload>()
  for (const vn of wantVns) {
    const cached = trackPayloads.value.get(vn)
    if (cached) next.set(vn, cached)
  }
  // Fetch newly-ticked vns in parallel — typically 1-3 at once
  const missing = wantVns.filter(vn => !next.has(vn))
  await Promise.all(missing.map(async vn => {
    try {
      const res = await fetch(overlaysUrl({ projectUid, imageUid, valueName: vn }),
                              { cache: 'no-store' })
      if (res.ok) next.set(vn, await res.json())
    } catch { /* one vn's failure must not take the others down */ }
  }))
  trackPayloads.value = next
  // Trackclust payload for the POP MANAGER's active vn — a SECOND fetch (different popType) only
  // when the panel's Trackclust master toggle is on. The pop manager's vn is where those pops are
  // authored, so a viewer with the pop manager on "A" and per-vn eyes elsewhere still lands the
  // trackclust ribbons on A. Cached in a Map<vn, payload> so switching vns keeps prior fetches.
  const trackclustOn = setUid.value ? settings.getPopVisible(setUid.value, 'trackclust') : false
  const popMgrVn = gatingCurrent.value.valueName
  if (trackclustOn && popMgrVn) {
    // Refetch every call — `loadTracks` fires on `cc.viewerOverlaysTick`, which the pop manager
    // pings on ANY gating edit, so a cached-only path would draw stale ribbons after the user
    // changes a trackclust pop. The payload is small; the round trip is cheaper than deciding
    // which mutations are safe to skip.
    try {
      const res = await fetch(overlaysUrl({ projectUid, imageUid, valueName: popMgrVn, popType: 'trackclust' }),
                              { cache: 'no-store' })
      if (res.ok) {
        const next = new Map(trackclustPayloads.value)
        next.set(popMgrVn, await res.json())
        trackclustPayloads.value = next
      }
    } catch { /* trackclust unavailable — leave cache untouched */ }
  } else if (!trackclustOn && trackclustPayloads.value.size) {
    // toggle-off drops the cache so a re-toggle-on refetches (a stale payload after a gating edit
    // would draw the wrong ribbons; the cost of the refetch is one small request per vn).
    trackclustPayloads.value = new Map()
  }
  rebuildOverlays()
}

function pushChannels() {
  const m = meta.value
  if (!m) return
  renderer.value?.setChannels(m.channels)
  tileRenderer.value?.setChannels(m.channels)
  frame.redraw()
  // Same LUT + contrast on the thumbnail so it moves with the main view — a background image the
  // main viewer disagrees with reads as broken.
  renderOverview()
}

/** Flip every channel's visibility in one go — a 24-channel image would otherwise want 23 toggle
 *  clicks to hide-then-solo a marker. */
function setAllChannels(visible: boolean) {
  const m = meta.value
  if (!m) return
  for (const ch of m.channels) ch.visible = visible
  pushChannels()
}
/** True when every channel is on. False when any is off — a mixed state reads as "not all on" so
 *  the toggle's next click will flip everything ON, not OFF. Same discipline as select-all
 *  checkboxes elsewhere. */
const allChannelsVisible = computed(() =>
  (meta.value?.channels ?? []).every(ch => ch.visible))

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
  // Brick renderer only: pull the atlas snapshot so the mini map redraws from the same tick as
  // the time strip. Cheap — `brickResidency` is a `pageTable.entries()` walk plus a Set snapshot.
  const br = r?.brickResidency?.()
  if (br) {
    brickResidents.value = br.resident
    brickInflight.value = br.inflight
    brickCurrentLevel.value = br.currentLevel
    brickSizeVox.value = br.brickSizeVox
    brickDisplayValid.value = br.displayValid
    brickMissing.value = br.missing ?? 0
    brickMissingAtBoundT.value = br.missingAtBoundT ?? 0
    brickDisplayT.value = br.displayT
    brickBoundT.value = br.boundT
  }
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
    const lvl = slabLevel.value
    const expectNX = renderNX.value, expectNY = renderNY.value
    // The MASK goes with the channels, in the same round trip and into the same texture slot. Fetching
    // it separately would let the two arrive apart, and an outline over the wrong frame is worse than
    // no outline: it still looks like an answer. `vn` is read once here so a picker change mid-flight
    // cannot label this response with a different segmentation's name.
    // P7: prefer the preview's vn when a task-preview is showing labels for THIS image and the user
    // has not picked one — a first-time segmentation preview must render even without a picker entry.
    const previewMatches = !!viewerStore.previewLabels &&
      viewerStore.previewLabels?.imageUid === imageUid
    const vn = labelName.value || (previewMatches ? viewerStore.previewLabels!.valueName : '')
    const [bufs, labelBuf] = await Promise.all([
      Promise.all(Array.from({ length: nChannels.value }, async (_, c) => {
        // P7.1: when an AF preview run has this channel in its corrected set, retarget its slab onto
        // the scratch AF store — same geometry (the store is channel-less full-image), so the
        // texture and the shape guard are unchanged; only the file on disk differs. `valueName`
        // stays the SOURCE image's vn so `resolve_image_version` finds the image dir the scratch
        // sits in; `previewValueName` carries the AF task's `outputValueName` (the scratch's key).
        const afPrev = previewImageFor(c)
        const url = slabUrl({
          projectUid, imageUid, valueName: valueName.value, t: tp, c, ...zq, enc, level: lvl,
          preview_af: !!afPrev,
          sourceChannel: afPrev ? c : undefined,
          previewValueName: afPrev?.valueName,
          // Same cache-bust as the labels preview: identical (vn, t, z, preview_af=1) URL across
          // two runs would otherwise return the FIRST run's bytes from disk cache.
          previewAfId: afPrev?.updateId,
        })
        const res = await fetch(url, { cache: 'default', signal: ac.signal })
        if (!res.ok) throw new Error(`Slab ${c} failed: ${res.status}`)
        const buf = await res.arrayBuffer()
        // The guard, not a formality: a mismatched slab uploads fine and renders the wrong thing. At a
        // coarser level the server returns a smaller frame, so the shape assertion needs the LEVEL's
        // dims (`renderNX`/`renderNY`), not L0's — otherwise every coarse-level fetch fails the guard.
        const bad = slabShapeError(
          res.headers.get('X-Slab-Shape'), buf.byteLength, m, zd, m.bytesPerVoxel, expectNX, expectNY)
        if (bad) throw new Error(bad)
        serverMs = Math.max(serverMs, Number(res.headers.get('X-Server-Read-Ms')) || 0)
        return buf
      })),
      (async () => {
        if (!vn) return null
        // P7: when a task-preview is showing labels for THIS vn, flip to the scratch
        // `<vn>__preview.ome.zarr` — same reader, same headers, same shape guard, only the file on
        // disk differs. The taskPreview store clears `previewLabelsActive` on stop/error.
        const usePreview = !!viewerStore.previewLabels &&
          viewerStore.previewLabels?.valueName === vn &&
          viewerStore.previewLabels?.imageUid === imageUid
        const url = slabUrl({
          projectUid, imageUid, valueName: valueName.value, t: tp, c: 0, ...zq, enc, labels: vn, level: lvl,
          preview: usePreview,
          // Bust the browser cache when the scratch store has been rewritten — same (vn, t, z, preview=1)
          // URL across two runs would otherwise return the FIRST run's bytes from disk cache.
          previewId: usePreview ? viewerStore.previewLabels?.updateId : undefined,
        })
        const res = await fetch(url, { cache: 'default', signal: ac.signal })
        if (!res.ok) throw new Error(`Mask failed: ${res.status}`)
        const buf = await res.arrayBuffer()
        // Same geometry as the image, its OWN dtype — so the guard is asked at the mask's width, which
        // the server reports. A store narrower than UInt32 is widened rather than refused: at half the
        // width it would render as a plausible mask of something else.
        const bpv = labelBpv(res.headers.get('X-Slab-Bpv'))
        const bad = slabShapeError(
          res.headers.get('X-Slab-Shape'), buf.byteLength, m, zd, bpv, expectNX, expectNY)
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
      autoWin.value = bufs.map(b => contrastFromSlab(slabView(b, m.bytesPerVoxel), m.nX))
    }
    seenMax.value = bufs.map((b, c) =>
      Math.max(seenMax.value[c] ?? 0, slabMax(slabView(b, m.bytesPerVoxel), m.nX)))

    const t1 = performance.now()
    await r.uploadFrame(tp, bufs, t.value, labelBuf)
    timing.value = {
      fetchMs: Math.round(fetchMs), uploadMs: Math.round(performance.now() - t1), serverMs,
    }
    lastMissMs.value = Math.round(performance.now() - missStart)
    return true
  })().catch((e: unknown) => {
    // An abort is the normal outcome for a prefetch the user scrubbed away from — not an error to show.
    if (e instanceof DOMException && e.name === 'AbortError') return false
    error.value = e instanceof Error ? e.message : String(e)
    vlog('error', 'Viewer timepoint ' + tp + ': ' + error.value)
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
    // No `shownT !== u` guard: after `r.setZPlane` bumps `planeVersion`, every slot is stale so
    // showT(t.value) failed (shownT stayed on the OLD-plane t.value), and skipping here left the
    // new bytes bound but unpainted — the z slider looked dead until the user nudged t (Dominik
    // 2026-08-31). showT is idempotent for an already-bound slot, and this line only runs when
    // u === t.value, so playback's per-tick paint isn't disturbed.
    if (u === t.value) showT(u)
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
 *  a request is collapsed and serialised.
 *
 *  Tile mode is different: there is no per-timepoint texture to bind, because the atlas holds
 *  mixed-t tiles and `drawTiles` filters resident tiles by `t.value`. So `gotoT` just moves `t`,
 *  blanks `shownT` (so the still-overlay chip reappears while the current-t tiles come in), and
 *  schedules the tile pump — which fetches for the new `t` and aborts stale-t fetches via
 *  `evictionKeepSet` (Phase F, decision 6). */
function gotoT(tp: number) {
  if (useTiles.value) {
    t.value = tp
    shownT.value = -1
    scheduleTilePump()
    frame.redraw()
    return
  }
  showT(tp)
  schedulePump(tp)
  // Brick renderer only: hint the playback window so the tick loop prefetches upcoming
  // timepoints. The flat renderer's `pump` already prefetches via `uploadFrame`; the brick
  // renderer streams per-viewport per-t, so the hint lives here rather than inside the pump.
  const r = renderer.value; const m = meta.value
  if (r?.setPrefetchTimepoints && m) {
    const dir = Math.sign(tp - lastT) || 1
    // Small depth — atlas has plenty of room, but every extra `t` fires 16-64 fetches. 4 covers
    // half a second of 8fps playback and lands cheaply; sane default until we measure.
    const cap = playing.value ? 4 : 1
    r.setPrefetchTimepoints(prefetchWindow(tp, dir, m.nT, cap))
  }
}

// ── Tile mode: per-viewport fetching with a halo prefetch ────────────────────────
//
// The 2D whole-slide code path (Phase C). Parallel to the timepoint pump above — visible tiles
// first, then a ring around them so a small pan is instant — one request at a time through the same
// `debouncedLatest` scheduler that runs the timepoint pump, so a fast wheel gesture across level
// thresholds cannot pile up requests. Same reason the timepoint pump exists.
//
// Load discipline mirrors the timepoint work: the visible tiles LOAD first (row-major inside the
// viewport), then one halo ring outward at Chebyshev distance 1. The atlas holds all channels of one
// tile atomically — a channel toggle costs zero and neither redraws nor refetches — so a fetch turns
// into one HTTP per channel and one `uploadTile(key, channelBytes[])` per tile.

/** One outward ring of tiles beyond the viewport is prefetched. Two rings quadruples the fetch
 *  count for a marginal gain — an outward pan is asymmetric like a scrub, but pans in every
 *  direction are equally likely so there is no direction to bias for. Halo 1 turns a small pan into
 *  a paint of resident bytes; anything more is a bet on the user's next gesture. */
const HALO_RINGS = 1

/**
 * Aborts for tile fetches in flight, keyed by tile-key string. A zoom/pan mid-fetch cancels tiles the
 * new viewport has no use for. Separate from `aborts` so the timepoint path is untouched.
 */
const tileAborts = new Map<string, AbortController>()

/**
 * Viewport in L0 pixels — the input `tilesInHalo` and `viewportTiles` take. Derived from the camera,
 * the canvas size and the image's µm-per-voxel. Null when the layout has not settled — asking the
 * server for L0 of a 20k×17k image before the client knows its own size is exactly the 687 MB request
 * this exists to avoid.
 */
function computeViewportL0(): ViewportL0 | null {
  const m = meta.value
  const el = canvas.value
  if (!m || !el || el.clientHeight <= 0 || el.clientWidth <= 0) return null
  const asp = canvasAspect()
  const halfHUm = cam.value.dist * VIEW_HALF_ANGLE
  const halfWUm = halfHUm * asp
  const vx = m.voxelUm[0] || 1
  const vy = m.voxelUm[1] || 1
  // Image origin (top-left) sits at (-ex/2, -ey/2) in world µm (matches the volume renderer's box).
  // The camera centre in image µm is (panX + ex/2, -panY + ey/2) — screen up is -y world, so a
  // positive panY points the camera at a SMALLER image_y (i.e. higher rows).
  const ex = m.nX * vx
  const ey = m.nY * vy
  const cxImg = cam.value.panX + ex / 2
  const cyImg = -cam.value.panY + ey / 2
  return {
    x0: Math.floor((cxImg - halfWUm) / vx),
    y0: Math.floor((cyImg - halfHUm) / vy),
    x1: Math.ceil((cxImg + halfWUm) / vx),
    y1: Math.ceil((cyImg + halfHUm) / vy),
  }
}

/** Tiles the tile renderer needs BUT DOES NOT YET HAVE, in fetch order — visible first, then halo.
 *  Keys bind `t.value` — a scrub past a resident (wrong-t) tile does NOT count as a hit; the pump
 *  fetches the current-t version and the ranker keeps the near-t entry around for a scrub back
 *  (see `docs/todo/VIEWER_TILES_PLAN.md` Phase F). */
function missingTiles(): TileKey[] {
  const tr = tileRenderer.value, m = meta.value
  if (!tr || !m) return []
  const level = slabLevel.value
  const lvl = levelMeta(m, level)
  const vp = computeViewportL0()
  if (!lvl || !vp) return []
  const coords = tilesInHalo(vp, level, lvl, HALO_RINGS)
  const tp = t.value
  const zp = zPlane.value
  const out: TileKey[] = []
  for (const [tx, ty] of coords) {
    const key: TileKey = { t: tp, z: zp, level, tx, ty }
    if (tr.hasTile(key)) { tr.touchTile(key); continue }
    out.push(key)
  }
  return out
}

/** The KEEP set for `tileEvictions` — every tile the current viewport wants right now, so an eviction
 *  round cannot take a tile that is either on screen or being drawn NEXT. Both, because during a pan
 *  those two disagree — the same lesson `lruEvictions` learned in the timecourse work. Current-t only
 *  by design (Phase F, decision 4 — no cross-t halo in MVP). */
function evictionKeepSet(): Set<string> {
  const tr = tileRenderer.value, m = meta.value
  if (!tr || !m) return new Set()
  const level = slabLevel.value
  const lvl = levelMeta(m, level)
  const vp = computeViewportL0()
  if (!lvl || !vp) return new Set()
  const coords = tilesInHalo(vp, level, lvl, HALO_RINGS)
  const tp = t.value
  const zp = zPlane.value
  return new Set(coords.map(([tx, ty]) => tileKeyStr({ t: tp, z: zp, level, tx, ty })))
}

/** Fetch one tile's channels in parallel and hand them to the atlas. Aborted by `tileAborts` when the
 *  viewport moves; a re-scheduled pump then queues the same tile again if it is still needed. */
async function fetchTile(key: TileKey): Promise<boolean> {
  const tr = tileRenderer.value, m = meta.value
  if (!tr || !m) return false
  const lvl = levelMeta(m, key.level)
  if (!lvl) return false
  const kStr = tileKeyStr(key)
  const existing = tileAborts.get(kStr)
  if (existing) return false                 // already in flight; the pump joins rather than races
  const ac = new AbortController()
  tileAborts.set(kStr, ac)
  // Sync AT fetch start (not just at fetch end) so the mini tile-map's amber cells appear the moment
  // a request goes out, not after it lands. Cheap: one shallow copy of a bounded key set.
  syncTileCacheState()
  const enc = settings.viewerCompress ? 'zstd' : 'identity'
  const nch = Math.min(m.nC, MAX_CHANNELS)
  const rect = tileFetchRect(key.tx, key.ty, lvl)
  try {
    // Channels in parallel — same shape as `fetchTimepoint`, and for the same reason: independent
    // reads on the server's thread pool. One HTTP per channel, one contiguous slab per response.
    const bufs = await Promise.all(Array.from({ length: nch }, async (_, c) => {
      const afPrev = previewImageFor(c)
      const url = slabUrl({
        projectUid, imageUid, valueName: valueName.value, t: key.t, c, enc, level: key.level,
        // 2D view is one plane; server drops z from the response. Follows the plane control (only
        // reachable at nZ > 1). `t` comes from the tile key so a scrub past a resident tile fetches
        // the current-t version — the atlas caches across timepoints (Phase F).
        z: zPlane.value,
        x: rect.x, xTo: rect.xTo, y: rect.y, yTo: rect.yTo,
        // P7.1: same AF preview swap as the volume path — one entry per corrected channel, keyed on
        // the source channel index.
        preview_af: !!afPrev,
        sourceChannel: afPrev ? c : undefined,
        previewValueName: afPrev?.valueName,
        previewAfId: afPrev?.updateId,
      })
      const res = await fetch(url, { cache: 'default', signal: ac.signal })
      if (!res.ok) throw new Error(`Tile L${key.level} (${key.tx},${key.ty}) c${c}: ${res.status}`)
      const buf = await res.arrayBuffer()
      return buf
    }))
    // Grow `seenMax` from the tile's own bytes — same discipline as `fetchTimepoint`. Without this
    // the contrast slider's ceiling fell back to `Math.max(ch.hi, 1)`, so dragging `hi` down shrank
    // the slider until it collapsed to 0-1 with no way back (Dominik, 2026-08-26). Only ever grows —
    // a ceiling that shrinks re-scales the slider under a value the user set.
    const rowLen = rect.xTo - rect.x + 1
    seenMax.value = bufs.map((b, c) =>
      Math.max(seenMax.value[c] ?? 0, slabMax(slabView(b, m.bytesPerVoxel), rowLen)))
    // Seed the auto-window off the first tile that lands — same p01/p999 percentile pass as the
    // volume path uses (`contrastFromSlab`). Once, held: recomputed per tile the window would chase
    // each tile's own distribution and the auto button would land somewhere new every time.
    if (autoWin.value.length === 0) {
      autoWin.value = bufs.map(b => contrastFromSlab(slabView(b, m.bytesPerVoxel), rowLen))
    }
    // Compute the evict list right BEFORE upload, not when the fetch started: the viewport may have
    // moved during the fetch, so the RIGHT tiles to evict now are different from the ones to evict
    // then. The ranker penalises cross-level distance too, so a stale coarser-level tile is dropped
    // before a fresh finer-level neighbour.
    //
    // `slotCapacity - 1` — NOT `slotCapacity` — so the ranker leaves one slot free for the tile we
    // are about to upload. Passing the full capacity meant `tileEvictions` returned nothing whenever
    // the atlas was full (its threshold is `entries.length <= capacity`); `uploadTile` then found no
    // free slot and returned -1, and the ring of tiles the new zoom needed never landed — the "right
    // half never resolves" case Dominik saw at L1 on f8gzA2 (2026-08-26).
    const vp = computeViewportL0()
    const centre = vp && lvl ? viewportCentreTile(vp, key.level, lvl) : { tx: key.tx, ty: key.ty }
    const keep = evictionKeepSet()
    const evictions = tileEvictions(
      tr.residentTiles(), Math.max(1, tr.slotCapacity() - 1), keep,
      { t: key.t, z: key.z, level: key.level, tx: centre.tx, ty: centre.ty },
    )
    const slot = await tr.uploadTile(key, bufs, keep, evictions)
    return slot >= 0
  } catch (e) {
    if (e instanceof DOMException && e.name === 'AbortError') return false
    error.value = e instanceof Error ? e.message : String(e)
    vlog('error', 'Viewer tile ' + kStr + ': ' + error.value)
    return false
  } finally {
    tileAborts.delete(kStr)
    syncTileCacheState()
  }
}

/** Per-tile cache readout for Debug + the mini tile map (the spatial analog of the timecourse strip).
 *  `tileResidents` is a snapshot of the atlas entries — the mini map filters it by `t + slabLevel` at
 *  paint time. `tileLoadingKeys` is a snapshot of the in-flight abort map, used the same way.
 *  Snapshotting rather than sharing the live structures so Vue reactivity fires on set. */
const tileResidentCount = ref(0)
const tileSlotCap = ref(0)
const tileInflight = ref(0)
const tileResidents = shallowRef<{ t: number; z: number; level: number; tx: number; ty: number }[]>([])
const tileLoadingKeys = shallowRef<Set<string>>(new Set())
function syncTileCacheState() {
  const tr = tileRenderer.value
  const res = tr?.residentTiles() ?? []
  tileResidentCount.value = res.length
  tileSlotCap.value = tr?.slotCapacity() ?? 0
  tileInflight.value = tileAborts.size
  tileResidents.value = res.map(e => ({ t: e.t, z: e.z, level: e.level, tx: e.tx, ty: e.ty }))
  tileLoadingKeys.value = new Set(tileAborts.keys())
}

/** Brick residency grid — one nBx × nBy panel per Z slice at the CURRENT level + `boundT` (via
 *  `shownT.value` which tracks the frame actually painted). Null when brick mode is off, the meta
 *  hasn't loaded, or the atlas hasn't ticked at least once. Template checks with `v-if`.
 *
 *  Grid dims are computed here from meta + `brickSizeVox` + `2^level` rather than plumbed back
 *  from the renderer, so a level swap doesn't need a second round trip. */
const brickMapGrid = computed(() => {
  const m = meta.value
  const lvl = brickCurrentLevel.value
  if (!bricksEnabled.value || !m || lvl === undefined) return null
  const [bx, by, bz] = brickSizeVox.value
  const scale = Math.pow(2, lvl)
  const zd = mode.value === 'plane' ? 1 : zDepth.value
  const nBx = Math.max(1, Math.ceil(m.nX / (bx * scale)))
  const nBy = Math.max(1, Math.ceil(m.nY / (by * scale)))
  const nBz = Math.max(1, Math.ceil(zd / (bz * scale)))
  return { nBx, nBy, nBz, level: lvl }
})

/** Cap on the mini-map's DISPLAY grid dimension per axis. At L0 on SispLk-shape (62×57) or
 *  f8gzA2-shape (159×132), an unaggregated map is thousands of ~1px cells — unreadable
 *  (Dominik, 2026-08-29: "way too fine grained ... a bit more coarser when there are a ton of
 *  brick to load"). Above this, cells aggregate `bucket × bucket` real bricks into one visual
 *  cell. `resident` if any inside are resident, `loading` if any loading and none resident,
 *  `absent` if all absent — biases toward "loading progress visible", same convention as the
 *  tile map. */
const BRICKMAP_MAX_CELLS_PER_AXIS = 8

/** Per-Z-slice residency cells at the current level + target `t`. One entry per slice; each
 *  entry is a row-major grid, aggregated to `BRICKMAP_MAX_CELLS_PER_AXIS` per axis when the
 *  underlying brick grid exceeds it. Also carries the display dims so the template's CSS grid
 *  matches. */
const brickMapSlices = computed(() => {
  const g = brickMapGrid.value
  if (!g) return { displayNBx: 0, displayNBy: 0, slices: [] as { z: number; cells: { key: string; state: 'absent' | 'loading' | 'resident' }[] }[] }
  // Filter by the TARGET t (`t.value`), same convention the tile map uses. The map is a
  // loading-progress indicator: what the user wants to see is bricks fetching toward the t
  // they just scrubbed to, not what the shader is still drawing. Filtering by `displayT`
  // (the timepoint the shader currently paints) made progress invisible during scrub-past-cold
  // — `displayT` stays on the OLD t until enough of the NEW t lands, so the map read "all
  // resident" while fetches were firing for boundT (Dominik, 2026-08-29: "the map only shows
  // loading progress on initial image load and never after").
  const tp = t.value
  const residentAtHere = new Set<string>()
  for (const e of brickResidents.value) {
    if (e.t === tp && e.level === g.level) residentAtHere.add(`${e.bx},${e.by},${e.bz}`)
  }
  const loadingAtHere = new Set<string>()
  for (const e of brickInflight.value) {
    if (e.t === tp && e.level === g.level) loadingAtHere.add(`${e.bx},${e.by},${e.bz}`)
  }
  // Aggregation: each display cell covers `bucketX × bucketY` real bricks. `ceil` so no brick
  // is uncounted. When the grid already fits, bucket is 1 and this is a straight passthrough.
  const bucketX = Math.max(1, Math.ceil(g.nBx / BRICKMAP_MAX_CELLS_PER_AXIS))
  const bucketY = Math.max(1, Math.ceil(g.nBy / BRICKMAP_MAX_CELLS_PER_AXIS))
  const displayNBx = Math.max(1, Math.ceil(g.nBx / bucketX))
  const displayNBy = Math.max(1, Math.ceil(g.nBy / bucketY))
  const slices: { z: number; cells: { key: string; state: 'absent' | 'loading' | 'resident' }[] }[] = []
  for (let bz = 0; bz < g.nBz; bz++) {
    const cells: { key: string; state: 'absent' | 'loading' | 'resident' }[] = []
    for (let dy = 0; dy < displayNBy; dy++) {
      for (let dx = 0; dx < displayNBx; dx++) {
        let anyResident = false, anyLoading = false
        const byLo = dy * bucketY, byHi = Math.min(g.nBy, byLo + bucketY)
        const bxLo = dx * bucketX, bxHi = Math.min(g.nBx, bxLo + bucketX)
        for (let by = byLo; by < byHi && !anyResident; by++) {
          for (let bx = bxLo; bx < bxHi; bx++) {
            const k = `${bx},${by},${bz}`
            if (residentAtHere.has(k)) { anyResident = true; break }
            if (loadingAtHere.has(k)) anyLoading = true
          }
        }
        const state: 'absent' | 'loading' | 'resident' = anyResident ? 'resident'
          : anyLoading ? 'loading' : 'absent'
        cells.push({ key: `${dx},${dy},${bz}`, state })
      }
    }
    slices.push({ z: bz, cells })
  }
  return { displayNBx, displayNBy, slices }
})

/** Grid dims of the current level — drives the mini tile map's aspect + cell count. Null when tile
 *  mode is off or the meta hasn't loaded yet, which the template checks with `v-if`. */
const tileMapGrid = computed(() => {
  const m = meta.value
  if (!m || !useTiles.value) return null
  const lvl = levelMeta(m, slabLevel.value)
  if (!lvl) return null
  const { nTx, nTy } = tileGridDims(lvl)
  if (nTx <= 0 || nTy <= 0) return null
  return { nTx, nTy, level: slabLevel.value }
})

/** Per-cell state at the current level + timepoint — one entry per tile in row-major order. Filters
 *  the resident and loading snapshots by `t + level` inline (cheaper than computing a Set of
 *  strings per cell). */
const tileMapCellsView = computed(() => {
  const g = tileMapGrid.value
  if (!g) return []
  const tp = t.value, zp = zPlane.value, lv = g.level
  const residentAtHere = new Set<string>()
  for (const e of tileResidents.value) {
    if (e.t === tp && e.z === zp && e.level === lv) residentAtHere.add(`${e.tx},${e.ty}`)
  }
  const loading = tileLoadingKeys.value
  const cells: { key: string; state: 'absent' | 'loading' | 'resident' }[] = []
  for (let ty = 0; ty < g.nTy; ty++) {
    for (let tx = 0; tx < g.nTx; tx++) {
      const kL = tileKeyStr({ t: tp, z: zp, level: lv, tx, ty })
      const state = loading.has(kL) ? 'loading'
        : residentAtHere.has(`${tx},${ty}`) ? 'resident' : 'absent'
      cells.push({ key: `${tx},${ty}`, state })
    }
  }
  return cells
})

/**
 * Fill the atlas around the current viewport — visible + halo, ALL IN PARALLEL. The browser's per-
 * origin concurrency cap (typically 6) throttles the HTTP fanout naturally, and tiles arriving in
 * different orders is not a correctness issue — the atlas caches by key. Serialising was a mistake
 * for the whole-slide case: 12 halo tiles × ~200 ms is ~2.4 s instead of ~500 ms, and Dominik
 * called it out ("takes ages to load. this should take a few seconds"). A stale pump's fetches are
 * aborted by `scheduleTilePump` via `tileAborts` — the abort has to happen at the mid-flight fetch,
 * not at a serial checkpoint, and this shape makes that its ONLY point of cancellation.
 */
const tilePump = debouncedLatest<null>(async (_, isCurrent) => {
  const tr = tileRenderer.value
  if (!tr) return
  const want = missingTiles()
  await Promise.allSettled(want.map(async key => {
    if (!isCurrent()) return
    if (tr.hasTile(key)) { tr.touchTile(key); return }
    const ok = await fetchTile(key)
    if (!ok) return
    // NO `isCurrent()` guard here — the tile is in the atlas either way, and painting it against
    // the CURRENT viewport is always correct (drawTiles filters by residency). Previously the
    // `drawTiles → scheduleTilePump` rescheduler made the pump supersede itself: `isCurrent` went
    // false mid-run, so tiles that landed after did NOT flip `shownT` and did NOT call
    // `frame.redraw`. Result: the chip stayed on "Loading image…" while the atlas was full, and
    // the canvas was blank until the user panned (Dominik, 2026-08-26). The gate came from the
    // timepoint pump where painting the wrong frame matters — here it never matters.
    // Flip on the first CURRENT-t tile to land — a stale-t fetch racing in after the user scrubbed
    // must not clear the loading chip. `t.value` at fetch time was baked into `key.t`; if they still
    // agree, the tile is fresh for what the user is looking at (Phase F, decision 6).
    if (key.t === t.value && shownT.value !== key.t) shownT.value = key.t
    frame.redraw()
  }))
}, { wait: 0, onError: e => { error.value = e instanceof Error ? e.message : String(e) } })

/**
 * Cancel tile fetches the new viewport has no use for, then reschedule the pump. Same shape as
 * `schedulePump` — the abort has to happen HERE and not inside the walk, because the walk is
 * `await`-ing the fetch by the time it reaches its next checkpoint.
 */
function scheduleTilePump() {
  const keep = evictionKeepSet()
  for (const [k, ac] of [...tileAborts]) if (!keep.has(k)) ac.abort()
  tilePump.schedule(null)
}

/**
 * Draw whatever tiles are resident, at the current camera. Never blanks — a level swap keeps the old
 * level's tiles resident while the new level's tiles stream in, so the frame is progressive rather
 * than empty-then-fresh. (The current MVP flushes the atlas on level swap, so this is aspirational
 * until Phase D — but the draw call already accepts a mixed-level list.)
 */
function drawTiles() {
  const tr = tileRenderer.value, m = meta.value
  if (!tr || !m) return
  const resized = tr.resize()
  seen.value = visibleExtentUm(cam.value.dist, canvasAspect())
  tr.setCamera(cam.value.panX, cam.value.panY, cam.value.dist)
  // A canvas resize changes what fits in the viewport — new edge tiles come into view without any
  // pointer input. Same trigger for both dimensions of size change.
  if (resized) scheduleTilePump()
  // Anything still missing → schedule the pump. Covers the initial-mount race where the first
  // `reallocate` fired the pump before the canvas had CSS layout (viewport was null, so nothing
  // was fetched) — after which the atlas stayed empty until the user moved the mouse (Dominik,
  // 2026-08-26). Debounced through `tilePump`, so a redraw per pointer notch does not spam.
  if (missingTiles().length > 0) scheduleTilePump()
  // Coarsest-first so finer tiles overpaint the coarse ones as they arrive — the whole point of
  // keeping cross-level tiles resident across a zoom threshold. The tile shader is opaque, so
  // "under" here is literal draw order: the coarse layer only shows where finer tiles have not yet
  // landed, and vanishes seamlessly the moment they do.
  //
  // Filter to CURRENT-(t, z) entries: the atlas caches across timepoints (Phase F) AND across
  // planes (SispLk/35uedD, 2026-08-27) so the ranker can prefer near-(t,z) tiles on a scrub back,
  // but painting a stale-t or stale-z tile would show wrong content. Without the z filter, changing
  // z past a tile that had already been swapped once left two co-located instances at the same
  // level in the draw list, and the second z-swap became visually a no-op because the underlying
  // instanced draw order was undefined between them.
  const tp = t.value
  const zp = zPlane.value
  const entries = tr.residentTiles().filter(e => e.t === tp && e.z === zp).slice()
    .sort((a, b) => b.level - a.level)
  const draws: TileDraw[] = []
  const vx = m.voxelUm[0] || 1
  const vy = m.voxelUm[1] || 1
  const ex = m.nX * vx
  const ey = m.nY * vy
  for (const e of entries) {
    const lvl = levelMeta(m, e.level)
    if (!lvl) continue
    const rect = tileFetchRect(e.tx, e.ty, lvl)
    const w = rect.xTo - rect.x + 1
    const h = rect.yTo - rect.y + 1
    // Level → L0 pixel scale is 2^level (clean-2× pyramid, same assumption `pickTileLevel` makes).
    const scale = 1 << Math.max(0, e.level)
    const l0X = rect.x * scale
    const l0Y = rect.y * scale
    const l0W = w * scale
    const l0H = h * scale
    draws.push({
      slot: e.slot,
      worldX: l0X * vx - ex / 2,
      worldY: l0Y * vy - ey / 2,
      worldW: l0W * vx,
      worldH: l0H * vy,
      sampledX: w,
      sampledY: h,
    })
  }
  tr.draw(draws)
  // The still overlay's `shownT >= 0` gate flips on when the first tile lands (`tilePump` handler).
  if (announce.value && shownT.value >= 0) {
    announce.value = false
    vlog('info',
         `Viewer drawing ${m.nX}×${m.nY} plane (tile mode)`,
         `${entries.length} tile(s) resident of ${tileSlotCap.value} slots · ` +
         `L${slabLevel.value} · pan ${cam.value.panX.toFixed(0)},${cam.value.panY.toFixed(0)} · ` +
         `dist ${cam.value.dist.toFixed(0)} µm`)
  }
  if (shownT.value >= 0) clearViewerAttempt()
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
    if (useTiles.value) {
      // Tile-mode playback: advance at the requested fps regardless of tile residency. The tile
      // pump chases behind `gotoT` — the loading chip appears while a fresh-t frame's tiles arrive
      // and clears the moment they do. Fetch-vs-frame-rate discipline (volume mode's residency
      // probe + wait-for-cached-frame path) needs measurement on real timelapses before it lands
      // here — see `docs/todo/VIEWER_TILES_PLAN.md` Phase F, decision "autoplay does NOT ship".
      const n = nT.value
      if (n <= 0) { stopPlay(); return }
      const next = t.value + 1
      if (next >= n) {
        if (!settings.viewerLoop) { stopPlay(); return }
        waitingFor.value = -1
        gotoT(0)
      } else {
        waitingFor.value = -1
        gotoT(next)
      }
      tick()
      return
    }
    const r = renderer.value
    // Brick renderer: advance every tick regardless of residency, so show(t) fires its
    // snap-to-boundT and the shader draws each frame with prev-t hole-fill for whatever hasn't
    // landed. Flat renderer: gate on cache residency (its frames are all-or-nothing).
    const readyProbe = bricksEnabled.value
      ? () => true
      : (u: number) => r?.hasTimepoint(u) ?? false
    const step = playbackAdvance(t.value, nT.value, settings.viewerLoop, readyProbe)
    if (step.ended) { stopPlay(); return }
    if (step.stalled) {
      // Pump around the frame we WANT, not the one we are on. At the end of a loop those are the one
      // pair that disagree, and a window centred on where we are fills backwards and never asks for
      // frame 0 — playback then waits forever for something nothing is fetching.
      waitingFor.value = step.next
      schedulePump(step.next)
      // Brick renderer only: schedulePump is a flat-renderer path — nothing on the brick side
      // hears it. Push step.next into the prefetch window so the brick tickScheduler starts
      // fetching for it now, or a stall never unstalls (nothing else is telling the scheduler
      // this t matters). Under playback cap=4 so the window covers the direction of travel.
      const m = meta.value
      if (r?.setPrefetchTimepoints && m) {
        const dir = Math.sign(step.next - t.value) || 1
        r.setPrefetchTimepoints(prefetchWindow(step.next, dir, m.nT, 4))
      }
      frame.redraw()
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
/**
 * Drag ROTATES in 3D and PANS in 2D, and shift always pans.
 *
 * Rotation in the plane view is not a lesser feature, it is a wrong one: the 2D view is one z plane
 * seen face-on under an orthographic projection, and tilting it shows that plane edge-on — a black
 * frame, from a control that looks like it should work. So the plane view spends its drag on the thing
 * it does have, which is where in the image you are looking. Without a pan there was no way to move
 * around at all once zoomed in (Dominik, 2026-08-25).
 */
const pans = (e: PointerEvent | MouseEvent) => mode.value === 'plane' || e.shiftKey
// `dragFrom` is the pointer's LAST position (updated per move so `onMove` computes per-event delta).
// `dragStart` is where the gesture began — untouched by move, used by `onUp` to decide click vs drag.
// The two are separate because collapsing them would need `onMove` to re-derive the total from a
// running sum, and a pan that started with a tap would then never register as a click.
let dragFrom:  { x: number; y: number } | null = null
let dragStart: { x: number; y: number } | null = null
/** Pointer-travel threshold below which a mouseup is a CLICK (px, cursor space). Tuned to match
 *  the OS's own drag-vs-click deadband — cursors jitter a pixel or two on click. */
const CLICK_MAX_TRAVEL_PX = 4
/** Visible rectangle overlay while a select-mode drag is in flight, in CANVAS pixel space.
 *  Null while nothing is being drawn; set on the first move that crosses the click deadband. */
const dragRect = ref<{ x: number; y: number; w: number; h: number } | null>(null)
function onDown(e: PointerEvent) {
  dragFrom  = { x: e.clientX, y: e.clientY }
  dragStart = { x: e.clientX, y: e.clientY }
  ;(e.target as HTMLElement).setPointerCapture?.(e.pointerId)
}
function onMove(e: PointerEvent) {
  if (!dragFrom || !canvas.value) return
  const dx = e.clientX - dragFrom.x, dy = e.clientY - dragFrom.y
  dragFrom = { x: e.clientX, y: e.clientY }
  // Select-mode + plain drag = RECTANGLE selection. Shift+drag stays a pan (the escape hatch when
  // you need to reposition while a selection tool is active). Anything else = the old pan / rotate
  // path. Two separate gestures on the same button, disambiguated by the mode and the modifier.
  if (selectModeActive.value && mode.value === 'plane' && !e.shiftKey && dragStart && canvas.value) {
    const rect = canvas.value.getBoundingClientRect()
    const x0 = dragStart.x - rect.left, y0 = dragStart.y - rect.top
    const x1 = e.clientX   - rect.left, y1 = e.clientY   - rect.top
    dragRect.value = { x: Math.min(x0, x1), y: Math.min(y0, y1),
                       w: Math.abs(x1 - x0), h: Math.abs(y1 - y0) }
    return
  }
  cam.value = pans(e)
    ? panDrag(cam.value, dx, dy, canvas.value.clientHeight)
    : orbitDrag(cam.value, dx, dy, canvas.value.clientWidth)
  frame.redraw()
  // Pan in tile mode changes what tiles the viewport needs — schedule the fetch. The frame just
  // painted uses whatever is already resident (which is why the pan feels instant); missing tiles
  // stream in behind it. `scheduleTilePump` cancels fetches the new viewport does not want first.
  if (useTiles.value) scheduleTilePump()
}
function onUp(e: PointerEvent) {
  // A click is a mouseup that did not travel — the pick fires here rather than on `onDown` so a
  // pan gesture that started with the same button is not misread as a pick. `dragStart` was
  // captured on `onDown` in canvas space; a small travel (see `CLICK_MAX_TRAVEL_PX`) IS a click.
  const start = dragStart
  dragFrom = null; dragStart = null
  if (!start) return
  const dx = e.clientX - start.x, dy = e.clientY - start.y
  const travelled = Math.hypot(dx, dy)
  // Two release paths in SELECT mode: a small-travel release is a CLICK → single-cell pick,
  // anything larger with a live `dragRect` is a RECTANGLE release → all labels in the rect.
  // Both route to the same endpoint (single-cell as a rect degenerates to one voxel); keeping
  // them separate here means a jittery click cannot accidentally become an empty rect fetch.
  const rect = dragRect.value
  dragRect.value = null
  if (mode.value !== 'plane' || !selectModeActive.value) return
  const pickMode: 'replace' | 'add' | 'toggle' =
    e.altKey ? 'toggle' : e.shiftKey ? 'add' : 'replace'
  if (travelled <= CLICK_MAX_TRAVEL_PX) {
    void pickCellAt(e, pickMode)
  } else if (rect && rect.w >= 4 && rect.h >= 4) {
    void pickRectAt(rect, pickMode)
  }
}

/**
 * Send a pick request to the server for the pixel under the pointer. The gating store's `popmap`
 * broadcast lights up the transient pop on the plots — this window never touches the gating store
 * directly. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md → P8.
 */
async function pickCellAt(e: PointerEvent, pickMode: 'replace' | 'add' | 'toggle' = 'replace') {
  const c = canvas.value, m = meta.value
  if (!c || !m) return
  const rect = c.getBoundingClientRect()
  const cx = e.clientX - rect.left
  const cy = e.clientY - rect.top
  const lvl = slabLevel.value
  const p = screenToImagePx(cx, cy, c.clientWidth, c.clientHeight, cam.value, m,
                            renderNX.value, renderNY.value)
  if (!p.in) return   // black margin around a zoomed-out image — nothing to pick
  const gc = gatingCurrent.value
  // `valueName` follows the pop manager's active segmentation — pick and plot must read the same
  // label store, else two different label number spaces yield unrelated cells on the plot.
  // `level` matches the display's LOD so nearest-neighbour label downsampling doesn't pick a
  // neighbour of the visible cell.
  const body = {
    projectUid, imageUid,
    valueName: gc.valueName || undefined,
    popType:   gc.popType   || 'flow',
    t: Math.max(0, Math.round(t.value)),
    z: Math.max(0, Math.min(m.nZ - 1, Math.round(zPlane.value))),
    x: p.x, y: p.y,
    level: lvl,
    mode: pickMode,
  }
  try {
    const res = await fetch('/api/viewer/pick-cell', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    })
    if (!res.ok) { vlog('warn', `Pick failed: ${res.status}`); return }
    // Nothing to do on the response — the popmap broadcast updates the plots. Log a background
    // click quietly so the user can tell the click landed off any cell.
    const j = await res.json() as { label?: number; nSelected?: number }
    if (!j.label) vlog('info', 'Pick: background (no cell)')
  } catch (err) {
    vlog('warn', 'Pick error: ' + (err instanceof Error ? err.message : String(err)))
  }
}

/**
 * Rectangle-drag pick: POST /api/viewer/pick-rect with the image-pixel rect the user drew.
 * `rect` is in CANVAS pixel space (from `onMove`); converted to image px through the same
 * `screenToImagePx` the click path uses so a rect and a click on the same cell agree. The server
 * reads the mask over the rect at the current (t, z) and extracts unique labels.
 */
async function pickRectAt(rect: { x: number; y: number; w: number; h: number },
                          pickMode: 'replace' | 'add' | 'toggle' = 'replace') {
  const c = canvas.value, m = meta.value
  if (!c || !m) return
  const lvl = slabLevel.value
  const nx = renderNX.value, ny = renderNY.value
  const p1 = screenToImagePx(rect.x,          rect.y,          c.clientWidth, c.clientHeight, cam.value, m, nx, ny)
  const p2 = screenToImagePx(rect.x + rect.w, rect.y + rect.h, c.clientWidth, c.clientHeight, cam.value, m, nx, ny)
  // Clamp inside the image and normalise order — a rect drawn from lower-right to upper-left
  // arrives with p2 < p1, and the server expects the low/high pair. Bail on an empty rect after
  // clamping (a drag entirely on the black margin around a zoomed-out image).
  const cl = (v: number, hi: number) => Math.max(0, Math.min(hi - 1, v))
  const x1 = Math.min(cl(p1.x, nx), cl(p2.x, nx))
  const x2 = Math.max(cl(p1.x, nx), cl(p2.x, nx))
  const y1 = Math.min(cl(p1.y, ny), cl(p2.y, ny))
  const y2 = Math.max(cl(p1.y, ny), cl(p2.y, ny))
  if (x1 === x2 && y1 === y2) return
  const gc = gatingCurrent.value
  const zc = Math.max(0, Math.min(m.nZ - 1, Math.round(zPlane.value)))
  // Z scope for the rect: the gating store publishes `cc.pickZScope = {mode, window}`; 'slice'
  // adds `zLo`/`zHi` to the POST so the reader spans that inclusive range instead of just `z`.
  // 'stack' or an absent bag falls through to the single-plane read. Read on demand — the scope
  // rarely changes during a single drag, and a per-frame subscription for a value we only need at
  // release is churn we don't need. See `CellSelectionTools.vue`.
  let zLo: number | undefined
  let zHi: number | undefined
  if (typeof localStorage !== 'undefined') {
    try {
      const s = JSON.parse(localStorage.getItem('cc.pickZScope') ?? '{}') as
                { mode?: string; window?: number }
      if (s.mode === 'slice') {
        const w = Math.max(0, Math.floor(Number(s.window) || 0))
        zLo = Math.max(0, zc - w)
        zHi = Math.min(m.nZ - 1, zc + w)
      }
    } catch { /* garbage bag → single-plane read */ }
  }
  // `valueName` = the pop manager's seg (which IS the plot's seg) — see the note in `pickCellAt`.
  const body = {
    projectUid, imageUid,
    valueName: gc.valueName || undefined,
    popType:   gc.popType   || 'flow',
    t: Math.max(0, Math.round(t.value)),
    z: zc,
    x1, y1, x2, y2,
    level: lvl,
    mode: pickMode,
    ...(zLo !== undefined && zHi !== undefined ? { zLo, zHi } : {}),
  }
  try {
    const res = await fetch('/api/viewer/pick-rect', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    })
    if (!res.ok) { vlog('warn', `Rect pick failed: ${res.status}`); return }
    const j = await res.json() as { nLabels?: number; nSelected?: number }
    vlog('info', `Rect picked ${j.nLabels ?? 0} cells (${j.nSelected ?? 0} in selection)`)
  } catch (err) {
    vlog('warn', 'Rect pick error: ' + (err instanceof Error ? err.message : String(err)))
  }
}

/**
 * Wheel zooms; SHIFT+wheel steps the z plane in the 2D view.
 *
 * Stepping z is the one control the plane view has that the wheel was not already spending, and it is
 * the natural pair to shift+drag panning. Not in the 3D view: there the wheel's job is the dolly and
 * the depth RANGE is a two-ended thing a single wheel cannot express.
 *
 * The plane change is scheduled, not applied per notch. Changing z drops every cached texture and
 * refetches, so firing per notch would queue a fetch the user has already scrolled past. The number
 * moves immediately (the readout must track the pointer) and the refetch collapses to the last
 * position through `debouncedLatest`, the canonical scheduler for exactly this.
 *
 * The z SLIDER shares this pump, on `@input`. It used to commit on `@change` — one refetch, on
 * release — which is not how a z slider reads: you drag it to find a plane, so the planes have to go
 * past. The wait that makes stepping a wheel feel live is the same wait that makes dragging feel
 * live, and the scheduler already guarantees the run that lands is the position the pointer stopped
 * at, so a drag costs one refetch too. The time slider is deliberately NOT on this path: `gotoT`
 * paints from a cache and is cheap enough to run per pointer move.
 */
// `maxWait` is the "planes go past while I scrub" cadence: a plain trailing debounce (what this
// was) resets its timer on every `@input` event, so a continuous drag never fires until the pointer
// stops — the release-only symptom Dominik hit 2026-08-31. `maxWait: 220` fires at most once per
// ~220 ms during a sustained drag with the latest z, which is long enough that the average slab
// fetch (~50-150 ms) has a chance to complete before the next `setZPlane` aborts inflight, and
// short enough that the user sees planes moving past rather than a step-then-freeze. Canonical
// primitive — do not hand-roll a scrub throttle. See `utils/debouncedLatest.ts` and docs/UI.md →
// *Continuous controls*.
const zPump = debouncedLatest<number>(async (zp) => {
  // Fast plane switch — both renderers implement setZPlane, so skip the full reallocate
  // (which destroys textures / atlas — the 200 ms-2 s freeze Dominik hit 2026-08-29). Abort
  // any in-flight slab fetches first: their URLs carry the OLD zPlane and would land in a
  // slot stamped with the NEW planeVersion, leaving wrong bytes on a "fresh" slot. Volume
  // mode and useTiles have different geometry / cache shapes → fall through to reallocate.
  const r = renderer.value
  if (r?.setZPlane && !useTiles.value && mode.value === 'plane') {
    for (const ac of aborts.values()) ac.abort()
    aborts.clear()
    inflight.clear()
    pump.cancel()
    r.setZPlane(zp)
    gotoT(t.value)
    return
  }
  await reallocate()
}, { wait: 120, maxWait: 220 })
/** Move to a plane: the readout follows the pointer, the refetch follows the pump. */
function stepZ(next: number) {
  if (next === zPlane.value) return
  zPlane.value = next
  zPump.schedule(next)
}
/**
 * 2D pyramid LOD swap on zoom. A wheel gesture crossing a `floor(log2(zoom))` threshold changes the
 * `slabLevel` computed, and the watch pumps a reallocate at that new level. Debounced (150 ms) because
 * a single gesture emits an event per pixel of travel — the smallest wait that lets a fast wheel scroll
 * settle before we refetch. Same discipline as the z pump: value moves immediately (the readout tracks
 * the pointer), refetch collapses to the last position through `debouncedLatest`.
 *
 * `loadedLevel` gates against the initial mount: a first `reallocate(true)` sets `loadedLevel` to the
 * fit-appropriate level; only DRIFT from that level fires a second reallocate.
 */
const levelPump = debouncedLatest<number>(async () => reallocate(false), { wait: 150 })
watch(slabLevel, (newLvl) => {
  if (!meta.value || mode.value !== 'plane') return
  if (newLvl !== loadedLevel.value) levelPump.schedule(newLvl)
})
/** Brick renderer: use the dropdown as a FLOOR (coarsest allowed), letting SSE pick finer as
 *  the user zooms in. Auto = coarsest possible = no effective restriction. Replaces 8b780fd's
 *  pin, which blocked adaptive LOD entirely and left SispLk stuck at L5 on deep zoom
 *  (screenshot 2026-08-29). Over-fetch on wide viewports (f8gzA2 fit distance) is now bounded
 *  by `MAX_INTERSECT_BRICKS` inside the scheduler. Fires whether or not the volume path is
 *  active — the setter is a no-op when the flat renderer is on and cheap when unchanged. */
watch(slabLevel, (newLvl) => {
  renderer.value?.setLevelFloor?.(newLvl)
}, { immediate: true })
function onWheel(e: WheelEvent) {
  e.preventDefault()
  const m = meta.value
  if (e.shiftKey && mode.value === 'plane' && m && m.nZ > 1) {
    const step = e.deltaY > 0 ? 1 : -1
    stepZ(Math.max(0, Math.min(m.nZ - 1, zPlane.value + step)))
    return
  }
  // 2D plane view is a bounded rectangle → wider zoom-in band so the user can reach 1:1 (and past)
  // and `pickTileLevel` gets down to L0. 3D volume band widened 0.15 → 0.05 (Dominik 2026-08-29:
  // "can't zoom in enough for L0 to be used") — with the brick renderer honouring SSE per zoom
  // there's a genuine payoff for going deeper, whereas the pre-brick pin made a deep zoom just
  // slower for the same L5 pixels. Rotation can still lose the box off-screen; Reset view is one
  // click away.
  const band = mode.value === 'plane'
    ? { min: 0.005, max: 6 }
    : { min: 0.05, max: 6 }
  // Cursor-anchored zoom (ImageJ): 2D plane only. The 3D wheel is a dolly on the orbit and adding
  // a pan-shift under a rotated basis moves the volume sideways in a way the user did not ask for.
  let anchor: { ndcX: number; ndcY: number; aspect: number } | undefined
  const c = canvas.value
  if (c && mode.value === 'plane') {
    const rect = c.getBoundingClientRect()
    const w = Math.max(rect.width, 1), h = Math.max(rect.height, 1)
    anchor = {
      ndcX: (2 * (e.clientX - rect.left)) / w - 1,
      ndcY: 1 - (2 * (e.clientY - rect.top)) / h,
      aspect: w / h,
    }
  }
  cam.value = orbitZoom(cam.value, e.deltaY, fitDist.value, band, anchor)
  frame.redraw()
  // Zoom exposes/hides tiles (extent changes and level may swap). Level swap is handled by the
  // `slabLevel` watcher → `levelPump` → reallocate; the tile pump handles the same-level case where
  // a smaller field of view drops some tiles and a larger one gains new ones.
  if (useTiles.value) scheduleTilePump()
}

/** What the canvas responds to. One list, rendered in the popover AND nowhere else — a shortcut that
 *  only exists in someone's memory is a shortcut nobody uses. */
/** Per-mode table so the popover can be read as "in mode X, what does gesture Y do". The same
 *  gesture (Drag, Click) means different things depending on pan vs select mode AND 2D vs 3D, so a
 *  flat list of `keys → what` conflated four contexts. `—` = a no-op in that context. */
const SHORTCUTS: { keys: string; pan: string; select: string }[] = [
  { keys: 'Drag',          pan: '2D: pan · 3D: rotate',   select: 'Rectangle (2D)' },
  { keys: 'Shift + drag',  pan: 'Pan',                    select: 'Pan (escape hatch)' },
  { keys: 'Wheel',         pan: 'Zoom',                   select: 'Zoom' },
  { keys: 'Shift + wheel', pan: '2D: step through z',     select: '2D: step through z' },
  { keys: 'Click',         pan: '—',                      select: 'Pick cell (replace)' },
  { keys: 'Shift + click', pan: '—',                      select: 'Add cell to selection' },
  { keys: 'Alt + click',   pan: '—',                      select: 'Toggle cell in selection' },
  { keys: 'Space',         pan: 'Play / pause',           select: 'Play / pause' },
  { keys: '← / →',         pan: 'Prev / next frame',      select: 'Prev / next frame' },
  { keys: 'Mode icon',     pan: 'Switch to select',       select: 'Switch to pan' },
]
const keysOpen = ref(false)
const keysBtn = ref<HTMLElement | null>(null)

/**
 * ONE panel section open at a time.
 *
 * Not tidiness: with several open, expanding Overlays pushed the 2D/3D toggle off the top of the panel
 * — "once i expand overlays the 2d/3d toggles disappear" (Dominik, 2026-08-25). The sections that stay
 * put (View, Timepoint) are deliberately not in the accordion, because they are the ones you reach for
 * while looking at something else.
 *
 * '' is a real state — everything collapsed — so clicking an open section shuts it rather than being
 * a no-op. Persisted, like every other user-settable option.
 */
const openSection = ref(localStorage.getItem('cc.vw.section') ?? 'channels')
watch(openSection, v => localStorage.setItem('cc.vw.section', v))
// Re-wire the renderer's frame-timings callback when Debug opens/closes OR when the bench
// toggle flips, so the query-set resolve path is only paid when someone is looking at the
// readouts. The bench toggle also gates whether the arrays grow unbounded (save-blob) or roll.
watch([openSection, benchEnabled], () => wireFrameTimings(lastWiredRenderer))
/**
 * Takes the key AND the event, rather than returning a handler.
 *
 * Binding `@update:open="sectionOpen('channels')"` looks like it binds the returned handler and does
 * not: Vue compiles a CALL expression as an inline STATEMENT, so it runs the outer function on every
 * event and throws the returned one away — the boolean never arrives and nothing ever expands
 * (Dominik, 2026-08-25). Only a bare identifier or a member expression is bound as the handler itself.
 */
const setSection = (key: string, v: boolean) => { openSection.value = v ? key : '' }
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

/**
 * QuPath-style overview minimap in the top-right of the canvas.
 *
 * A schematic: the whole image as a bordered rect, the current viewport as an inner rect. No tissue
 * thumbnail yet — the whole-slide coarsest level for f8gzA2 is still ~1300×1000 × 24 ch × 2 B ≈ 60 MB,
 * which is a lot of bytes to fetch for a corner overlay. The rectangle alone answers "where in the
 * slide am I" and "where do I want to be next", which is the ask (Dominik, 2026-08-26).
 *
 * Click or drag anywhere on the minimap → re-center the camera at that image point. Persists like
 * every other user-settable option; only rendered when there is a real slide to navigate (tile mode).
 */
const overviewShown = ref(localStorage.getItem('cc.vw.overview') !== 'false')
watch(overviewShown, v => localStorage.setItem('cc.vw.overview', String(v)))
/** Advanced viewer popover — open state + anchor. NOT persisted: a popover that's OPEN on
 *  reload is a distinct bug from a slider whose value should survive. Anchor is the trigger
 *  button so `TeleportPopover` positions from its rect. */
const advancedOpen = ref(false)
const advancedTrigger = ref<HTMLElement | null>(null)
/** Tile-cache mini map, same persist-in-localStorage pattern as `overviewShown`. Default ON — the
 *  user asked for it — but collapsible because tile-heavy views make it visually busy and someone
 *  who never scrubs won't want it. */
const tilesMapShown = ref(localStorage.getItem('cc.vw.tilemap') !== 'false')
watch(tilesMapShown, v => localStorage.setItem('cc.vw.tilemap', String(v)))
/** Brick-atlas residency mini map — spatial analog of the tile map for `?bricks=1`. Default ON so
 *  the volume path has the same at-a-glance diagnostic the plane path does; the atlas is 3D, so
 *  the map draws one nBx × nBy grid per Z slice. */
const bricksMapShown = ref(localStorage.getItem('cc.vw.brickmap') !== 'false')
watch(bricksMapShown, v => localStorage.setItem('cc.vw.brickmap', String(v)))
/** Snapshot of the brick atlas — refreshed on the same tick that syncs the time strip. Kept as a
 *  plain array + Set so cell painting is O(nBx × nBy × nBz) at the CURRENT boundT + level. */
const brickResidents = shallowRef<{ t: number; level: number; bx: number; by: number; bz: number }[]>([])
const brickInflight = shallowRef<{ t: number; level: number; bx: number; by: number; bz: number }[]>([])
/** Bench diagnostic: how many bricks at the CURRENT atlas level are resident vs still fetching.
 *  Filters by target `t` (matches the mini-map convention). Populated only when `bricksEnabled`. */
const brickResidentsAtLevel = computed(() => {
  const lv = brickCurrentLevel.value; const tp = t.value
  if (lv === undefined) return 0
  let n = 0
  for (const e of brickResidents.value) if (e.level === lv && e.t === tp) n++
  return n
})
const brickInflightAtLevel = computed(() => {
  const lv = brickCurrentLevel.value; const tp = t.value
  if (lv === undefined) return 0
  let n = 0
  for (const e of brickInflight.value) if (e.level === lv && e.t === tp) n++
  return n
})
const brickCurrentLevel = ref<number | undefined>(undefined)
/** Diagnostic mirror of `brickResidency().missing` — how many CORE viewport bricks the shader
 *  wants that aren't in the atlas. Feeds the bench chip so we can spot the "stalled" case
 *  (missing > 0 AND inflight == 0). */
const brickMissing = ref<number>(0)
/** Same as `brickMissing` but at `boundT` — the timepoint the scheduler is chasing rather than
 *  the one drawn. Splits "chip stuck because we're waiting on the target frame's bricks"
 *  (`missingAtBoundT > 0`) from "chip stuck despite the shader having what it needs"
 *  (both 0 → suggests the auto-advance path). */
const brickMissingAtBoundT = ref<number>(0)
/** Timepoint currently on the canvas (mirror of `brickResidency().displayT`). */
const brickDisplayT = ref<number>(-1)
/** Timepoint the scheduler is chasing (mirror of `brickResidency().boundT`). */
const brickBoundT = ref<number>(0)
const brickSizeVox = shallowRef<readonly [number, number, number]>([128, 128, 1])
/** Whether the canvas reflects the target the user asked for AND is complete — see the JSDoc
 *  on `brickResidency().displayValid`. False covers both hold-on-cold stale frames (shader
 *  drawing an OLDER t while the scheduler chases the target) and unblank partial frames
 *  (target t drawn with `EMPTY_SLOT` holes). Drives the canvas-invalid chip. Initial `true`
 *  = we haven't drawn anything yet, so nothing to warn about. */
const brickDisplayValid = ref<boolean>(true)
/** Bricks path only: is the canvas out of sync with the target? True gates the amber chip.
 *  `shownT >= 0` gate = don't fire during initial load (the "Loading timepoint…" chip
 *  already owns that state). */
const canvasPartial = computed(() =>
  bricksEnabled.value && shownT.value >= 0 && !brickDisplayValid.value)
/** Fractional viewport rect within the image, clamped to [0, 1]. Reads from `cam` and `meta`, so
 *  it re-derives every time either changes without a separate signal. Empty when the viewport is
 *  degenerate — the SVG then draws just the outer frame. */
const overviewRect = computed(() => {
  const m = meta.value
  const el = canvas.value
  if (!m || !el || el.clientHeight <= 0 || el.clientWidth <= 0) return null
  const asp = canvasAspect()
  const halfHUm = cam.value.dist * VIEW_HALF_ANGLE
  const halfWUm = halfHUm * asp
  const vx = m.voxelUm[0] || 1
  const vy = m.voxelUm[1] || 1
  const ex = m.nX * vx
  const ey = m.nY * vy
  const cxImg = cam.value.panX + ex / 2
  const cyImg = -cam.value.panY + ey / 2
  return {
    x: Math.max(0, Math.min(1, (cxImg - halfWUm) / ex)),
    y: Math.max(0, Math.min(1, (cyImg - halfHUm) / ey)),
    w: Math.max(0, Math.min(1, (halfWUm * 2) / ex)),
    h: Math.max(0, Math.min(1, (halfHUm * 2) / ey)),
  }
})
/** Fixed height in CSS px; the width follows the image aspect so the fractional rect maps 1:1. */
const OVERVIEW_H = 110
const overviewSize = computed(() => {
  const m = meta.value
  if (!m) return { w: 0, h: 0 }
  const vx = m.voxelUm[0] || 1
  const vy = m.voxelUm[1] || 1
  const asp = (m.nX * vx) / Math.max(m.nY * vy, 1)
  return { w: Math.min(220, Math.round(OVERVIEW_H * asp)), h: OVERVIEW_H }
})
/**
 * Tissue thumbnail behind the minimap rect. Fetched ONCE per image — the deepest pyramid level's
 * whole slab, all channels — and composited on the CPU using the same LUTs the shader uses. Small
 * enough to be cheap: for a 20k×17k slide with 5 levels, the deepest is ~635×528 × 24 ch × 2 B ≈
 * 16 MB, one-off, and the composite is ~350k pixels × 24 ch ≈ 8M multiplies (~10-30 ms). Redrawn on
 * every channel change (visibility, contrast, colour) because pushChannels calls `renderOverview`.
 */
const overviewCanvas = ref<HTMLCanvasElement | null>(null)
const overviewChans = shallowRef<(Uint16Array | Uint8Array)[] | null>(null)
const overviewDims = ref<{ nX: number; nY: number } | null>(null)
async function loadOverviewThumbnail() {
  const m = meta.value
  if (!m || !m.levels || m.levels.length === 0) return
  const lvl = m.levels[m.levels.length - 1]
  const nch = Math.min(m.nC, MAX_CHANNELS)
  const enc = settings.viewerCompress ? 'zstd' : 'identity'
  try {
    const bufs = await Promise.all(Array.from({ length: nch }, async (_, c) => {
      const url = slabUrl({
        projectUid, imageUid, valueName: valueName.value, t: 0, c, enc, level: lvl.level,
        z: zPlane.value,
        x: 0, xTo: lvl.nX - 1, y: 0, yTo: lvl.nY - 1,
      })
      const res = await fetch(url, { cache: 'default' })
      if (!res.ok) throw new Error(`Overview c${c}: ${res.status}`)
      return slabView(await res.arrayBuffer(), m.bytesPerVoxel)
    }))
    overviewChans.value = bufs
    overviewDims.value = { nX: lvl.nX, nY: lvl.nY }
    renderOverview()
  } catch (e) {
    vlog('warn', 'Overview thumbnail unavailable', e instanceof Error ? e.message : String(e))
  }
}
// The canvas is `v-if`'d off when Overview toggles off, so its ref goes null. A fresh canvas mounts
// blank when the user toggles back on — nothing repaints it until the next channel change, so the
// thumbnail stays black (Dominik, 2026-08-26). Watch the ref: whenever it mounts (null → element),
// repaint. Cheap — one full pass per toggle.
watch(overviewCanvas, cv => { if (cv) renderOverview() })
function renderOverview() {
  const cv = overviewCanvas.value
  const dims = overviewDims.value
  const chans = overviewChans.value
  const m = meta.value
  if (!cv || !dims || !chans || !m) return
  const w = dims.nX, h = dims.nY
  if (cv.width !== w) cv.width = w
  if (cv.height !== h) cv.height = h
  const ctx = cv.getContext('2d')
  if (!ctx) return
  const img = ctx.createImageData(w, h)
  const px = img.data
  const nch = chans.length
  const specs = m.channels.slice(0, nch).map(ch => {
    const top = ch.lut[ch.lut.length - 1] ?? [1, 1, 1]
    return {
      visible: ch.visible, lo: ch.lo, span: Math.max(ch.hi - ch.lo, 1),
      r: top[0], g: top[1], b: top[2],
    }
  })
  const N = w * h
  for (let i = 0; i < N; i++) {
    let r = 0, g = 0, b = 0
    for (let c = 0; c < nch; c++) {
      const s = specs[c]
      if (!s.visible) continue
      const v = Math.max(0, Math.min(1, (chans[c][i] - s.lo) / s.span))
      r += v * s.r; g += v * s.g; b += v * s.b
    }
    const pi = i * 4
    px[pi] = Math.min(255, r * 255) | 0
    px[pi + 1] = Math.min(255, g * 255) | 0
    px[pi + 2] = Math.min(255, b * 255) | 0
    px[pi + 3] = 255
  }
  ctx.putImageData(img, 0, 0)
}
let overviewDragging = false
function overviewNavigate(e: PointerEvent) {
  const m = meta.value
  const el = e.currentTarget as HTMLElement | null
  if (!m || !el) return
  const rect = el.getBoundingClientRect()
  const fx = Math.max(0, Math.min(1, (e.clientX - rect.left) / rect.width))
  const fy = Math.max(0, Math.min(1, (e.clientY - rect.top) / rect.height))
  const vx = m.voxelUm[0] || 1
  const vy = m.voxelUm[1] || 1
  const ex = m.nX * vx
  const ey = m.nY * vy
  // Center the camera on the clicked image point. Reverses the panY sign convention (see
  // computeViewportL0): positive panY looks at SMALLER image_y.
  cam.value = { ...cam.value, panX: fx * ex - ex / 2, panY: -(fy * ey - ey / 2) }
  frame.redraw()
  if (useTiles.value) scheduleTilePump()
}
function onOverviewDown(e: PointerEvent) {
  overviewDragging = true
  ;(e.currentTarget as HTMLElement).setPointerCapture?.(e.pointerId)
  overviewNavigate(e)
  e.stopPropagation()          // do not bubble to the canvas pan/orbit handlers
}
function onOverviewMove(e: PointerEvent) {
  if (!overviewDragging) return
  overviewNavigate(e)
  e.stopPropagation()
}
function onOverviewUp(e: PointerEvent) {
  overviewDragging = false
  ;(e.currentTarget as HTMLElement).releasePointerCapture?.(e.pointerId)
}
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
 *  the projection — is the one that is easy to forget and shows up as a 3D reset that clips.
 *  Perspective flag has to match the shader's projection: 3D+Persp needs the near-face margin, but
 *  3D+Ortho (Imaris-style head-on, no depth foreshortening) does NOT — passing `true` there leaves
 *  black margins around the volume (Dominik 2026-09-01). 2D is always ortho. */
const fitNow = (m: ViewerMeta) =>
  fitCamera(extentUm(m, zDepth.value), canvasAspect(),
            mode.value === 'volume' && settings.viewerVolumeProjection === 'persp')
function resetView() {
  cam.value = { ...fitNow(meta.value!) }
  frame.redraw()
}

/**
 * Ensure the RIGHT renderer for the current mode+image is alive. The canvas has one WebGPU context,
 * so the two renderers are alternates — swapping mode destroys one before creating the other.
 *
 * **Serialised against itself.** `start()` awaits it, `reallocate()` awaits it — but the level watch
 * fires `levelPump` which calls `reallocate` on a debounce, and if that fires DURING the first
 * `ensureRenderer` (a real race — the tile atlas allocation takes ~100 ms and the level watch fires
 * within 150 ms of the first `slabLevel` change), two `createTileRenderer` calls execute concurrently.
 * That is the source of the `TextureView … cannot be used with [Device]` error (Dominik, 2026-08-26):
 * D1 configures the canvas, then D2 reconfigures it, and D1's draws now hit D2's canvas texture.
 * The guard turns the second call into an await on the first, and only ever creates one device.
 */
let _rendererCreating: Promise<void> | null = null
async function ensureRenderer() {
  if (_rendererCreating) return _rendererCreating
  _rendererCreating = (async () => {
    if (useTiles.value) {
      if (tileRenderer.value) return
      if (renderer.value) { renderer.value.destroy(); renderer.value = null }
      const tr = await createTileRenderer(canvas.value!, msg => {
        error.value = 'GPU: ' + msg
        vlog('error', 'Tile GPU error: ' + msg)
      })
      tileRenderer.value = tr
      void tr.lost.then(info => {
        tilePump.cancel()
        for (const ac of tileAborts.values()) ac.abort()
        lostDevice.value = true
        error.value = 'The GPU dropped the connection: ' + (info?.message || 'unknown')
        vlog('error', 'Viewer lost the GPU device', info?.message || 'no reason given')
      })
    } else {
      if (renderer.value) return
      if (tileRenderer.value) { tileRenderer.value.destroy(); tileRenderer.value = null }
      // `?bricks=1` swaps in the KILN_BRICK_PLAN P5 renderer instead of the flat volume one.
      // Same interface, different backing — the caller doesn't branch, only the constructor
      // does. P5a's brick renderer is a magenta-clear proof-of-plumbing; the real shader lands
      // in P5b.
      const construct = bricksEnabled.value ? createBrickVolumeRenderer : createVolumeRenderer
      const r = await construct(canvas.value!, msg => {
        error.value = 'GPU: ' + msg
        vlog('error', 'GPU error: ' + msg)
      })
      renderer.value = r
      // Apply the initial LOD knobs. `maxIntersect` = URL override if present, else the
      // persisted quality tier; a subsequent tier change goes through the watcher below without
      // a reallocate. `bias`/`hold` stay URL-only (dev knobs). No-ops on the flat renderer.
      // See `parseNumQuery` block at the top of the module for the param names.
      r.setSchedulerKnobs?.({ maxIntersect: effectiveMaxIntersect.value, bias: brickKnobBias })
      r.setHoldFinerEnabled?.(brickKnobHold)
      // Brick renderer fetches asynchronously; a landed brick has to nudge the frame pump or
      // its bytes render one interaction late. Also refresh the residency snapshot — otherwise
      // the mini-map only updates on brick LAND (`setOnBrickLoaded`), and the "amber = fetching"
      // phase is invisible because `syncCacheState` only ever sees fetches that already
      // resolved. `syncCacheState` is a couple of linear walks; fine at rAF rate. No-op on the
      // flat renderer.
      r.setNeedsRedraw?.(() => { syncCacheState(); frame.redraw() })
      // Brick renderer only: grow `seenMax` from real data as bricks arrive — same discipline
      // the flat path runs in `pump`. Without it the contrast slider's ceiling stays at the
      // initial server-shipped `hi` and dragging `hi` below it locks the range.
      r.setOnBrickLoaded?.(perChannelMax => {
        seenMax.value = perChannelMax.map((v, c) => Math.max(seenMax.value[c] ?? 0, v))
        // Time-strip animation reads `resident.value` from `residentTimepoints()`. Bricks land
        // asynchronously, and the flat-path pump's `syncCacheState` calls don't run — refresh
        // here so the strip lights up as prefetch fills.
        syncCacheState()
      })
      // When a scrub past cold-cache advances the DISPLAYED t asynchronously (via the brick
      // scheduler auto-catch-up), sync `shownT` so overlays match. `showT` handles the
      // synchronous path directly; this covers the case where `show(t)` returned false and
      // residency finished later.
      r.setOnDisplayAdvanced?.(t => {
        if (shownT.value !== t) {
          shownT.value = t
          frame.redraw()
        }
      })
      // Per-writeBrick timings: only useful for save-blob analysis, kept gated on `?bench=1`.
      if (benchEnabled.value) {
        r.setOnBrickWritten?.((durationMs, bytes) => {
          benchWrites.value = [...benchWrites.value, {
            atMs: performance.now(), durationMs, bytes,
          }]
        })
      }
      // Per-frame GPU + sub-frame CPU timings: wired only when the Debug section is open (or
      // `?bench=1` is set). Wiring/unwiring lets the renderer skip the GPU query-set resolve
      // entirely when nothing is watching — Debug is a developer diagnostic surface, not
      // something a normal viewer session pays for. Delivered asynchronously (frame N+K).
      wireFrameTimings(r)
      void r.lost.then(info => {
        stopPlay()
        pump.cancel()
        for (const ac of aborts.values()) ac.abort()
        lostDevice.value = true
        error.value = 'The GPU dropped the connection: ' + (info?.message || 'unknown')
        vlog('error', 'Viewer lost the GPU device', info?.message || 'no reason given')
      })
    }
  })()
  try { await _rendererCreating } finally { _rendererCreating = null }
}

/**
 * Re-allocate for the current mode and z, then reload. Every cached texture goes: at a different depth
 * they are a different shape, and at a different z they hold different pixels. That is a full refetch —
 * ~4 s for a 181-frame plane movie, ~90 s for the volume — which is the honest cost of the switch and
 * the reason the plane view is the default rather than something you opt into.
 *
 * Handles both renderer types. Async because a mode swap can involve destroying the old device and
 * acquiring a new one; every caller either awaits or is fire-and-forget (the debounced pumps and the
 * chip/range handlers, all of which just want the effect to happen eventually).
 */
async function reallocate(refit = false) {
  const m = meta.value
  if (!m) return
  // The VOLUME path is a hard boundary — mode/plane/depth change is a full refetch, everything on the
  // wire is for a shape we no longer want. The TILE path is progressive: a level swap keeps the atlas
  // (chunks stay 1024² at every level), keeps in-flight fetches (many will still be wanted at the
  // new viewport — `scheduleTilePump` selectively aborts what the new viewport does NOT want), and
  // keeps `shownT` so the overlay does not flash "Loading timepoint 0…" on every wheel notch. Without
  // this split, a burst of level swaps aborted every fetch mid-air and the pump got stuck retrying
  // aborted work — reported (Dominik, 2026-08-26) as "the loading got stuck and never returned".
  pump.cancel()
  tilePump.cancel()
  if (!useTiles.value) {
    for (const ac of aborts.values()) ac.abort()
    aborts.clear(); inflight.clear()
    shownT.value = -1
    hits.value = 0; misses.value = 0
    autoWin.value = []                     // Auto windows on what is loaded, so re-derive per plane
    waitingFor.value = -1
  }
  announce.value = true
  // Refit BEFORE `setImage`: `slabLevel` and `useTiles` react to `cam.dist`, so a stale distance
  // would allocate the pipeline for the wrong level (or the wrong pipeline entirely).
  const c = fitNow(m)
  fitDist.value = c.dist
  if (refit) cam.value = c

  await ensureRenderer()

  if (useTiles.value) {
    const tr = tileRenderer.value
    if (!tr) return
    const lvl = levelMeta(m, slabLevel.value)
    const nch = Math.min(m.nC, MAX_CHANNELS)
    tr.setImage(m, slabLevel.value, effectiveCacheBytes.value,
                lvl?.chunkX ?? m.nX, lvl?.chunkY ?? m.nY, nch)
    tr.setChannels(m.channels)
    tr.resize()
    loadedLevel.value = slabLevel.value
    syncTileCacheState()
    scheduleTilePump()
    frame.redraw()
  } else {
    const r = renderer.value
    if (!r) return
    // P7: allocate the labels texture when the preview is showing labels for THIS image, even without
    // a picker selection — a first-time preview would otherwise have nowhere to upload its bytes.
    const wantLabels = !!labelName.value || (!!viewerStore.previewLabels &&
      viewerStore.previewLabels?.imageUid === imageUid)
    // `?bench=1`: reset the bench recorder BEFORE setImage so t0 stamps the actual boundary
    // between "nothing loaded" and "first user-visible frame". Any prior samples belonged to a
    // different image or a different mode swap and would poison the summary.
    if (benchEnabled.value) benchReset()
    r.setImage(m, effectiveCacheBytes.value, zDepth.value,
               mode.value === 'plane' ? zPlane.value : zRange.value[0], wantLabels,
               renderNX.value, renderNY.value)
    // Brick renderer only: give the fetch loop the base URL identity — projectUid, imageUid, vn.
    // No-op on the flat renderer via the optional chain.
    r.setBrickSource?.({
      projectUid, imageUid,
      valueName: valueName.value || undefined,
      // Fire label brick fetches when the picker or the preview marks THIS image as showing
      // labels — same predicate `wantLabels` above uses to decide whether the texture is
      // allocated. `undefined` when no mask is picked, which lets the brick loader skip label
      // requests entirely on projects with no segmentation.
      labelName: wantLabels ? (labelName.value || undefined) : undefined,
    })
    // Brick scheduler floor = slabLevel (dropdown). SSE picks finer as user zooms in; over-fetch
    // on wide viewports is bounded by MAX_INTERSECT_BRICKS. See the slabLevel watcher above.
    r.setLevelFloor?.(slabLevel.value)
    loadedLevel.value = slabLevel.value
    r.setCapacity(settings.viewerCacheFrames || m.nT)
    r.setOrthographic(mode.value === 'plane' || settings.viewerVolumeProjection === 'ortho')
    r.setSteps(mode.value === 'plane' ? 1 : settings.viewerSteps)
    syncCacheState()
    gotoT(t.value)
  }
}

/**
 * Reset the channel's contrast to the values the server shipped (napari-saved, or first-load
 * percentile). NOT an auto-window from data anymore: user's expectation is "put it back the way it
 * was" (Dominik, 2026-08-26), and `autoWin` was a volume-mode-only helper that never fired for
 * whole-slide tiles, so the button did nothing there.
 */
function resetContrast(c: number) {
  const init = initialContrast.value[c], m = meta.value
  if (!init || !m) return
  m.channels[c].lo = init.lo
  m.channels[c].hi = init.hi
  pushChannels()
}
/** Reset every channel's contrast in one click — the master pair to the per-channel button. Only
 *  the CONTRAST, not the visibility or colour: those have their own controls above. */
function resetAllContrast() {
  const m = meta.value
  if (!m) return
  for (let i = 0; i < m.channels.length; i++) {
    const init = initialContrast.value[i]
    if (!init) continue
    m.channels[i].lo = init.lo
    m.channels[i].hi = init.hi
  }
  pushChannels()
}
/** Auto-window a channel on the percentiles of the first frame/tile loaded — the ImageJ-style
 *  quick adjust. Feeds off `autoWin`, which is a one-shot per image (see `fetchTile` /
 *  `fetchTimepoint`) so consecutive presses land at the same place. Free — no refetch. */
function autoContrast(c: number) {
  const w = autoWin.value[c], m = meta.value
  if (!w || !m) return
  m.channels[c].lo = w.lo
  m.channels[c].hi = w.hi
  pushChannels()
}
function autoAllContrast() {
  const m = meta.value
  if (!m) return
  autoWin.value.forEach((w, c) => {
    if (!w || !m.channels[c]) return
    m.channels[c].lo = w.lo
    m.channels[c].hi = w.hi
  })
  pushChannels()
}

// ── Lifecycle ────────────────────────────────────────────────────────────────────

// PY — per-image viewer props autosave. Same on-disk file napari's autosave writes to
// (`<task_dir>/data/<basename(zarr)>.json`), so an animation-card snapshot stays portable across
// viewers. The debounce is trailing so a slider/wheel gesture writes ONCE per settle, not per input
// event. `duringRestore` suppresses the echo when a load applies mutations that would otherwise
// trip these same watchers. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md → PY.
const propsSink = debouncedSave(async () => {
  const m = meta.value
  if (!m || !settings.viewerAutoSaveLayerProps) return
  const vs = captureViewState({
    meta: m, channels: m.channels, cam: cam.value,
    mode: mode.value, zPlane: zPlane.value, zRange: zRange.value,
    t: t.value, valueName: valueName.value,
  })
  await saveViewerProps({ projectUid, imageUid, valueName: valueName.value || undefined }, vs)
}, { wait: 800 })

/**
 * Fetch meta for the current VERSION, allocate the right renderer (tile vs volume), and put the
 * first frame on screen. Called from `start()` (fresh window) AND from `changeVersion` (picker), so
 * every path through the viewer initialisation goes through here — one place that knows about tile
 * routing + PY restore + overlay/tracks kickoff. `refit` is true from `start` (no prior camera) and
 * false from `changeVersion` (the user's pose was for a related image and is worth carrying over).
 */
async function loadVersion(refit: boolean) {
  starting.value = 'Reading image'
  const res = await fetch(metaUrl({ projectUid, imageUid, valueName: valueName.value }))
  const m = await readJson<ViewerMeta>(res, 'Metadata')
  meta.value = m
  // What the server RESOLVED, so the picker shows the active version rather than an empty box. Only
  // when we asked for nothing in particular — otherwise this is already what we asked for.
  valueName.value ||= m.valueName ?? ''
  // Snapshot the server's contrast + LUTs so Reset Contrast has a target that survives every drag.
  initialContrast.value = m.channels.map(ch => ({ lo: ch.lo, hi: ch.hi }))
  initialLUTs.value = m.channels.map(ch => ch.lut.map(stop => [...stop]))
  // Plane is the default in EVERY case. It's what plays, it's cheaper, and it's the view the pyramid
  // was wired for. 3D is opt-in via the View chip — the honest cost belongs behind a click.
  mode.value = 'plane'
  zPlane.value = Math.floor(Math.max(m.nZ - 1, 0) / 2)
  zRange.value = [0, Math.max(m.nZ - 1, 0)]
  autoWin.value = []                     // a different version has its own distribution
  seenMax.value = []
  // Fit BEFORE reallocate: `useTiles` and `slabLevel` derive from `cam.dist`, so a stale dist=1
  // would allocate the wrong pipeline for a big image. `fitDist` is seeded here so the reset button
  // works even if a saved camera pose is later restored on top.
  const fit = fitNow(m)
  cam.value = fit
  fitDist.value = fit.dist
  // PY — read saved viewer props BEFORE the reallocate, so pipeline-selecting fields (mode, zPlane,
  // zRange) are the RESTORED ones by the time `ensureRenderer` picks tile vs volume. If we let
  // reallocate run first on defaults and then applied a restored `mode = 'volume'`, the tile
  // pipeline picked for a whole-slide plane would be wrong and would need a second reallocate.
  // Post-alloc bits (channels, camera pose, T) go through `duringRestore` AFTER, since they don't
  // change which renderer gets built. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md → PY.
  const saved = settings.viewerAutoSaveLayerProps
    ? await loadViewerProps({ projectUid, imageUid, valueName: valueName.value || undefined })
    : null
  if (saved) {
    propsSink.duringRestore(() => {
      applyViewState(saved, m, {
        applyChannel: () => { /* deferred to the post-alloc apply below */ },
        applyCamera:  () => { /* deferred */ },
        applyMode:    md => { mode.value = md },
        applyZ:       (zp, zr) => { zPlane.value = zp; zRange.value = zr },
        applyT:       () => { /* deferred — T is post-alloc, no pipeline effect */ },
      })
    })
  }
  // `reallocate(false)` creates the right renderer (tile vs volume via `ensureRenderer`), sets
  // image, sets channels, and kicks off the first fetch. Same code the mode-swap path uses, so the
  // two entry points cannot drift apart.
  await reallocate(false)
  // Post-alloc restore: channels + camera + T. Only applies to volume mode — tile mode has no
  // per-channel contrast surface or camera pose hooked into the sink today (VIEWER_TILES_PLAN.md →
  // open). Wrapped in `duringRestore` again so the autosave watchers below don't echo it back.
  const r = renderer.value
  const tBeforeRestore = t.value
  if (r && saved) {
    propsSink.duringRestore(() => {
      applyViewState(saved, m, {
        applyChannel: (c, patch) => {
          const ch = m.channels[c]; if (!ch) return
          if (patch.lo !== undefined) ch.lo = patch.lo
          if (patch.hi !== undefined) ch.hi = patch.hi
          if (patch.visible !== undefined) ch.visible = patch.visible
          if (patch.hex) ch.lut = lutFromHex(patch.hex)
        },
        applyCamera: c => { cam.value = { ...c } },
        applyMode:   () => { /* handled pre-alloc */ },
        applyZ:      () => { /* handled pre-alloc */ },
        applyT:      tp => { if (tp < m.nT) t.value = Math.max(0, Math.floor(tp)) },
      })
    })
    pushChannels()   // channel mutations landed on `m.channels` — push them to the LUT texture
    // Reallocate's `gotoT` already fired for the PRE-restore t (usually 0). If the restore moved
    // t (usually because the panel remembered a mid-timecourse frame), kick a fresh pump so the
    // frame the user actually wants doesn't wait until they nudge a control. Without this, the
    // still-overlay reads "Loading timepoint N…" forever — the fetches for 0..prefetch fired
    // but the restored t never got its turn.
    if (t.value !== tBeforeRestore) gotoT(t.value)
  }
  if (refit) { /* nothing more — fitDist already seeded, cam already fit */ }
  starting.value = ''
  // After the first frame is on its way: overlays + track ribbons are separate, small requests and
  // must not delay the pixels. Tissue thumbnail for whole-slide (tile) view is the same idea.
  void loadOverlays()
  void loadTracks()
  // Fetched for any plane view; if the user never opens the minimap, the bytes stay in the HTTP
  // cache and the canvas just isn't drawn.
  if (mode.value === 'plane') void loadOverviewThumbnail()
}

// Autosave triggers — every watcher settles into one write per debounce window. `deep` on channels
// because `pushChannels` mutates `meta.value.channels[c].{lo,hi,visible,lut}` in place.
watch(() => meta.value?.channels, () => propsSink.schedule(), { deep: true })
watch(cam,       () => propsSink.schedule(), { deep: true })
watch(mode,      () => propsSink.schedule())
watch(zPlane,    () => propsSink.schedule())
watch(zRange,    () => propsSink.schedule())
watch(t,         () => propsSink.schedule())
watch(valueName, () => propsSink.schedule())

// The panel's 3D button (`ViewerPanel.vue` → `settings.setShow3D`) reaches the popup viewer
// through the `cc.viewerSetPrefs` bag sync (see `utils/viewerBagChannel.ts`). Watch the setter's
// derived value here so a panel-side flip drives THIS viewer's mode. Guarded on a real change to
// avoid re-entrant loops with `onModeChange` (chip → setShow3D).
watch(() => setUid.value ? settings.getShow3D(setUid.value) : null, want => {
  if (want === null) return
  const next: 'plane' | 'volume' = want ? 'volume' : 'plane'
  if (mode.value !== next) { mode.value = next; reallocate(true) }
})

/** View chip handler — flip the popup's mode AND reverse-sync to the panel's per-set setting so
 *  the two stay in lockstep (`settings.getShow3D` on the panel side, the watcher above on this
 *  side). The watcher short-circuits when `mode` already matches, so this write can't loop. */
function onModeChange(v: 'plane' | 'volume'): void {
  mode.value = v
  reallocate(true)
  if (setUid.value) settings.setShow3D(setUid.value, v === 'volume')
}

// ── Task-preview integration (P7) ─────────────────────────────────────────────
// One scheduler per this specific emit: pan/zoom fires per frame, but the preview API is expensive
// and its own store already debounces. This coalesces bursts so the store doesn't schedule per event.
const publishRegionSink = debouncedLatest<void>(async (_v, isCurrent) => {
  if (!isCurrent()) return
  const m = meta.value
  const c = canvas.value
  if (!m || !c) { viewerStore.setVisibleRegion(null); return }
  // The volume camera's basis is µm-across-the-screen; the visibleRegion helper wants image-pixel
  // pan/zoom. Convert here so the helper stays pure and testable.
  const umPerL0X = m.voxelUm?.[0] || 1
  const umPerL0Y = m.voxelUm?.[1] || 1
  const canvasW = Math.max(1, c.clientWidth)
  const canvasH = Math.max(1, c.clientHeight)
  const visibleHeightUm = 2 * Math.max(cam.value.dist, 0) * VIEW_HALF_ANGLE
  const visibleL0H = visibleHeightUm / umPerL0Y
  // Zoom in this helper's units: >1 = zoomed in (visible window shrinks). A "fit" camera shows the
  // whole image height in `visibleL0H` L0 pixels, so `zoom = m.nY / visibleL0H`.
  const zoom = (m.nY || 1) / Math.max(1, visibleL0H)
  const region = computeVisibleRegion({
    panX: cam.value.panX / umPerL0X,
    panY: -cam.value.panY / umPerL0Y,     // screen-up is negative image-Y (see panDrag)
    zoom, canvasW, canvasH,
    imageW: m.nX, imageH: m.nY,
    currentZ: mode.value === 'plane' ? zPlane.value : Math.floor((m.nZ - 1) / 2),
    currentT: t.value,
    ndisplay: mode.value === 'plane' ? 2 : 3,
  })
  viewerStore.setVisibleRegion(region)
}, { wait: 100 })

watch([() => cam.value.panX, () => cam.value.panY, () => cam.value.dist,
       zPlane, t, mode, meta],
      () => publishRegionSink.schedule(undefined))

// ── Publish a napari-shaped viewState alongside the visibleRegion ─────────────
// Same signal set + same debounce as the region sink — a keyframe capture (AnimationPanel) or a
// title-card reader is a downstream subscriber of the SAME viewer state the region publishes,
// so they should coalesce identically. Shape lives in `utils/viewer/viewState.ts` so a bug in the
// arithmetic is a unit-test failure rather than a rendered-mp4 discovery. Emits `null` when meta /
// canvas aren't ready — same guard as the region sink.
const publishViewStateSink = debouncedLatest<void>(async (_v, isCurrent) => {
  if (!isCurrent()) return
  const m = meta.value
  const c = canvas.value
  if (!m || !c) { viewerStore.setViewState(null); return }
  const canvasW = Math.max(1, c.clientWidth)
  const canvasH = Math.max(1, c.clientHeight)
  const vs = buildViewState({
    cam: cam.value, meta: m, t: t.value, zPlane: zPlane.value,
    ndisplay: mode.value === 'plane' ? 2 : 3,
    canvasW, canvasH, viewHalfAngle: VIEW_HALF_ANGLE,
  })
  viewerStore.setViewState(vs)
}, { wait: 100 })

// Watch the SAME signals as the region sink, PLUS the per-channel contrast/visibility, because
// contrast_limits + visible are the two layer fields the offline renderer reads. `meta.channels`
// mutations in place fire meta reactively; the deep watch on channels catches the lo/hi updates
// that don't replace the array reference.
watch([() => cam.value.panX, () => cam.value.panY, () => cam.value.dist,
       () => cam.value.yaw, () => cam.value.pitch,
       zPlane, t, mode, meta],
      () => publishViewStateSink.schedule(undefined))
watch(() => meta.value?.channels?.map(ch => `${ch.name}|${ch.visible}|${ch.lo}|${ch.hi}`).join(','),
      () => publishViewStateSink.schedule(undefined))
// Canvas size is a viewState field (`canvas.width/height`), which the movie surfaces read as the
// size fields' placeholder. Without this a popout resize wouldn't refresh the placeholder — the
// existing watchers only cover cam / t / z / mode / meta / channel changes, and `usePlotResize` owns
// only its own render loop. Dedicated observer; the sink is `debouncedLatest`, so multiple sources
// coalesce.
let publishResizeObs: ResizeObserver | null = null
onMounted(() => {
  if (!canvas.value || typeof ResizeObserver === 'undefined') return
  publishResizeObs = new ResizeObserver(() => publishViewStateSink.schedule(undefined))
  publishResizeObs.observe(canvas.value)
})
onUnmounted(() => { publishResizeObs?.disconnect(); publishResizeObs = null })

// ── Consume a pending viewState from the AnimationPanel ──────────────────────
// AnimationPanel writes a `PendingViewState` when the user clicks a keyframe (Sync napari on) or
// toggles Sync while a keyframe is selected. We convert the napari-shaped snapshot back to the
// orbit-camera form via the SAME `applyViewStateToBrowser` reader the unit tests exercise, then
// apply — mutating cam / t / zPlane / mode / channels in place so the existing renderer paths
// (`pushChannels`, `frame.redraw`) handle the actual GPU update. Value is a signal, not a queue:
// the publisher immediately re-emits from the applied state, which is exactly what the user
// wants (the animation page's next capture would see the new state anyway).
// Re-fires on updateId change (a fresh setPendingViewState arriving through the store setter or the
// storage bridge) AND on meta/canvas becoming ready — the store may seed pendingViewState from
// localStorage on init (an openViewerWindow handoff wrote it before the popup mounted), and the
// apply has to happen once the renderer's ready, not when the ref was seeded. `immediate: true` so
// a fresh popup with a seed applies as soon as meta and canvas resolve; the guard bails otherwise.
watch(() => [viewerStore.pendingViewState?.updateId, !!meta.value, !!canvas.value] as const, async () => {
  const pending = viewerStore.pendingViewState
  if (!pending) return
  const vs = pending.viewState as ViewerViewState | null
  if (!vs) return
  const m = meta.value
  const c = canvas.value
  if (!m || !c) return
  const applyingId = pending.updateId
  const canvasH = Math.max(1, c.clientHeight)
  const applied = applyViewStateToBrowser({
    vs, meta: m, currentCam: cam.value, canvasH, viewHalfAngle: VIEW_HALF_ANGLE,
  })

  // Per-channel state first so the reallocated GPU pipeline picks up the visibility / contrast on
  // the next fetch — same order the initial mount uses (contrast/vis applied around reallocate).
  for (const src of applied.channels) {
    const dst = m.channels.find(ch => ch.name === src.name)
    if (!dst) continue
    dst.lo = src.lo
    dst.hi = src.hi
    dst.visible = src.visible
  }

  // Camera pose is cheap — one write + a redraw; the draw loop reads `cam.value` inside.
  cam.value = applied.cam

  // Mode switch (2D ↔ 3D) needs a `reallocate` — the tile vs volume renderer chain is picked at
  // that moment (`ensureRenderer`). A bare `mode.value = ...` writes the ref but leaves the wrong
  // renderer active, which is what the user hit: controls updated but the canvas never redrew
  // because the plane renderer's watchers didn't fire for the new mode.
  const modeChanged = mode.value !== (applied.ndisplay === 3 ? 'volume' : 'plane')
  mode.value = applied.ndisplay === 3 ? 'volume' : 'plane'

  // z uses the canonical `stepZ` (writes the ref + schedules the reallocate pump); a bare
  // `zPlane.value = …` moves the number but leaves the tile atlas / volume texture on the old
  // plane. `gotoT` is the canonical t-setter for the same reason: it schedules the tile pump or
  // the timepoint pump depending on the render path, then redraws.
  if (modeChanged) {
    await reallocate(false)
  } else if (zPlane.value !== applied.zPlane) {
    stepZ(applied.zPlane)
  }

  if (t.value !== applied.t) gotoT(applied.t)
  pushChannels()
  // Consume the seed so a popup reload doesn't silently re-apply it. Idempotent: if a fresh
  // pending arrived mid-apply, `consumePendingViewState` sees a different `updateId` and no-ops.
  viewerStore.consumePendingViewState(applyingId)
}, { immediate: true })

// Open image → the store. Published from meta so `zarrPath`/`taskDir` reach the browser through the
// same route as the pixels: the meta response is the one authoritative resolution of an image
// version.
watch(meta, m => {
  if (!m) { viewerStore.setOpenImage(null); return }
  viewerStore.setOpenImage({
    projectUid, imageUid,
    valueName: m.valueName ?? valueName.value ?? '',
    zarrPath: m.zarrPath ?? '',
    taskDir: m.taskDir ?? '',
    nLevels: m.levels?.length ?? 1,
  })
})

// The panel is now the single version picker (VIEWER_CONTROLS_SPLIT_PLAN.md P3 extended). Its
// `<select>` writes `cc.viewerImageVersion` via `settings.setImageVersion`; the popup's own copy
// of the settings ref rehydrates through the storage bridge (P2). Watch the getter so a panel
// change here calls `changeVersion` internally — no picker in this window, but the version still
// updates. Skip when the value matches (initial rehydrate on mount, echo from our own writes).
watch(() => settings.getImageVersion(imageUid), vn => {
  if (vn && vn !== valueName.value) void changeVersion(vn)
})

/**
 * Switch version. Everything in flight is for the OLD pixels, so it is abandoned rather than allowed
 * to land in the new textures — a slab that arrives after the switch has the right shape and the
 * wrong content, which renders as a plausible image of something else.
 */
async function changeVersion(vn: string) {
  if (vn === valueName.value) return
  pump.cancel()
  tilePump.cancel()
  for (const ac of aborts.values()) ac.abort()
  aborts.clear(); inflight.clear()
  for (const ac of tileAborts.values()) ac.abort()
  tileAborts.clear()
  shownT.value = -1
  announce.value = true
  hits.value = 0; misses.value = 0
  waitingFor.value = -1
  valueName.value = vn
  try { await loadVersion(false) }
  catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    vlog('error', 'Viewer version ' + vn + ': ' + error.value)
    starting.value = ''
  }
}

async function start() {
  heldAfterCrash.value = false
  error.value = ''
  try {
    starting.value = 'Checking the GPU'
    // ASKED BEFORE ANYTHING IS BUILT. `probeWebGpu` never throws — every failure is a report field —
    // so an adapter that cannot do what the viewer needs becomes a sentence here rather than a blank
    // canvas three steps later. It is the same probe as Settings → Diagnostics, so the two can never
    // disagree about this machine.
    const probe = await probeWebGpu()
    if (probe.verdict === 'unavailable') {
      error.value = probe.reason
      vlog('error', 'Viewer will not start: ' + probe.reason)
      starting.value = ''
      return
    }
    if (probe.verdict === 'reduced') vlog('warn', 'Viewer: ' + probe.reason)

    // The breadcrumb goes down BEFORE the device is created, because that is the line the driver dies
    // on. Cleared when a frame is on screen, not here.
    markViewerAttempt(imageUid)
    // `loadVersion(true)` does the whole "meta → renderer → setImage → PY restore → first fetch"
    // sequence. It logs the adapter (via `reallocate` → `ensureRenderer`) and kicks off overlays,
    // tracks and (for whole-slide) the overview thumbnail. Same code the picker's version-swap uses.
    await loadVersion(true)
    const active = renderer.value ?? tileRenderer.value
    if (active) {
      const named = adapterNameText(active.adapter.name)
      const gpuDetail = `maxTextureDimension3D=${active.adapter.maxTextureDimension3D}, `
        + `timestamp-query=${active.adapter.hasTimestamps}` + (named ? '' : ', adapter reports no name')
      const gpuLine = 'Viewer GPU: ' + (named
        || (active.adapter.looksDiscrete ? 'looks discrete' : 'looks integrated'))
      vlog(active.adapter.looksDiscrete ? 'info' : 'warn', gpuLine, gpuDetail)
      console.info(gpuLine, gpuDetail)
    }
  } catch (e) {
    error.value = e instanceof WebGpuUnavailable
      ? e.message + ' — the viewer needs WebGPU'
      : (e instanceof Error ? e.message : String(e))
    vlog('error', 'Viewer failed to start: ' + error.value,
         e instanceof Error ? e.stack : undefined)
    clearViewerAttempt()          // it failed in a way we could catch: not the crash this guards
    starting.value = ''
  }
}

onMounted(() => {
  if (!projectUid || !imageUid) { error.value = 'No image — open this window from the viewer panel'; return }
  if (viewerCrashedLastTime(imageUid)) {
    heldAfterCrash.value = true
    void probeWebGpu().then(p => {
      heldProbe.value = p.reason + (adapterNameText(p.name) ? ' — ' + adapterNameText(p.name) : '')
      vlog(p.verdict === 'ready' ? 'info' : 'warn', 'Viewer held after a crash: ' + heldProbe.value)
    })
    return
  }
  void start()
})

/**
 * Keyboard, on the WINDOW rather than the canvas: this is a bare popup whose whole content is the
 * viewer, so demanding that the canvas be focused first would make the shortcuts feel broken. Skipped
 * while a field or a slider has focus, or space would type into a text box and the arrows would fight
 * the slider they are meant to be an alternative to.
 */
function onKey(e: KeyboardEvent) {
  const el = e.target as HTMLElement | null
  if (el && (el.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(el.tagName))) return
  if (e.key === ' ') { e.preventDefault(); togglePlay(); return }
  const step = e.key === 'ArrowRight' ? 1 : e.key === 'ArrowLeft' ? -1 : 0
  if (step === 0 || nT.value <= 1) return
  e.preventDefault()
  stopPlay()
  gotoT(Math.max(0, Math.min(nT.value - 1, t.value + step)))
}
onMounted(() => window.addEventListener('keydown', onKey))

/**
 * `?bench=1`: watch `PerformanceResourceTiming` entries for slab responses and add their
 * transferred bytes to the recorder. This catches BOTH renderers without a hook in either —
 * flat fetches, brick image bricks and brick label bricks all hit `/api/viewer/slab`. Kept
 * outside the resource-timing buffer default (which is bounded); listen live and read
 * `transferSize` per entry. `transferSize` is zero for a cache hit, which is the honest
 * network cost (repeat scrubs shouldn't inflate the fetch tally).
 */
let benchPerfObs: PerformanceObserver | null = null
onMounted(() => {
  // Attached unconditionally: bytes-fetched tallies for the Debug panel's Bytes readout, which
  // is visible even when the bench harness toggle is off. The observer filters to slab URLs so
  // it doesn't count other network traffic; overhead is a callback per slab response.
  try {
    benchPerfObs = new PerformanceObserver(list => {
      let sum = 0
      for (const e of list.getEntries()) {
        if (!(e instanceof PerformanceResourceTiming)) continue
        if (!e.name.includes('/api/viewer/slab')) continue
        sum += e.transferSize || e.encodedBodySize || 0
      }
      if (sum > 0) benchBytes.value += sum
    })
    benchPerfObs.observe({ type: 'resource', buffered: false })
  } catch (e) {
    vlog('warn', 'Bench: PerformanceObserver unavailable',
         e instanceof Error ? e.message : String(e))
  }
})
onUnmounted(() => {
  if (benchPerfObs) { benchPerfObs.disconnect(); benchPerfObs = null }
})

// The pop manager (in the main window) writes `pop.show` to the server and pings a localStorage
// key; this listener is the popup's side of the P5 bridge. The tick's value is `<imageUid>:<ts>`,
// so this window only refetches on changes to the image IT shows — a viewer on image A stays
// still when the user gates image B in the main window. See
// docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P5.
function onOverlaysTick(e: StorageEvent) {
  if (e.key !== 'cc.viewerOverlaysTick' || !e.newValue) return
  const [uid] = e.newValue.split(':')
  if (uid !== imageUid) return
  // The gating store writes `cc.gatingCurrent` BEFORE the tick, so re-read the current selection
  // here rather than in a separate storage listener — one storage event, one refetch.
  gatingCurrent.value = readGatingCurrent()
  void loadOverlays()
  // The panel's track eye also fires this ping (see ViewerPanel.vue `toggleTrack`), so refetching
  // per-vn track payloads here is what actually gets ribbons on screen after a toggle. Cheap when
  // the set of ticked vns is unchanged — `loadTracks` reuses the cache and only fetches new vns.
  void loadTracks()
}
// Cross-window mode sync — the gating toolbar's pencil writes `cc.viewerSelectMode`; here we
// mirror it into the local ref so the pointer path sees the update without a Pinia round trip.
function onSelectModeTick(e: StorageEvent) {
  if (e.key !== 'cc.viewerSelectMode') return
  selectModeActive.value = readSelectMode()
}

// A task rewrote a label store on disk (e.g. segment, correction). If it's THIS window's mask, the
// cached slabs are stale — force a reallocate. `labelName` didn't change, so its own watcher never
// fires. Payload: `<imageUid>:<valueName>:<ts>`. Guards on both uid AND valueName so an unrelated
// segmentation's task doesn't refetch this window's pixels.
function onSlabsTick(e: StorageEvent) {
  if (e.key !== 'cc.viewerSlabsTick' || !e.newValue) return
  const [uid, vn] = e.newValue.split(':')
  if (uid === imageUid && vn === labelName.value) reallocate()
}
// Tell the main window which image THIS popup is on, so the ViewerPanel's per-set controls (pop
// toggles etc) key off the FOCUSED popup's image — not whichever image the ImageTable eye was last
// clicked. Mirrors the napari path, where `viewerImageUid` is set by napari's WS `open` event so
// panel + napari always agree on WHICH image the toggles govern. Written on mount + on every focus
// so switching between several open popups follows attention (Dominik, 2026-08-31: "popup shows
// dots despite panel off" — panel keyed to M2b, popup to fXgbTl).
function publishViewerFocus() {
  if (imageUid && typeof localStorage !== 'undefined') {
    // Timestamp suffix so repeat focuses on the same popup fire the storage event too — the browser
    // only fires on VALUE CHANGE, and re-focusing the fXgbTl popup after clicking M2b's eye needs
    // to bounce the panel back to fXgbTl even though the payload is unchanged.
    localStorage.setItem('cc.viewerFocus', `${imageUid}:${Date.now()}`)
  }
}
onMounted(() => {
  window.addEventListener('storage', onOverlaysTick)
  window.addEventListener('storage', onSlabsTick)
  window.addEventListener('storage', onSelectModeTick)
  window.addEventListener('focus', publishViewerFocus)
  publishViewerFocus()
})

onUnmounted(() => {
  window.removeEventListener('keydown', onKey)
  window.removeEventListener('storage', onOverlaysTick)
  window.removeEventListener('storage', onSlabsTick)
  window.removeEventListener('storage', onSelectModeTick)
  window.removeEventListener('focus', publishViewerFocus)
  stopPlay()
  pump.cancel()
  zPump.cancel()
  tilePump.cancel()
  for (const ac of aborts.values()) ac.abort()
  for (const ac of tileAborts.values()) ac.abort()
  renderer.value?.destroy()
  tileRenderer.value?.destroy()
})
</script>

<template>
  <div class="vw">
    <div class="vw-canvas-wrap">
      <canvas
        ref="canvas" class="vw-canvas" :class="{ 'vw-canvas-select': selectModeActive }"
        @pointerdown="onDown" @pointermove="onMove" @pointerup="onUp" @pointercancel="onUp"
        @wheel="onWheel"
      />
      <!-- Rubber-band selection rectangle in select mode. Absolute-positioned inside the canvas
           wrap so its coordinates are in the same space as the pointer events. Non-interactive so
           it doesn't intercept the pointerup that ends the drag. -->
      <div v-if="dragRect" class="vw-select-rect"
           :style="{ left: dragRect.x + 'px', top: dragRect.y + 'px',
                     width: dragRect.w + 'px', height: dragRect.h + 'px' }" />
      <!-- `chrome="fixed"`: on a full-bleed interactive canvas the still's proportional sizing renders a
           35 px label that also changes size as you zoom. The bar's LENGTH is physical either way. -->
      <StillOverlay
        v-if="meta && shownT >= 0" :extent-um="overlayExtent" :time-label="timeLabel" chrome="fixed"
        :show-scale-bar="settings.viewerScaleBar" :show-timestamp="settings.viewerTimestamp"
        :bar-font-px="settings.viewerScaleBarPx" :time-font-px="settings.viewerTimestampPx"
      />
      <!-- Held after a crash — centred, needs attention. Offered rather than refused: the breadcrumb
           cannot tell a driver crash from a force-quit, so the honest statement is what it saw. -->
      <div v-if="heldAfterCrash" class="cc-empty cc-empty-overlay cc-muted-warn">
        The last attempt to show this image did not finish
        <span v-if="heldProbe" class="cc-muted cc-fs-2xs">{{ heldProbe }}</span>
        <button class="cc-btn cc-btn-primary" @click="start"
                v-tooltip.top="'Open this image again'">Try again</button>
      </div>
      <!-- Rendered, not displayed. Above `starting`/`error` because it outlives both: the load
           succeeded, nothing threw, and the canvas is still blank. -->
      <div v-else-if="displayFault" class="cc-empty cc-empty-overlay cc-muted-warn">
        This browser is drawing the image but not displaying it
        <span class="cc-muted cc-fs-2xs">A browser or graphics-driver fault, not this image.</span>
        <span class="cc-muted cc-fs-2xs">Try another browser, or route this one to the discrete GPU.</span>
      </div>
      <!-- Startup, mid-load, and errors ALL go in a bottom-left chip: white-on-black stays legible
           against any tile content, and out of the way of the pointer. The error variant carries the
           canonical severity icon and colour (`lib/severity.ts`) — grey-on-image was unreadable
           against a bright whole-slide render, and a centred error left the app looking dead when it
           was still holding the previous frame (Dominik, 2026-08-26). -->
      <div v-else-if="error" class="vw-status-chip vw-status-chip-error">
        <i class="pi pi-exclamation-triangle vw-status-chip-icon" />
        <span>{{ error }}</span>
        <button v-if="lostDevice" class="cc-btn cc-btn-ghost cc-btn-micro vw-status-chip-btn"
                @click="reload" v-tooltip.top="'Reopen the viewer'">Reload</button>
      </div>
      <!-- The timepoint ASKED FOR, not a literal 0: this overlay is not only the first load. A 2D/3D
           switch clears every texture, so it comes back at whatever timepoint the slider is on, and a
           hardcoded 0 there said the wrong thing (Dominik, 2026-08-24). -->
      <div v-else-if="starting" class="vw-status-chip">{{ starting }}…</div>
      <!-- "Loading timepoint N" is a lie for a still image — say what's actually being waited on. -->
      <div v-else-if="shownT < 0" class="vw-status-chip">
        {{ nT > 1 ? `Loading timepoint ${t}…` : 'Loading image…' }}
      </div>
      <!-- Bricks path only: the canvas is out of sync with the target timepoint. Two shapes:
           STALE (hold-on-cold keeps displayT on the last-good t while the scheduler chases
           the new one — canvas shows an OLDER frame than the user scrubbed to) and PARTIAL
           (unblank rule advanced displayT before every core brick landed — target frame with
           EMPTY_SLOT holes). Same amber chip in the "Loading timepoint…" slot; gated on
           `shownT >= 0` so it never fights the initial-load message. -->
      <div v-else-if="canvasPartial" class="vw-status-chip vw-status-chip-warn"
           v-tooltip.top="'The canvas is not yet showing every brick for the current timepoint'">
        <i class="pi pi-exclamation-triangle vw-status-chip-icon" />
        <span>Loading bricks…</span>
      </div>
      <!-- Overview minimap. Offered for any 2D plane view — not just whole-slide tile mode — because
           a small image still benefits from a corner reference while zoomed in (Dominik, 2026-08-26).
           Canvas holds the tissue thumbnail (fetched once); SVG on top holds the viewport rect and
           takes all the pointer events (click/drag to reposition). -->
      <div v-if="overviewShown && mode === 'plane' && meta && overviewRect" class="vw-overview-wrap"
           :style="{width: overviewSize.w + 'px', height: overviewSize.h + 'px'}">
        <canvas ref="overviewCanvas" class="vw-overview-tissue" />
        <svg class="vw-overview-svg"
             :viewBox="`0 0 ${overviewSize.w} ${overviewSize.h}`"
             preserveAspectRatio="none"
             @pointerdown="onOverviewDown" @pointermove="onOverviewMove"
             @pointerup="onOverviewUp" @pointercancel="onOverviewUp">
          <rect x="0" y="0" :width="overviewSize.w" :height="overviewSize.h" class="vw-overview-bg" />
          <rect :x="overviewRect.x * overviewSize.w" :y="overviewRect.y * overviewSize.h"
                :width="Math.max(2, overviewRect.w * overviewSize.w)"
                :height="Math.max(2, overviewRect.h * overviewSize.h)"
                class="vw-overview-vp" />
        </svg>
      </div>
    </div>

    <!-- The controls sidebar is a CollapsiblePanel — one handle folds it away, the strip on its
         left edge drags to resize, and the width persists per `storage-key`. Same primitive the
         module pages use for their right panel, so the affordances are one shared thing rather
         than a viewer-only fourth. The COLLAPSE flag is a viewer-window own field
         (`viewerWindowSideCollapsed`), not the app-wide `rightPanelCollapsed` — the viewer's
         controls and the module page's task list hold different things, and sharing meant every
         collapse fold both. -->
    <CollapsiblePanel storage-key="viewerWindowSide" label="viewer controls"
                      :default-width="290" :min="240" :max="480"
                      collapsed-key="viewerWindowSideCollapsed">
      <div class="vw-side">
      <div class="vw-title cc-fs-sm">{{ imageName || imageUid }}</div>
      <TeleportPopover v-model="keysOpen" :anchor="keysBtn" placement="bottom-end">
        <div class="cc-eyebrow cc-fs-2xs">Shortcuts</div>
        <!-- Table with a column per mode: same gesture does different things in pan vs select
             (Dominik, 2026-08-26). The current row's active-mode column is highlighted so a user
             can read "what does drag do RIGHT NOW" in one glance. -->
        <table class="vw-keys">
          <thead>
            <tr class="cc-muted cc-fs-3xs">
              <th></th>
              <th :class="{ 'vw-keys-active': !selectModeActive }">Pan</th>
              <th :class="{ 'vw-keys-active': selectModeActive }">Select</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="s in SHORTCUTS" :key="s.keys">
              <td><kbd class="cc-fs-3xs vw-kbd">{{ s.keys }}</kbd></td>
              <td class="cc-muted cc-fs-2xs" :class="{ 'vw-keys-active': !selectModeActive }">{{ s.pan }}</td>
              <td class="cc-muted cc-fs-2xs" :class="{ 'vw-keys-active': selectModeActive }">{{ s.select }}</td>
            </tr>
          </tbody>
        </table>
      </TeleportPopover>
      <!-- Advanced viewer popover — Renderer + brick Quality tier. Trigger is the sidebar's
           gear (`advancedTrigger` above). Renderer flip persists in `settings.viewerBricksMode`
           and forces a full reallocate via the existing watcher (short canvas flash). Quality
           tier writes `settings.viewerBrickTier` and rides through `effectiveMaxIntersect` +
           watcher — no reallocate, applies within a few scheduler ticks. Tier hidden when the
           effective renderer is Flat (`?brickThr=` overriding is still visible in the label).
           URL `?bricks=` / `?brickThr=` win over both controls; the tier chip shows disabled
           with the URL value in its tooltip in that case, so the user isn't quietly ignored. -->
      <TeleportPopover v-model="advancedOpen" :anchor="advancedTrigger" placement="bottom-end">
        <div class="cc-eyebrow cc-fs-2xs">Advanced viewer</div>
        <div class="vw-adv-body">
          <div class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="'Auto picks based on movie size vs cache'">Renderer</span>
            <ChipSelect
              :options="BRICKS_MODES" :model-value="settings.viewerBricksMode"
              variant="segmented" aria-label="Renderer"
              @update:model-value="v => (settings.viewerBricksMode = v as 'auto' | 'brick' | 'flat')"
            />
          </div>
          <div class="cc-fs-3xs vw-adv-using cc-sev-ok">{{ effectiveRendererLabel }}</div>
          <div class="cc-row cc-row-tight" v-if="bricksEnabled">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="brickKnobThrFromUrl
                    ? `?brickThr=${brickKnobThr} overrides the tier`
                    : 'Caps bricks per view — limits detail at wide zoom'">Quality</span>
            <ChipSelect
              :options="BRICK_TIERS" :model-value="settings.viewerBrickTier"
              variant="segmented" aria-label="Brick quality tier"
              :disabled="brickKnobThrFromUrl"
              @update:model-value="v => (settings.viewerBrickTier = v as 'quick' | 'balanced' | 'detailed')"
            />
          </div>
          <div class="cc-muted cc-fs-3xs vw-adv-note" v-else>
            Quality tier applies to the Brick renderer.
          </div>
          <!-- Cache size — one budget for both renderers. Flat uses it as its timepoint-cache
               ceiling; brick uses it as its atlas ceiling. Auto = 1500 MB, the pre-setting default.
               `?cacheMB=N` in the URL disables the chip and shows the override in its tooltip. -->
          <div class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="cacheMBFromUrl
                    ? `?cacheMB=${cacheMBUrl} overrides the setting`
                    : 'VRAM the viewer may hold — bigger = smoother scrub'">Cache</span>
            <ChipSelect
              :options="CACHE_MB_OPTIONS" :model-value="cacheMBAsString"
              variant="segmented" aria-label="Viewer cache size"
              :disabled="cacheMBFromUrl"
              @update:model-value="v => (settings.viewerCacheMB = v === 'auto' ? -1 : Number(v))"
            />
          </div>
          <div class="cc-fs-3xs vw-adv-using" :class="`cc-sev-${cacheSeverity}`">{{ effectiveCacheMBLabel }}</div>
        </div>
      </TeleportPopover>
      <!-- Which VERSION is on screen — read-only chip. The picker lives in the main-window
           ViewerPanel now (VIEWER_CONTROLS_SPLIT_PLAN.md P3 extended); a change there reaches this
           window via the storage bridge and calls `changeVersion` internally. -->
      <div v-if="valueName" class="cc-muted cc-fs-2xs vw-version-name"
           v-tooltip.bottom="'Set the version in the Viewer panel of the main window'"
           :title="valueName">{{ valueName }}</div>

      <div v-if="activeAdapter && !activeAdapter.looksDiscrete" class="cc-muted-warn cc-fs-2xs"
           v-tooltip.bottom="'The browser picked the integrated GPU — expect much slower frames'">
        Integrated GPU
      </div>

      <template v-if="meta">
        <div class="cc-eyebrow cc-fs-2xs">View</div>
        <div class="cc-row cc-row-tight">
          <ChipSelect
            :options="MODES" :model-value="mode" variant="segmented" aria-label="View mode"
            @update:model-value="v => onModeChange(v as 'plane' | 'volume')"
          />
          <!-- 3D projection: Ortho (default) matches the offline movie renderer's parallel-ray MIP
               and reads head-on; Persp adds foreshortening. Hidden in 2D because the plane view is
               always orthographic (perspective would foreshorten a flat plane). Imaris analogue. -->
          <ChipSelect
            v-if="mode === 'volume'"
            :options="PROJECTIONS" :model-value="settings.viewerVolumeProjection"
            variant="segmented" aria-label="3D projection"
            @update:model-value="v => (settings.viewerVolumeProjection = v as 'ortho' | 'persp')"
          />
          <!-- Mode indicator + toggle. Pencil = SELECT mode (click picks cells), arrows = PAN mode
               (click does nothing, drag pans/rotates). Same knob the pop-manager pencil writes so the
               user can stay in the viewer without reaching back to the module page (Dominik,
               2026-08-26). Icon shows the CURRENT mode, not the ACTION. -->
          <button class="cc-btn cc-btn-ghost cc-btn-icon"
                  :class="{ 'cc-btn-on cc-btn-on-tint': selectModeActive }"
                  @click="toggleSelectMode"
                  v-tooltip.top="selectModeActive
                    ? 'Selection mode — click for pan mode'
                    : 'Pan mode — click for selection mode'"
                  aria-label="Toggle selection mode">
            <i :class="selectModeActive ? 'pi pi-pencil' : 'pi pi-arrows-alt'" />
          </button>
          <!-- Advanced viewer popover — Renderer + brick quality tier. Grouped with the mode
               toggle so all viewer-wide controls sit next to the 2D/3D chip. -->
          <button ref="advancedTrigger" class="cc-btn cc-btn-ghost cc-btn-icon"
                  aria-label="Advanced viewer settings"
                  v-tooltip.top="'Renderer and brick quality'"
                  @click="advancedOpen = !advancedOpen">
            <i class="pi pi-sliders-h" />
          </button>
          <!-- Shortcuts sits at the far right — a reference popover, not a live control, so it
               reads as separate from the mode/renderer group (Dominik 2026-08-31). -->
          <div class="vw-grow" />
          <button ref="keysBtn" class="cc-btn cc-btn-ghost cc-btn-icon" @click="keysOpen = !keysOpen"
                  v-tooltip.left="'Mouse and keyboard shortcuts'" aria-label="Shortcuts">
            <i class="pi pi-question-circle" />
          </button>
        </div>
        <!-- The 3D view's own depth control. Caption row (label + readout) above; slider on its
             own row so it can span the sidebar (Dominik 2026-08-31: "they should take the whole
             width"). `@change`, not `@update:*`: the range reallocates every cached texture, so
             it commits on release rather than per pointer move. -->
        <template v-if="mode === 'volume' && meta.nZ > 1">
          <div class="vw-cap">
            <span class="cc-muted cc-fs-2xs">Depth</span>
            <span class="cc-readout cc-fs-2xs">{{ zRange[0] }}–{{ zRange[1] }}</span>
          </div>
          <RangeSlider
            v-tooltip.top="'Planes to project — fewer is faster, in proportion'"
            :lo="zRange[0]" :hi="zRange[1]" :min="0" :max="Math.max(meta.nZ - 1, 0)" :step="1"
            @update:lo="v => (zRange = [v, zRange[1]])"
            @update:hi="v => (zRange = [zRange[0], v])"
            @change="reallocate()"
          />
        </template>
        <!-- 3D pyramid level. napari also renders 3D at the coarsest resolution, and a full-res volume
             of a wide-XY image exceeds the WebGPU max buffer (`f8gzA2` → 1.28 GB against a 256 MB cap).
             So auto = the deepest level; the dropdown lets a user step finer if their card can hold it.
             `@change` (not `@update:*`) so it commits on release, same discipline as Depth. -->
        <div v-if="mode === 'volume' && (meta.levels?.length ?? 0) > 1" class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Level</span>
          <select v-model.number="settings.viewerVolumeLevel" class="vw-grow"
                  v-tooltip.top="'Pyramid resolution — lower = finer, but bigger'"
                  @change="reallocate()">
            <!-- 3D-mode Auto: the dropdown is a FLOOR the SSE picker clamps against. On the brick
                 renderer the atlas's active level moves with zoom; show it in the label so the
                 3D control matches the 2D one's live readout (Dominik 2026-08-29). Flat renderer:
                 `brickCurrentLevel` is undefined and the label collapses to plain "Auto". -->
            <option :value="-1">Auto{{ bricksEnabled && brickCurrentLevel !== undefined
              ? ` (L${brickCurrentLevel} — zoom-driven)` : '' }}</option>
            <option v-for="lv in meta.levels" :key="lv.level" :value="lv.level">
              L{{ lv.level }} — {{ lv.nX }}×{{ lv.nY }}
            </option>
          </select>
        </div>
        <!-- 2D pyramid level. Different policy from 3D: auto is ZOOM-DRIVEN — the level whose native
             pixel is closest to (without going finer than) one device pixel, so we never ship pixels
             the screen cannot show. At fit-to-window on a 20k×17k image that is L4 or L5; as the user
             zooms in past a `floor(log2)` threshold the level drops and the textures reallocate. The
             dropdown lets a user pin a specific level, same as the 3D control. Phase B of
             VIEWER_TILES_PLAN.md. Only shown when there IS a pyramid to pick from. -->
        <div v-if="mode === 'plane' && (meta.levels?.length ?? 0) > 1" class="cc-row cc-row-tight">
          <span class="cc-muted cc-fs-2xs cc-lbl-col">Level</span>
          <select v-model.number="settings.viewerPlaneLevel" class="vw-grow"
                  v-tooltip.top="'Pyramid resolution — auto picks by camera zoom'"
                  @change="reallocate()">
            <option :value="-1">Auto (L{{ slabLevel }} — zoom-driven)</option>
            <option v-for="lv in meta.levels" :key="lv.level" :value="lv.level">
              L{{ lv.level }} — {{ lv.nX }}×{{ lv.nY }}
            </option>
          </select>
        </div>
        <template v-if="mode === 'plane' && meta.nZ > 1">
          <div class="vw-cap">
            <span class="cc-muted cc-fs-2xs">Plane</span>
            <span class="cc-readout cc-fs-2xs">{{ zPlane }} / {{ meta.nZ - 1 }}</span>
          </div>
          <input
            type="range" class="vw-grow" :min="0" :max="meta.nZ - 1" :step="1"
            :value="zPlane" @input="stepZ(Number(($event.target as HTMLInputElement).value))"
            v-tooltip.bottom="'Which z plane to show — changing it reloads the timecourse'"
          >
        </template>

        <!-- No time = no time controls. A still image has nothing to scrub, buffer or loop, and an
             `nT == 1` slider stuck at "0 / 0" looks broken. -->
        <template v-if="nT > 1">
          <div class="vw-cap">
            <span class="cc-eyebrow cc-fs-2xs">Timepoint</span>
            <span class="cc-readout cc-fs-2xs">{{ t }} / {{ nT - 1 }}</span>
          </div>
          <div class="cc-row cc-row-tight">
            <button class="cc-btn cc-btn-ghost cc-btn-icon" @click="togglePlay"
                    v-tooltip.bottom="playing ? 'Pause' : 'Play through the timecourse'">
              <i class="pi" :class="playing ? 'pi-pause' : 'pi-play'" />
            </button>
            <input
              type="range" class="vw-grow" :min="0" :max="nT - 1" :step="1"
              :value="t" @pointerdown="stopPlay()"
              @input="gotoT(Number(($event.target as HTMLInputElement).value))"
              v-tooltip.bottom="'Scrub the timecourse — cached timepoints are instant'"
            >
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
        </template>

        <!-- Compact controls block: toggles column on the left, Reset view button on the right with
             the tile/brick residency map slotted directly under it. Reset used to sit BETWEEN Loop
             and Overview, and the residency maps landed at the bottom after every other section,
             so the reset action was in the middle of a row of switches and the map wasn't visibly
             tied to Reset's siblings (Dominik 2026-08-31). Left column carries a compact `Fps`
             control too — no need to spend a full sidebar row on a 1–30 slider. -->
        <div class="vw-compact">
          <div class="vw-compact-left">
            <div v-if="nT > 1" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col"
                    :class="{ 'vw-fps-warn': playing && waitingFor >= 0 }">Fps</span>
              <input
                type="range" class="vw-grow" :min="1" :max="30" :step="1"
                v-model.number="settings.viewerFps"
                v-tooltip.bottom="'Playback rate — waits rather than skip an uncached frame'"
                aria-label="Playback rate (fps)"
              >
              <!-- Amber readout is the throttled cue; carry the state tooltip HERE — the label /
                   slider have the plain description so a hover on either still explains the
                   control (`uiCopy.ts` requires an `<input>` to carry its own tooltip). -->
              <span class="cc-readout cc-fs-2xs vw-fps-val"
                    :class="{ 'vw-fps-warn': playing && waitingFor >= 0 }"
                    v-tooltip.left="playing && waitingFor >= 0
                      ? 'Playback throttled — fetches are behind the requested Fps'
                      : 'Requested playback rate'">{{ settings.viewerFps }}</span>
            </div>
            <div v-if="nT > 1" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col"
                    v-tooltip.right="'Restart from the first timepoint at the end'">Loop</span>
              <CcToggle v-model="settings.viewerLoop" aria-label="Loop playback" />
            </div>
            <!-- Overview minimap, offered for any 2D plane view — small images benefit too once
                 you zoom in. Volume mode has no minimap: a rotated MIP has no useful "where am I"
                 answer. -->
            <div v-if="mode === 'plane'" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col"
                    v-tooltip.right="'Show a small overview in the corner — click to jump'">Overview</span>
              <CcToggle v-model="overviewShown" aria-label="Show the overview minimap" />
            </div>
            <div v-if="tileMapGrid" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col"
                    v-tooltip.right="'Tile cache — blue is loaded, amber is fetching'">Tiles</span>
              <CcToggle v-model="tilesMapShown" aria-label="Show the tile cache map" />
            </div>
            <div v-if="brickMapGrid" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col"
                    v-tooltip.right="'Brick cache — blue is loaded, amber is fetching'">Bricks</span>
              <CcToggle v-model="bricksMapShown" aria-label="Show the brick cache map" />
            </div>
          </div>
          <div class="vw-compact-right">
            <button class="cc-btn cc-btn-ghost vw-reset"
                    @click="resetView"
                    v-tooltip.top="'Face the volume square to the screen again'">Reset view</button>
            <!-- Tile residency mini map: the spatial analog of the timecourse strip above. One
                 cell per tile at the current level; blue = in the atlas, amber = fetching, empty
                 = absent. Only shown when its toggle in the left column is on. -->
            <div v-if="tilesMapShown && tileMapGrid" class="vw-tilemap"
                 :style="{ gridTemplateColumns: `repeat(${tileMapGrid.nTx}, 1fr)`,
                           gridTemplateRows: `repeat(${tileMapGrid.nTy}, 1fr)`,
                           aspectRatio: `${tileMapGrid.nTx} / ${tileMapGrid.nTy}` }">
              <span v-for="c in tileMapCellsView" :key="c.key"
                    class="vw-tilemap-cell" :class="'is-' + c.state" />
            </div>
            <!-- Brick residency mini map: 3D analog of the tile map. One nBx × nBy grid per Z
                 slice at the CURRENT level + timepoint. Same colour language as the tile map
                 (blue = resident, amber = fetching). -->
            <div v-if="bricksMapShown && brickMapGrid" class="vw-brickmaprow">
              <div v-for="s in brickMapSlices.slices" :key="s.z" class="vw-brickmap-col">
                <div class="vw-tilemap vw-brickmap-slice"
                     :style="{ gridTemplateColumns: `repeat(${brickMapSlices.displayNBx}, 1fr)`,
                               gridTemplateRows: `repeat(${brickMapSlices.displayNBy}, 1fr)`,
                               aspectRatio: `${brickMapSlices.displayNBx} / ${brickMapSlices.displayNBy}` }">
                  <span v-for="c in s.cells" :key="c.key"
                        class="vw-tilemap-cell" :class="'is-' + c.state" />
                </div>
                <!-- Only meaningful when there are MULTIPLE Z slices to disambiguate — in 2D plane
                     mode `nBz === 1` so a single "z0" label just added a row of vertical space
                     under the grid with nothing to compare it against (Dominik 2026-08-31). -->
                <span v-if="brickMapSlices.slices.length > 1"
                      class="cc-muted cc-fs-3xs vw-brickmap-zlabel">z{{ s.z }}</span>
              </div>
            </div>
          </div>
        </div>

        <!-- Annotations sits with the viewport controls above rather than beside the layer sections
             below: scale bar + timestamp are burnt into the render, they are not a layer whose
             visibility/colour/opacity you tune. Layer-list order (Channels / Segmentation / Overlays)
             is what changes while you look; Annotations is set once. See
             docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md (P1: sort what exists). -->
        <CollapsibleSection label="Annotations" tip="Scale bar and timestamp burnt into the view"
                            :open="openSection === 'ann'"
                            @update:open="v => setSection('ann', v)" max-height="none">
          <!-- Toggle and text size share a row: the size is only ever adjusted with the thing it sizes
               in front of you, and a separate row for each would double the group's height. -->
          <div class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="'Physical scale of the current zoom'">Scale bar</span>
            <CcToggle v-model="settings.viewerScaleBar" aria-label="Show the scale bar" />
            <!-- The size slider appears WITH the thing it sizes. A control you cannot move is noise in
                 a panel this dense (Dominik, 2026-08-25) — it says "there is something here" and then
                 refuses. Nothing is lost: the toggle beside it is how you get the slider back. -->
            <template v-if="settings.viewerScaleBar">
              <input
                type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
                v-model.number="settings.viewerScaleBarPx"
                v-tooltip.bottom="'Scale-bar text size'" aria-label="Scale bar text size"
              >
              <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerScaleBarPx }}</span>
            </template>
          </div>
          <div class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="'Elapsed time, or the frame index if uncalibrated'">Timestamp</span>
            <CcToggle v-model="settings.viewerTimestamp" aria-label="Show the timestamp" />
            <template v-if="settings.viewerTimestamp">
              <input
                type="range" class="vw-grow vw-px" :min="8" :max="32" :step="1"
                v-model.number="settings.viewerTimestampPx"
                v-tooltip.bottom="'Timestamp text size'" aria-label="Timestamp text size"
              >
              <span class="cc-readout cc-fs-3xs vw-px-val">{{ settings.viewerTimestampPx }}</span>
            </template>
          </div>
        </CollapsibleSection>

        <!-- Channels list scrolls INSIDE the section (default max-height, not `none`) — a whole-slide
             image has 24+ channels and the sidebar's own scroll never engages, so the section body was
             flooding off the bottom of the panel with no way to reach the controls below (Dominik,
             2026-08-26). All-on/all-off buttons at the top so a 24-channel image can be soloed to one
             marker in two clicks instead of 23. -->
        <CollapsibleSection label="Channels" tip="Colour and contrast per channel"
                            :open="openSection === 'channels'"
                            @update:open="v => setSection('channels', v)">
          <!-- One canonical toggle (docs/ui/PRIMITIVES.md → CcToggle), same idiom as every other
               on/off in the app — a pair of buttons was a second variant of a decision that already
               has one right way to render it. A little breathing room below so the master row does
               not run into the first channel card (Dominik, 2026-08-26). -->
          <div class="cc-row cc-row-tight vw-ch-master">
            <span class="cc-muted cc-fs-2xs cc-lbl-col">All channels</span>
            <CcToggle :model-value="allChannelsVisible" @update:model-value="setAllChannels"
                      aria-label="Toggle every channel" />
            <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="autoAllContrast"
                    v-tooltip.left="'Auto contrast on every channel'"
                    aria-label="Auto contrast every channel">
              <i class="pi pi-sliders-h" />
            </button>
            <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="resetAllContrast"
                    v-tooltip.left="'Reset every channel to the server default'"
                    aria-label="Reset every channel contrast">
              <i class="pi pi-history" />
            </button>
          </div>
          <div class="cc-row cc-row-tight vw-ch-master">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="'Golden-angle hue rotation — quick visual separation of every marker'">
              Distinct
            </span>
            <CcToggle v-model="distinctChannelColours"
                      aria-label="Assign a distinct colour to each channel" />
            <!-- Spacer that occupies the same slot as the reset button in the row above, so the two
                 toggles line up in the same column. Non-interactive, hidden from a11y. -->
            <span class="vw-ch-master-slot" aria-hidden="true" />
          </div>
          <div v-for="(ch, c) in meta!.channels.slice(0, MAX_CHANNELS)" :key="c" class="vw-ch cc-card cc-card-2">
            <div class="cc-row cc-row-tight">
              <span class="vw-ch-name cc-fs-xs"
                    v-tooltip.right="'Show this channel in the composite'">{{ ch.name }}</span>
              <!-- P7.1: says which channels are reading from the AF preview scratch store rather than
                   the source image, so a corrected/uncorrected mixup is not silent. Click to A/B:
                   suspend the swap on THIS channel (badge dims, channel reads source) while the
                   other corrected channels stay swapped; click again to re-arm. Rendered whenever an
                   AF correction exists for this channel, so a dimmed badge tells the user WHY the
                   channel looks uncorrected. Resets on every new AF run. -->
              <button v-if="hasAfPreview(c)"
                      :class="['vw-ch-af-badge cc-fs-3xs', { 'vw-ch-af-badge-off': afSuspended.has(c) }]"
                      @click="toggleAfSuspended(c)"
                      v-tooltip.top="afSuspended.has(c)
                        ? 'Reading SOURCE pixels — click to switch back to the AF correction'
                        : 'Reading corrected pixels — click to compare against the source'">AF</button>
              <ColourPicker
                :model-value="channelHex(ch)" :palette="CHANNEL_PALETTE" :tip="'Colour for ' + ch.name"
                @update:model-value="v => setChannelColour(c, v)"
              />
              <CcToggle v-model="ch.visible" :aria-label="'Show ' + ch.name" @update:modelValue="pushChannels" />
              <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="autoContrast(c)"
                      v-tooltip.left="'Auto contrast — window on the loaded pixels (ImageJ-style)'">
                <i class="pi pi-sliders-h" />
              </button>
              <button class="cc-btn cc-btn-bare cc-btn-icon cc-btn-micro" @click="resetContrast(c)"
                      v-tooltip.left="'Reset contrast to the server default'">
                <i class="pi pi-history" />
              </button>
            </div>
            <!-- RangeSlider is a flex-ROW item by construction (`flex: 1`, i.e. `flex-basis: 0`), so in a
                 column it collapses to no height and its absolutely-positioned thumbs escape the card.
                 Every other consumer wraps it in a row with a readout beside it; so does this one. -->
            <div class="cc-row cc-row-tight">
              <RangeSlider
                v-tooltip.top="'Contrast window — values outside it clip'"
                :lo="ch.lo" :hi="ch.hi" :min="0" :max="Math.max(chMax[c] ?? 1, ch.hi, 1)" :step="1"
                @update:lo="v => { ch.lo = v; pushChannels() }"
                @update:hi="v => { ch.hi = v; pushChannels() }"
              />
              <span class="cc-readout cc-fs-3xs vw-ch-val">{{ ch.lo }}–{{ ch.hi }}</span>
            </div>
          </div>
          <div v-if="clipped" class="cc-muted-warn cc-fs-2xs">
            Showing {{ MAX_CHANNELS }} of {{ meta!.nC }} channels
          </div>

        </CollapsibleSection>
        <CollapsibleSection label="Segmentation" tip="Draw a segmentation mask over the image"
                            :open="openSection === 'seg'"
                            @update:open="v => setSection('seg', v)" max-height="none">
          <!-- No picker: locked decision 3 — the viewer has no selectors. WHICH segmentation is shown
               is decided in the ViewerPanel per image and reaches this window via the P2
               storage-event bridge. The row below just SHOWS what's on and offers opacity + contour
               for it. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P3.
               `labelNames` is the server's directory check, not the label registry, so an imported
               track set with a table and no mask does not offer a phantom row.
               Empty state: same shape as Populations and Tracks — one-liner "No X shown — action in
               the viewer panel" using .cc-empty-inline, so the three sections read coherently
               (Dominik, 2026-08-26). -->
          <template v-if="labelName">
            <div class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Mask</span>
              <span class="cc-fs-2xs vw-grow" :title="labelName">{{ labelName }}</span>
            </div>
            <!-- More than one ticked: only the first renders because the compositor's bind group has
                 one label slot. Multi-mask rendering is a later phase; naming the limit here is the
                 alternative to silently dropping the others. -->
            <div v-if="shownLabelCount > 1" class="cc-muted-warn cc-fs-3xs">
              {{ shownLabelCount }} segmentations ticked — showing {{ labelName }} only
            </div>
            <div class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Opacity</span>
              <input
                type="range" class="vw-grow" :min="0" :max="1" :step="0.05"
                v-model.number="settings.viewerLabelOpacity" @input="frame.redraw()"
                v-tooltip.bottom="'How strongly the mask covers the signal'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerLabelOpacity.toFixed(2) }}</span>
            </div>
            <div class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Outline</span>
              <input
                type="range" class="vw-grow" :min="0" :max="5" :step="1"
                v-model.number="settings.viewerLabelContour" @input="frame.redraw()"
                v-tooltip.bottom="'Outline width in voxels — 0 fills each cell'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerLabelContour || 'fill' }}</span>
            </div>
            <div v-if="mode === 'volume'" class="cc-muted cc-fs-3xs">
              3D shows the nearest mask surface
            </div>
          </template>
          <div v-else class="cc-empty-inline cc-fs-2xs">
            No mask shown — tick one in the viewer panel
          </div>

        </CollapsibleSection>
        <CollapsibleSection label="Populations" tip="Gated cell populations drawn as coloured points"
                            :open="openSection === 'pops'"
                            @update:open="v => setSection('pops', v)" max-height="none">
          <!-- Populations. Only when there is something to say: an unsegmented image has no cell table
               and no populations, and an empty group would read as a broken feature rather than as an
               image that has not been through segmentation yet.
               Panel gate off = the whole section reads as inactive rather than as a cell-count summary
               (Dominik, 2026-08-26: "how you can show a pops stats when the pops toggle in the viewer
               controls is off").
               Empty-state phrasing lines up with Segmentation and Tracks (Dominik, 2026-08-26). -->
          <template v-if="!popsPanelOn">
            <div class="cc-empty-inline cc-fs-2xs">
              No populations shown — enable in the viewer panel
            </div>
          </template>
          <template v-else-if="summary.cells > 0 || overlaysErr">
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
                  :model-value="!hiddenPops.has(pop.path)"
                  v-tooltip.bottom="'Draw this population over the image'"
                  :aria-label="'Show ' + pop.name" @update:modelValue="togglePop(pop.path)"
                />
              </div>
              <!-- No colour-by picker: locked decision 3. The CHOICE lives in ViewerPanel's Colour by
                   section, keyed per set. This row shows what it resolved to; the legend below shows
                   its scale. See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P4. -->
              <div class="cc-row cc-row-tight">
                <span class="cc-muted cc-fs-2xs cc-lbl-col">Colour by</span>
                <span class="cc-fs-2xs vw-grow" :title="colourBy || 'population'">
                  {{ colourBy || 'population' }}
                </span>
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

              <div v-if="mode === 'plane' && meta!.nZ > 1" class="cc-row cc-row-tight">
                <span class="cc-muted cc-fs-2xs cc-lbl-col">Z reach</span>
                <input
                  type="range" class="vw-grow" :min="0" :max="10" :step="1"
                  v-model.number="settings.viewerPointZTol" @input="frame.redraw()"
                  v-tooltip.bottom="'Planes either side that still show a cell — 0 is the exact plane'"
                >
                <span class="cc-readout cc-fs-2xs vw-num">±{{ settings.viewerPointZTol }}</span>
              </div>
              <div class="cc-row cc-row-tight">
                <span class="cc-muted cc-fs-2xs cc-lbl-col">Point size</span>
                <input
                  type="range" class="vw-grow" :min="2" :max="24" :step="1"
                  v-model.number="pointSize" @input="frame.redraw()"
                  v-tooltip.bottom="'Marker size on screen, not in µm'"
                >
                <span class="cc-readout cc-fs-2xs vw-num">{{ pointSize }}</span>
              </div>
              <div class="cc-row cc-row-tight">
                <span class="cc-muted cc-fs-2xs cc-lbl-col">Border</span>
                <input
                  type="range" class="vw-grow" :min="0" :max="6" :step="1"
                  v-model.number="pointBorder" @input="frame.redraw()"
                  v-tooltip.bottom="'Black outline around every point (0 = none)'"
                >
                <span class="cc-readout cc-fs-2xs vw-num">{{ pointBorder }}</span>
              </div>
              <div class="cc-muted cc-fs-3xs">
                <template v-if="overlays!.valueName">{{ overlays!.valueName }} · </template>
                {{ pointCount }} drawn · {{ summary.cells }} cells
                <template v-if="summary.dropped">· {{ summary.dropped }} without a centroid</template>
                <template v-if="mode === 'plane'">· this plane only</template>
              </div>
            </template>
          </template>

        </CollapsibleSection>
        <CollapsibleSection label="Tracks" tip="Track ribbons from each ticked segmentation"
                            :open="openSection === 'tracks'"
                            @update:open="v => setSection('tracks', v)" max-height="none">
          <!-- Tracks. The per-segmentation "directions" eye in the ViewerPanel ticks vns on; this
               section colours + shapes the ribbons. Empty state names both the "nothing ticked" case
               and the "ticked but no tracked cells" case rather than showing an empty block. -->
          <template v-if="segCount > 0">
            <!-- Per-pop ribbon rows: one line per gated pop with tracks (isTrack || hasTracks),
                 mirroring the Populations section above (swatch/name/count/toggle). The eye hides
                 the ribbon layer WITHOUT touching the point layer's eye — `hiddenTrackPops` is a
                 separate set from `hiddenPops` (MULTI_POP_TRACKING_PLAN.md Decision 5). -->
            <div v-for="pop in trackDrawablePops" :key="pop.path" class="cc-row cc-row-tight">
              <span class="vw-swatch" :style="{ background: pop.colour }" />
              <span class="cc-fs-2xs vw-pop-name" :title="pop.path">{{ pop.name }}</span>
              <span class="cc-readout cc-fs-3xs">{{ pop.count }}</span>
              <CcToggle
                :model-value="!hiddenTrackPops.has(pop.path)"
                v-tooltip.bottom="'Draw this population as a track ribbon layer'"
                :aria-label="'Show tracks for ' + pop.name" @update:modelValue="toggleTrackPop(pop.path)"
              />
            </div>
            <div class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Colour by</span>
              <select v-model="trackColorMode" class="cc-fs-2xs vw-grow"
                      v-tooltip.bottom="'How ribbons are coloured'">
                <option value="track">track</option>
                <option value="speed">speed</option>
                <option value="solid">source</option>
              </select>
            </div>
            <!-- Numeric ramp for speed mode — same shape as the point colour-by legend, so the reading
                 stays consistent across overlay kinds. µm per frame (Δt = 1 hop). -->
            <div v-if="trackColorMode === 'speed' && trackSpeedRange"
                 class="cc-row cc-row-tight cc-fs-3xs">
              <span class="cc-muted">{{ trackSpeedRange[0].toPrecision(3) }}</span>
              <span class="vw-ramp" :style="rampStyle" />
              <span class="cc-muted">{{ trackSpeedRange[1].toPrecision(3) }} µm/frame</span>
            </div>
            <!-- Per-source legend for solid mode — one row per ticked vn, showing a clickable
                 swatch (the shared ColourPicker) + count. Same picker + palette as the population
                 manager, so a source's colour authored here matches the visual language elsewhere.
                 Doubles as a diagnostic: a vn with count 0 would be ticked but have no tracked
                 cells; buildMultiTrackBuffer filters those out already, so the legend only lists
                 sources that actually drew. -->
            <template v-if="trackColorMode === 'solid' && trackSources.length">
              <div v-for="src in trackSources" :key="src.vn" class="cc-row cc-row-tight cc-fs-3xs">
                <ColourPicker :model-value="src.hex"
                              @update:model-value="hex => setTrackSourceColour(src.vn, hex)"
                              :tip="'Colour for ' + src.vn" />
                <span class="cc-fs-2xs vw-pop-name" :title="src.vn">{{ src.vn }}</span>
                <span class="cc-readout cc-fs-3xs">{{ src.count }}</span>
              </div>
            </template>
            <div v-if="mode === 'plane' && meta!.nZ > 1" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Z reach</span>
              <input
                type="range" class="vw-grow" :min="0" :max="10" :step="1"
                v-model.number="settings.viewerTrackZTol" @input="frame.redraw()"
                v-tooltip.bottom="'Planes either side that still show a tail'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">±{{ settings.viewerTrackZTol }}</span>
            </div>
            <div class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Tail</span>
              <input
                type="range" class="vw-grow" :min="0" :max="60" :step="1"
                v-model.number="settings.viewerTailLength" @input="frame.redraw()"
                v-tooltip.bottom="'Track history in frames — 0 hides the tails'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerTailLength }}</span>
            </div>
            <div v-if="settings.viewerTailLength > 0" class="cc-row cc-row-tight">
              <span class="cc-muted cc-fs-2xs cc-lbl-col">Tail width</span>
              <input
                type="range" class="vw-grow" :min="1" :max="12" :step="1"
                v-model.number="settings.viewerTailWidth" @input="frame.redraw()"
                v-tooltip.bottom="'Tail thickness on screen, not in µm'"
              >
              <span class="cc-readout cc-fs-2xs vw-num">{{ settings.viewerTailWidth }}</span>
            </div>
            <div class="cc-muted cc-fs-3xs">
              {{ segCount }} segments · {{ trackSources.length }} source{{ trackSources.length === 1 ? '' : 's' }}
            </div>
          </template>
          <div v-else class="cc-empty-inline cc-fs-2xs">
            No tracks shown — tick a segmentation in the viewer panel
          </div>

        </CollapsibleSection>
        <CollapsibleSection label="Debug" tip="Render knobs, live perf and diagnostics"
                            :open="openSection === 'debug'"
                            @update:open="v => setSection('debug', v)" max-height="none">
          <!-- ── Controls (throttles first): the knobs that shape everything the readouts below
               measure. Steps + Keep for volume mode, then three toggles. Eyebrow matches the
               readouts below so the whole panel reads as one column of labelled sub-blocks. -->
          <div class="cc-eyebrow cc-fs-2xs vw-debug-head">Render</div>
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
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="'Composite the canvas as opaque — same pixels, different browser path'">Opaque</span>
            <CcToggle :model-value="opaqueCanvas" aria-label="Composite the canvas as opaque"
                      @update:modelValue="v => { opaqueCanvas = v; frame.redraw(); void checkFill() }" />
          </div>
          <div class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="'Fill the canvas with one colour and draw nothing — is the canvas shown at all?'">Test fill</span>
            <CcToggle :model-value="testPattern" aria-label="Fill the canvas with a test colour"
                      @update:modelValue="v => { testPattern = v; frame.redraw(); void checkFill() }" />
          </div>
          <div class="cc-row cc-row-tight">
            <span class="cc-muted cc-fs-2xs cc-lbl-col"
                  v-tooltip.right="'Uncap perf arrays for a full-session Save (default is a 200-frame rolling snapshot)'">Bench</span>
            <CcToggle v-model="benchEnabled" aria-label="Bench harness — uncap perf arrays" />
          </div>

          <!-- ── Perf — always-on p50/p95 for CPU draw + GPU render pass (brick only for GPU).
               Under `?bench=1` the arrays grow unbounded (Save covers the full session); otherwise
               they roll over a fixed window. -->
          <div class="cc-eyebrow cc-fs-2xs vw-debug-head">
            Perf <span class="cc-muted cc-fs-3xs">· {{ bricksEnabled ? 'brick' : 'flat' }}</span>
            <span v-if="benchEnabled" class="vw-rec-pill cc-fs-3xs"
                  v-tooltip.top="'Bench on: uncapped arrays, Save covers the whole session'">● REC</span>
          </div>
          <div class="vw-bench-grid cc-fs-3xs">
            <span class="cc-muted">First frame</span>
            <span>{{ benchFirstFrameMs !== null ? benchFirstFrameMs.toFixed(0) + ' ms' : '—' }}</span>
            <span class="cc-muted">Frames</span>
            <span>{{ benchLive.nFrames }}
              <span v-if="benchEnabled" class="vw-rec cc-fs-3xs">(session)</span>
              <span v-else class="cc-muted cc-fs-3xs">(rolling {{ PERF_RING }})</span>
            </span>
            <span class="cc-muted" v-tooltip.left="'CPU-side draw() submission — GPU still runs after we return'">Draw p50/p95</span>
            <span>
              <template v-if="benchLive.drawMedianMs !== null">
                {{ benchLive.drawMedianMs.toFixed(2) }} / {{ benchLive.drawP95Ms!.toFixed(2) }} ms
              </template>
              <template v-else>—</template>
            </span>
            <template v-if="bricksEnabled">
              <span class="cc-muted" v-tooltip.left="'GPU render pass p50/p95 (raycast + overlays)'">GPU p50/p95</span>
              <span :class="benchLive.gpuSummary?.gpuFrameMs95 != null
                  ? (benchLive.gpuSummary.gpuFrameMs95 > 33 ? 'vw-gpu-fail'
                    : benchLive.gpuSummary.gpuFrameMs95 > 16 ? 'vw-gpu-warn' : '')
                  : ''">{{ benchLive.gpuSummary?.gpuFrameMs50 != null ? benchLive.gpuSummary.gpuFrameMs50.toFixed(2) + ' / ' + benchLive.gpuSummary.gpuFrameMs95!.toFixed(2) + ' ms' : (benchLive.gpuSummary ? 'n/a' : '—') }}</span>
              <span class="cc-muted" v-tooltip.left="'CPU split p50·p95: scheduler tick / page-table upload / encoder+submit'">CPU tick/pt/es</span>
              <span>
                <template v-if="benchLive.gpuSummary">
                  {{ benchLive.gpuSummary.tickSchedulerCpuMs50.toFixed(2) }}·{{ benchLive.gpuSummary.tickSchedulerCpuMs95.toFixed(2) }}
                  / {{ benchLive.gpuSummary.writePageTableCpuMs50.toFixed(2) }}·{{ benchLive.gpuSummary.writePageTableCpuMs95.toFixed(2) }}
                  / {{ benchLive.gpuSummary.encoderSubmitCpuMs50.toFixed(2) }}·{{ benchLive.gpuSummary.encoderSubmitCpuMs95.toFixed(2) }} ms
                </template>
                <template v-else>—</template>
              </span>
            </template>
            <span class="cc-muted">Bytes</span>
            <span>{{ (benchBytes / 1e6).toFixed(1) }} MB</span>
          </div>

          <!-- ── Cache / bricks — mode-conditional state. -->
          <template v-if="bricksEnabled">
            <div class="cc-eyebrow cc-fs-2xs vw-debug-head">Bricks</div>
            <div class="vw-bench-grid cc-fs-3xs">
              <span class="cc-muted">Level</span>
              <span>{{ brickCurrentLevel !== undefined ? 'L' + brickCurrentLevel : '—' }}</span>
              <span class="cc-muted">Cam</span>
              <span>d {{ cam.dist.toFixed(0) }} / p {{ cam.panX.toFixed(0) }},{{ cam.panY.toFixed(0) }}</span>
              <span class="cc-muted" v-tooltip.left="'missing@dis: bricks the shader wants at displayT · missing@bnd: bricks needed at boundT'">Bricks</span>
              <span>{{ brickResidentsAtLevel }} res / {{ brickInflightAtLevel }} inflight / {{ brickMissing }}@dis / {{ brickMissingAtBoundT }}@bnd</span>
              <span class="cc-muted" v-tooltip.left="'displayT (drawn) → boundT (scheduler target); divergence = hold-on-cold'">t</span>
              <span>{{ brickDisplayT }} → {{ brickBoundT }}</span>
              <span class="cc-muted" v-tooltip.left="'?brickThr=N · ?brickBias=N · ?brickHold=0|1'">Knobs</span>
              <span>thr {{ effectiveMaxIntersect }}{{ brickKnobThrFromUrl ? '' : ` (${settings.viewerBrickTier})` }} · bias {{ brickKnobBias }} · hold {{ brickKnobHold ? 'on' : 'off' }}</span>
            </div>
          </template>
          <template v-else>
            <div class="cc-eyebrow cc-fs-2xs vw-debug-head">Cache</div>
            <div class="vw-bench-grid cc-fs-3xs">
              <span class="cc-muted">Timepoints</span>
              <span>{{ resident.length }} / {{ gpu.capacity }}{{ gpu.capped ? ' (GPU limit)' : '' }}</span>
              <span class="cc-muted">Per-t</span>
              <span>{{ (gpu.bytesPerTimepoint / 1e6).toFixed(1) }} MB</span>
              <span class="cc-muted" v-tooltip.left="'Cache hits / misses / last miss cost'">Hits</span>
              <span>{{ hits }} / {{ misses }}<template v-if="lastMissMs"> · miss {{ lastMissMs }} ms</template></span>
              <template v-if="timing">
                <span class="cc-muted">Last fetch</span>
                <span>{{ timing.fetchMs }} ms (server {{ timing.serverMs }}) · upload {{ timing.uploadMs }} ms</span>
              </template>
            </div>
          </template>

          <!-- ── Image — dims, level, MB/channel, contrast. -->
          <div class="cc-eyebrow cc-fs-2xs vw-debug-head">Image</div>
          <div class="vw-bench-grid cc-fs-3xs">
            <span class="cc-muted">Dims</span>
            <span>{{ meta!.nX }} × {{ meta!.nY }} × {{ meta!.nZ }} · {{ meta!.nT }} t · {{ meta!.nC }} ch</span>
            <span class="cc-muted">Level</span>
            <span>
              <template v-if="slabLevel > 0">L{{ slabLevel }} @ {{ renderNX }}×{{ renderNY }}</template>
              <template v-else>L0</template>
              · {{ (renderNX * renderNY * gpu.zDepth * meta!.bytesPerVoxel / 1e6).toFixed(1) }} MB/ch
            </span>
            <span class="cc-muted">Contrast</span>
            <span>{{ meta!.contrastSource }}</span>
          </div>

          <!-- ── Shader — what the WGSL saw on the last frame. -->
          <div v-if="shader" class="cc-eyebrow cc-fs-2xs vw-debug-head">Shader</div>
          <div v-if="shader" class="vw-bench-grid cc-fs-3xs">
            <span class="cc-muted">Box</span>
            <span>{{ shader.ext[0].toFixed(0) }} × {{ shader.ext[1].toFixed(0) }} × {{ shader.ext[2].toFixed(1) }} µm · {{ shader.nch }} ch</span>
            <span class="cc-muted">Camera</span>
            <span>{{ shader.dist.toFixed(0) }} µm · pan {{ shader.pan[0].toFixed(0) }},{{ shader.pan[1].toFixed(0) }} · {{ shader.steps }} step{{ shader.steps === 1 ? '' : 's' }} · {{ shader.ortho ? 'ortho' : 'perspective' }}</span>
            <span class="cc-muted">Canvas</span>
            <span>{{ shader.canvas[0] }} × {{ shader.canvas[1] }}</span>
            <template v-if="probe">
              <span class="cc-muted">Shader probe</span>
              <span>{{ (probe.lit * 100).toFixed(1) }}% lit · {{ (probe.max * 100).toFixed(1) }}% max</span>
            </template>
            <template v-if="canvasProbe">
              <span class="cc-muted">Canvas probe</span>
              <span>{{ (canvasProbe.lit * 100).toFixed(1) }}% lit · {{ (canvasProbe.max * 100).toFixed(1) }}% max</span>
            </template>
          </div>

          <!-- ── Save / Reset — always visible. Save downloads the current perf arrays plus the
               shader/bricks/image/knobs snapshot; when Bench is off that's the 200-frame rolling
               window, when Bench is on it's the full session. Reset clears the arrays either way. -->
          <div class="cc-row cc-row-tight vw-bench-btns">
            <button class="cc-btn cc-btn-bare cc-btn-micro" @click="benchReset"
                    v-tooltip.top="benchEnabled ? 'Restart the session capture' : 'Clear the rolling window'">Reset</button>
            <button class="cc-btn cc-btn-primary cc-btn-micro" @click="benchSave"
                    v-tooltip.top="benchEnabled ? 'Download the full session as JSON' : 'Download the rolling snapshot as JSON'">Save</button>
          </div>
        </CollapsibleSection>
      </template>
      </div>
    </CollapsiblePanel>
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
.vw-version-name { width: 100%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vw-key { white-space: nowrap; }
/* Shortcuts table — gesture rows × mode columns. Highlight the active mode's column so a user
   reads the CURRENT effect first; the other column stays legible as a reference for the swap. */
/* Caption row above a full-width slider — label on left, readout on right. Compact so it
   reads as a slider title, not another row of controls (Dominik 2026-08-31). */
.vw-cap { display: flex; align-items: baseline; justify-content: space-between; gap: 0.4rem; }
/* Resolved-value caption under a control in the Advanced popover: sits just under the chip, one
   line, "Using: X" — Dominik 2026-08-31: an Auto option must show what was picked. */
.vw-adv-using { margin: -0.15rem 0 0.15rem calc(var(--cc-lbl-col) + 0.4rem); }
.vw-adv-using.cc-sev-ok { color: var(--cc-sev-ok); }
.vw-adv-using.cc-sev-warn { color: var(--cc-sev-warn); }
.vw-adv-body { display: flex; flex-direction: column; gap: 0.4rem; margin-top: 4px; min-width: 16rem; }
.vw-adv-note { padding: 0.2rem 0 0.1rem; }
.vw-keys { border-collapse: collapse; margin-top: 4px; }
.vw-keys th, .vw-keys td { padding: 3px 8px 3px 0; text-align: left; vertical-align: middle; white-space: nowrap; }
.vw-keys th { font-weight: normal; }
.vw-keys .vw-keys-active { color: var(--cc-text); font-weight: 500; }
.vw-keys th.vw-keys-active { color: var(--cc-accent-strong); }
.vw-kbd { flex: none; min-width: 6.5rem; padding: 0.1rem 0.3rem; border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs); background: var(--cc-surface-2); font-family: inherit; }
.vw-ramp { flex: 1; min-width: 2rem; height: 0.5rem; border-radius: var(--cc-radius-xs); }
.vw-canvas-wrap { position: relative; flex: 1; min-width: 0; }
/* Bottom-left status chip: white-on-black so it stays legible against a bright tile composite. Same
   role as the centred `cc-empty-overlay` but out of the way of what the user is trying to look at.
   Error variant uses the canonical severity colour + icon (`lib/severity.ts` → `fail`), and re-enables
   `pointer-events` so the Reload button (device-lost case) is clickable. */
.vw-status-chip {
  position: absolute; left: 0.75rem; bottom: 0.75rem;
  padding: 0.3rem 0.55rem; border-radius: var(--cc-radius-xs);
  background: rgba(0, 0, 0, 0.78); color: #fff;
  font-size: var(--cc-fs-xs); pointer-events: none;
  display: inline-flex; align-items: center; gap: 0.35rem; max-width: calc(100% - 1.5rem);
}
.vw-status-chip-error { color: var(--cc-sev-fail); pointer-events: auto; }
.vw-status-chip-warn { color: var(--cc-sev-warn); pointer-events: auto; }
.vw-status-chip-icon { font-size: 1em; }
.vw-status-chip-btn { margin-left: 0.35rem; color: #fff; }
/* Overview minimap — top-right of the canvas. Same visual language as the status chip (dark
   translucent panel) so it reads as a peer overlay. Cursor is `crosshair` because a click is a
   navigation, not a select. */
.vw-overview-wrap {
  position: absolute; top: 0.75rem; right: 0.75rem;
  background: rgba(0, 0, 0, 0.78); border: 1px solid var(--cc-border);
  border-radius: var(--cc-radius-xs); overflow: hidden;
  cursor: crosshair; touch-action: none;
}
.vw-overview-tissue {
  position: absolute; inset: 0; width: 100%; height: 100%;
  /* Downscaled from the deepest pyramid level; smoothing looks fine at tissue scale. */
  image-rendering: auto;
  pointer-events: none;
}
.vw-overview-svg { position: absolute; inset: 0; width: 100%; height: 100%; }
/* Faint tint over the (possibly empty) canvas so the frame stays visible before the thumbnail
   arrives. Once tissue is drawn behind, it shows through. */
.vw-overview-bg { fill: rgba(255, 255, 255, 0.04); }
.vw-overview-vp { fill: rgba(255, 255, 255, 0.10); stroke: var(--cc-accent); stroke-width: 1.5; }
/* No background: the renderer clears to black, and the overlay covers the pre-first-frame gap. */
.vw-canvas { display: block; width: 100%; height: 100%; cursor: grab; touch-action: none; }
.vw-canvas:active { cursor: grabbing; }
/* Crosshair when the viewer is in selection mode — the cursor is the honest indicator that a click
   will pick a cell rather than pan. Same intent as the mode icon in the sidebar, right at the
   pointer where the user is looking. */
.vw-canvas-select { cursor: crosshair; }
.vw-canvas-select:active { cursor: crosshair; }
/* Rubber-band rectangle overlay for a select-mode drag. Bright enough to see over any content,
   but semi-transparent so the cells underneath stay readable. Non-interactive so it never eats
   the pointerup that ends the gesture (that would strand the drag). */
.vw-select-rect {
  position: absolute; pointer-events: none;
  border: 1px solid var(--cc-accent-strong);
  background: color-mix(in srgb, var(--cc-accent-strong) 15%, transparent);
}
.vw-side {
  /* Fills the CollapsiblePanel slot; width and left border come from the panel. Padding + overflow
     stay here — the panel deliberately owns no padding so its consumers pad their own root. */
  flex: 1; min-width: 0; padding: 0.6rem;
  /* x:hidden, y:auto — leaving x at the default `visible` lets the RangeSlider's thumb overhang
     and any tight row visually escape past the sidebar's right edge (Dominik, 2026-08-26). Only
     the vertical axis needs to scroll. */
  overflow: hidden auto;
  display: flex; flex-direction: column; gap: 0.35rem;
}
/* Every row is its own height — no vertical shrinking. In a tall column with more content than
   fits, the default `flex-shrink: 1` was compressing every row (the 2D/3D segmented toggle,
   Reset view button, brick-map grid) instead of letting `.vw-side` scroll (Dominik, 2026-08-29).
   `CollapsibleSection` already sets `flex-shrink: 0` on its own root; this covers everything else.
   `flex-grow: 0` for the mirror case: the Depth range slider (`.rs` has `flex: 1` so it grows in
   its intended horizontal `.cc-row` parent) and the Plane / Fps `<input type=range>` (`.vw-grow`
   is `flex: 1` for the same horizontal fill) are direct children of `.vw-side` in the current
   template. In a column flex parent, `flex-grow: 1` stretches them along the VERTICAL axis,
   leaving huge gaps between the caption and the slider, and the slider and the next control
   (Dominik, 2026-08-31). The `.vw-side > .vw-grow`/`.rs` overrides carry a third simple selector
   so they beat `.vw-grow { flex: 1 }` on specificity — under Vue scoped compilation only the LAST
   simple selector gets the `[data-v-…]` suffix, so `.vw-side > *` and `.vw-grow` tied at (0,2,0)
   and declaration order let `.vw-grow` win, which is why the earlier `.vw-side > * { flex-grow: 0 }`
   didn't actually take (Dominik 2026-08-31, follow-up). */
.vw-side > * { flex-shrink: 0; flex-grow: 0; }
.vw-side > .vw-grow, .vw-side > .rs { flex-grow: 0; }
.vw-title { font-weight: 600; word-break: break-word; }
.vw-ch { padding: 0.35rem 0.4rem; display: flex; flex-direction: column; gap: 0.2rem;
  /* RangeSlider now self-contains its thumbs, but keep the belt on: any content that outgrows the
     card (a stray-wide readout, a wrapping row) is clipped rather than poking past the card border
     (Dominik, 2026-08-26). */
  min-width: 0; overflow: clip; }
.vw-ch :deep(.rs) { min-width: 0; }
.vw-ch-name { flex: 1; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.vw-ch-af-badge { flex: none; padding: 0 0.35rem; border-radius: var(--cc-radius-pill);
  border: 1px solid var(--cc-accent); background: transparent; color: var(--cc-accent);
  font-weight: 600; letter-spacing: 0.02em; cursor: pointer; line-height: 1.2; }
.vw-ch-af-badge:hover { background: color-mix(in srgb, var(--cc-accent) 15%, transparent); }
.vw-ch-af-badge-off { border-color: var(--cc-border); color: var(--cc-text-dim); text-decoration: line-through; }
.vw-ch-af-badge-off:hover { background: color-mix(in srgb, var(--cc-text-dim) 12%, transparent); }
/* The thumbs are centred on their value, so half of one overhangs at either end of the rail. Room for
   that, or they sit on the card's border. */
.vw-ch { padding-left: 0.7rem; padding-right: 0.7rem; }
.vw-ch-master { margin-bottom: 0.35rem; }
/* Force EXACT label width in the master rows, so a longer word ("Distinct colours") does not push
   its toggle further right than the row above ("All channels"). Base `.cc-lbl-col` is a min-width
   that lets the label grow; the whole point of these master rows is that the toggles stack. */
.vw-ch-master > .cc-lbl-col {
  width: var(--cc-lbl-col); min-width: var(--cc-lbl-col);
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
/* Same footprint as `cc-btn-micro` icon (`i.pi` inside a bare button, ~1.1rem square). Reserves the
   trailing slot in a row that has no icon, so the toggle in that row lines up with the toggle in the
   row that DOES carry an icon. */
/* TWO icon slots (auto + reset) worth of space, plus the gap between them. Keeps the toggle in the
   Distinct row lined up with the toggle in the All-channels row where two buttons follow it. */
.vw-ch-master-slot { flex: none; width: calc(2 * 1.1rem + 0.35rem); height: 1.1rem; }
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
/* Tile residency mini map — spatial analog of the timecourse strip. Same visual language: cells on
   `--cc-surface-2`, filled `--cc-accent` when resident. Loading is amber (`--cc-sev-warn`) rather
   than the strip's `--cc-accent-tint`, per Dominik: "just blue dots and amber for loading"
   (2026-08-26) — a loading tile in flight has a different meaning from a queued next timepoint. */
.vw-tilemaprow { display: flex; align-items: flex-start; gap: 0.3rem; }
.vw-tilemap {
  display: grid; gap: 1px; flex: 1; min-width: 0;
  max-height: 6rem; background: var(--cc-surface-2);
  padding: 1px; border-radius: var(--cc-radius-xs);
}
.vw-tilemap-cell { background: var(--cc-surface-2); border-radius: var(--cc-radius-xs); }
.vw-tilemap-cell.is-resident { background: var(--cc-accent); }
.vw-tilemap-cell.is-loading { background: var(--cc-sev-warn); }
/* Bench-chip GPU-cost budget hints — color only, no font-size change (the chip uses `cc-fs-3xs`,
   and `cc-muted-warn`/`cc-muted-error` also bump size). Thresholds: p95 > 16 ms = 60 Hz budget
   slipped; p95 > 33 ms = 30 Hz too. Standard fps split. */
.vw-gpu-warn { color: var(--cc-sev-warn); }
.vw-gpu-fail { color: var(--cc-sev-fail); }
/* Brick residency map: one Z slice per column. Slices sit side by side and each carries a small
   z index below. Same cell language as the tile map (blue = resident, amber = fetching). */
.vw-brickmaprow { display: flex; align-items: flex-start; gap: 0.4rem; flex-wrap: wrap; }
.vw-brickmap-col { display: flex; flex-direction: column; align-items: center; gap: 2px; min-width: 0; }
.vw-brickmap-slice { flex: none; min-width: 3rem; max-width: 5rem; }
.vw-brickmap-zlabel { line-height: 1; }
/* Compact controls block — toggles (Fps, Loop, Overview, Tiles, Bricks) stacked on the left,
   Reset view on the right with residency maps nested directly below it. Right column takes its
   own width from the button + map so the left column can grow to fill the rest of the sidebar. */
.vw-compact { display: flex; gap: 0.5rem; align-items: flex-start; }
.vw-compact-left { display: flex; flex-direction: column; gap: 0.25rem; flex: 1; min-width: 0; }
.vw-compact-right { display: flex; flex-direction: column; align-items: center; gap: 0.35rem; flex: 0 0 auto; min-width: 0; }
.vw-reset { white-space: nowrap; }
/* Compact Fps readout — same size language as `.cc-readout` but with a fixed slot so the digits
   don't jitter the slider on every playback tick. */
.vw-fps-val { min-width: 1.4rem; text-align: right; flex: none; }
/* Debug panel readout — two-column grid so labels and values line up without a table. Flush
   with the controls above (no horizontal inset), consistent vertical rhythm. */
.vw-bench-grid {
  display: grid; grid-template-columns: auto 1fr;
  column-gap: 0.5rem; row-gap: 0.15rem;
  padding: 0.1rem 0 0.2rem;
}
/* Every eyebrow inside the Debug section gets the same top rhythm. Scoped via `.vw-debug-head`
   so it doesn't reach the other sidebar sections. The first eyebrow inside CollapsibleSection
   drops its top gap so the section header isn't followed by dead space. */
.vw-debug-head { margin-top: 0.55rem; }
.vw-debug-head:first-child { margin-top: 0.2rem; }
.vw-bench-btns { justify-content: flex-end; gap: 0.3rem; margin-top: 0.55rem; }
/* Bench-recording state visibility. `.vw-rec-pill` sits inline in the Perf eyebrow — a small
   amber dot + "REC" that's hard to miss without stealing space. `.vw-rec` tints the "(session)"
   suffix on the Frames row so the mode is legible from the number itself. */
.vw-rec-pill {
  color: var(--cc-sev-warn); font-weight: 700; letter-spacing: 0.04em;
  margin-left: 0.4rem;
}
.vw-rec { color: var(--cc-sev-warn); }
/* Playback-throttled state: repaint the Fps readout number amber. No extra element, no width
   change — the Fps slider stays the size it was. */
.vw-num.vw-fps-warn { color: var(--cc-sev-warn); font-weight: 600; }
</style>
