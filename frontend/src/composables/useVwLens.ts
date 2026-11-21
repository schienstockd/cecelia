// The viewer window's channel lens (`utils/viewer/channelLens.ts`, `docs/UI.md` → *Channel lens*):
// 2D only, like the readout. It follows the pointer (`lensPointer`, KEPT when the pointer leaves — a
// lens parked on a spot stays there while the Record button is reached) until a click PINS it to the
// tissue (`lensPin`, level-0 px): then a pan or zoom carries it, the chip beside it
// (`VwLensChip.vue`) takes clicks, and a click unpins.
//
// The window keeps owning what the lens counts (its population layers, the frame and planes they are
// drawn for) and passes it in as `counts`; this owns where the lens is, how big, the renderers' copy,
// and its MCP surface (`lensReport` / `placeLens`).
import { computed, ref, watch } from 'vue'
import { useSettingsStore } from '../stores/settings'
import { channelHexFrom } from '../utils/viewerProps'
import { MAX_CHANNELS, type ViewerMeta, type OrbitCamera, type ViewerLens } from '../utils/volumeViewer'
import type { ViewerViewState } from '../utils/viewer/viewState'
import type { LensReadiness } from '../utils/viewer/lensReadiness'
import {
  lensUniform, lensForRecording, lensCircleL0, lensChipCounts, lensRadiusRangeUm, lensRadiusUm,
  lensScreenAt, lensRadiusCss,
} from '../utils/viewer/channelLens'

export interface VwLensDeps {
  meta: () => ViewerMeta | null
  mode: () => 'plane' | 'volume'
  canvas: () => HTMLCanvasElement | null
  canvasSize: () => { w: number; h: number }
  cam: () => OrbitCamera
  /** every renderer alive now — each gets the lens uniform */
  renderers: () => ({ setLens(l: ViewerLens | null): void } | null)[]
  redraw: () => void
  /** push the channel set to the GPU (the tile fetch set changes with the lens) */
  pushChannels: () => void
  /** what the chip counts inside the lens circle (level-0 px) — the window's drawn layers */
  counts: (circle: { x: number; y: number; r: number }) => ReturnType<typeof lensChipCounts>
  /** the layers that add nothing to the count, and why — not drawn or not run (`lensReadiness`) */
  readiness?: () => LensReadiness[]
}

export type LensChip = ReturnType<typeof lensChipCounts> & {
  picked: { name: string; colour: string }[]
  pinned: boolean
  /** a time-lapse: the chip offers This frame / All frames */
  timed: boolean
  /** counting every frame (cells inside at any frame), not the one on screen */
  allFrames: boolean
  /** a layer that adds nothing to the count: not drawn (Show) or not run (its task) */
  ready: LensReadiness[]
}

export function useVwLens(d: VwLensDeps) {
  const settings = useSettingsStore()
  const lensOn = computed(() => settings.viewerLens && d.mode() === 'plane' && !!d.meta())
  const lensPointer = ref<{ cx: number; cy: number } | null>(null)
  const lensPin = ref<{ x: number; y: number } | null>(null)
  /** A time-lapse lens counting every frame, not the one on screen — the chip's counts and Find
   *  similar's lens alike. Per image, saved with the lens (`viewerProps.ts` → `lens.allFrames`). */
  const lensAllFrames = ref(false)

  /** Where the lens is on the canvas, CSS px — the pin through the camera, else the pointer. */
  const lensAt = computed(() => {
    const m = d.meta(), { w, h } = d.canvasSize()
    return lensPin.value && m ? lensScreenAt(lensPin.value, d.cam(), m, w, h) : lensPointer.value
  })
  /** The radius lives on the tissue, µm (`settings.viewerLensRadiusUm`, held in this image's slider
   *  range), so it scales with zoom as the population dots do; `lensRadiusPx` is how big it draws now. */
  const lensRange = computed(() => { const m = d.meta(); return m ? lensRadiusRangeUm(m) : null })
  const lensRadiusUmNow = computed(() => lensRange.value ? lensRadiusUm(settings.viewerLensRadiusUm, lensRange.value) : 0)
  const lensRadiusPx = computed(() => {
    const m = d.meta(), { w, h } = d.canvasSize()
    if (!m || !lensRange.value) return 0
    return lensRadiusCss(lensRadiusUmNow.value / umPerPx(m), d.cam(), m, w, h)
  })
  /** The lens on the tissue now (level-0 px), or null — what the chip counts, Kiwi gets and MCP reports. */
  const lensCircle = computed(() => {
    const m = d.meta(), { w, h } = d.canvasSize()
    return lensOn.value && m ? lensCircleL0(lensAt.value, lensRadiusPx.value, w, h, d.cam(), m) : null
  })

  function trackLens(e: PointerEvent) {
    const c = d.canvas()
    if (!lensOn.value || !c) return
    const rect = c.getBoundingClientRect()
    lensPointer.value = { cx: e.clientX - rect.left, cy: e.clientY - rect.top }
  }
  /** A click with the lens on: pin it where it is, or unpin it back onto the pointer. */
  function toggleLensPin(e: PointerEvent) {
    if (lensPin.value) { lensPin.value = null; trackLens(e); return }
    const c = lensCircle.value
    if (c) lensPin.value = { x: c.x, y: c.y }
  }
  watch(lensOn, on => { if (!on) lensPin.value = null })

  /** Hand the lens to whichever renderer is alive, then redraw. Device px: the canvas's own backing
   *  scale, not `devicePixelRatio` — the drawing buffer is what the shader's positions count. */
  function applyLens() {
    const k = backingScale()
    lensScale = k
    const l = lensOn.value ? lensUniform(lensAt.value, lensRadiusPx.value, k) : null
    for (const r of d.renderers()) r?.setLens(l)
    d.redraw()
  }
  const backingScale = () => { const c = d.canvas(); return c && c.clientWidth > 0 ? c.width / c.clientWidth : 1 }
  /** The scale the lens was last handed over at. A pinned lens does not move, so nothing re-applies it
   *  when the renderer resizes the backing store — on a fresh window (a lens figure's Go to, a saved pin)
   *  the first apply saw the default 300 px canvas and drew the lens at a quarter of its place and size. */
  let lensScale = 1
  /** Every paint, after the renderer's resize: re-apply when the backing store changed under the lens. */
  function syncLensScale() { if (lensOn.value && backingScale() !== lensScale) applyLens() }
  watch([lensOn, lensAt, lensRadiusPx], applyLens)
  // Switching the lens on/off changes the tile fetch set (`tileChannels`) — refetch for the lens ones.
  watch(lensOn, () => d.pushChannels())

  /** The lens channels by name (the Kiwi ref's copy). */
  function lensChannelNames(): string[] {
    return (d.meta()?.channels ?? []).slice(0, MAX_CHANNELS).filter(ch => ch.lens).map(ch => ch.name)
  }

  /** The chip beside the lens: what it draws (its channels, or "Spotlight") and what it holds. */
  const lensChip = computed<LensChip | null>(() => {
    const m = d.meta(), { w, h } = d.canvasSize()
    if (!lensOn.value || !m) return null
    const circle = lensCircle.value
    if (!circle || w <= 0 || h <= 0) return null
    const picked = m.channels.slice(0, MAX_CHANNELS).filter(ch => ch.lens)
      .map(ch => ({ name: ch.name, colour: channelHexFrom(ch.lut) }))
    return { picked, pinned: !!lensPin.value, timed: m.nT > 1, allFrames: m.nT > 1 && lensAllFrames.value,
             ready: d.readiness?.() ?? [], ...d.counts(circle) }
  })

  /** The lens a recording draws, or null (`lensForRecording`). */
  function recordedLens() {
    const m = d.meta(), c = d.canvas()
    if (!lensOn.value || !m || !c) return null
    return lensForRecording(lensAt.value, lensRadiusPx.value, c.clientWidth, c.clientHeight, d.cam(), m, m.channels)
  }

  /** The pin the per-image autosave keeps (`viewerProps.ts` → `lensPin`): only a pinned, shown lens. */
  const savedPin = computed(() => lensOn.value && lensPin.value ? { ...lensPin.value } : undefined)
  /** Restore a saved pin — only while the lens is switched on, so a lens switched off since does not
   *  come back pinned somewhere the next time it is switched on. */
  function restorePin(pin: { x: number; y: number }) {
    if (settings.viewerLens && Number.isFinite(pin.x) && Number.isFinite(pin.y)) lensPin.value = { x: pin.x, y: pin.y }
  }

  /** The lens for an MCP answer: where it is (level-0 px), pinned or on the pointer, what it draws (its
   *  channels; none = the spotlight) and what it holds — the chip's numbers, every group
   *  (`trackClusters`, `regions` only when the chip lists them). */
  function lensReport(): Record<string, unknown> | null {
    const c = lensCircle.value, chip = lensChip.value
    if (!c || !chip) return null
    return { pinned: !!lensPin.value, x: Math.round(c.x), y: Math.round(c.y), r: Math.round(c.r),
             channels: chip.picked.map(p => p.name), cells: chip.cells,
             pops: chip.shown.map(p => ({ name: p.name, n: p.n, share: p.share })), morePops: chip.more,
             ...Object.fromEntries(chip.groups.map(g => [g.kind === 'tracks' ? 'trackClusters' : 'regions', {
               ...(g.kind === 'tracks' ? { tracks: g.of } : {}),
               pops: g.shown.map(p => ({ name: p.name, n: p.n, share: p.share })), morePops: g.more }])),
             // why a group is missing: a layer that is off ("notDrawn") or never run ("notRun") — not "none here"
             ...(chip.ready.length ? { missingLayers: chip.ready.map(r => ({ layer: r.label, state: r.state, task: r.fun })) } : {}) }
  }

  /** `set_viewer_view`'s `lens`: `false` switches it off; `{x, y, r?, channels?}` (level-0 px, channel
   *  names) pins it there. The same lens the screenshot's view reported is left as it is, so a view read
   *  and sent back whole does not pin a lens the user is moving. */
  function placeLens(l: unknown, live: ViewerViewState | null) {
    if (l === false) { settings.viewerLens = false; return }
    if (!l || typeof l !== 'object') return
    const m = d.meta()
    if (!m) return
    if (JSON.stringify(l) === JSON.stringify(live?.lens ?? null)) return
    if (d.mode() !== 'plane') throw new Error('the lens is 2D only — set dims.ndisplay to 2 first')
    const o = l as { x?: unknown; y?: unknown; r?: unknown; channels?: unknown }
    const x = Number(o.x), y = Number(o.y)
    if (!Number.isFinite(x) || !Number.isFinite(y)) throw new Error('lens needs x and y (level-0 px)')
    settings.viewerLens = true
    settings.viewerCursorReadout = false   // the pointer tools are exclusive
    if (Array.isArray(o.channels)) {
      const want = new Set(o.channels.map(String))
      for (const ch of m.channels.slice(0, MAX_CHANNELS)) ch.lens = want.has(ch.name)
      d.pushChannels()
    }
    if (Number.isFinite(Number(o.r)) && Number(o.r) >= 1)   // level-0 px, as the Kiwi schema's `r >= 1`
      settings.viewerLensRadiusUm = lensRadiusUm(Number(o.r) * umPerPx(m), lensRadiusRangeUm(m))
    lensPin.value = { x, y }
  }

  return {
    lensOn, lensPin, lensAllFrames, lensAt, lensRange, lensRadiusUmNow, lensRadiusPx, lensCircle, lensChip,
    trackLens, toggleLensPin, applyLens, syncLensScale, lensChannelNames, recordedLens, savedPin, restorePin,
    lensReport, placeLens,
  }
}

function umPerPx(m: Pick<ViewerMeta, 'voxelUm'>): number {
  return m.voxelUm[0] > 0 ? m.voxelUm[0] : 1
}
