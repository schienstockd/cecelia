<!--
  Image / filmstrip slot for the Analysis board (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase D). One
  slot holding N images (a single image = a 1-cell strip) — for pipeline montages
  (raw → denoised → segmented → tracked). Each cell's image is a napari-screenshot-equivalent capture
  of the popped-out viewer's WebGPU canvas — reached via the exposed `Window.__cceceliaViewerCapture`
  on the same-origin viewer popup. Overlays (tracks / populations / mask outlines) are already baked
  into the canvas by the shader, so what the user sees IS what the strip stores — no server
  re-render, no 2D/3D branching. The PNG uploads to POST /api/board-assets/save and lands as a
  SIDECAR file (settings/board-assets/, served via /api/board-assets) — NOT stored inline — so the
  board JSON stays small and autosaves cheaply; the cell keeps only the assetId + the viewState
  snapshot + imageUid (provenance for zoom-to-source). See docs/todo/ANIMATION_PLAN.md. Orientation
  H/V; separators STRAIGHT (gap + rule) or ANGLED (clip-path parallelograms — cheap because the slot
  stays rectangular, decision 10).
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef, nextTick, onMounted } from 'vue'
import { elementToImageURL } from '../../plots/export'
import TeleportPopover from '../TeleportPopover.vue'
import { useSettingsStore } from '../../stores/settings'
import { useProjectStore } from '../../stores/project'
import { useViewerStore } from '../../stores/viewer'
import type { ViewerViewState } from '../../utils/viewer/viewState'
import { openViewerWindow } from '../../utils/viewerWindow'
import { channelLegend } from '../../utils/viewLegend'
import { elapsedLabel } from '../../utils/stillOverlay'
import { captureViewLegend } from '../../utils/viewerOverlays'
import { parseOverlays, overlayPushConfig } from '../../utils/overlayLayers'
import { getOpenPopoutWindow } from '../../lib/popout'
import StripCell from './StripCell.vue'
import ChipSelect, { type ChipOption } from '../ChipSelect.vue'
import CcToggle from '../CcToggle.vue'

const settings = useSettingsStore()
const project = useProjectStore()
const viewer = useViewerStore()

// `snapshot` (viewer view state) + `imageUid` are the frame's provenance — persisted with the board so
// zoom-to-source can reopen the image and restore the exact camera/contrast/colours months later
// (docs/todo/ANIMATION_PLAN.md). Captured atomically with the screenshot.
// `assetId` → the frame's PNG is a sidecar file (settings/board-assets/), served on demand; NOT stored
// inline, so the board JSON stays small (autosave-friendly). `src` is the legacy inline data-URL, kept
// only for back-compat (migrated to a sidecar on load). `snapshot`+`imageUid` are the view provenance
// for zoom-to-source. See docs/todo/ANIMATION_PLAN.md.
interface ExtentUm { x?: number; y?: number; unit?: string | null }
// captured overlay legend (populations + colour-by), fetched at capture from /api/viewer/overlay-legend
interface OverlaysLegend {
  colourBy?: { column: string; items: { value: string; colour: string; label: string }[] }
  populations?: { name: string; colour: string }[]
}
// `colourBy` = the colour-by measure the overlays were coloured by when captured (not encoded in the
// snapshot's layer names), so zoom-to-source can restore the tracks/pops in the same colours.
// `overlaysLegend` = the pop + colour-by legend for this frame (durable, drawn under the channels).
interface Cell { assetId?: string; src?: string; snapshot?: Record<string, unknown>; imageUid?: string | null; extentUm?: ExtentUm | null; colourBy?: string; overlaysLegend?: OverlaysLegend }
const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  state: { cells?: Cell[]; orientation?: 'h' | 'v'; separator?: 'straight' | 'angled'; sepAngle?: number; sepThick?: number; showLegend?: boolean; showScaleBar?: boolean; showTimestamp?: boolean; legendFontPx?: number; scaleBarFontPx?: number; timestampFontPx?: number }
}>()

// seed defaults into the persisted state bag (the slot starts as {})
if (!props.state.cells) props.state.cells = [{}]
if (!props.state.orientation) props.state.orientation = 'h'
if (!props.state.separator) props.state.separator = 'straight'

const cells = computed(() => props.state.cells!)
const orientation = computed({ get: () => props.state.orientation ?? 'h', set: v => (props.state.orientation = v) })
const separator = computed({ get: () => props.state.separator ?? 'straight', set: v => (props.state.separator = v) })
// angled separators: `skew` = the horizontal lean (angle), `thick` = the white gap width between frames
const skew = computed({ get: () => props.state.sepAngle ?? 22, set: v => (props.state.sepAngle = v) })
const thick = computed({ get: () => props.state.sepThick ?? 2, set: v => (props.state.sepThick = v) })
// optional channel-colour legend, read from the frame's snapshot (viewer layer colormaps). Default ON —
// pre-napari the viewer's own legend was baked into the screenshot; the browser thumbnail carries none,
// so the DOM legend restores that (drawn as a bottom-left chip over the image, captured by PDF export).
const showLegend = computed({ get: () => props.state.showLegend ?? true, set: v => (props.state.showLegend = v) })
// still overlays (E2): a vector scale bar (from the captured frame's physical extent) + an elapsed-time
// timestamp — drawn crisp on the clean capture (the viewer's own hidden via E1). Off by default.
const showScaleBar  = computed({ get: () => props.state.showScaleBar ?? false,  set: v => (props.state.showScaleBar = v) })
const showTimestamp = computed({ get: () => props.state.showTimestamp ?? false, set: v => (props.state.showTimestamp = v) })
// Text-size sliders, matching the viewer's own scale-bar / timestamp sliders (8..32 px). The strip's
// chrome switches to `fixed` mode when set so the user's px choice takes — proportional (fraction of
// extent) is what would ignore these numbers.
const legendFontPx    = computed({ get: () => props.state.legendFontPx    ?? 12, set: v => (props.state.legendFontPx    = v) })
const scaleBarFontPx  = computed({ get: () => props.state.scaleBarFontPx  ?? 14, set: v => (props.state.scaleBarFontPx  = v) })
const timestampFontPx = computed({ get: () => props.state.timestampFontPx ?? 14, set: v => (props.state.timestampFontPx = v) })
// elapsed-time label for a frame: its snapshot T index × the source image's frame interval
function frameTime(c: Cell): string {
  const step = (c.snapshot?.dims as { current_step?: number[] } | undefined)?.current_step
  const t = Array.isArray(step) ? step[0] : undefined
  if (t === undefined || t === null) return ''
  const img = project.sets.flatMap(s => s.images).find(im => im.uid === c.imageUid)
  const lbl = elapsedLabel(t, img?.timeIncrement, img?.timeIncrementUnit)
  return /^t\d/.test(lbl) ? '' : lbl        // hide the bare "t{N}" fallback (no real time on a still)
}
// angled separators are horizontal-only (the clip leans across the row) — snap back to straight if the
// strip is switched to vertical.
watch(orientation, o => { if (o === 'v' && separator.value === 'angled') separator.value = 'straight' })

// segmented selectors (ChipSelect). Orientation is always available; the `angled` separator is
// disabled while the strip is vertical (angled clips are horizontal-only), so its options recompute.
const orientationOpts: ChipOption[] = [
  { value: 'h', label: '', icon: 'pi pi-arrows-h' },
  { value: 'v', label: '', icon: 'pi pi-arrows-v' },
]
const separatorOpts = computed<ChipOption[]>(() => [
  { value: 'straight', label: 'straight' },
  { value: 'angled', label: 'angled', disabled: orientation.value === 'v',
    tip: orientation.value === 'v' ? 'Angled separators are horizontal-only' : '' },
])

// separator options (angle / width) live in a ⚙ popover (like the heatmap panel's options) so they
// never widen the toolbar; close on an outside click.
const optsOpen = ref(false)
const gearEl = useTemplateRef<HTMLElement>('gearEl')   // anchor for the teleported settings popover

const capturing = ref(-1)
const err = ref('')
// assetId → data URL, populated ONLY during PDF export: html2canvas can't reliably draw a served
// (network) <img> src, so we temporarily inline each sidecar frame as a data URL for the capture.
const exportSrcs = ref<Record<string, string>>({})

// Capture the current browser viewer into cell i. Reads the WebGPU canvas directly from the popped
// out viewer window — same-origin, so a `Window.__cceceliaViewerCapture` exposed by `ViewerWindow.vue`
// hands back the current pixels. The napari-screenshot equivalent: what the user is looking at IS
// what gets stored, so tracks / populations / mask outlines (already baked into the canvas by the
// shader) come along by construction — no server re-render, no 2D/3D branching. `viewState` +
// `imageUid` still ride along as provenance so zoom-to-source can restore the exact camera months
// later. The PNG uploads to the same sidecar the previous server-render path used.
//
// Fails cleanly when the viewer popup is not open (or is on a different image / same-session handle
// was lost after a main-window reload): the caller cannot capture what they cannot see.
async function capture(i: number) {
  capturing.value = i
  err.value = ''
  try {
    const openImage = viewer.openImage
    const snapshot = viewer.viewState as ViewerViewState | null
    const imageUid = openImage?.imageUid ?? null
    const projectUid = openImage?.projectUid ?? props.projectUid
    if (!imageUid || !snapshot) {
      err.value = 'Open the image in the viewer first to capture a frame.'
      return
    }
    const vw = getOpenPopoutWindow('/viewer-window')
    const cap = (vw as unknown as { __cceceliaViewerCapture?: () =>
      { png: string; extentUm: { x: number; y: number; unit?: string | null } | null; imageUid: string; overlayLayers?: Record<string, { visible: true }> } })?.__cceceliaViewerCapture
    if (!cap) {
      err.value = 'Open the viewer window on this image and try again.'
      return
    }
    let shot
    try { shot = cap() }
    catch (e) { err.value = 'Viewer capture failed: ' + (e instanceof Error ? e.message : String(e)); return }
    if (shot.imageUid !== imageUid) {
      err.value = 'Viewer is showing a different image — open this image first.'
      return
    }
    // Splice the viewer's overlay layer NAMES back into the snapshot before persisting: the browser
    // viewer's `captureViewState` only records channel layers, but the strip's legend reads pop /
    // track / mask names from `snapshot.layers` (napari carried them; this restores parity).
    const augmentedSnapshot = shot.overlayLayers && Object.keys(shot.overlayLayers).length
      ? { ...(snapshot as unknown as Record<string, unknown>),
          layers: { ...((snapshot as { layers?: Record<string, unknown> }).layers ?? {}), ...shot.overlayLayers } }
      : snapshot as unknown as Record<string, unknown>
    // Persist the PNG as a board-assets sidecar (same storage path used by the movie recorder and by
    // the legacy migration below).
    const saveRes = await fetch('/api/board-assets/save', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, png: shot.png }),
    })
    if (!saveRes.ok) {
      err.value = ((await saveRes.json().catch(() => ({}))) as { error?: string }).error ?? 'Failed to save capture'
      return
    }
    const saved = (await saveRes.json()) as { assetId?: string }
    if (!saved.assetId) { err.value = 'Save returned no assetId'; return }
    const capturedColourBy = props.setUid ? settings.getColourBy(props.setUid) : ''
    const colourOverridesForLegend = (props.setUid && capturedColourBy)
      ? settings.getColourOverrides(props.setUid, capturedColourBy) : {}
    const c = cells.value[i]
    c.assetId = saved.assetId; c.src = undefined
    c.snapshot = augmentedSnapshot
    c.imageUid = imageUid
    // Physical extent for the still scale bar: taken from the viewer's own `overlayExtent` (the same
    // value its scale bar reads). Absent / non-positive extent → leave unset and `StillOverlay` hides
    // the vector bar rather than drawing a wrong one.
    c.extentUm = shot.extentUm
    // remember the colour-by measure so zoom-to-source restores overlays in the same colours (it isn't
    // encoded in the snapshot's layer names). Per the open image's set.
    c.colourBy = capturedColourBy
    // capture the overlay legend (pops + colour-by) for this frame — read-only, durable (drawn below the
    // channel legend). ALL pop overlays (points AND track/track-cluster ribbons) are sent, parsed from
    // the snapshot's overlay layer names; the backend skips any that aren't a named population (e.g. the
    // whole-segmentation "/_tracked" layer), so track-cluster + gated track pops get legend entries too.
    if (c.imageUid) {
      // shared capture-legend path (also used by the single-record movie card) — best-effort.
      // `colourOverridesForLegend` above is the same per-set recolour map (an HMM state with no
      // population wins over the default colour), so the captured legend matches what's shown.
      const leg = await captureViewLegend(props.projectUid, c.imageUid, c.snapshot as { layers?: Record<string, unknown> }, c.colourBy ?? '', colourOverridesForLegend)
      c.overlaysLegend = { colourBy: leg.colourBy, populations: leg.populations }
    }
  } catch (e) { err.value = e instanceof Error ? e.message : String(e) }
  finally { capturing.value = -1 }
}

// per-frame legend (utils/viewLegend + <ViewLegend>): colour-by + populations from the captured overlay
// legend, channels from the snapshot layers. Ordered colour-by → populations → CHANNELS so that, in the
// bottom-anchored overlay, the sections pile UP from the bottom with channels lowest and pops/tracks
// above them (docs/todo/ANIMATION_PLAN.md C). Section headings show only when >1 section.
function legendSections(c: Cell) {
  const layers = (c.snapshot?.layers ?? {}) as Record<string, { colormap?: string; visible?: boolean }>
  const channels = channelLegend(layers)
  const populations = (c.overlaysLegend?.populations ?? []).map(p => ({ label: p.name, colour: p.colour }))
  const colourBy = (c.overlaysLegend?.colourBy?.items ?? [])
    .filter(it => it.colour).map(it => ({ label: it.label, colour: it.colour }))
  const cbyTitle = c.overlaysLegend?.colourBy?.column || 'Colour by'
  // Mask layers — `"(vn) Labels"` in the snapshot. `parseOverlays` deliberately skips these (they
  // aren't a pop / track), so name the mask directly. Outline colour follows the pops when any are
  // gated (the shader draws outlines in each cell's pop colour); grey elsewhere — same neutral the
  // movie rail's `all_cells_colour` uses when the mask paints every cell.
  const masks: { label: string; colour: string }[] = []
  const MASK_RE = /^\(([^)]+)\) Labels$/
  for (const name of Object.keys(layers)) {
    const m = MASK_RE.exec(name)
    if (m) masks.push({ label: m[1], colour: '#9ca3af' })
  }
  const secs: { title: string; items: { label: string; colour: string }[] }[] = []
  if (colourBy.length)    secs.push({ title: cbyTitle, items: colourBy })
  if (populations.length) secs.push({ title: 'Populations', items: populations })
  if (masks.length)       secs.push({ title: 'Masks', items: masks })
  if (channels.length)    secs.push({ title: 'Channels', items: channels })
  return secs
}

// resolve a cell's <img> src: during PDF export, the inlined data URL (html2canvas can't draw a served
// URL); otherwise the sidecar asset served on demand, or the legacy inline data-URL.
function cellSrc(c: Cell): string | undefined {
  if (c.assetId) {
    return exportSrcs.value[c.assetId]
      ?? `/api/board-assets?projectUid=${encodeURIComponent(props.projectUid)}&assetId=${encodeURIComponent(c.assetId)}`
  }
  return c.src
}

// Migrate legacy boards: a cell with an inline base64 `src` (and no assetId) is spilled to a sidecar
// file once, then the inline copy is dropped — the mutation triggers the board autosave, which persists
// the slimmed cell. Runs on mount; leaves the inline src in place if the migration call fails.
async function migrateLegacyAssets() {
  for (const c of cells.value) {
    if (c.assetId || !c.src) continue
    try {
      const res = await fetch('/api/board-assets/save', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: props.projectUid, png: c.src }),
      })
      if (!res.ok) continue
      const { assetId } = (await res.json()) as { assetId?: string }
      if (assetId) { c.assetId = assetId; c.src = undefined }
    } catch { /* keep the inline src */ }
  }
}
// ── Zoom to source ──────────────────────────────────────────────────────────
// Reopen the frame's source image in the browser viewer and re-apply its saved snapshot
// (camera + T/Z + per-layer contrast/colours + overlay visibility) — the "reconstruct my figure
// months later" path. Two writes, no wait:
//  1. `setPendingViewState(snapshot)` — the store persists to localStorage; the popup viewer's
//     watcher applies as soon as its meta and canvas are ready. If the popup is already up, the
//     `storage` event fires immediately; if not, the popup seeds from localStorage on mount and
//     applies then. The store consumes the entry after applying so a later reload doesn't reapply.
//  2. `openViewerWindow({...})` — opens or focuses the popup on the target image. No-op when it's
//     already showing this uid (the popout registry keys by name; see `lib/popout.ts`).
// Overlays travel through the shared settings bag (P2 storage bridge), same shape a panel toggle
// would produce, so the popup re-derives its overlay set on the next tick.
const zooming = ref(-1)

async function zoomToSource(i: number) {
  const c = cells.value[i]
  if (!c.imageUid || !c.snapshot) return
  zooming.value = i
  err.value = ''
  try {
    const snapshot = c.snapshot as unknown as ViewerViewState
    viewer.setPendingViewState(snapshot)
    openViewerWindow({ projectUid: props.projectUid, imageUid: c.imageUid })
    // Overlay restore: parse the captured snapshot's overlay layer names and write the shared bag
    // the popup viewer subscribes to. Trans-window via `storage` events (P2), so a viewer opened
    // on a different image than the one being zoomed to sees the setting change but not the tick,
    // and doesn't redraw.
    const cfg = overlayPushConfig(parseOverlays((snapshot.layers ?? {}) as Record<string, unknown>))
    if (cfg.trackValueNames.length) {
      const cur = settings.getTrackVisibility(c.imageUid, cfg.trackValueNames)
      const bag: Record<string, boolean> = { ...cur }
      for (const vn of cfg.trackValueNames) bag[vn] = true
      settings.setTrackVisibility(c.imageUid, bag)
    }
    const setUid = project.setUidOfImage(c.imageUid)
    if (setUid) {
      if (cfg.showGatedTracks) settings.setShowGatedTracks(setUid, true)
      if (cfg.showTrackclust)  settings.setPopVisible(setUid, 'trackclust', true)
      for (const pt of cfg.popTypes) settings.setPopVisible(setUid, pt, true)
      if (c.colourBy) settings.setColourBy(setUid, c.colourBy)
    }
    if (typeof localStorage !== 'undefined') {
      localStorage.setItem('cc.viewerOverlaysTick', `${c.imageUid}:${Date.now()}`)
    }
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
  } finally {
    zooming.value = -1
  }
}

onMounted(() => { migrateLegacyAssets() })

function addCell() { cells.value.push({}) }
function removeCell(i: number) {
  if (cells.value.length <= 1) return
  const c = cells.value[i]
  if (c.assetId) {   // best-effort delete of the sidecar PNG so it doesn't orphan
    fetch('/api/board-assets/delete', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: props.projectUid, assetId: c.assetId }),
    }).catch(() => {})
  }
  cells.value.splice(i, 1)
}

// angled separators: clip each frame to a parallelogram leaning by `skew`; the WHITE strip background
// shows through the `thick` gap between frames as the diagonal separator line. First/last frames keep
// their outer edge square. Horizontal strip only (the common montage); vertical stays straight.
function clipFor(i: number): string | undefined {
  if (separator.value !== 'angled' || orientation.value !== 'h' || cells.value.length < 2) return undefined
  const first = i === 0, last = i === cells.value.length - 1
  const s = `${skew.value}px`
  const tl = first ? '0' : s
  const br = last ? '100%' : `calc(100% - ${s})`
  return `polygon(${tl} 0, 100% 0, ${br} 100%, 0 100%)`
}
const stripStyle = computed(() => ({
  ...((separator.value === 'angled' && orientation.value === 'h')
    ? { '--sk': `${skew.value}px`, '--sep-thick': `${thick.value}px` } : {}),
}))

// PDF export: just the STRIP (frames), no toolbar/per-frame buttons. The `capturing` class
// hides the in-frame controls; the strip is HTML + <img> (data URLs), so serialise via elementToImageURL.
const stripRef = useTemplateRef<HTMLElement>('stripRef')
const capturingStrip = ref(false)
// fetch a sidecar asset and return it as a data URL (html2canvas-safe for the PDF export)
async function assetToDataUrl(assetId: string): Promise<string | null> {
  try {
    const res = await fetch(`/api/board-assets?projectUid=${encodeURIComponent(props.projectUid)}&assetId=${encodeURIComponent(assetId)}`)
    if (!res.ok) return null
    const bytes = new Uint8Array(await res.arrayBuffer())
    let bin = ''
    for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i])
    return 'data:image/png;base64,' + btoa(bin)
  } catch { return null }
}
async function exportImage(): Promise<string | null> {
  // inline each sidecar frame as a data URL so html2canvas can draw it (a served <img> URL renders
  // blank in the PDF), then serialise the strip DOM.
  const map: Record<string, string> = {}
  for (const c of cells.value) {
    if (c.assetId) { const d = await assetToDataUrl(c.assetId); if (d) map[c.assetId] = d }
  }
  exportSrcs.value = map
  capturingStrip.value = true
  await nextTick()
  try { return await elementToImageURL(stripRef.value, 'png', '#ffffff') }
  finally { capturingStrip.value = false; exportSrcs.value = {} }
}
defineExpose({ exportImage })
</script>

<template>
  <div class="is-view">
    <div class="is-bar cc-row cc-panel-controls">
      <div v-tooltip.bottom="'Strip direction'">
        <ChipSelect variant="segmented" aria-label="Strip direction" :options="orientationOpts"
                    :model-value="orientation" @update:model-value="v => orientation = v as 'h' | 'v'" />
      </div>
      <div v-tooltip.bottom="'Separator style'">
        <ChipSelect variant="segmented" aria-label="Separator style" :options="separatorOpts"
                    :model-value="separator" @update:model-value="v => separator = v as 'straight' | 'angled'" />
      </div>
      <div class="is-opts">
        <button ref="gearEl" class="is-gear cc-btn cc-btn-ghost cc-btn-icon" :class="{ 'cc-btn-on': optsOpen }" @click="optsOpen = !optsOpen"
                v-tooltip.bottom="'Caption size & separator'"><i class="pi pi-cog" /></button>
        <TeleportPopover v-model="optsOpen" :anchor="gearEl" placement="bottom-end">
          <div class="is-pop">
            <!-- Toggle + px slider on one row per overlay — same pattern as the viewer's own
                 scale-bar / timestamp controls (`ViewerWindow.vue`), 8..32 px range. The slider hides
                 with its toggle so a control the user cannot use doesn't sit in a dense popover.
                 Each row is a grid so the sliders and readouts line up across rows even when the
                 toggle labels differ in width. -->
            <div class="is-tr">
              <CcToggle class="is-check cc-muted cc-fs-xs" label="legend"
                v-tooltip.bottom="'Show channels · populations · colour-by under the strip'"
                :model-value="showLegend" @update:model-value="showLegend = $event" />
              <input v-if="showLegend" type="range" class="is-px" min="8" max="32" step="1"
                :value="legendFontPx" @input="legendFontPx = +($event.target as HTMLInputElement).value"
                v-tooltip.bottom="'Legend text size'" aria-label="Legend text size" />
              <span v-if="showLegend" class="is-val">{{ legendFontPx }}</span>
            </div>
            <div class="is-tr">
              <CcToggle class="is-check cc-muted cc-fs-xs" label="scale bar"
                v-tooltip.bottom="'Draw a vector scale bar on each frame (from the image\'s physical pixel size)'"
                :model-value="showScaleBar" @update:model-value="showScaleBar = $event" />
              <input v-if="showScaleBar" type="range" class="is-px" min="8" max="32" step="1"
                :value="scaleBarFontPx" @input="scaleBarFontPx = +($event.target as HTMLInputElement).value"
                v-tooltip.bottom="'Scale-bar text size'" aria-label="Scale-bar text size" />
              <span v-if="showScaleBar" class="is-val">{{ scaleBarFontPx }}</span>
            </div>
            <div class="is-tr">
              <CcToggle class="is-check cc-muted cc-fs-xs" label="timestamp"
                v-tooltip.bottom="'Draw the elapsed-time timestamp on each frame'"
                :model-value="showTimestamp" @update:model-value="showTimestamp = $event" />
              <input v-if="showTimestamp" type="range" class="is-px" min="8" max="32" step="1"
                :value="timestampFontPx" @input="timestampFontPx = +($event.target as HTMLInputElement).value"
                v-tooltip.bottom="'Timestamp text size'" aria-label="Timestamp text size" />
              <span v-if="showTimestamp" class="is-val">{{ timestampFontPx }}</span>
            </div>
            <template v-if="separator === 'angled' && orientation === 'h'">
              <label class="is-slider cc-muted cc-fs-xs" v-tooltip.bottom="'Slant of the separator between frames'">angle
                <input type="range" min="0" max="80" :value="skew" @input="skew = +($event.target as HTMLInputElement).value" />
                <span class="is-val">{{ skew }}</span></label>
              <label class="is-slider cc-muted cc-fs-xs" v-tooltip.bottom="'Thickness of the separator between frames'">width
                <input type="range" min="1" max="12" :value="thick" @input="thick = +($event.target as HTMLInputElement).value" />
                <span class="is-val">{{ thick }}</span></label>
            </template>
          </div>
        </TeleportPopover>
      </div>
      <button class="is-btn" @click="addCell" v-tooltip.bottom="'Add a frame'"><i class="pi pi-plus" /> frame</button>
      <span v-if="err" class="is-err">{{ err }}</span>
    </div>

    <div ref="stripRef" class="is-strip" :class="[orientation === 'h' ? 'row' : 'col', separator, { capturing: capturingStrip }]" :style="stripStyle">
      <StripCell v-for="(c, i) in cells" :key="i"
                 class="is-cell" :style="{ clipPath: clipFor(i) }"
                 :src="(c.assetId || c.src) ? cellSrc(c) : undefined"
                 alt="viewer screenshot"
                 :extent-um="c.extentUm" :time-label="frameTime(c)"
                 :show-scale-bar="showScaleBar" :show-timestamp="showTimestamp"
                 :show-legend="showLegend" :legend-sections="legendSections(c)"
                 :legend-font-px="legendFontPx" :scale-bar-font-px="scaleBarFontPx"
                 :timestamp-font-px="timestampFontPx">
        <template #empty>
          <button class="is-capture" @click="capture(i)" :disabled="capturing === i"
                  v-tooltip.bottom="'Capture the current viewer view'">
            <i class="pi pi-camera" /> {{ capturing === i ? 'capturing…' : 'viewer view' }}
          </button>
        </template>
        <template #actions>
          <template v-if="!capturingStrip">
            <button v-if="(c.assetId || c.src) && c.imageUid && c.snapshot" class="is-mini cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="zoomToSource(i)"
                    :disabled="zooming === i" v-tooltip.top="'Zoom to source: reopen this image in Viewer and restore the exact view'">
              <i class="pi pi-directions" /></button>
            <button v-if="c.assetId || c.src" class="is-mini cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="capture(i)" v-tooltip.top="'Recapture'"><i class="pi pi-camera" /></button>
            <button v-if="cells.length > 1" class="is-mini cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="removeCell(i)" v-tooltip.top="'Remove frame'"><i class="pi pi-times" /></button>
          </template>
        </template>
      </StripCell>
    </div>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .is-bar (.cc-panel-controls) anchors to the strip box */
.is-view { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
/* angle/width live in a ⚙ popover (below), so the bar stays short and never wraps */
.is-bar { padding: 6px 8px; font-size: var(--cc-fs-sm); }
.is-opts { position: relative; display: inline-flex; }
/* .is-gear → cc-btn cc-btn-ghost cc-btn-icon */
.is-gear:hover { color: var(--cc-text); border-color: var(--cc-accent-strong); }
/* inner layout only — the teleported TeleportPopover shell provides surface/border/shadow/position */
.is-pop { display: flex; flex-direction: column; gap: 6px; }   /* padding: TeleportPopover */
.is-val { min-width: 1.2rem; text-align: right; font-weight: 700; color: var(--cc-text); }
.is-err { color: #fca5a5; font-size: var(--cc-fs-xs); }
.is-slider { display: inline-flex; align-items: center; gap: 4px; }
.is-slider input[type="range"] { width: 4.5rem; }
/* Three-column grid for the overlay toggle rows: toggle | slider | readout. Fixed widths on cols
   1 and 3 so the sliders line up across rows even when the toggle labels differ in length. */
.is-tr { display: grid; grid-template-columns: 8.5rem 5rem 1.5rem; align-items: center;
  column-gap: 8px; }
.is-tr .is-val { text-align: right; }
.is-px { width: 100%; }
.is-check { display: inline-flex; align-items: center; gap: 6px; }
/* cell chrome (image + legend + StillOverlay + hover-actions slot) lives in <StripCell> so this
   view and CellCardsView draw the cell identically. Only strip-level layout stays here. */
.is-strip { flex: 1; min-height: 0; display: flex; padding: 6px; gap: 0; overflow: auto; }
.is-strip.col { flex-direction: column; }
/* straight: no box around each frame — just a thin rule BETWEEN frames */
.is-strip.straight.row .is-cell + .is-cell { border-left: 1px solid var(--cc-border); }
.is-strip.straight.col .is-cell + .is-cell { border-top: 1px solid var(--cc-border); }
/* angled: frames overlap by (skew − thickness) so the white strip background shows through as a diagonal
   line whose width is EXACTLY --sep-thick, independent of the angle (--sk). */
/* padding:0 here — the base 6px padding + white bg would draw a white frame around the whole strip;
   in angled mode white must ONLY show through the diagonal gaps between frames */
.is-strip.angled.row { gap: 0; background: #fff; padding: 0; }
.is-strip.angled.row .is-cell { border: none; border-radius: 0; background: transparent; }
.is-strip.angled.row .is-cell + .is-cell { margin-left: calc(var(--sep-thick, 2px) - var(--sk, 22px)); }
.is-capture { flex: 1; display: flex; align-items: center; justify-content: center; gap: 6px;
  border: 1px dashed var(--cc-border); background: transparent; color: var(--cc-text-dim); cursor: pointer; font-size: var(--cc-fs-sm); }
.is-capture:hover { color: var(--cc-text); border-color: var(--cc-accent-strong); }
/* per-frame action buttons — sit inside StripCell's #actions slot (auto-hide + positioning owned there). */
.is-mini { transition: color 0.1s, border-color 0.1s, background 0.1s; }   /* + cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense */
.is-mini:hover { color: var(--cc-text); border-color: var(--cc-accent-strong); background: var(--cc-surface-1); }
.is-mini:disabled { opacity: 0.5; cursor: not-allowed; }
/* while capturing for the PDF: hide the per-frame buttons (and empty-frame capture prompts) so the
   exported strip is just the images. Slotted content carries this SFC's scope id, so `.is-mini` /
   `.is-capture` inside StripCell's #actions and #empty slots match without :deep. */
.is-strip.capturing .is-mini, .is-strip.capturing .is-capture { display: none; }
.is-btn { display: inline-flex; align-items: center; gap: 4px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs); padding: 3px 8px; cursor: pointer; font-size: var(--cc-fs-xs); }
.is-btn:hover { color: var(--cc-text); }
</style>
