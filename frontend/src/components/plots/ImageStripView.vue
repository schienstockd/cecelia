<!--
  Image / filmstrip slot for the Analysis board (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase D). One
  slot holding N images (a single image = a 1-cell strip) — for pipeline montages
  (raw → denoised → segmented → tracked). Each cell's image is a WebGPU-viewer thumbnail
  (POST /api/viewer/thumbnail with the popup's live viewState → JSON {assetId, imageUid, width,
  height}). The PNG is a SIDECAR file (settings/board-assets/, served via /api/board-assets) — NOT
  stored inline — so the board JSON stays small and autosaves cheaply; the cell keeps only the
  assetId + the viewState snapshot + imageUid (provenance for zoom-to-source). See
  docs/todo/ANIMATION_PLAN.md. Orientation H/V; separators STRAIGHT (gap + rule) or ANGLED
  (clip-path parallelograms — cheap because the slot stays rectangular, decision 10).
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
import ViewLegend from '../ViewLegend.vue'
import StillOverlay from '../StillOverlay.vue'
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
  state: { cells?: Cell[]; orientation?: 'h' | 'v'; separator?: 'straight' | 'angled'; sepAngle?: number; sepThick?: number; showLegend?: boolean; showScaleBar?: boolean; showTimestamp?: boolean }
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
// optional channel-colour legend, read from the frame's snapshot (viewer layer colormaps). Off by default.
const showLegend = computed({ get: () => props.state.showLegend ?? false, set: v => (props.state.showLegend = v) })
// still overlays (E2): a vector scale bar (from the captured frame's physical extent) + an elapsed-time
// timestamp — drawn crisp on the clean capture (the viewer's own hidden via E1). Off by default.
const showScaleBar  = computed({ get: () => props.state.showScaleBar ?? false,  set: v => (props.state.showScaleBar = v) })
const showTimestamp = computed({ get: () => props.state.showTimestamp ?? false, set: v => (props.state.showTimestamp = v) })
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

// Capture the current browser viewer into cell i. Reads the viewer's published viewState
// (`viewerStore.viewState` — the popup writes it on every camera / channel change) and POSTs it to
// /api/viewer/thumbnail, which renders one frame through the same offline path the movie recorder
// uses so the thumbnail matches what a movie made from this look would produce.
//
// Fails cleanly when no browser viewer is open on this image: the caller cannot capture what they
// cannot see. The browser thumbnail renders channels-only for MVP — no baked scale bar / timestamp
// to strip.
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
    const valueName = openImage?.valueName || undefined
    const res = await fetch('/api/viewer/thumbnail', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, imageUid, valueName, viewState: snapshot }),
    })
    if (!res.ok) { err.value = ((await res.json().catch(() => ({}))) as { error?: string }).error ?? 'Thumbnail render failed'; return }
    const data = (await res.json()) as { ok?: boolean; assetId?: string; imageUid?: string; width?: number; height?: number }
    const c = cells.value[i]
    if (data.assetId) { c.assetId = data.assetId; c.src = undefined }
    c.snapshot = snapshot as unknown as Record<string, unknown>
    c.imageUid = imageUid
    // Physical extent for a still scale bar (E2). Derived from the viewState's canvas + the image's
    // voxel size, both of which the browser viewer already carries in its snapshot; the endpoint's
    // MVP doesn't echo it back yet, so leave it unset — the still falls back to a screen-space
    // scale bar drawn from the canvas dimensions rather than physical units.
    c.extentUm = null
    // remember the colour-by measure so zoom-to-source restores overlays in the same colours (it isn't
    // encoded in the snapshot's layer names). Per the open image's set.
    c.colourBy = props.setUid ? settings.getColourBy(props.setUid) : ''
    // capture the overlay legend (pops + colour-by) for this frame — read-only, durable (drawn below the
    // channel legend). ALL pop overlays (points AND track/track-cluster ribbons) are sent, parsed from
    // the snapshot's overlay layer names; the backend skips any that aren't a named population (e.g. the
    // whole-segmentation "/_tracked" layer), so track-cluster + gated track pops get legend entries too.
    if (c.imageUid) {
      // include the set's user recolours for this colour-by so the captured legend matches what's shown
      // (a recoloured category — e.g. an HMM state with no population — wins over the default colour).
      const colourOverrides = (props.setUid && c.colourBy)
        ? settings.getColourOverrides(props.setUid, c.colourBy) : {}
      // shared capture-legend path (also used by the single-record movie card) — best-effort
      const leg = await captureViewLegend(props.projectUid, c.imageUid, c.snapshot as { layers?: Record<string, unknown> }, c.colourBy ?? '', colourOverrides)
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
  const secs: { title: string; items: { label: string; colour: string }[] }[] = []
  if (colourBy.length)    secs.push({ title: cbyTitle, items: colourBy })
  if (populations.length) secs.push({ title: 'Populations', items: populations })
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
            <CcToggle class="is-check cc-muted cc-fs-xs" label="legend (channels · pops · colour-by)"
              v-tooltip.bottom="'Show the channel and population key under the strip'"
              :model-value="showLegend" @update:model-value="showLegend = $event" />
            <CcToggle class="is-check cc-muted cc-fs-xs" label="scale bar"
              v-tooltip.bottom="'Draw a vector scale bar on each frame (from the image\'s physical pixel size)'"
              :model-value="showScaleBar" @update:model-value="showScaleBar = $event" />
            <CcToggle class="is-check cc-muted cc-fs-xs" label="timestamp"
              v-tooltip.bottom="'Draw the elapsed-time timestamp on each frame'"
              :model-value="showTimestamp" @update:model-value="showTimestamp = $event" />
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
      <div v-for="(c, i) in cells" :key="i" class="is-cell" :style="{ clipPath: clipFor(i) }">
        <img v-if="c.assetId || c.src" :src="cellSrc(c)" class="is-img" alt="viewer screenshot" />
        <!-- vector scale bar + timestamp (E2), drawn on the clean capture from the frame's physical extent -->
        <StillOverlay v-if="(c.assetId || c.src) && (showScaleBar || showTimestamp)"
                      :extent-um="c.extentUm" :time-label="frameTime(c)"
                      :show-scale-bar="showScaleBar" :show-timestamp="showTimestamp" />
        <!-- optional view legend (channels now; pops + colour-by plug in later), from the frame snapshot -->
        <ViewLegend v-if="showLegend && (c.assetId || c.src) && legendSections(c).length"
                    :sections="legendSections(c)" :swatch="9" vertical class="is-legend" />
        <button v-if="!(c.assetId || c.src)" class="is-capture" @click="capture(i)" :disabled="capturing === i"
                v-tooltip.bottom="'Capture the current viewer view'">
          <i class="pi pi-camera" /> {{ capturing === i ? 'capturing…' : 'viewer view' }}
        </button>
        <!-- per-frame actions (hidden while capturing) -->
        <div v-if="!capturingStrip" class="is-actions">
          <button v-if="(c.assetId || c.src) && c.imageUid && c.snapshot" class="is-mini cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="zoomToSource(i)"
                  :disabled="zooming === i" v-tooltip.top="'Zoom to source: reopen this image in Viewer and restore the exact view'">
            <i class="pi pi-directions" /></button>
          <button v-if="c.assetId || c.src" class="is-mini cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="capture(i)" v-tooltip.top="'Recapture'"><i class="pi pi-camera" /></button>
          <button v-if="cells.length > 1" class="is-mini cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="removeCell(i)" v-tooltip.top="'Remove frame'"><i class="pi pi-times" /></button>
        </div>
      </div>
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
.is-check { display: inline-flex; align-items: center; gap: 6px; }
/* view legend (shared <ViewLegend>): overlaid bottom-left of the frame (z above the auto-hide toolbar
   like the caption/actions); included in the PDF export (not hidden while capturing). The container
   sets the on-image styling (white text, shadow, dark chip); ViewLegend inherits color + font-size. */
.is-legend { position: absolute; bottom: 6px; left: 6px; padding: 3px 5px; border-radius: var(--cc-radius-xs);
  background: rgba(0,0,0,0.45); pointer-events: none; z-index: 7;
  color: #fff; font-size: var(--cc-fs-2xs); font-weight: 600; text-shadow: 0 1px 2px rgba(0,0,0,0.85); }
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
.is-cell { position: relative; flex: 1; min-width: 0; min-height: 120px; display: flex; flex-direction: column;
  overflow: hidden; background: var(--cc-bg); }
/* contain (not cover) so the WHOLE captured frame is shown — cover cropped the edges, cutting the viewer's
   scale bar (bottom-right) and timestamp (top-left). Trade-off: letterbox bars when the cell aspect ≠
   the image aspect; acceptable for figures (nothing is clipped). ANIMATION_PLAN E (clean capture +
   Cecelia-drawn scale bar) will let frames go edge-to-edge again without losing the annotations. */
.is-img { flex: 1; width: 100%; object-fit: contain; min-height: 0; }
.is-capture { flex: 1; display: flex; align-items: center; justify-content: center; gap: 6px;
  border: 1px dashed var(--cc-border); background: transparent; color: var(--cc-text-dim); cursor: pointer; font-size: var(--cc-fs-sm); }
.is-capture:hover { color: var(--cc-text); border-color: var(--cc-accent-strong); }
/* per-frame actions (zoom-to-source / recapture / remove): BOTTOM-left, revealed on cell hover.
   They used to sit TOP-right (z-index 7) — but that's exactly where the CanvasPanel's OWN controls
   live (pin / duplicate / ⋯ menu, z-index 6), so the always-on strip actions sat on top and blocked
   them. Bottom-right holds the scale bar and the panel footer (Duplicate/Export), so bottom-left is
   the free corner. Auto-hide (like the toolbar) so they never permanently obscure the frame or the
   optional channel legend that also anchors bottom-left; z-index 8 keeps them above it while hovering,
   and the `.is-strip.capturing` rule below drops them from the PDF export. */
.is-actions { position: absolute; bottom: 4px; left: 4px; display: flex; gap: 4px; z-index: 8;
  opacity: 0; transition: opacity 0.12s ease; }
.is-cell:hover .is-actions { opacity: 1; }
/* per-frame action buttons — match the app's icon buttons (like .is-gear / .opt-btn) rather than the
   old dark translucent pills: solid surface + border, purple accent on hover. Sit over the image, so a
   solid surface reads cleanly. */
.is-mini { transition: color 0.1s, border-color 0.1s, background 0.1s; }   /* + cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense */
.is-mini:hover { color: var(--cc-text); border-color: var(--cc-accent-strong); background: var(--cc-surface-1); }
.is-mini:disabled { opacity: 0.5; cursor: not-allowed; }
/* while capturing for the PDF: hide the per-frame buttons (and empty-frame capture prompts) so the
   exported strip is just the images */
.is-strip.capturing .is-mini, .is-strip.capturing .is-capture { display: none; }
.is-btn { display: inline-flex; align-items: center; gap: 4px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  border: 1px solid var(--cc-border); border-radius: var(--cc-radius-xs); padding: 3px 8px; cursor: pointer; font-size: var(--cc-fs-xs); }
.is-btn:hover { color: var(--cc-text); }
</style>
