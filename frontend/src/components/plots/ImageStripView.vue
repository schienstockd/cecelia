<!--
  Image / filmstrip slot for the Analysis board (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase D). One
  slot holding N images (a single image = a 1-cell strip) — for pipeline montages
  (raw → denoised → segmented → tracked). Each cell's image is a napari CANVAS screenshot
  (POST /api/napari/screenshot → JSON {assetId, viewState, imageUid}). The PNG is a SIDECAR file
  (settings/board-assets/, served via /api/board-assets) — NOT stored inline — so the board JSON stays
  small and autosaves cheaply; the cell keeps only the assetId + the viewState snapshot + imageUid
  (provenance for zoom-to-source). See docs/todo/ANIMATION_PLAN.md. Orientation H/V; separators STRAIGHT
  (gap + rule) or ANGLED (clip-path parallelograms — cheap because the slot stays rectangular, decision 10).
-->
<script setup lang="ts">
import { ref, computed, watch, useTemplateRef, nextTick, onMounted, onUnmounted } from 'vue'
import { elementToImageURL } from '../../plots/export'
import TeleportPopover from '../TeleportPopover.vue'
import { useWsStore } from '../../stores/ws'
import { useSettingsStore } from '../../stores/settings'
import { useProjectStore } from '../../stores/project'
import { channelLegend } from '../../utils/viewLegend'
import { elapsedLabel } from '../../utils/stillOverlay'
import { parseOverlays, overlayPushConfig } from '../../utils/overlayLayers'
import { restoreOverlays } from '../../utils/napariOverlays'
import ViewLegend from '../ViewLegend.vue'
import StillOverlay from '../StillOverlay.vue'

const ws = useWsStore()
const settings = useSettingsStore()
const project = useProjectStore()

// `snapshot` (napari view state) + `imageUid` are the frame's provenance — persisted with the board so
// zoom-to-source can reopen the image and restore the exact camera/contrast/colours months later
// (docs/todo/ANIMATION_PLAN.md). Captured atomically with the screenshot.
// `assetId` → the frame's PNG is a sidecar file (settings/board-assets/), served on demand; NOT stored
// inline, so the board JSON stays small (autosave-friendly). `src` is the legacy inline data-URL, kept
// only for back-compat (migrated to a sidecar on load). `snapshot`+`imageUid` are the view provenance
// for zoom-to-source. See docs/todo/ANIMATION_PLAN.md.
interface ExtentUm { x?: number; y?: number; unit?: string | null }
// captured overlay legend (populations + colour-by), fetched at capture from /api/napari/overlay-legend
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
// optional channel-colour legend, read from the frame's snapshot (napari layer colormaps). Off by default.
const showLegend = computed({ get: () => props.state.showLegend ?? false, set: v => (props.state.showLegend = v) })
// still overlays (E2): a vector scale bar (from the captured frame's physical extent) + an elapsed-time
// timestamp — drawn crisp on the clean capture (napari's own hidden via E1). Off by default.
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

// separator options (angle / width) live in a ⚙ popover (like the heatmap panel's options) so they
// never widen the toolbar; close on an outside click.
const optsOpen = ref(false)
const gearEl = useTemplateRef<HTMLElement>('gearEl')   // anchor for the teleported settings popover

const capturing = ref(-1)
const err = ref('')
// assetId → data URL, populated ONLY during PDF export: html2canvas can't reliably draw a served
// (network) <img> src, so we temporarily inline each sidecar frame as a data URL for the capture.
const exportSrcs = ref<Record<string, string>>({})

// capture the current napari canvas into cell i. The endpoint returns JSON { png(base64), viewState,
// imageUid } — the view snapshot is captured atomically with the shot, so the frame carries its exact
// provenance (for zoom-to-source / animation). See docs/todo/ANIMATION_PLAN.md.
async function capture(i: number) {
  capturing.value = i
  err.value = ''
  try {
    const res = await fetch('/api/napari/screenshot', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: props.projectUid, clean: settings.cleanCapture }),
    })
    if (!res.ok) { err.value = ((await res.json().catch(() => ({}))) as { error?: string }).error ?? 'Screenshot failed'; return }
    const data = (await res.json()) as { assetId?: string; png?: string; viewState?: Record<string, unknown>; imageUid?: string | null; extentUm?: ExtentUm | null }
    const c = cells.value[i]
    if (data.assetId) { c.assetId = data.assetId; c.src = undefined }         // sidecar PNG (normal path)
    else if (data.png) { c.src = 'data:image/png;base64,' + data.png; c.assetId = undefined }  // inline fallback
    c.snapshot = data.viewState
    c.imageUid = data.imageUid ?? null
    c.extentUm = data.extentUm ?? null            // physical size → still scale bar (E2)
    // remember the colour-by measure so zoom-to-source restores overlays in the same colours (it isn't
    // encoded in the snapshot's layer names). Per the open image's set.
    c.colourBy = props.setUid ? settings.getColourBy(props.setUid) : ''
    // capture the overlay legend (pops + colour-by) for this frame — read-only, durable (drawn below the
    // channel legend). Point-pops parsed from the snapshot's overlay layer names.
    if (c.imageUid) {
      const pointPops = parseOverlays(c.snapshot?.layers as Record<string, unknown>)
        .filter(o => !o.isTrack).map(o => ({ valueName: o.valueName, popType: o.popType, path: o.path }))
      try {
        const lr = await fetch('/api/napari/overlay-legend', {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ projectUid: props.projectUid, imageUid: c.imageUid, colourBy: c.colourBy, pointPops }),
        })
        if (lr.ok) {
          const j = await lr.json() as OverlaysLegend & { ok?: boolean }
          c.overlaysLegend = { colourBy: j.colourBy, populations: j.populations }
        }
      } catch { /* legend is best-effort */ }
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
// Reopen the frame's source image in napari and re-apply its saved snapshot (camera + T/Z + per-layer
// contrast/colours) — the "reconstruct my figure months later" path. Open is async (napari may need to
// start), and both open paths broadcast `napari:opened`, so we apply on that event (handles the already-
// running AND cold-start cases uniformly) rather than racing the open response. See ANIMATION_PLAN B.
const zooming = ref(-1)
const pendingApply = ref<{ imageUid: string; snapshot: Record<string, unknown>; colourBy?: string } | null>(null)

async function zoomToSource(i: number) {
  const c = cells.value[i]
  if (!c.imageUid || !c.snapshot) return
  zooming.value = i
  err.value = ''
  pendingApply.value = { imageUid: c.imageUid, snapshot: c.snapshot, colourBy: c.colourBy }
  try {
    const res = await fetch('/api/napari/open', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: props.projectUid, imageUid: c.imageUid }),
    })
    if (!res.ok && res.status !== 202) {
      pendingApply.value = null
      err.value = ((await res.json().catch(() => ({}))) as { error?: string }).error ?? 'Open in Napari failed'
    }
  } catch (e) {
    pendingApply.value = null
    err.value = e instanceof Error ? e.message : String(e)
  } finally {
    zooming.value = -1
  }
}

// napari finished opening — if it's the image we're zooming to, apply the saved snapshot now (layers
// exist). Fire-and-forget; the bridge skips any layers not present.
async function onNapariOpened(payload: { imageUid?: string }) {
  const p = pendingApply.value
  if (!p || !payload?.imageUid || payload.imageUid !== p.imageUid) return
  pendingApply.value = null
  try {
    await fetch('/api/napari/apply-view-state', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ viewState: p.snapshot }),
    })
    // re-push the tracks/pops the frame had (open only recreates channel layers; overlays come from
    // show-tracks/show-populations, which zoom-to-source must re-request). Derived from the snapshot's
    // overlay layer names + the captured colour-by, so the overlays reappear as they were.
    const cfg = overlayPushConfig(parseOverlays(p.snapshot.layers as Record<string, unknown>))
    if (cfg.trackValueNames.length || cfg.showGatedTracks || cfg.showTrackclust || cfg.popTypes.length) {
      await restoreOverlays(props.projectUid, p.imageUid, { ...cfg, colourBy: p.colourBy })
    }
  } catch { /* best-effort restore */ }
}
onMounted(() => { ws.on('napari:opened', onNapariOpened); migrateLegacyAssets() })
onUnmounted(() => ws.off('napari:opened', onNapariOpened))

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
    <div class="is-bar cc-panel-controls">
      <div class="seg" v-tooltip.bottom="'Strip direction'">
        <button :class="{ on: orientation === 'h' }" @click="orientation = 'h'"><i class="pi pi-arrows-h" /></button>
        <button :class="{ on: orientation === 'v' }" @click="orientation = 'v'"><i class="pi pi-arrows-v" /></button>
      </div>
      <div class="seg" v-tooltip.bottom="'Separator style'">
        <button :class="{ on: separator === 'straight' }" @click="separator = 'straight'">straight</button>
        <button :class="{ on: separator === 'angled' }" :disabled="orientation === 'v'"
                @click="separator = 'angled'"
                v-tooltip.bottom="orientation === 'v' ? 'Angled separators are horizontal-only' : ''">angled</button>
      </div>
      <div class="is-opts">
        <button ref="gearEl" class="is-gear" :class="{ on: optsOpen }" @click="optsOpen = !optsOpen"
                v-tooltip.bottom="'Caption size & separator'"><i class="pi pi-cog" /></button>
        <TeleportPopover v-model="optsOpen" :anchor="gearEl" placement="bottom-end">
          <div class="is-pop">
            <label class="is-check"><input type="checkbox" :checked="showLegend"
              @change="showLegend = ($event.target as HTMLInputElement).checked" /> channel legend</label>
            <label class="is-check" v-tooltip.bottom="'Hide napari\'s scale bar + timestamp when capturing, for a clean publication still (add your own externally)'">
              <input type="checkbox" :checked="settings.cleanCapture"
              @change="settings.cleanCapture = ($event.target as HTMLInputElement).checked" /> clean capture</label>
            <label class="is-check" v-tooltip.bottom="'Draw a vector scale bar on each frame (from the image\'s physical pixel size)'">
              <input type="checkbox" :checked="showScaleBar"
              @change="showScaleBar = ($event.target as HTMLInputElement).checked" /> scale bar</label>
            <label class="is-check" v-tooltip.bottom="'Draw the elapsed-time timestamp on each frame'">
              <input type="checkbox" :checked="showTimestamp"
              @change="showTimestamp = ($event.target as HTMLInputElement).checked" /> timestamp</label>
            <template v-if="separator === 'angled' && orientation === 'h'">
              <label class="is-slider">angle
                <input type="range" min="0" max="80" :value="skew" @input="skew = +($event.target as HTMLInputElement).value" />
                <span class="is-val">{{ skew }}</span></label>
              <label class="is-slider">width
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
        <img v-if="c.assetId || c.src" :src="cellSrc(c)" class="is-img" alt="napari screenshot" />
        <!-- vector scale bar + timestamp (E2), drawn on the clean capture from the frame's physical extent -->
        <StillOverlay v-if="(c.assetId || c.src) && (showScaleBar || showTimestamp)"
                      :extent-um="c.extentUm" :time-label="frameTime(c)"
                      :show-scale-bar="showScaleBar" :show-timestamp="showTimestamp" />
        <!-- optional view legend (channels now; pops + colour-by plug in later), from the frame snapshot -->
        <ViewLegend v-if="showLegend && (c.assetId || c.src) && legendSections(c).length"
                    :sections="legendSections(c)" :swatch="9" vertical class="is-legend" />
        <button v-if="!(c.assetId || c.src)" class="is-capture" @click="capture(i)" :disabled="capturing === i"
                v-tooltip.bottom="'Capture the current napari view'">
          <i class="pi pi-camera" /> {{ capturing === i ? 'capturing…' : 'napari view' }}
        </button>
        <!-- per-frame actions (hidden while capturing) -->
        <div v-if="!capturingStrip" class="is-actions">
          <button v-if="(c.assetId || c.src) && c.imageUid && c.snapshot" class="is-mini" @click="zoomToSource(i)"
                  :disabled="zooming === i" v-tooltip.top="'Zoom to source: reopen this image in Napari and restore the exact view'">
            <i class="pi pi-directions" /></button>
          <button v-if="c.assetId || c.src" class="is-mini" @click="capture(i)" v-tooltip.top="'Recapture'"><i class="pi pi-camera" /></button>
          <button v-if="cells.length > 1" class="is-mini" @click="removeCell(i)" v-tooltip.top="'Remove frame'"><i class="pi pi-times" /></button>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
/* position: relative so the overlaid .is-bar (.cc-panel-controls) anchors to the strip box */
.is-view { position: relative; display: flex; flex-direction: column; height: 100%; min-height: 0; }
/* angle/width live in a ⚙ popover (below), so the bar stays short and never wraps */
.is-bar { display: flex; align-items: center; gap: 8px; padding: 6px 8px; flex-wrap: wrap;
  font-size: 12px; }
.is-opts { position: relative; display: inline-flex; }
.is-gear { display: inline-flex; align-items: center; justify-content: center; width: 1.7rem; height: 1.6rem;
  border: 1px solid var(--cc-border); border-radius: 4px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.72rem; }
.is-gear:hover, .is-gear.on { color: var(--cc-text); border-color: #7c3aed; }
/* inner layout only — the teleported TeleportPopover shell provides surface/border/shadow/position */
.is-pop { display: flex; flex-direction: column; gap: 6px; padding: 8px 10px; }
.is-val { min-width: 1.2rem; text-align: right; font-weight: 700; color: var(--cc-text); }
.is-err { color: #fca5a5; font-size: 11px; }
.is-slider { display: inline-flex; align-items: center; gap: 4px; color: var(--cc-text-dim); font-size: 11px; }
.is-slider input[type="range"] { width: 4.5rem; }
.is-check { display: inline-flex; align-items: center; gap: 6px; color: var(--cc-text-dim); font-size: 11px; }
/* view legend (shared <ViewLegend>): overlaid bottom-left of the frame (z above the auto-hide toolbar
   like the caption/actions); included in the PDF export (not hidden while capturing). The container
   sets the on-image styling (white text, shadow, dark chip); ViewLegend inherits color + font-size. */
.is-legend { position: absolute; bottom: 6px; left: 6px; padding: 3px 5px; border-radius: 3px;
  background: rgba(0,0,0,0.45); pointer-events: none; z-index: 7;
  color: #fff; font-size: 10px; font-weight: 600; text-shadow: 0 1px 2px rgba(0,0,0,0.85); }
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
/* contain (not cover) so the WHOLE captured frame is shown — cover cropped the edges, cutting napari's
   scale bar (bottom-right) and timestamp (top-left). Trade-off: letterbox bars when the cell aspect ≠
   the image aspect; acceptable for figures (nothing is clipped). ANIMATION_PLAN E (clean capture +
   Cecelia-drawn scale bar) will let frames go edge-to-edge again without losing the annotations. */
.is-img { flex: 1; width: 100%; object-fit: contain; min-height: 0; }
.is-capture { flex: 1; display: flex; align-items: center; justify-content: center; gap: 6px;
  border: 1px dashed var(--cc-border); background: transparent; color: var(--cc-text-dim); cursor: pointer; font-size: 12px; }
.is-capture:hover { color: var(--cc-text); border-color: #7c3aed; }
/* per-frame actions (recapture / remove): TOP-right. The BOTTOM band is owned by the panel footer
   overlay (Duplicate / Export) — the actions used to collide with the Duplicate button there. The top
   band only holds the LEFT-aligned strip toolbar, so top-right is clear; z-index 7 keeps them above
   the auto-hide toolbar (z-index 6) so hovering never masks them (the earlier "retake masked" bug). */
.is-actions { position: absolute; top: 4px; right: 4px; display: flex; gap: 4px; z-index: 7; }
/* per-frame action buttons — match the app's icon buttons (like .is-gear / .opt-btn) rather than the
   old dark translucent pills: solid surface + border, purple accent on hover. Sit over the image, so a
   solid surface reads cleanly. */
.is-mini { width: 1.5rem; height: 1.5rem; display: inline-flex; align-items: center; justify-content: center;
  border: 1px solid var(--cc-border); border-radius: 4px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  cursor: pointer; font-size: 0.7rem; transition: color 0.1s, border-color 0.1s, background 0.1s; }
.is-mini:hover { color: var(--cc-text); border-color: #7c3aed; background: var(--cc-surface-1); }
.is-mini:disabled { opacity: 0.5; cursor: not-allowed; }
/* while capturing for the PDF: hide the per-frame buttons (and empty-frame capture prompts) so the
   exported strip is just the images */
.is-strip.capturing .is-mini, .is-strip.capturing .is-capture { display: none; }
.seg { display: inline-flex; border: 1px solid var(--cc-border); border-radius: 5px; overflow: hidden; }
.seg button { background: var(--cc-surface-2); color: var(--cc-text-dim); border: none; padding: 4px 8px; cursor: pointer; font-size: 11px; }
.seg button + button { border-left: 1px solid var(--cc-border); }
.seg button.on { background: var(--cc-accent); color: #fff; }
.seg button:disabled { opacity: 0.4; cursor: not-allowed; }
.is-btn { display: inline-flex; align-items: center; gap: 4px; background: var(--cc-surface-2); color: var(--cc-text-dim);
  border: 1px solid var(--cc-border); border-radius: 4px; padding: 3px 8px; cursor: pointer; font-size: 11px; }
.is-btn:hover { color: var(--cc-text); }
</style>
