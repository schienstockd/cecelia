<!--
  Image / filmstrip slot for the Analysis board (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase D). One
  slot holding N captioned images (a single image = a 1-cell strip) — for pipeline montages
  (raw → denoised → segmented → tracked). Each cell's image is a napari CANVAS screenshot
  (POST /api/napari/screenshot → PNG bytes → stored as a data URL in the slot state, so it persists and
  embeds straight into the PDF). Orientation H/V; separators STRAIGHT (gap + rule) or ANGLED (clip-path
  parallelograms — cheap because the slot stays rectangular and holds image-only content, decision 10).
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted, onUnmounted, useTemplateRef, nextTick } from 'vue'
import { elementToImageURL } from '../../plots/export'

interface Cell { src?: string; caption?: string }
const props = defineProps<{
  projectUid: string; imageUids: string[]; setUid: string | null
  state: { cells?: Cell[]; orientation?: 'h' | 'v'; separator?: 'straight' | 'angled'; sepAngle?: number; sepThick?: number; capSize?: number }
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
// caption text size (px) — driven into the caption overlay via a CSS var
const capSize = computed({ get: () => props.state.capSize ?? 13, set: v => (props.state.capSize = v) })
// angled separators are horizontal-only (the clip leans across the row) — snap back to straight if the
// strip is switched to vertical.
watch(orientation, o => { if (o === 'v' && separator.value === 'angled') separator.value = 'straight' })

// separator options (angle / width) live in a ⚙ popover (like the heatmap panel's options) so they
// never widen the toolbar; close on an outside click.
const optsOpen = ref(false)
const optsRef = useTemplateRef<HTMLElement>('optsRef')
function onDocClick(e: MouseEvent) { if (optsOpen.value && optsRef.value && !optsRef.value.contains(e.target as Node)) optsOpen.value = false }
onMounted(() => document.addEventListener('mousedown', onDocClick))
onUnmounted(() => document.removeEventListener('mousedown', onDocClick))

const capturing = ref(-1)
const err = ref('')

function toDataUrl(buf: ArrayBuffer): string {
  const bytes = new Uint8Array(buf)
  let bin = ''
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i])
  return 'data:image/png;base64,' + btoa(bin)
}

// capture the current napari canvas into cell i
async function capture(i: number) {
  capturing.value = i
  err.value = ''
  try {
    const res = await fetch('/api/napari/screenshot', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: props.projectUid }),
    })
    if (!res.ok) { err.value = ((await res.json().catch(() => ({}))) as { error?: string }).error ?? 'Screenshot failed'; return }
    cells.value[i].src = toDataUrl(await res.arrayBuffer())
  } catch (e) { err.value = e instanceof Error ? e.message : String(e) }
  finally { capturing.value = -1 }
}
function addCell() { cells.value.push({}) }
function removeCell(i: number) { if (cells.value.length > 1) cells.value.splice(i, 1) }
function setCaption(i: number, v: string) { cells.value[i].caption = v }

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
  '--cap-size': `${capSize.value}px`,
  ...((separator.value === 'angled' && orientation.value === 'h')
    ? { '--sk': `${skew.value}px`, '--sep-thick': `${thick.value}px` } : {}),
}))

// PDF export: just the STRIP (frames + captions), no toolbar/per-frame buttons. The `capturing` class
// hides the in-frame controls; the strip is HTML + <img> (data URLs), so serialise via elementToImageURL.
const stripRef = useTemplateRef<HTMLElement>('stripRef')
const capturingStrip = ref(false)
async function exportImage(): Promise<string | null> {
  capturingStrip.value = true
  await nextTick()
  try { return await elementToImageURL(stripRef.value, 'png', '#ffffff') }
  finally { capturingStrip.value = false }
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
      <div ref="optsRef" class="is-opts">
        <button class="is-gear" :class="{ on: optsOpen }" @click="optsOpen = !optsOpen"
                v-tooltip.bottom="'Caption size & separator'"><i class="pi pi-cog" /></button>
        <div v-if="optsOpen" class="is-pop">
          <label class="is-slider">caption
            <input type="range" min="8" max="28" :value="capSize" @input="capSize = +($event.target as HTMLInputElement).value" />
            <span class="is-val">{{ capSize }}</span></label>
          <template v-if="separator === 'angled' && orientation === 'h'">
            <label class="is-slider">angle
              <input type="range" min="0" max="80" :value="skew" @input="skew = +($event.target as HTMLInputElement).value" />
              <span class="is-val">{{ skew }}</span></label>
            <label class="is-slider">width
              <input type="range" min="1" max="12" :value="thick" @input="thick = +($event.target as HTMLInputElement).value" />
              <span class="is-val">{{ thick }}</span></label>
          </template>
        </div>
      </div>
      <button class="is-btn" @click="addCell" v-tooltip.bottom="'Add a frame'"><i class="pi pi-plus" /> frame</button>
      <span v-if="err" class="is-err">{{ err }}</span>
    </div>

    <div ref="stripRef" class="is-strip" :class="[orientation === 'h' ? 'row' : 'col', separator, { capturing: capturingStrip }]" :style="stripStyle">
      <div v-for="(c, i) in cells" :key="i" class="is-cell" :style="{ clipPath: clipFor(i) }">
        <img v-if="c.src" :src="c.src" class="is-img" alt="napari screenshot" />
        <button v-else class="is-capture" @click="capture(i)" :disabled="capturing === i"
                v-tooltip.bottom="'Capture the current napari view'">
          <i class="pi pi-camera" /> {{ capturing === i ? 'capturing…' : 'napari view' }}
        </button>
        <!-- caption: white centred text overlaid on the image (editable inline; plain text while
             capturing since an <input>'s live value isn't cloned into the export) -->
        <div class="is-cap">
          <span v-if="capturingStrip" class="is-cap-text">{{ c.caption ?? '' }}</span>
          <input v-else class="is-cap-input" :value="c.caption ?? ''" placeholder="caption…"
                 @input="setCaption(i, ($event.target as HTMLInputElement).value)" />
        </div>
        <!-- per-frame actions (hidden while capturing) -->
        <div v-if="!capturingStrip" class="is-actions">
          <button v-if="c.src" class="is-mini" @click="capture(i)" v-tooltip.top="'Recapture'"><i class="pi pi-camera" /></button>
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
.is-pop { position: absolute; top: calc(100% + 4px); left: 0; z-index: 30; display: flex; flex-direction: column; gap: 6px;
  padding: 8px 10px; background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: 6px;
  box-shadow: 0 6px 24px rgba(0,0,0,0.4); }
.is-val { min-width: 1.2rem; text-align: right; font-weight: 700; color: var(--cc-text); }
.is-err { color: #fca5a5; font-size: 11px; }
.is-slider { display: inline-flex; align-items: center; gap: 4px; color: var(--cc-text-dim); font-size: 11px; }
.is-slider input[type="range"] { width: 4.5rem; }
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
/* cover (not contain) so the frame fills edge-to-edge — no black letterbox "border" around the image */
.is-img { flex: 1; width: 100%; object-fit: cover; min-height: 0; }
.is-capture { flex: 1; display: flex; align-items: center; justify-content: center; gap: 6px;
  border: 1px dashed var(--cc-border); background: transparent; color: var(--cc-text-dim); cursor: pointer; font-size: 12px; }
.is-capture:hover { color: var(--cc-text); border-color: #7c3aed; }
/* caption overlay: white, centred, near the bottom of the image (legible via text-shadow). z-index
   above the auto-hide toolbar (.cc-panel-controls, z-index 6) so it's never masked when hovering. */
.is-cap { position: absolute; left: 0; right: 0; bottom: 8px; display: flex; justify-content: center;
  padding: 0 3.4rem 0 10px; pointer-events: none; z-index: 7; }
.is-cap-input, .is-cap-text { pointer-events: auto; max-width: 100%; text-align: center; color: #fff;
  font-size: var(--cap-size, 13px); font-weight: 600; text-shadow: 0 1px 4px rgba(0,0,0,0.9); }
.is-cap-input { width: 100%; background: transparent; border: none; border-radius: 3px; padding: 1px 4px; }
.is-cap-input:focus { outline: none; background: rgba(0,0,0,0.35); }
.is-cap-input::placeholder { color: rgba(255,255,255,0.55); font-weight: 400; }
.is-cap-text { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
/* per-frame actions (recapture / remove): BOTTOM-right, above the auto-hide toolbar (z-index 6) which
   overlays the TOP of the frame on hover and used to mask them (the "retake button is masked" bug) */
.is-actions { position: absolute; bottom: 4px; right: 4px; display: flex; gap: 4px; z-index: 7; }
.is-mini { width: 1.4rem; height: 1.4rem; display: inline-flex; align-items: center; justify-content: center;
  border: 1px solid var(--cc-border); border-radius: 3px; background: rgba(0,0,0,0.45); color: #fff;
  cursor: pointer; font-size: 0.6rem; }
.is-mini:hover { background: rgba(0,0,0,0.7); }
/* while capturing for the PDF: hide the per-frame buttons (and empty-frame capture prompts) so the
   exported strip is just the images + captions */
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
