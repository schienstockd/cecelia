<script setup lang="ts">
// In-app crop panel (Phase 1: show the coloured, scrubbable MIP). The backend renders one composite
// z-MIP PNG per timepoint (Julia, in-process — /api/crop/frame) using the viewer's saved colours; this
// just scrubs timepoints and displays them. Phase 2 adds the draw-rectangle + z/t ranges + Save.
// See docs/todo/CROP_PANEL_PLAN.md.
import { ref, watch, onUnmounted } from 'vue'

const props = defineProps<{ projectUid: string; imageUid: string; valueName: string }>()

interface Info { nT: number; nZ: number; fullW: number; fullH: number; frameW: number; frameH: number; maxPx: number }
const info     = ref<Info | null>(null)
const t        = ref(0)
const frameUrl = ref<string | null>(null)
const loading  = ref(false)
const err      = ref('')
const urlCache = new Map<number, string>()   // t → object URL (client-side cache; revoked on reload/unmount)

const qs = () =>
  `projectUid=${encodeURIComponent(props.projectUid)}&imageUid=${encodeURIComponent(props.imageUid)}` +
  `&valueName=${encodeURIComponent(props.valueName)}`

function clearCache() {
  urlCache.forEach(u => URL.revokeObjectURL(u)); urlCache.clear(); frameUrl.value = null
}

async function loadInfo() {
  clearCache(); err.value = ''; info.value = null
  if (!props.projectUid || !props.imageUid) return
  try {
    const r = await fetch(`/api/crop/info?${qs()}`)
    const d = await r.json()
    if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`)
    info.value = d as Info
    t.value = Math.floor(((d.nT as number) - 1) / 2)   // start mid-movie (usually the clearest footprint)
    loadFrame()
  } catch (e) { err.value = e instanceof Error ? e.message : String(e) }
}

async function loadFrame() {
  const ti = t.value
  const cached = urlCache.get(ti)
  if (cached) { frameUrl.value = cached; return }
  loading.value = true
  try {
    const r = await fetch(`/api/crop/frame?${qs()}&t=${ti}`)
    if (!r.ok) throw new Error(((await r.json().catch(() => ({}))) as { error?: string }).error ?? `HTTP ${r.status}`)
    const url = URL.createObjectURL(new Blob([await r.arrayBuffer()], { type: 'image/png' }))
    urlCache.set(ti, url)
    if (t.value === ti) frameUrl.value = url            // ignore a stale response if the user scrubbed on
  } catch (e) { err.value = e instanceof Error ? e.message : String(e) }
  finally { loading.value = false }
}

// Debounce scrubbing so a drag renders on pause, not on every intermediate value.
let deb: ReturnType<typeof setTimeout> | null = null
watch(t, () => { if (deb) clearTimeout(deb); deb = setTimeout(loadFrame, 120) })

watch(() => [props.projectUid, props.imageUid, props.valueName], loadInfo, { immediate: true })
onUnmounted(() => { if (deb) clearTimeout(deb); clearCache() })
</script>

<template>
  <div class="crop-panel">
    <div v-if="err" class="crop-err">{{ err }}</div>
    <template v-else-if="info">
      <div class="crop-view">
        <img v-if="frameUrl" :src="frameUrl" class="crop-img" alt="crop projection" />
        <div v-else class="crop-img crop-placeholder"><i class="pi pi-spin pi-spinner" /></div>
        <div v-if="loading && frameUrl" class="crop-loading"><i class="pi pi-spin pi-spinner" /></div>
      </div>
      <div v-if="info.nT > 1" class="crop-scrub">
        <span class="crop-lbl" v-tooltip.top="'Scrub timepoints to pick the clearest footprint'">t</span>
        <input type="range" min="0" :max="info.nT - 1" v-model.number="t" />
        <span class="crop-tval">{{ t + 1 }}/{{ info.nT }}</span>
      </div>
      <span class="crop-hint">Coloured max-projection over Z{{ info.nZ > 1 ? ` (${info.nZ} slices)` : '' }}. Drawing + save coming next.</span>
    </template>
    <div v-else class="crop-hint">Loading crop preview…</div>
  </div>
</template>

<style scoped>
.crop-panel { display: flex; flex-direction: column; gap: 0.4rem; }
.crop-view { position: relative; display: inline-flex; }
.crop-img { max-width: 100%; border: 1px solid var(--cc-border, #2a2a2a); border-radius: 0.3rem; background: #000; image-rendering: auto; }
.crop-placeholder { display: flex; align-items: center; justify-content: center; width: 100%; min-height: 8rem; color: var(--cc-text-muted, #888); }
.crop-loading { position: absolute; top: 0.3rem; right: 0.3rem; color: var(--cc-accent); font-size: 0.8rem; }
.crop-scrub { display: flex; align-items: center; gap: 0.4rem; }
.crop-scrub input[type=range] { flex: 1 1 auto; }
.crop-lbl { color: var(--cc-text-muted, #888); font-size: 0.7rem; width: 0.8rem; }
.crop-tval { font-size: 0.62rem; color: var(--cc-text); width: 3.4rem; text-align: right; font-variant-numeric: tabular-nums; }
.crop-hint { font-size: 0.68rem; color: var(--cc-text-muted, #888); font-style: italic; }
.crop-err { font-size: 0.7rem; color: #f85149; }
</style>
