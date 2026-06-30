<!--
  Cluster UMAP panel. Fetches the joint UMAP embedding + cluster codes for the selected image(s)
  (GET /api/plots/umap → binary Float32 [x,y,code,…]) and renders a colour-by-cluster WebGL scatter
  (ScatterGL category mode) with a legend. Clustering is set-scope, so all selected images share one
  UMAP space and one cluster numbering — we fetch per image and concatenate.

  popType:
   • clust      → cell-level clusters, read from the cell labelProps table
   • trackclust → track-level clusters, read from the per-track table (one point per track)

  Used by ClusterCellsModule / ClusterTracksModule (below the image table). The suffix selects which
  clustering run (clusters.{suffix}); it persists across navigation via the canvas shared bag.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import { useProjectMetaStore } from '../../stores/projectMeta'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'
import { useViewState } from '../../composables/useViewState'
import { useLogStore } from '../../stores/log'
import ScatterGL from '../plots/ScatterGL.vue'

const props = defineProps<{ imageUids: string[]; popType: 'clust' | 'trackclust' }>()
const meta = useProjectMetaStore()
const log = useLogStore()
const projectUid = computed(() => meta.current?.uid ?? '')

// suffix + pointSize persist across navigation (canvas shared bag survives remount — see useViewState)
const cps = useCanvasPanelsStore()
const bag = ref(cps.ensure(`clust-umap:${props.popType}`).shared)
const { suffix, pointSize } = useViewState(bag, { suffix: 'default', pointSize: 4 })

// 20-hue categorical palette (distinct, dark-bg friendly); unclustered (code -1) is a muted grey
const PALETTE = [
  '#ef4444', '#f59e0b', '#10b981', '#3b82f6', '#a78bfa', '#ec4899', '#14b8a6', '#eab308',
  '#f97316', '#22d3ee', '#84cc16', '#8b5cf6', '#f43f5e', '#06b6d4', '#a3e635', '#d946ef',
  '#fb7185', '#2dd4bf', '#fbbf24', '#60a5fa',
]
const UNCLUSTERED = '#555a6e'

const points = ref<Float32Array | null>(null)
const categories = ref<Float32Array | null>(null)
const palette = ref<string[]>([])
const legend = ref<{ code: number; colour: string; n: number }[]>([])
const extents = ref({ xMin: 0, xMax: 1, yMin: 0, yMax: 1 })
const loading = ref(false)
const err = ref('')
const total = computed(() => (points.value ? points.value.length / 2 : 0))
const unit = computed(() => (props.popType === 'trackclust' ? 'tracks' : 'cells'))

async function load() {
  err.value = ''
  if (!projectUid.value || !props.imageUids.length) { points.value = null; legend.value = []; return }
  loading.value = true
  try {
    // fetch each image's slice and concatenate (shared UMAP space + cluster numbering across the set)
    const triples: number[] = []
    for (const uid of props.imageUids) {
      const q = new URLSearchParams({
        projectUid: projectUid.value, imageUid: uid, popType: props.popType, suffix: suffix.value,
      })
      const res = await fetch(`/api/plots/umap?${q}`)
      if (!res.ok) continue   // 404 = this image has no UMAP at this suffix; others may
      const f = new Float32Array(await res.arrayBuffer())
      for (let i = 0; i < f.length; i++) triples.push(f[i])
    }
    const n = Math.floor(triples.length / 3)
    if (n === 0) {
      points.value = null; categories.value = null; legend.value = []
      err.value = `No UMAP for the selected image(s) at suffix “${suffix.value}”. Run clustering with “Calculate UMAP” enabled.`
      return
    }
    const pts = new Float32Array(n * 2), codes = new Float32Array(n)
    let xMin = Infinity, xMax = -Infinity, yMin = Infinity, yMax = -Infinity
    for (let i = 0; i < n; i++) {
      const x = triples[3 * i], y = triples[3 * i + 1]
      pts[2 * i] = x; pts[2 * i + 1] = y; codes[i] = triples[3 * i + 2]
      if (x < xMin) xMin = x; if (x > xMax) xMax = x
      if (y < yMin) yMin = y; if (y > yMax) yMax = y
    }
    const px = (xMax - xMin || 1) * 0.04, py = (yMax - yMin || 1) * 0.04
    extents.value = { xMin: xMin - px, xMax: xMax + px, yMin: yMin - py, yMax: yMax + py }

    // distinct codes → palette slot + per-point index (ScatterGL maps index → colour)
    const counts = new Map<number, number>()
    for (let i = 0; i < n; i++) counts.set(codes[i], (counts.get(codes[i]) ?? 0) + 1)
    const distinct = [...counts.keys()].sort((a, b) => a - b)
    const colourFor = new Map<number, string>(), idxOf = new Map<number, number>()
    let pi = 0
    distinct.forEach((c, i) => {
      idxOf.set(c, i)
      colourFor.set(c, c < 0 ? UNCLUSTERED : PALETTE[pi++ % PALETTE.length])
    })
    const cats = new Float32Array(n)
    for (let i = 0; i < n; i++) cats[i] = idxOf.get(codes[i])!
    palette.value = distinct.map(c => colourFor.get(c)!)
    categories.value = cats
    points.value = pts
    legend.value = distinct.map(c => ({ code: c, colour: colourFor.get(c)!, n: counts.get(c)! }))
  } catch (e) {
    err.value = e instanceof Error ? e.message : String(e)
    log.error(`UMAP: ${err.value}`, { source: 'cluster' })
  } finally { loading.value = false }
}

watch([projectUid, () => props.imageUids.join(','), suffix, () => props.popType], load)
onMounted(load)
</script>

<template>
  <div class="umap">
    <div class="umap-ctrl">
      <label>suffix
        <input v-model="suffix" class="suffix-in" spellcheck="false"
               v-tooltip.bottom="'Which clustering run to show (clusters.&lt;suffix&gt;)'" />
      </label>
      <label>point
        <input type="range" min="1" max="10" step="1" v-model.number="pointSize" />
      </label>
      <button class="cc-btn cc-btn-ghost" @click="load" v-tooltip.bottom="'Reload'"><i class="pi pi-refresh" /></button>
      <span class="spacer" />
      <span v-if="total" class="count">{{ total.toLocaleString() }} {{ unit }} · {{ legend.length }} clusters</span>
    </div>

    <div class="umap-body">
      <div class="umap-plot">
        <ScatterGL v-if="points && points.length" :points="points" :extents="extents"
                   color-mode="category" :categories="categories" :palette="palette"
                   :point-size="pointSize" :opacity="0.9" />
        <div v-else class="umap-empty">
          <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-chart-bubble']" />
          <p v-if="loading">Loading…</p>
          <p v-else-if="err">{{ err }}</p>
          <p v-else>Select clustered image(s) to view the UMAP.</p>
        </div>
      </div>

      <div v-if="legend.length" class="umap-legend">
        <div v-for="l in legend" :key="l.code" class="leg-row">
          <span class="leg-dot" :style="{ background: l.colour }" />
          <span class="leg-lbl">{{ l.code < 0 ? 'unclustered' : `cluster ${l.code}` }}</span>
          <span class="leg-n">{{ l.n.toLocaleString() }}</span>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.umap { display: flex; flex-direction: column; gap: 0.5rem; }
.umap-ctrl { display: flex; align-items: center; gap: 0.8rem; font-size: 0.78rem; color: var(--cc-text-dim); }
.umap-ctrl label { display: flex; align-items: center; gap: 0.35rem; }
.suffix-in { width: 7rem; background: var(--cc-bg); color: var(--cc-text); border: 1px solid var(--cc-border);
  border-radius: 0.25rem; padding: 0.1rem 0.4rem; font-size: 0.78rem; }
.umap-ctrl .spacer { flex: 1; }
.umap-ctrl .count { font-variant-numeric: tabular-nums; }
.umap-body { display: flex; gap: 0.75rem; align-items: stretch; }
.umap-plot { position: relative; flex: 1; min-height: 440px; background: #0d0b1a;
  border: 1px solid var(--cc-border); border-radius: 0.4rem; overflow: hidden; }
.umap-empty { position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center;
  justify-content: center; gap: 0.5rem; color: var(--cc-text-dim); text-align: center; padding: 1rem; }
.umap-empty .pi { font-size: 1.5rem; opacity: 0.6; }
.umap-empty p { margin: 0; font-size: 0.82rem; max-width: 26rem; }
.umap-legend { width: 11rem; flex-shrink: 0; overflow-y: auto; max-height: 480px;
  border: 1px solid var(--cc-border); border-radius: 0.4rem; padding: 0.4rem; }
.leg-row { display: flex; align-items: center; gap: 0.4rem; padding: 0.12rem 0.2rem; font-size: 0.75rem; }
.leg-dot { width: 0.7rem; height: 0.7rem; border-radius: 50%; flex-shrink: 0; }
.leg-lbl { flex: 1; color: var(--cc-text); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.leg-n { color: var(--cc-text-dim); font-variant-numeric: tabular-nums; }
</style>
