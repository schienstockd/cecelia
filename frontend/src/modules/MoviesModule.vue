<script setup lang="ts">
// Movies page (/movies) — a simple in-app player for the project's rendered mp4s (single-image,
// animation and batch recordings, all under {proj}/movies/). Native <video> element (no player
// library), streamed from the range-capable backend route so seeking works; playback speed + zoom on
// top. Motivated by there being no good desktop player to rely on. See docs/todo/ANIMATION_PLAN.md.
import { ref, computed, watch, nextTick, onMounted, onBeforeUnmount } from 'vue'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useSettingsStore } from '../stores/settings'
import { useLogStore } from '../stores/log'
import { formatBytes } from '../utils/storage'
import { movieStreamUrl, movieDisplayName, sortMovies, anchoredScroll, type MovieEntry } from '../utils/movies'

const projectMeta = useProjectMetaStore()
const settings = useSettingsStore()
const log = useLogStore()

const projectUid = computed(() => projectMeta.current?.uid ?? '')
const hasProject = computed(() => projectMeta.hasProject)

const movies = ref<MovieEntry[]>([])
const selected = ref<string>('')          // name of the currently-loaded movie
const loading = ref(false)
const videoEl = ref<HTMLVideoElement | null>(null)
const viewportEl = ref<HTMLElement | null>(null)

// Layout-based zoom: give the <video> a concrete pixel box (fit-to-viewport × zoom) rather than a CSS
// transform, so a zoomed movie actually grows the layout and the overflow:auto viewport can scroll/pan
// to any edge. Needs the video's intrinsic size (from loadedmetadata) and the live viewport size (a
// ResizeObserver — the viewport reflows with the sidebar/window).
const natW = ref(0), natH = ref(0)        // video intrinsic pixel size
const vpW = ref(0), vpH = ref(0)          // viewport content size
let ro: ResizeObserver | null = null

const displaySize = computed(() => {
  if (!natW.value || !natH.value || !vpW.value || !vpH.value) return null
  const fit = Math.min(vpW.value / natW.value, vpH.value / natH.value)   // "contain" at zoom 1
  const s = fit * settings.moviesZoom
  return { w: Math.round(natW.value * s), h: Math.round(natH.value * s) }
})
// concrete px box once measured; before metadata, fall back to a plain max-fit so it still shows
const videoStyle = computed(() =>
  displaySize.value
    ? { width: `${displaySize.value.w}px`, height: `${displaySize.value.h}px` }
    : { maxWidth: '100%', maxHeight: '100%' })

// ── Cursor/centre-anchored zoom (mirrors the plot canvas's shift+wheel / shift +/-) ──────────────
// 1× = fit; up to 8×. Wheel anchors to the cursor, keyboard to the viewport centre, the slider too. We
// change the zoom (→ new px box) then, on the next tick, set the scroll so the focal point stays put
// (anchoredScroll) — so it grows about the cursor, not the top-left corner.
const MOVIES_ZOOM_MIN = 1, MOVIES_ZOOM_MAX = 8, ZOOM_STEP = 1.15
const clampZoom = (z: number) => Math.max(MOVIES_ZOOM_MIN, Math.min(MOVIES_ZOOM_MAX, z))
const zoomLabel = computed(() => `${Math.round(settings.moviesZoom * 10) / 10}×`)

function zoomAround(target: number, fx: number, fy: number) {
  const vp = viewportEl.value
  const before = displaySize.value
  const next = clampZoom(target)
  if (!vp || !before) { settings.moviesZoom = next; return }
  const scroll = { left: vp.scrollLeft, top: vp.scrollTop }
  settings.moviesZoom = next
  nextTick(() => {
    const after = displaySize.value
    if (!after) return
    const s = anchoredScroll(before, after, { w: vp.clientWidth, h: vp.clientHeight }, { x: fx, y: fy }, scroll)
    vp.scrollLeft = s.left
    vp.scrollTop = s.top
  })
}
function onWheel(e: WheelEvent) {
  if (!e.shiftKey) return          // plain wheel scrolls the viewport; shift+wheel zooms (like the canvas)
  e.preventDefault()
  const vp = viewportEl.value
  if (!vp) return
  const r = vp.getBoundingClientRect()
  zoomAround(settings.moviesZoom * (e.deltaY < 0 ? ZOOM_STEP : 1 / ZOOM_STEP), e.clientX - r.left, e.clientY - r.top)
}
function onKey(e: KeyboardEvent) {
  if (!e.shiftKey) return
  const t = e.target as HTMLElement | null
  if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)) return
  const vp = viewportEl.value
  if (!vp) return
  const cx = vp.clientWidth / 2, cy = vp.clientHeight / 2
  if (e.key === '+' || e.key === '=') { e.preventDefault(); zoomAround(settings.moviesZoom * ZOOM_STEP, cx, cy) }
  else if (e.key === '-' || e.key === '_') { e.preventDefault(); zoomAround(settings.moviesZoom / ZOOM_STEP, cx, cy) }
  else if (e.key === '0' || e.key === ')') { e.preventDefault(); settings.moviesZoom = 1 }
}
// slider zooms about the viewport centre too, so it's consistent with wheel/keys
function onZoomSlider(v: number) {
  const vp = viewportEl.value
  if (!vp) { settings.moviesZoom = clampZoom(v); return }
  zoomAround(v, vp.clientWidth / 2, vp.clientHeight / 2)
}

watch(viewportEl, (el, prev) => {
  ro?.disconnect()
  prev?.removeEventListener('wheel', onWheel)
  if (el) {
    const measure = () => { vpW.value = el.clientWidth; vpH.value = el.clientHeight }
    ro = new ResizeObserver(measure)
    ro.observe(el)
    measure()
    el.addEventListener('wheel', onWheel, { passive: false })   // passive:false → we can preventDefault
  }
})
onBeforeUnmount(() => {
  ro?.disconnect()
  viewportEl.value?.removeEventListener('wheel', onWheel)
  window.removeEventListener('keydown', onKey)
})

const currentUrl = computed(() =>
  selected.value && projectUid.value ? movieStreamUrl(projectUid.value, selected.value) : '')

async function refresh() {
  if (!projectUid.value) { movies.value = []; return }
  loading.value = true
  try {
    const res = await fetch(`/api/movies?projectUid=${encodeURIComponent(projectUid.value)}`)
    const body = await res.json().catch(() => ({}))
    movies.value = sortMovies((body.movies ?? []) as MovieEntry[])
    // keep the selection if it still exists, else pick the newest
    if (!movies.value.some(m => m.name === selected.value)) {
      selected.value = movies.value[0]?.name ?? ''
    }
  } catch (e) {
    log.error(`Could not list movies: ${e}`, { source: 'movies' })
    movies.value = []
  } finally {
    loading.value = false
  }
}

function select(name: string) {
  selected.value = name
}

// <video> resets playbackRate to 1 whenever a new source loads, so re-apply the persisted speed on
// load (and whenever the user changes it). Also grab the movie's intrinsic size for the zoom box, and
// start playback if autoplay is on (covers selecting a new movie, not just the first load — the native
// `autoplay` attr only reliably fires on first load; the .play() call catches the re-select case).
function onLoadedMeta() {
  const v = videoEl.value
  if (!v) return
  v.playbackRate = settings.moviesPlaybackRate
  natW.value = v.videoWidth
  natH.value = v.videoHeight
  if (settings.moviesAutoplay) v.play().catch(() => { /* autoplay may be blocked until user gesture */ })
}
watch(() => settings.moviesPlaybackRate, () => { if (videoEl.value) videoEl.value.playbackRate = settings.moviesPlaybackRate })

const SPEEDS = [0.25, 0.5, 1, 1.5, 2, 4]

onMounted(() => { refresh(); window.addEventListener('keydown', onKey) })
// re-list when the project changes (opening a different project) — the page can be left mounted
watch(projectUid, refresh)

function movieTime(mtime: number): string {
  return new Date(mtime * 1000).toLocaleString()
}
</script>

<template>
  <div class="mov-page">
    <header class="mov-head">
      <div>
        <h1>Movies</h1>
        <p class="mov-sub">Play the movies rendered for this project (single image, animation and batch).
          Native player with adjustable speed and zoom.</p>
      </div>
      <div class="mov-head-ctl">
        <label class="mov-ctl" v-tooltip.bottom="'Playback speed'">
          <i class="pi pi-forward" />
          <select v-model.number="settings.moviesPlaybackRate" class="mov-select">
            <option v-for="s in SPEEDS" :key="s" :value="s">{{ s }}×</option>
          </select>
        </label>
        <label class="mov-ctl" v-tooltip.bottom="'Zoom the video. Shift + mouse wheel zooms to the cursor; Shift +/− and Shift + 0 (reset) also work. Scroll/pan when zoomed in.'">
          <i class="pi pi-search-plus" />
          <input type="range" :min="MOVIES_ZOOM_MIN" :max="MOVIES_ZOOM_MAX" step="0.25" :value="settings.moviesZoom"
                 @input="onZoomSlider(($event.target as HTMLInputElement).valueAsNumber)" class="mov-range" />
          <span class="mov-num">{{ zoomLabel }}</span>
        </label>
        <label class="mov-ctl" v-tooltip.bottom="'Play a movie automatically when you select it'">
          <input type="checkbox" v-model="settings.moviesAutoplay" /> Autoplay
        </label>
        <label class="mov-ctl" v-tooltip.bottom="'Repeat the movie when it reaches the end'">
          <input type="checkbox" v-model="settings.moviesLoop" /> Loop
        </label>
        <button class="btn-sm" :disabled="loading || !hasProject" @click="refresh"
                v-tooltip.bottom="'Re-scan the project movies folder'">
          <i :class="['pi', loading ? 'pi-spin pi-spinner' : 'pi-refresh']" /> Refresh
        </button>
      </div>
    </header>

    <p v-if="!hasProject" class="mov-empty">Open a project to browse its movies.</p>
    <p v-else-if="!movies.length && !loading" class="mov-empty">No movies yet — record one from the
      Animation, Batch movies or Viewer panels; they appear here.</p>

    <div v-else class="mov-body">
      <!-- Player -->
      <div class="mov-stage">
        <div v-if="currentUrl" ref="viewportEl" class="mov-viewport">
          <video ref="videoEl" class="mov-video" :src="currentUrl" controls
                 :autoplay="settings.moviesAutoplay" :loop="settings.moviesLoop"
                 :style="videoStyle" @loadedmetadata="onLoadedMeta" />
        </div>
        <p v-else class="mov-empty">Select a movie to play.</p>
        <div v-if="selected" class="mov-caption">{{ movieDisplayName(selected) }}</div>
      </div>

      <!-- Playlist -->
      <aside class="mov-list">
        <div class="mov-list-head">{{ movies.length }} movie{{ movies.length === 1 ? '' : 's' }}</div>
        <button v-for="m in movies" :key="m.name" class="mov-item"
                :class="{ active: m.name === selected }" @click="select(m.name)">
          <i class="pi pi-video mov-item-ico" />
          <span class="mov-item-body">
            <span class="mov-item-name">{{ movieDisplayName(m.name) }}</span>
            <span class="mov-item-meta">{{ formatBytes(m.size) }} · {{ movieTime(m.mtime) }}</span>
          </span>
        </button>
      </aside>
    </div>
  </div>
</template>

<style scoped>
.mov-page { padding: 1rem 1.25rem; display: flex; flex-direction: column; height: 100%; min-height: 0; }
.mov-head { display: flex; justify-content: space-between; align-items: flex-start; gap: 1rem; flex-wrap: wrap; }
.mov-head h1 { margin: 0; font-size: 1.15rem; }
.mov-sub { margin: 0.2rem 0 0; font-size: 0.78rem; color: var(--cc-text-dim); max-width: 46rem; }
.mov-head-ctl { display: flex; align-items: center; gap: 0.75rem; flex-wrap: wrap; }
.mov-ctl { display: flex; align-items: center; gap: 0.35rem; font-size: 0.78rem; color: var(--cc-text-dim); }
.mov-select {
  background: var(--cc-surface-2); color: var(--cc-text); border: 1px solid var(--cc-border);
  border-radius: 0.3rem; padding: 0.15rem 0.35rem; font-size: 0.78rem;
}
.mov-range { width: 6rem; }
.mov-num { font-variant-numeric: tabular-nums; min-width: 2.2rem; }

.mov-empty { color: var(--cc-text-dim); font-size: 0.85rem; margin-top: 2rem; }

.mov-body { display: flex; gap: 1rem; flex: 1; min-height: 0; margin-top: 1rem; }

/* Player stage — the video area. The viewport is the scroll container; the video is sized to a
   concrete px box (fit × zoom, see videoStyle). `margin:auto` on a flex child centres it when it fits
   AND lets the viewport scroll to every edge when zoomed (plain justify/align-center would clip the
   top-left and block scrolling there — the well-known flexbox overflow bug). */
.mov-stage { flex: 1; min-width: 0; display: flex; flex-direction: column; gap: 0.5rem; }
.mov-viewport {
  flex: 1; min-height: 0; display: flex; overflow: auto;
  background: #000; border: 1px solid var(--cc-border); border-radius: 0.4rem;
}
.mov-video { margin: auto; display: block; flex-shrink: 0; }
.mov-caption { font-size: 0.8rem; color: var(--cc-text); word-break: break-all; }

/* Playlist */
.mov-list {
  width: 18rem; flex-shrink: 0; overflow-y: auto; border: 1px solid var(--cc-border);
  border-radius: 0.4rem; background: var(--cc-surface-1); padding: 0.35rem;
}
.mov-list-head {
  font-size: 0.65rem; text-transform: uppercase; letter-spacing: 0.06em; color: var(--cc-text-dim);
  padding: 0.35rem 0.5rem 0.5rem;
}
.mov-item {
  display: flex; align-items: center; gap: 0.5rem; width: 100%; text-align: left;
  background: none; border: none; cursor: pointer; color: var(--cc-text-dim);
  padding: 0.45rem 0.5rem; border-radius: 0.35rem; transition: background 0.12s, color 0.12s;
}
.mov-item:hover { background: var(--cc-surface-2); color: var(--cc-text); }
.mov-item.active { background: var(--cc-surface-2); color: var(--cc-text); }
.mov-item-ico { font-size: 0.85rem; flex-shrink: 0; color: var(--cc-accent); }
.mov-item-body { display: flex; flex-direction: column; min-width: 0; }
.mov-item-name { font-size: 0.8rem; font-weight: 500; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.mov-item-meta { font-size: 0.68rem; color: var(--cc-text-dim); }
</style>
