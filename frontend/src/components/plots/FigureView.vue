<!--
  A rendered figure on the Analysis board (docs/todo/FIGURES_PLAN.md): one of the project's figures
  (`{project}/figures/`, listed by GET /api/movies), picked by file name and shown as made. The board
  never re-renders it — the Movies & figures page and its editor do that — so the slot holds only the
  name. PDF export hands over the figure file itself, at its full resolution, rather than a DOM capture
  of the shrunk preview.
-->
<script setup lang="ts">
import { ref, computed, watch, onMounted } from 'vue'
import { usePushStore } from '../../stores/push'
import { movieStreamUrl, movieDisplayName, isFigure, sortMovies, type MovieEntry } from '../../utils/movies'

const props = defineProps<{ projectUid: string; state: { name?: string } }>()

const figures = ref<MovieEntry[]>([])
const loaded = ref(false)

async function refresh() {
  if (!props.projectUid) { figures.value = []; return }
  try {
    const res = await fetch(`/api/movies?projectUid=${encodeURIComponent(props.projectUid)}`)
    const body = await res.json().catch(() => ({}))
    figures.value = sortMovies(((body.movies ?? []) as MovieEntry[]).filter(isFigure))
    // a new slot shows the newest figure rather than an empty picker
    if (!props.state.name && figures.value.length) props.state.name = figures.value[0].name
  } finally {
    loaded.value = true
  }
}
onMounted(refresh)
watch(() => props.projectUid, refresh)
// a figure is saved or re-made without a task — the server broadcasts `movies:changed`
const push = usePushStore()
watch(() => push.tick, () => {
  const ev = push.lastEvent
  if (ev?.kind === 'movies:changed' && (!ev.projectUid || ev.projectUid === props.projectUid)) refresh()
})

const current = computed(() => figures.value.find(f => f.name === props.state.name))
// a re-made figure keeps its name — its mtime busts the browser's copy of the old bytes
const src = computed(() => current.value
  ? movieStreamUrl(props.projectUid, current.value.name, current.value.mtime) : '')

// PDF export: the figure file as a data URL — the full-resolution render, not the panel's preview
async function exportImage(): Promise<string | null> {
  if (!src.value) return null
  const res = await fetch(src.value)
  if (!res.ok) return null
  const blob = await res.blob()
  return await new Promise(resolve => {
    const r = new FileReader()
    r.onload = () => resolve(typeof r.result === 'string' ? r.result : null)
    r.onerror = () => resolve(null)
    r.readAsDataURL(blob)
  })
}
defineExpose({ exportImage })
</script>

<template>
  <div class="fv">
    <div v-if="figures.length > 1" class="fv-ctrl cc-panel-controls">
      <select class="select-input" :value="state.name" v-tooltip.top="'Which figure'"
              @change="state.name = ($event.target as HTMLSelectElement).value">
        <option v-for="f in figures" :key="f.name" :value="f.name">{{ movieDisplayName(f) }}</option>
      </select>
    </div>
    <img v-if="src" class="fv-img" :src="src" :alt="current ? movieDisplayName(current) : ''" />
    <div v-else-if="loaded" class="fv-empty cc-muted cc-fs-sm">
      {{ figures.length ? 'Pick a figure' : state.name ? `Figure ${state.name} is gone` : 'No figures yet — make one from the viewer' }}
    </div>
    <span v-if="current?.figureStale" class="fv-stale cc-muted-warn"
          v-tooltip.top="current.staleReason ?? 'Out of date'">out of date</span>
  </div>
</template>

<style scoped>
.fv { position: relative; display: flex; align-items: center; justify-content: center; height: 100%; min-height: 0; background: var(--cc-paper); }
.fv-img { max-width: 100%; max-height: 100%; object-fit: contain; }
.fv-ctrl { position: absolute; top: 0; left: 0; padding: 4px 6px; }
.fv-empty { padding: 1rem; text-align: center; }
.fv-stale { position: absolute; bottom: 4px; right: 6px; }
</style>
