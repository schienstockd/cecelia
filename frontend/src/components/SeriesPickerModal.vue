<!--
  SeriesPickerModal — asks the user which series of a multi-series microscopy file (LIF today) to
  import, ImageJ-style. The parent (ManageImagesModule) probes /api/import/series/probe BEFORE
  opening this modal, so we only see it when there are 2+ series to choose from — a single-series
  LIF is imported straight through without the "pick 1 of 1" click-through. The modal itself is
  therefore pure UI over an already-resolved ProbeResult.
-->
<script setup lang="ts">
import { ref, computed } from 'vue'
import BaseModal from './BaseModal.vue'
import { seriesLabel, type ProbeResult, type SeriesEntry } from '../utils/seriesPicker'

const props = defineProps<{
  filepath: string
  probe:    ProbeResult
}>()

const emit = defineEmits<{
  (e: 'save', picks: SeriesEntry[]): void
  (e: 'cancel'): void
}>()

const picked = ref<Set<number>>(new Set())

const basename = computed(() => props.filepath.replace(/^.*[\\/]/, ''))

function toggle(idx: number) {
  const s = new Set(picked.value)
  s.has(idx) ? s.delete(idx) : s.add(idx)
  picked.value = s
}

function pickAll() {
  picked.value = new Set(props.probe.series.map(s => s.index))
}

function save() {
  const chosen = props.probe.series.filter(s => picked.value.has(s.index))
  emit('save', chosen)
}
</script>

<template>
  <BaseModal width="720px" @close="emit('cancel')">
    <template #title>
      <i class="pi pi-images" /> Which series? — {{ basename }}
    </template>

    <div class="sp-panel">
      <div class="sp-grid">
        <label v-for="s in probe.series" :key="s.index"
               class="sp-card" :class="{ 'sp-picked': picked.has(s.index) }"
               v-tooltip.top="`Import series ${s.index} (${seriesLabel(s)})`">
          <input type="checkbox" class="sp-cb"
                 :checked="picked.has(s.index)"
                 @change="toggle(s.index)"
                 :aria-label="`Series ${s.index}`" />
          <div class="sp-thumb">
            <img v-if="s.thumbnailPngB64"
                 :src="`data:image/png;base64,${s.thumbnailPngB64}`"
                 :alt="`series ${s.index}`" draggable="false" />
            <div v-else class="sp-thumb-empty"><i class="pi pi-image" /></div>
          </div>
          <div class="sp-meta">
            <span class="sp-name">{{ s.name || `Series ${s.index}` }}</span>
            <span class="sp-dims cc-muted cc-fs-2xs">{{ seriesLabel(s) }}</span>
          </div>
        </label>
      </div>
    </div>

    <template #footer>
      <button class="cc-btn cc-btn-ghost" @click="emit('cancel')">Skip file</button>
      <button class="cc-btn cc-btn-ghost" @click="pickAll"
              v-tooltip.top="'Import every series as a separate image'">All</button>
      <button class="cc-btn cc-btn-primary" :disabled="picked.size === 0" @click="save">
        <i class="pi pi-check" /> Import {{ picked.size || '' }}
      </button>
    </template>
  </BaseModal>
</template>

<style scoped>
.sp-panel { display: flex; flex-direction: column; gap: 0.5rem; }
.sp-grid {
  display: grid; gap: 0.5rem;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
}
.sp-card {
  position: relative; display: flex; flex-direction: column; gap: 0.25rem;
  padding: 0.4rem; border: 1px solid var(--cc-border); border-radius: var(--cc-radius-sm);
  cursor: pointer; background: var(--cc-bg-2);
}
.sp-card:hover { border-color: var(--cc-accent); }
.sp-picked { border-color: var(--cc-accent); background: color-mix(in srgb, var(--cc-accent) 8%, var(--cc-bg-2)); }
.sp-cb { position: absolute; top: 0.4rem; left: 0.4rem; }
.sp-thumb {
  aspect-ratio: 1 / 1; width: 100%; background: #000;
  border-radius: var(--cc-radius-sm); overflow: hidden;
  display: flex; align-items: center; justify-content: center;
}
.sp-thumb img { width: 100%; height: 100%; object-fit: contain; }
.sp-thumb-empty { color: var(--cc-text-dim); font-size: var(--cc-fs-lg); }
.sp-meta { display: flex; flex-direction: column; gap: 0.1rem; padding-left: 1.4rem; }
.sp-name { font-size: var(--cc-fs-xs); color: var(--cc-text); }
.sp-dims { font-variant-numeric: tabular-nums; }
</style>
