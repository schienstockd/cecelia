<!--
  "Check images": where a following rectangle / polygon lands on each image of the set, before it is
  copied there. One server picture per image (`GET /api/gating/rule-preview`): the source gate — anchor
  and all, as a copy carries it — resolved on that image's population at the same parent path and drawn
  alone on its plot, dashed where it found no cloud, a cross on the peak where it did — evaluated on the
  map the copy would write (the source's parent gates, its channels matched by name), and an image the
  copy would skip says why. Nothing is written.
  Images are fetched a few at a time so the grid fills as they arrive. docs/POPULATION.md → Rule gates.
-->
<script setup lang="ts">
import { ref, computed, onMounted } from 'vue'
import BaseModal from '../../components/BaseModal.vue'
import PlotSpinner from '../../components/plots/PlotSpinner.vue'
import { useProjectStore } from '../../stores/project'
import { imagesWithValueName } from '../../lib/valueNames'
import { getJson, errorMessage } from '../../utils/api'
import { previewStatus, previewSummary, type RulePreviewImage } from '../../plots/followRule'

const props = defineProps<{
  setUid: string
  sourceUid: string
  projectUid: string
  valueName: string
  path: string             // the following gate's population
  plotQuery: string        // the plot's own query (axes + transforms), so every picture matches it
}>()
const emit = defineEmits<{ (e: 'close'): void }>()

const project = useProjectStore()
const setImages = computed(() => project.sets.find(s => s.uid === props.setUid)?.images ?? [])
const nameOf = (uid: string) => setImages.value.find(i => i.uid === uid)?.name ?? uid
const popName = computed(() => props.path.split('/').filter(Boolean).pop() ?? props.path)

const order = ref<string[]>([])                       // source first, then the set's other images
const got = ref<Record<string, RulePreviewImage>>({})
const skipped = ref<string[]>([])                     // no such segmentation
const done = computed(() => order.value.filter(u => got.value[u]))
const summary = computed(() => previewSummary(done.value.map(u => got.value[u])))

const BATCH = 3
onMounted(async () => {
  const others = setImages.value.map(i => i.uid).filter(u => u !== props.sourceUid)
  const { available } = others.length ? await imagesWithValueName(props.projectUid, props.valueName, others)
                                      : { available: [] as string[] }
  const have = new Set(available)
  skipped.value = others.filter(u => !have.has(u))
  order.value = [props.sourceUid, ...others.filter(u => have.has(u))]
  for (let i = 0; i < order.value.length; i += BATCH) {
    const targets = order.value.slice(i, i + BATCH)
    try {
      // quiet: a failure is shown on its own cards
      const d = await getJson<{ images?: RulePreviewImage[] }>(
        `/api/gating/rule-preview?${props.plotQuery}&path=${encodeURIComponent(props.path)}` +
        `&targets=${targets.map(encodeURIComponent).join(',')}`, { quiet: true })
      for (const im of d.images ?? []) got.value = { ...got.value, [im.uid]: im }
    } catch (e) {
      const error = errorMessage(e)
      for (const uid of targets) got.value = { ...got.value, [uid]: { uid, error } }
    }
  }
})
</script>

<template>
  <BaseModal width="760px" @close="emit('close')">
    <template #title><i class="pi pi-images" /> Where {{ popName }} lands</template>
    <div class="rp-body">
      <span class="cc-muted">{{ done.length ? summary : 'Drawing…' }}<template v-if="done.length < order.length"> · {{ order.length - done.length }} to go</template></span>
      <div class="rp-grid">
        <figure v-for="uid in order" :key="uid" class="rp-card">
          <figcaption class="rp-name" v-tooltip.top="nameOf(uid)">{{ nameOf(uid) }}<span v-if="uid === sourceUid" class="cc-muted"> · drawn here</span></figcaption>
          <img v-if="got[uid]?.png" :src="`data:image/png;base64,${got[uid].png}`" :alt="`${popName} on ${nameOf(uid)}`" />
          <div v-else class="rp-wait"><PlotSpinner v-if="!got[uid]" /></div>
          <span v-if="got[uid]" :class="previewStatus(got[uid]).ok ? 'cc-muted' : 'cc-muted-warn'">{{ previewStatus(got[uid]).text }}</span>
        </figure>
      </div>
      <span v-if="skipped.length" class="cc-muted">No “{{ valueName }}” segmentation: {{ skipped.map(nameOf).join(', ') }}</span>
    </div>
    <template #footer>
      <button class="cc-btn cc-btn-primary" @click="emit('close')">Close</button>
    </template>
  </BaseModal>
</template>

<style scoped>
.rp-body { display: flex; flex-direction: column; gap: 8px; }
.rp-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(220px, 1fr)); gap: 10px; }
.rp-card { margin: 0; display: flex; flex-direction: column; gap: 3px; min-width: 0; }
.rp-name { font-size: var(--cc-fs-xs); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.rp-card img { width: 100%; height: auto; display: block; border-radius: var(--cc-radius-xs); }
/* the server picture's shape (plot + axis padding, gate_plot_render.jl), so the grid doesn't jump;
   relative for PlotSpinner, which fills its container */
.rp-wait { position: relative; aspect-ratio: 406 / 368; background: var(--cc-surface-2); border-radius: var(--cc-radius-xs); }
</style>
