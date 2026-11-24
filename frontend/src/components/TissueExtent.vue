<!--
  TissueExtent — how big the imaged tissue is: X × Y × Z, the XY area and the volume, for the image's
  ACTIVE version (its pixel counts come from `/api/images/geometry` via `frameGeometry`, since a crop or
  drift correction changes them per version). Arithmetic + formatting: `utils/tissueExtent.ts`.

  ONE component for both surfaces, so they can't drift:
    - `compact` — the image table's "Tissue" cell: a short "2.56 × 2.56 mm" that opens a popover
      with the full readout.
    - default  — the full readout; the metadata modal's Dimensions section renders it as-is.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import TeleportPopover from './TeleportPopover.vue'
import type { CciaImage } from '../stores/project'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { frameGeometry, type FrameGeometry } from '../tasks/paramAdvisors'
import { tissueExtent, extentShort, formatLength, formatArea, formatVolume } from '../utils/tissueExtent'

const props = defineProps<{ image: CciaImage; compact?: boolean }>()

const project = useProjectStore()
const projectMeta = useProjectMetaStore()

// undefined = loading, null = nothing to read (not imported, or the store has no geometry)
const geom = ref<FrameGeometry | null | undefined>(undefined)
let req = 0
watch(() => [projectMeta.current?.uid, props.image.uid, props.image.status, props.image.activeValueName,
              props.image.filepath, project.dataVersion[props.image.uid] ?? 0] as const,
  async ([pu, uid, status, vn, fp, rev]) => {
    const mine = ++req
    if (!pu || status !== 'done') { geom.value = null; return }
    geom.value = undefined
    const g = await frameGeometry(pu, uid, null, { version: vn ?? null, rev: `${fp ?? ''}|${rev}` })
    if (mine === req) geom.value = g
  }, { immediate: true })

const extent = computed(() => geom.value ? tissueExtent(geom.value, props.image) : null)
const versionCount = computed(() => Object.keys(props.image.filepaths ?? {}).length)
const n = (v: number) => v.toLocaleString()

const rows = computed(() => {
  const e = extent.value
  if (!e) return []
  const len = (um: number | null, px: number) => um != null ? `${formatLength(um)} · ${n(px)} px` : `${n(px)} px`
  const out: { k: string; v: string }[] = [
    { k: 'Width (X)', v: len(e.um.x, e.px.x) },
    { k: 'Height (Y)', v: len(e.um.y, e.px.y) },
  ]
  if (e.px.z > 1)
    out.push({ k: 'Depth (Z)', v: e.um.z != null ? `${formatLength(e.um.z)} · ${n(e.px.z)} slices` : `${n(e.px.z)} slices` })
  if (e.areaUm2 != null) out.push({ k: 'Area (XY)', v: formatArea(e.areaUm2) })
  if (e.volumeUm3 != null) out.push({ k: 'Volume', v: formatVolume(e.volumeUm3) })
  if (versionCount.value > 1 && props.image.activeValueName)
    out.push({ k: 'Version', v: props.image.activeValueName })
  return out
})
const uncalibrated = computed(() => !!extent.value && extent.value.um.x == null)

const open = ref(false)
const anchor = ref<HTMLElement | null>(null)
</script>

<template>
  <template v-if="compact">
    <button v-if="extent" ref="anchor" class="te-cell cc-btn cc-btn-bare"
      v-tooltip.left="'Tissue size — click for area and volume'" @click.stop="open = !open">
      {{ extentShort(extent) }}
    </button>
    <span v-else-if="geom === undefined" class="te-cell dim">…</span>
    <span v-else class="te-cell dim">—</span>
    <TeleportPopover v-model="open" :anchor="anchor" placement="bottom-end">
      <div class="te-pop">
        <div class="te-hd cc-eyebrow cc-fs-2xs">Tissue size</div>
        <div class="te-grid">
          <template v-for="r in rows" :key="r.k">
            <span class="te-k">{{ r.k }}</span><span class="te-v">{{ r.v }}</span>
          </template>
        </div>
        <div v-if="uncalibrated" class="te-note cc-muted cc-fs-xs">Pixel size unknown — pixels only</div>
      </div>
    </TeleportPopover>
  </template>

  <div v-else class="te-full">
    <div v-if="rows.length" class="te-grid">
      <template v-for="r in rows" :key="r.k">
        <span class="te-k">{{ r.k }}</span><span class="te-v">{{ r.v }}</span>
      </template>
    </div>
    <span v-else class="cc-muted">{{ geom === undefined ? 'Reading…' : '—' }}</span>
    <div v-if="uncalibrated" class="te-note cc-muted cc-fs-xs">Pixel size unknown — pixels only</div>
  </div>
</template>

<style scoped>
.te-cell { font-size: var(--cc-fs-sm); white-space: nowrap; }
button.te-cell { padding: 0; color: inherit; cursor: pointer; }
button.te-cell:hover { color: var(--cc-accent); }
.te-cell.dim { color: var(--cc-text-dim); }
.te-pop { min-width: 13rem; }
.te-hd { margin-bottom: 0.35rem; }
/* same two-column key/value grid as the metadata modal (.md-grid) */
.te-grid {
  display: grid; grid-template-columns: max-content 1fr;
  gap: 0.25rem 1rem; font-size: var(--cc-fs-md); align-items: baseline;
}
.te-k { color: var(--cc-text-dim); white-space: nowrap; }
.te-v { color: var(--cc-text); white-space: nowrap; }
.te-note { margin-top: 0.35rem; }
</style>
