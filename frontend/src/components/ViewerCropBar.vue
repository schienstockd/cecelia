<!--
  ViewerCropBar — the crop controls on the viewer while a task form is waiting for a crop area
  (`viewerStore.cropPick`, `utils/viewer/cropPick.ts`). The box itself is ViewerWindow's preview box
  in crop mode; this bar carries what a box on one plane cannot: the z and t ranges to keep, the
  Square constraint, and Use / Cancel. Ranges commit on release (`RangeSlider` `change`), so a drag
  writes the cross-window record once, not per pixel.
-->
<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import CcToggle from './CcToggle.vue'
import RangeSlider from './RangeSlider.vue'
import type { CropPick } from '../utils/viewer/cropPick'

const props = defineProps<{
  pick: CropPick
  nZ: number
  nT: number
  /** Why Use is off (no box yet, wrong version, 3D view), or '' when it can be used. */
  blocked: string
}>()
const emit = defineEmits<{
  (e: 'update', patch: Partial<CropPick>): void
  (e: 'use'): void
  (e: 'cancel'): void
}>()

const full = (n: number): [number, number] => [0, Math.max(0, n - 1)]
const z = ref<[number, number]>(props.pick.z ?? full(props.nZ))
const t = ref<[number, number]>(props.pick.t ?? full(props.nT))
watch(() => [props.pick.z, props.nZ] as const, ([v, n]) => { z.value = v ?? full(n) })
watch(() => [props.pick.t, props.nT] as const, ([v, n]) => { t.value = v ?? full(n) })

const square = computed({
  get: () => props.pick.square,
  set: v => emit('update', { square: v }),
})
const rangeLabel = (r: [number, number]) => `${r[0] + 1}–${r[1] + 1}`
const sizeLabel = computed(() => {
  const b = props.pick.box
  return b ? `${Math.abs(b.x1 - b.x0)}×${Math.abs(b.y1 - b.y0)}` : ''
})
</script>

<template>
  <div class="vcb cc-row cc-row-tight" @pointerdown.stop>
    <span class="vcb-title">Crop</span>
    <span v-if="sizeLabel" class="vcb-val">{{ sizeLabel }}</span>
    <CcToggle v-model="square" label="Square" v-tooltip.bottom="'Keep the sides equal'" />
    <template v-if="nZ > 1">
      <span class="vcb-lbl cc-muted">z</span>
      <RangeSlider class="vcb-range" :min="0" :max="nZ - 1"
                   :lo="z[0]" :hi="z[1]" @update:pair="v => z = v"
                   @change="emit('update', { z: z })"
                   v-tooltip.bottom="'Planes to keep'" />
      <span class="vcb-val">{{ rangeLabel(z) }}</span>
    </template>
    <template v-if="nT > 1">
      <span class="vcb-lbl cc-muted">t</span>
      <RangeSlider class="vcb-range" :min="0" :max="nT - 1"
                   :lo="t[0]" :hi="t[1]" @update:pair="v => t = v"
                   @change="emit('update', { t: t })"
                   v-tooltip.bottom="'Frames to keep'" />
      <span class="vcb-val">{{ rangeLabel(t) }}</span>
    </template>
    <button class="cc-btn cc-btn-bare cc-btn-icon" aria-label="Cancel crop" @click="emit('cancel')"
            v-tooltip.bottom="'Cancel'"><i class="pi pi-times" /></button>
    <button class="cc-btn cc-btn-primary cc-btn-sm vcb-use" :disabled="!!blocked" @click="emit('use')"
            v-tooltip.bottom="blocked || 'Send this area to the crop task'">
      <i class="pi pi-check" /> Use
    </button>
  </div>
</template>

<style scoped>
.vcb {
  position: absolute; top: 0.6rem; left: 50%; transform: translateX(-50%);
  flex-wrap: nowrap; white-space: nowrap; max-width: calc(100% - 1rem);
  padding: 0.2rem 0.4rem;
  background: var(--cc-surface-1); border: 1px solid var(--cc-border); border-radius: var(--cc-radius-md);
  font-size: var(--cc-fs-xs); pointer-events: auto; z-index: 5;
}
.vcb-title { font-weight: 600; display: inline-flex; align-items: center; gap: 0.3rem; }
.vcb-range { width: 5.5rem; flex: 0 1 5.5rem; min-width: 3rem; }
.vcb-use { padding: 0.15rem 0.5rem; }
.vcb-val { font-variant-numeric: tabular-nums; color: var(--cc-text-dim); }
</style>
