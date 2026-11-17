<!--
  The viewer window's overlay switches — scale bar, timestamp, cursor readout — as one segmented
  multi-select in "How it looks"'s top row beside what a click does (shown folded too), the same control
  as 2D / 3D beside it, so they are found without opening a section. Their text sizes, and the grid /
  landscape captures use, stay in the Annotations section (`VwAnnotationsSection`). Settings live in the
  settings store.
-->
<script setup lang="ts">
import { computed } from 'vue'
import ChipSelect from '../ChipSelect.vue'
import { useSettingsStore } from '../../stores/settings'

/** `plane`: 2D view — the readout reads a plane, so in 3D its tip says so rather than it doing nothing.
 *  `timed`: more than one frame — a static image has no time to stamp, so no timestamp switch. */
const props = defineProps<{ plane: boolean; timed: boolean }>()
const settings = useSettingsStore()

const options = computed(() => [
  { value: 'scaleBar', label: '', icon: 'pi pi-scale-bar', tip: 'Scale bar' },
  ...(props.timed
    ? [{ value: 'timestamp', label: '', icon: 'pi pi-timestamp', tip: 'Timestamp — elapsed time, or the frame if uncalibrated' }]
    : []),
  { value: 'readout', label: '', icon: 'pi pi-readout',
    tip: props.plane ? 'Readout — position, intensities and cell under the pointer' : 'Readout works in 2D' },
])
const on = computed<string[]>({
  get: () => [
    ...(settings.viewerScaleBar ? ['scaleBar'] : []),
    ...(settings.viewerTimestamp ? ['timestamp'] : []),
    ...(settings.viewerCursorReadout ? ['readout'] : []),
  ],
  set: v => {
    settings.viewerScaleBar = v.includes('scaleBar')
    if (props.timed) settings.viewerTimestamp = v.includes('timestamp')   // hidden: keep the saved choice
    settings.viewerCursorReadout = v.includes('readout')
  },
})
</script>

<template>
  <ChipSelect v-model="on" :options="options" multiple variant="segmented" aria-label="Overlays on the image" />
</template>
