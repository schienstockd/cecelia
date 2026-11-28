<!--
  The play / pause button of a timecourse: the viewer's playback bar (`VwTimeControls`) and the
  population positions plot. The loop behind it is `composables/usePlaybackTimer.ts`.
-->
<script setup lang="ts">
defineProps<{ playing: boolean }>()
const emit = defineEmits<{ toggle: [] }>()
</script>

<template>
  <button class="cc-btn cc-btn-ghost cc-btn-icon cc-btn-dense" @click="emit('toggle')"
          v-tooltip.top="playing ? 'Pause' : 'Play through the timecourse'"
          :aria-label="playing ? 'Pause' : 'Play'">
    <i class="pi" :class="playing ? 'pi-pause' : 'pi-play'" />
  </button>
</template>
