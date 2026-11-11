<!-- The numbered stage markers on a light-path figure; the one under the pointer in the stage list lights up. -->
<script setup lang="ts">
defineProps<{ marks: Record<number, [number, number]>; hover: number | null }>()
</script>

<template>
  <g v-for="(xy, k) in marks" :key="k" :class="['lp-mark', { on: hover === +k }]">
    <circle :cx="xy[0]" :cy="xy[1]" r="7" />
    <text :x="xy[0]" :y="xy[1] + 3">{{ k }}</text>
  </g>
</template>
